(in-package :cl-user)
(defpackage :indoor-positioning
  (:use :cl 
        :types.core.indoor-positioning
        :cl-annot
        )
  (:import-from 
    :cl-fad
    :directory-exists-p
    :file-exists-p
    )
  (:import-from 
    :measurement.core.indoor-positioning
    :perform-measurement
    )
  (:import-from 
    :util.core.indoor-positioning
    :restore-fingerprint
    :read-csvdir
    :save-fingerprint
    )
  (:import-from 
    :plotter.core.indoor-positioning
    :plot
    )
  (:import-from 
    :positioningmethod.core.indoor-positioning
    :positioning
    )
  )

(in-package :indoor-positioning)

(enable-annot-syntax)


(defvar *proc-table* 
  (make-hash-table :test 'eql))

(defvar *current-fingerprint*
  nil)

(defvar *default-graph-output-dir* nil)


(defun help ()
  "ヘルプメッセージの表示"
  (let ((mes 
          '("-------------------------------------------------------------------------------------------------------------"
            "0 Perform measurement of Wi-Fi fingerprint"
            "1 Draw a graph"
            "2 Load fingerprint"
            "3 Save fingerprint (dump)"
            "4 Perform indoor positioning "
            "5 Show this help"
            ""
            "* EXECUTE FOLLOWING COMMAND TO RUN THIS REPL"
            "* $ ros run -s qlot -e '(qlot:quickload :indoor-positioning)' -e '(indoor-positioning:main)'"
            ""
            "* EXECUTE FOLLOWING COMMAND TO RUN THE ESTIMATION SERVER"
            "* $ ros run -s qlot -e '(qlot:quickload :indoor-positioning)' -e '(server.server.indoor-positioning:start)'"
            "-------------------------------------------------------------------------------------------------------------")))
    (format *standard-output* "~%~{~A~%~}" mes))
  (force-output *standard-output*))



(defun prompt (text)
  "プロンプトの表示"
  (format *standard-output* "~%~A" text)
  (force-output *standard-output*))

(defun prompt-read (text)
  "何か表示して読み込む"
  (prompt text)
  (read-line *standard-input* nil nil))

(defun anything-input (text converter)
  "適当なオブジェクトが入力されるまで受ける"
  (handler-case 
    (loop 
      named exit 
      for raw = (prompt-read text)
      for obj = (funcall converter raw)
      do 
      (when (or (null raw) obj)
        (return-from exit obj)))
    ;; depend on SBCL
    (SB-SYS:INTERACTIVE-INTERRUPT (c) nil)))


(defun number-input (text)
  "標準入力から数を読み込む"
  (anything-input 
    text 
    (lambda (raw)
      (when raw 
        (parse-integer raw :junk-allowed t)))))


(defun directory-input (text)
  "標準入力から存在するディレクトリ名が入力されるまで繰り返し読む"
  (anything-input 
    text 
    (lambda (raw)
      (when raw
        (directory-exists-p raw)))))


(defun file-input (text)
  "標準入力から存在するファイル名が入力されるまで繰り返し読む"
  (anything-input 
    text 
    (lambda (raw)
      (when (and raw (file-exists-p raw) (not (directory-exists-p raw)))
        (file-exists-p raw))))) 

(defun file-or-dir-input (text)
  "標準入力から存在するファイル又はディレクトリが入力されるまで繰り返し読む"
  (anything-input 
    text 
    (lambda (raw)
      (when (and raw (file-exists-p raw))
        (file-exists-p raw)))))

(defun keyword-input (text)
  "キーワドシンボルを読み込む"
  (anything-input 
    text 
    (lambda (raw)
      (when raw 
        (intern (string-upcase raw) :keyword)))))

(defun bssid-input (text)
  "BSSIDを読み込む"
  (anything-input 
    text 
    (lambda (raw)
      (when raw 
        (if (string= raw "t")
          t
          raw)))))


(defun input-wrapper (text func tag)
  (let ((result (funcall func text)))
    (if result 
      result
      (throw tag nil))))


(defun proc (n)
  (when (< n (hash-table-count *proc-table*))
    (funcall (gethash n *proc-table*))))


@export
(defun main ()
  (help)
  (loop 
    for select = (number-input ">>> ")
    while select
    do
    (proc select)))





(setf (gethash 0 *proc-table*)
      (lambda ()
        ;; 測定の実行
        (catch 'exit 
          (let ((dst (input-wrapper "save directory name?(no input for current): " #'directory-input 'exit))
                (times (input-wrapper "how many times do you measure?: " #'number-input 'exit))
                (wait  (input-wrapper "measurement interval?: " #'number-input 'exit)))
            (format *standard-output* "performing...~%")
            (force-output *standard-output*)
            (perform-measurement dst times :wait wait)
            (format *standard-output* "finished.~%")
            (force-output *standard-output*)))))


(setf (gethash 1 *proc-table*)
      (lambda ()
        (if *current-fingerprint* 
          (catch 'exit 
            (let ((dst 
                    (if *default-graph-output-dir*
                      *default-graph-output-dir*
                      (setf *default-graph-output-dir* 
                            (input-wrapper "save directory name?(no input for current): " #'directory-input 'exit))))
                  (place 
                    (progn 
                      (format *standard-output* "~%VALID PLACE LIST~%~{ ~A~%~}"
                              (loop for key being the hash-keys of *current-fingerprint*
                                    collect key))
                      (force-output *standard-output*)
                      (input-wrapper "place name?: " #'keyword-input 'exit)))
                  (bssid (input-wrapper "bssid?(t for all): " #'bssid-input 'exit)))
              (plot dst *current-fingerprint* place bssid)))
          (format *standard-output* "fingerprinting must be loaded in advance.~%"))))


(setf (gethash 2 *proc-table*)
      (lambda ()
        (catch 'exit 
            (let ((src (input-wrapper "file or directory name?(no input for current): " #'file-or-dir-input 'exit)))
              (format *standard-output* "loading...~%")
              (force-output *standard-output*)
              (handler-case 
                (progn
                  (setf *current-fingerprint* (restore-fingerprint src))
                  (format *standard-output* "loaded.~%"))
                (t (c)
                   (format *standard-output* "can't read from ~A~%" src)))
              (force-output *standard-output*))))) 


(setf (gethash 3 *proc-table*) 
      (lambda ()
        (if *current-fingerprint*
          (catch 'exit 
               (let ((dst (input-wrapper "save file name?: "
                                  (lambda (text)
                                    (anything-input 
                                      text (lambda (x) x)) )
                                  'exit)))
          (format *standard-output* "saving...~%")
          (force-output *standard-output*)
          (save-fingerprint dst *current-fingerprint*)
          (format *standard-output* "saved.~%")))
          (format *standard-output* "fingerprint must be loaded in advance."))))


(setf (gethash 4 *proc-table*)
      (lambda ()
        (if *current-fingerprint*  
          (catch 'exit
           (let* ((src (input-wrapper "CSVs directory name?(no input for current): " #'directory-input 'exit))
                  (result (positioning 
                           (make-instance 'normal-dist-method)
                           *current-fingerprint*
                           (read-csvdir src))))
             (loop 
               for (place . confidence) in result
               for i from 1 
               do (format *standard-output* "~A ~A  ~A~%" i place confidence))))
          (progn 
            (format *standard-output* "fingerprint must be loaded in advance.~%")
            (force-output *standard-output*)))))


(setf (gethash 5 *proc-table*) #'help)
 

