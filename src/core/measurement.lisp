

(in-package :cl-user)
(defpackage 
  :measurement.core.indoor-positioning
  (:use :cl 
        :cl-annot
        :errors.core.indoor-positioning
        :types.core.indoor-positioning
        )
  (:import-from 
    :cl-ppcre
    :split))
(in-package :measurement.core.indoor-positioning)


(enable-annot-syntax)


@export
(defun perform-measurement (path times &key (wait 8.5) (ercnt 2))
  "測定をtimes回行ってpathディレクトリに結果のcsvファイルを
   書き込む"
  (loop 
    named exit
    with os = (check-os)
    with noerrorcnt = 0
    with errorcnt   = 0
    for i from 1 upto times
    for name = (merge-pathnames path (format nil "SCAN~A_at_~A.csv" i (get-universal-time)))
    do 
    (with-open-file (out name :direction :output)
      (handler-case 
        (progn 
          (mapc 
            (lambda (x) (format out "~A~%" x)) 
            (measure os))
          (incf noerrorcnt))
        (error (c)
          (format *standard-output* "something error occurs")
          (print c)
          (incf errorcnt)
          (when (and (zerop noerrorcnt) (> errorcnt ercnt))
            (format *standard-output* "measurement aborted")
            (return-from exit nil)))))
    (sleep wait)))


(defun check-os ()
  "software-typeとかからOSを当てる"
  (let ((type (software-type)))
    (cond 
      ((or (search "linux" type) 
           (search "Linux" type))
       (make-instance 'linux))
      (t (make-instance 'os)))))


@export
(defmethod measure ((os os))
  " アクセスポイントのスキャンを1回行って結果をmeasured-accesspoint構造体のリストで返す"
  (error 
    (make-condition 
      'measurement-not-implemented)))


(defmethod measure ((os linux))
  (multiple-value-bind 
    (res-str err-str code)
    (trivial-shell:shell-command "sudo iwlist wlan0 scan")
    ;; wlan0 でない場合を考慮してない...
    (convert os res-str)))







(defmethod convert ((os os) raw)
  "シェルコマンドの結果をパースしてmeasured-accesspoint構造体のリストに変換する"
  (error 
    (make-condition 
      'measurement-not-implemented)))


(defmethod convert ((os linux) raw)
  (labels 
    ((cutout (target)
       (with-input-from-string (in target)
         (let (result)
           (loop 
             with cnt = 0
             with acc = nil 
             for line = (read-line in nil nil)
             while line do 
             (let* ((fix (string-trim '(#\Space #\Tab #\Linefeed #\Newline) line))
                    (index (search "Cell" fix)))
               (when (and index (zerop index))
                 (incf cnt 6))
               (when (> cnt 0)
                 (decf cnt)
                 (push fix acc)
                 (when (zerop cnt)
                   (push (reverse acc) result)
                   (setf acc nil)))))
           result)))
     (->object (rough)
       (destructuring-bind 
         (bssidf channelf freqf q_rssif _ essidf) rough
         (destructuring-bind (qj rssij) 
           (split "Signal level=" q_rssif)
           (make-measured-accesspoint 
             :bssid (car (last (split "Address: " bssidf)))
             :essid (car (last (split "ESSID:" essidf)))
             :freq  (read-from-string 
                      (car (split "GHz" (car (last (split "Frequency:" freqf))))))
             :channel (parse-integer (car (last (split ":" channelf))))
             :rssi (parse-integer (car (split " dBm" rssij)))
             :quality (float (read-from-string (car (last (split "Quality=" qj))))))))))
    (mapcar #'->object (cutout raw))))

