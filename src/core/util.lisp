

(in-package :cl-user)
(defpackage :util.core.indoor-positioning
  (:use :cl 
        :cl-annot
        :cl-annot.class
        :types.core.indoor-positioning
        ))

(in-package :util.core.indoor-positioning)

(enable-annot-syntax)




@export 
(defun save-fingerprint (path fingerprint)
  "pathにfingerprintを書き込む
   fingerprintはハッシュ"
  (let (tmp)
    (maphash 
      (lambda (k v)
        (push (cons k v) tmp))
      fingerprint)
    (with-open-file (out path :direction :output :if-exists :supersede :if-does-not-exist :create)
      (print tmp out))))


@export 
(defun restore-fingerprint (path)
  (cond 
    ((not (cl-fad:file-exists-p path))
     (error (make-condition 'file-not-found)))
    ((and (not (cl-fad:directory-exists-p path)) 
          (cl-fad:file-exists-p path))
     (%restore-fingerprint-dump path))
    ((cl-fad:directory-exists-p path)
     (%restore-fingerprint-csvdir path))))


@export
(defun %restore-fingerprint-dump (path)
  "fingerprintを表すファイルpathを読み込み
   ハッシュを返す"
  (let ((result (make-hash-table :test 'eq)))
    (with-open-file (in path :direction :input)
      (loop 
        for (k . v) in (read in) do 
        (setf (gethash k result) v)))
    result))


@export
(defun %restore-fingerprint-csvdir (path)
  "pathで表されるディレクトリは
   path 
     - A
       - a
       - b
       - c
     - B
       - o
       - p
       - q
   のようなディレクトリ構成になっている
   なお, a,b,c, o,p,qは全てCSVファイルであり
   それぞれ一回分のスキャンを表す

   この関数は、path以下のcsvファイルを読み込んで
   {:A (#<> #<> #<>) , :B (#<> #<> #<>) , }　のハッシュテーブルを返す"
  (declare (type pathname path))
  (let ((result (make-hash-table :test 'eq)))
    (declare (type hash-table result))
    (loop 
      for path pathname in (cl-fad:list-directory path)
      if (cl-fad:directory-exists-p path) do 
      (let* ((last-name (pathname-directory path))
             (place-label 
               (intern (string-upcase (car (last last-name))) :KEYWORD)))
        (declare (type list last-name)
                 (type symbol place-label))
        (setf (gethash place-label result) (convertin path))))
    result))



(defun %read-csvdir (path)
  "read-csvdirはこの関数をpath名で反復するだけ"
  (with-open-file 
    (in path :direction :input)
    (loop 
      for line = (read-line in nil nil)
      while line
      collect 
      (destructuring-bind 
        (essid bssid frequency channel rssi quality)  
        (cast (cl-ppcre:split #\, line))
        (make-measured-accesspoint 
          :essid essid 
          :bssid bssid 
          :freq frequency
          :channel channel 
          :rssi rssi 
          :quality quality)))))


@export 
(defun read-csvdir (path)
  "pathディレクトリにある全てのCSVファイルを読み込んで
   measured-accesspointのリストにして返す"
  (loop 
    for file pathname in (cl-fad:list-directory path)
    if (and (not (cl-fad:directory-exists-p file)) (cl-fad:file-exists-p file)) 
    append (%read-csvdir file)))






(defun cast (fields)
  "フィールドの値をintegerとかに変換する"
  (destructuring-bind 
    (essid bssid frequency channel rssi quality) fields
    (list 
      essid 
      bssid 
      (read-from-string frequency)
      (parse-integer channel)
      (parse-integer rssi)
      (read-from-string quality))))


(defun %aggrigate (list)
  " list: ((field1 field2 field3 field4 field5 field6) ...)
    のbssidは全て同じもの
    これを解析してaccesspoint-distribution構造体にして返す"
  (let* 
    ((sample  (car list))
     (bssid   (second sample))
     (essid   (first sample))
     (freq    (third sample))
     (channel (fourth sample))
     (q nil)
     (rssis nil)
     (hist nil))
    (loop 
      for each in list do 
      (push (fifth each) rssis)
      (push (sixth each) q)) 
    (loop 
      for each in (remove-duplicates rssis :test #'=)
      do (push (cons each (count each rssis :test #'=)) hist))
    (make-accesspoint-distribution
      :bssid bssid 
      :essid essid 
      :freq freq 
      :channel channel
      :qavg (float (alexandria:mean q))
      :avg (float (alexandria:mean rssis))
      :var (float (alexandria:standard-deviation rssis))
      :hist (sort hist (lambda (x y) (< (car x) (car y)))))))

(defun aggregate-bssid (whole)
  " ((field1 field2 field3 field4 field5 field6) ...) 
   なリストを集計してaccesspoint-distribution構造体のリストに変換する "
  (declare (type list whole))
  (let ((bssidlist 
          (remove-duplicates
            (loop for each in whole collect (second each))
            :test #'string=))
        (result nil))
    (declare (type list bssid)
             (type list result))
    (loop 
      for target-bssid in bssidlist 
      do (push 
           (%aggrigate 
             (remove-if-not 
               (lambda (x)
                 (string= (second x) target-bssid))
               whole)) 
           result))
    result))


(defun convertin (path)
  "path以下にある全てのcsvファイルを元に
   accesspoint-distribution構造体のリストを作って返す
   CSVファイルのフォーマットは
   ESSID,BSSID,FREQUENCY,CHANNEL,RSSI,QUALITY"
  (declare (type pathname path))
  (let (acc)
    (declare (type list acc))
    (loop 
      for file pathname in (cl-fad:list-directory path)
      if (and (not (cl-fad:directory-exists-p file)) (cl-fad:file-exists-p file)) do 
      (with-open-file (in file :direction :input)
        (loop 
          for line = (read-line in nil nil)
          while line 
          do (push (cast (cl-ppcre:split #\, line)) acc))))
    (aggregate-bssid acc)))

