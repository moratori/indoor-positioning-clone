

(in-package :cl-user)
(defpackage 
  :positioningmethod.core.indoor-positioning
  (:use :cl 
        :errors.core.indoor-positioning
        :types.core.indoor-positioning
        :util.core.indoor-positioning
        :cl-annot 
        :cl-annot.class
        ))
(in-package :positioningmethod.core.indoor-positioning)

(enable-annot-syntax)


(defvar +NEPIER+ 2.7182818284)


@export
(defmethod positioning ((methods t) fingerprint aps)
  (error 
    (make-condition 
      'method-not-implemented))) 


(defmethod positioning ((methods normal-dist-method) fingerprint aps)
  "正規分布の確率密度関数を用いて位置推定を行う
   fingerorintはハッシュテーブルを表す
   apsはmeasured-accesspoint構造体のリスト"
  (let (result)
    (maphash 
      (lambda (place apdatalist)
        (let ((score 0))
          (loop 
            for each in aps do 
            (let ((same 
                    (find-if 
                      (lambda (x) 
                        (string= (ap.bssid x) (map.bssid each))) 
                      apdatalist)))
              (when (and same (> (length (ap.hist same)) 1))
                (incf score 
                      (normal-distribution 
                        (map.rssi each)
                        (ap.avg same)
                        (ap.var same))))))
          (push (cons place score) result)))
      fingerprint)
    (sort 
      result 
      (lambda (x y)
        (> (cdr x) (cdr y))))))



(defun normal-distribution (x a s)
  "正規分布の確率密度関数をつかって計算する"
  (*
    (/ 1 (* (sqrt (* 2 pi)) s))
    (expt +NEPIER+ (- (/ (* (- x a) (- x a))  (* 2 s s))))))


