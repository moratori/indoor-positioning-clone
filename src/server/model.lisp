(in-package :cl-user)
(defpackage :model.server.indoor-positioning
  (:use :cl 
        :cl-annot
        :types.core.indoor-positioning
        :types.server.indoor-positioning
        )
  (:import-from 
    :util.core.indoor-positioning 
    :restore-fingerprint
    )
  (:import-from 
    :cl-json 
    :encode-json
    )
  (:import-from 
    :positioningmethod.core.indoor-positioning
    :positioning
    )
  (:import-from 
    :util.server.indoor-positioning 
    :convert-estimation-request
    :convert-registration-request
    )
  (:import-from 
    :errors.core.indoor-positioning
    :malformed-request
    )
  )

(in-package :model.server.indoor-positioning)

(enable-annot-syntax)



(defvar *current-fingerprint* nil)
(defvar *current-location-info* 
  (make-hash-table :test 'equal))

(defvar *path* 
  #.(merge-pathnames 
    (make-pathname 
      :directory '(:relative "default_fingerprint")
      :name "default"
      :type "fp") 
    (asdf:system-source-directory :indoor-positioning)))



@export 
(defun init-fingerprint ()
  "位置指紋の読み込み"
  (setf *current-fingerprint*
        (restore-fingerprint *path*)))



@export
(defgeneric perform (model)
  (:documentation "各々の処理"))





(defmethod perform :around ((access html-output-model))
  (setf (lack.response:response-headers ningle:*response*)
        (content-type access))
  (call-next-method))



(defun make-memberplist ()
  (let (result)
    (maphash 
      (lambda (name v)
        (destructuring-bind (time  req) v
          (let ((place (format nil "~{~A~^ or ~}" (req-reg.placelist req)))
                (rt 
                  (multiple-value-bind 
                    (sec min hour date month year)
                    (decode-universal-time time -9)
                    (format nil "~A/~A ~A:~A" month date hour min))))
            (push 
              (list 
                :name  (req-reg.name req)
                :place place
                :devicetype (req-reg.devicetype req)
                :time rt)
            result))))
      *current-location-info*
      )
    (list :member result)))



(defmethod perform ((access show-members))
  (let ((tmp 
          #.(merge-pathnames 
            "index.tmpl"
            (merge-pathnames 
              (make-pathname :directory 
                             '(:relative "templates"))
              (asdf:system-source-directory :indoor-positioning)))))
    (emb:execute-emb 
      tmp 
      :env       
      (make-memberplist))))
 




(defmethod perform :around ((access json-output-model))
  (setf (lack.response:response-headers ningle:*response*)
        (content-type access))
  (let ((raw 
          (handler-case
            (call-next-method)
            (malformed-request (c)
              '((:success . "false") 
                (:reason . "malformed json request")))
            (t (c)
              '((:success . "false") 
                (:reason . "unexpected error"))))))
    (cl-json:encode-json-to-string raw)))





(defmethod perform ((access estimation))
  "クライアントのjsonを読んで位置推定を行う
   とりあえずaccesspoints key だけを読んで位置推定を行う"

  (let* ((alist (param access))
         (req (convert-estimation-request alist))
         (result 
          (positioning 
            (make-instance 'normal-dist-method)
            *current-fingerprint*
            (req-est.accesspoints req)))
         (tmp 
           (remove-if 
             (lambda (x)
               (destructuring-bind (a . b) x
                 (declare (ignore a))
                 (zerop b)))
             result)))
    (subseq
      (sort tmp (lambda (x y) (> (cdr x) (cdr y))))
      0 (min (length tmp) (req-est.require req))))) 


(defmethod perform ((access registration))
  "ユーザの屋内位置を登録"
  (let* ((alist (param access))
         (req (convert-registration-request alist)))
    (setf (gethash (req-reg.name req) 
                   *current-location-info*) 
          (list (get-universal-time) req))
    '((:success . "true"))))


(defun remove-timeouts ()
  "タイムアウトしたものを削除する"
  (let (keys)
    (maphash 
      (lambda (k v)
        (destructuring-bind (time req) v 
          (when (> (get-universal-time)
                   (+ time (* (req-reg.timeout req) 3600)))
            (push k keys))))
      *current-location-info*)
    (loop 
      for key in keys 
      do (remhash key *current-location-info*))))


(defmethod perform ((access listup))
  "現在のユーザの位置を返す "
  (let ((alist (param access))
        (result nil))
    (remove-timeouts)
    (maphash 
      (lambda (k v)
        (declare (ignore k))
        (destructuring-bind (time req) v
          (push 
            `(("registered-at" . ,time)
              ("devicetype" . ,(req-reg.devicetype req))
              ("devicename" . ,(req-reg.devicename req))
              ("name" . ,(req-reg.name req))
              ("timeout" . ,(req-reg.timeout req))
              ("placelist" . ,(req-reg.placelist req)))
            result)))
      *current-location-info*)
    result))

