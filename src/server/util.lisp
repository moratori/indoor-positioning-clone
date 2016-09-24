
(in-package :cl-user)
(defpackage :util.server.indoor-positioning
  (:use :cl 
        :cl-annot
        :types.core.indoor-positioning
        )
  (:import-from 
    :errors.core.indoor-positioning
    :malformed-request
    )
  )

(in-package :util.server.indoor-positioning)

(enable-annot-syntax)



(defstruct json-type)


(defstruct (keyvalue-type 
             (:include json-type)
             (:conc-name kv.)
             (:constructor keyvalue-type (keys values)))
  (keys nil :type list)
  (values nil :type list)) 

(defstruct (same-array-type 
             (:include json-type)
             (:conc-name sar.)
             (:constructor same-array-type (type)))
  (type nil :type t)) 


(defstruct (different-array-type 
             (:include json-type)
             (:conc-name dar.)
             (:constructor different-array-type (types)))
  (types nil :type list))





(defun throw-malformed ()
  (error 
    (make-condition 'malformed-request)))



(defgeneric validate-json (typedefinition alist)
  (:documentation 
    "alistをチェックする
     alistはningleによってjsonからマッピングされたもの"))


(defmethod validate-json ((json-type symbol) obj)
  (unless (typep obj json-type)
    (throw-malformed)))


(defmethod validate-json ((json-type keyvalue-type) obj)
  (let ((accessor (lambda (k) 
                    (handler-case
                      (assoc k obj :test #'string=)
                      (error (c)
                           (throw-malformed))))))
    (loop 
      for key in (kv.keys json-type)
      for valuetype in (kv.values json-type)
      for cons = (funcall accessor key)
      do 
      (progn 
        (unless cons 
          (throw-malformed))
        (validate-json valuetype (cdr cons))))))


(defmethod validate-json ((json-type same-array-type) obj)
  (unless (typep obj 'list)
    (throw-malformed))
  (let ((typedef (sar.type json-type)))
    (loop 
      for each in obj 
      do (validate-json typedef each)))) 


(defmethod validate-json ((json-type different-array-type) obj)
  (unless (typep obj 'list)
    (throw-malformed))
  (loop 
    for value in obj
    for type in (dar.types json-type)
    do (validate-json type value)))



(defvar *estimation-request-json-type*
  (keyvalue-type 
    '("devicetype" "devicename" "require" "accesspoints")
    `(string string number 
             ,(same-array-type 
                (different-array-type 
                  '(string string number number number number))))))

(defvar *registration-request-json-type*
  (keyvalue-type 
    '("devicetype" "devicename" "timeout" "name" "placelist")
    `(string string number string
             ,(same-array-type 'string))))



@export 
(defun convert-estimation-request (alist)

  (validate-json *estimation-request-json-type* alist)


  (let* ((func (lambda (k) (cdr (assoc k alist :test #'string=))))
         (devicetype (funcall func "devicetype"))
         (devicename (funcall func "devicename"))
         (require    (funcall func "require"))
         (accesspoints (funcall func "accesspoints")))
    
    (make-estimation-request
      :devicetype devicetype 
      :devicename devicename
      :require require 
      :accesspoints 
      (loop 
        for (essid bssid freq channel rssi quality) in accesspoints 
        collect 
        (make-measured-accesspoint 
          :bssid bssid 
          :essid essid 
          :freq freq 
          :channel channel 
          :rssi rssi 
          :quality quality)))))

@export 
(defun convert-registration-request (alist)

  (validate-json *registration-request-json-type* alist)
  
  (let* ((func (lambda (k) (cdr (assoc k alist :test #'string=))))
         (devicetype (funcall func "devicetype"))
         (devicename (funcall func "devicename"))
         (timeout    (funcall func "timeout"))
         (name       (funcall func "name"))
         (placelist (funcall func "placelist")))

    (make-register-request 
      :devicetype devicetype
      :devicename devicename
      :timeout timeout
      :name name 
      :placelist placelist))) 

