
(in-package :cl-user)
(defpackage :server.server.indoor-positioning
  (:use :cl 
        :cl-annot
        :types.server.indoor-positioning
        )
  (:import-from 
    :clack
    :clackup
    )
  (:import-from 
    :model.server.indoor-positioning
    :init-fingerprint
    :perform
    )
  )

(in-package :server.server.indoor-positioning)

(enable-annot-syntax)

(defvar *app* 
  (make-instance 'ningle:<app>))

(defvar *handler* nil)




;; メンバーの位置情報を照会しhtmlで返す
(setf (ningle:route *app* "/" :method :get)
      (lambda (param)
        (perform 
          (make-instance 
            'show-members
            :path "/"
            :method :get
            :param param))))



;; 屋内位置推定を行う
(setf (ningle:route *app* "/estimate/location" :method :post)
      (lambda (param)
        (perform 
          (make-instance 
            'estimation 
            :path "/estimate/location"
            :method :post 
            :param param)))) 



;; 現在の屋内位置情報を登録する
(setf (ningle:route *app* "/register/location" :method :post)
      (lambda (param)
        (perform 
          (make-instance 
            'registration
            :path "/register/location"
            :method :post 
            :param param))))
 


;; メンバーの位置情報を照会しJsonで返す
(setf (ningle:route *app* "/list/location" :method :get)
      (lambda (param)
        (perform 
          (make-instance 
            'listup
            :path "/list/location"
            :method :get 
            :param param))))




@export
(defun start ()
  (handler-case 
    (init-fingerprint)
    (t (c)
       (format *standard-output* "can't read default fingerprint file")
       (force-output *standard-output*)
       (return-from start nil)))
  (unless *handler*
    (setf *handler* (clackup *app*))))


@export
(defun stop ()
  (when *handler*
    (clack:stop *handler*)))



