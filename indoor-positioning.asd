#|
  This file is a part of indoor-positioning project.
  Copyright (c) 2015 morator (teldev@live.jp)
|#

#|
  Author: morator (teldev@live.jp)
|#

(in-package :cl-user)
(defpackage indoor-positioning-asd
  (:use :cl :asdf))
(in-package :indoor-positioning-asd)

(defsystem indoor-positioning
  :version "0.1"
  :author "morator"
  :license ""
  :depends-on (cl-fad trivial-shell cl-annot cl-ppcre alexandria clack ningle cl-json cl-emb)
  :components 
    ((:module "src"
      :components 
        ((:module "core"
          :components 
            ((:file "errors")
             (:file "types")
             (:file "measurement")
             (:file "plotter")
             (:file "util")
             (:file "positioningmethods")
             (:file "indoor-positioning")))

         (:module "server"
          :components 
            ((:file "types")
             (:file "util")
             (:file "model")
             (:file "server") 
             ))))) 

  :description ""
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op indoor-positioning-test))))
