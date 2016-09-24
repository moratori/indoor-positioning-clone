#|
  This file is a part of indoor-positioning project.
  Copyright (c) 2015 morator (teldev@live.jp)
|#

(in-package :cl-user)
(defpackage indoor-positioning-test-asd
  (:use :cl :asdf))
(in-package :indoor-positioning-test-asd)

(defsystem indoor-positioning-test
  :author "morator"
  :license ""
  :depends-on (:indoor-positioning
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "indoor-positioning"))))

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
