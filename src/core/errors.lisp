

(in-package :cl-user)
(defpackage :errors.core.indoor-positioning
  (:use :cl 
        :cl-annot
        :cl-annot.class
        ))

(in-package :errors.core.indoor-positioning)


(enable-annot-syntax)



@export
(define-condition method-not-implemented (error)
  ()
  )

@export 
(define-condition measurement-not-implemented (error)
 ()
  )
 

@export 
(define-condition malformed-request (error)
  ()
  )


@export
(define-condition file-not-found (error)
  ()
  )
