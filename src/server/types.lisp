
(in-package :cl-user)
(defpackage :types.server.indoor-positioning
  (:use :cl 
        :cl-annot
        :annot.class
        )
  )

(in-package :types.server.indoor-positioning)
(enable-annot-syntax)



@export-class
(defclass model () 
  ((path 
     :initform ""
     :initarg :path
     :accessor path)
   (accessmethod
     :initform :get
     :initarg :method 
     :accessor accessmethod)
   (param 
     :initform nil 
     :initarg :param
     :accessor param)))


@export-class
(defclass html-output-model (model)
  ((content-type 
     :initform '(:content-type "text/html")
     :initarg :content-type
     :accessor content-type)))

@export-class
(defclass show-members (html-output-model)
  ()
  )


@export-class
(defclass json-output-model (model) 
  ((content-type 
     :initform '(:content-type "application/json")
     :initarg :content-type
     :accessor content-type))) 

@export-class
(defclass estimation (json-output-model)
  ()
  )

@export-class
(defclass registration (json-output-model)
  ()
  )

@export-class
(defclass listup (json-output-model)
  ()
  )
 
