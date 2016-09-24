
(in-package :cl-user)
(defpackage :types.core.indoor-positioning
  (:use :cl 
        :cl-annot
        :cl-annot.class
        )
  (:import-from 
    :cl-json
    :encode-json
    )
  )

(in-package :types.core.indoor-positioning)


(enable-annot-syntax)





@export-class
(defclass positioning-method ()
  ((name 
     :initform ""
     :accessor name)))


@export-class
(defclass normal-dist-method (positioning-method)
  ((name 
     :initform "normal-distribution-method"
     :reader name))) 




@export-class
(defclass os ()
  ((name 
     :initform "unknown os"
     :reader name)))


@export-class
(defclass linux (os)
  ((name 
     :initform "linux"
     :reader name)))


@export-class
(defclass mac (os)
  ((name 
     :initform "mac"
     :reader name)))


@export-class
(defclass windows (os)
  ((name 
     :initform "windows"
     :reader name)))



 

@export-structure
(defstruct (accesspoint-distribution
             (:conc-name ap.))
  (bssid "" :type string)
  (essid "" :type string)
  (freq 0.0 :type single-float)
  (channel 0 :type fixnum)
  (qavg 0.0 :type single-float)
  (avg 0.0 :type single-float)
  (var 0.0 :type single-float)
  (hist nil :type list))


@export-structure
(defstruct (measured-accesspoint 
             (:conc-name map.)
             (:print-object 
               (lambda (obj stream)
                 (format stream "~A,~A,~A,~A,~A,~A" 
                         (map.essid obj)
                         (map.bssid obj)
                         (map.freq obj)
                         (map.channel obj)
                         (map.rssi obj)
                         (map.quality obj)))))
  (bssid "" :type string)
  (essid "" :type string)
  (freq 0.0 :type single-float)
  (channel 0 :type fixnum)
  (rssi 0 :type fixnum)
  (quality 0.0 :type single-float))




#|位置推定要求
  {"devicetype": "PC",
   "devicename": "sh76hg",
   "require": 3,
   "accesspoints": 
    [["ESSID", "BSSID", 2.4, 13, -45, 0.89],
     ["ESSID", "BSSID", 2.4, 13, -45, 0.89]]
  }
|# 

@export-structure
(defstruct (estimation-request 
             (:conc-name req-est.)
             (:print-object
               (lambda (object stream)
                 (encode-json
                   (list 
                     (cons "devicetype" (req-est.devicetype object))
                     (cons "devicename" (req-est.devicename object))
                     (cons "require"    (req-est.require object))
                     (cons "accesspoints" 
                           (mapcar 
                             (lambda (maps)
                               (list 
                                 (string-trim '(#\") (map.essid maps))
                                 (map.bssid maps)
                                 (map.freq maps)
                                 (map.channel maps)
                                 (map.rssi maps)
                                 (map.quality maps)))
                             (req-est.accesspoints object)))) stream))))
  (devicetype "" :type string)
  (devicename "" :type string)
  (require 0 :type integer)
  (accesspoints nil :type list))


#|
  位置登録要求
  {
   "devicetype": "PC",
   "devicename": "sh76hg",
   "timeout": 18
   "name": "foo"
   "placelist": ["icslab","inelab ]
  } 
|#
@export-structure
(defstruct (register-request
             (:conc-name req-reg.)
             (:print-object
               (lambda (object stream)
                 (encode-json
                   (list 
                     (cons "devicetype"  (req-reg.devicetype object))
                     (cons "devicename"  (req-reg.devicename object))
                     (cons "timeout"     (req-reg.timeout object))
                     (cons "name"        (req-reg.name object))
                     (cons "placelist"   (req-reg.placelist object))) stream))))
  (devicetype "" :type string)
  (devicename "" :type string)
  (timeout 0 :type integer)
  (name "" :type string)
  (placelist nil :type list))

