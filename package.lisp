
;;;; MS-DTYP Microsoft Data Types

;;; 
;;; Defines the data types commonly used in Microsoft protocols.
;;;
;;; Copyright (C) Frank James, July 2014
;;;


(defpackage :cl-dtyp
  (:use :cl :packet)
  (:export  #:string-guid
            #:guid-string
            #:guid=
            #:guid
            #:pack-guid
            #:unpack-guid
            #:make-guid
            #:make-sid
            #:sid=
            #:sid-string
            #:string-sid
            #:pack-sid
            #:unpack-sid
            #:wellknown-sid
            #:wellknown-sid-by-string
            #:pack-access-mask
            #:unpack-access-mask
            #:pack-acl
            #:unpack-acl
            #:make-security-descriptor
            #:copy-security-descriptor
            #:pack-security-descriptor
            #:unpack-security-descriptor
            #:make-claim-attribute
            #:pack-claim-attribute
            #:pack-claim-attribute*
            #:unpack-claim-attribute
            #:claim-attribute-name
            #:make-ace
            #:pack-ace
            #:pack-ace*
            #:unpack-ace
            #:ace-flags
            #:ace-sid
            #:ace-access-mask
            #:ace-object
            #:ace-inherited-object
            #:ace-conditional-expression
            #:ace-type
            #:security-descriptor-string
            #:string-security-descriptor
            #:ntstatus
            #:access-check))
            










