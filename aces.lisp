
;;;; MS-DTYP Microsoft Data Types

;;; 
;;; Defines the data types commonly used in Microsoft protocols.
;;;
;;; Copyright (C) Frank James, July 2014
;;;


(in-package :cl-dtyp)

;; aces:
;; all aces start with a header which includes the type of ace, some flags and the total size 
;; we want to be able to define and ace type and its packer/unpacker in a "nice" way
;; there are actually only a few fundamental structure types, but several different ace types
;; share the same structures

(defmacro def-ace-print-object-method (ace-type-name)
  `(defmethod print-object ((ace ,ace-type-name) stream)
     (print-unreadable-object (ace stream :type t)
       (format stream "~A" (ace-string ace)))))

;;       (format stream ":MASK ~S :SID ~A" 
;;	       (slot-value ace 'mask)
;;	       (let ((sid (slot-value ace 'sid)))
;;		 (when sid (sid-string (slot-value ace 'sid))))))))

;; lots of the ACE types have identical internal structures
(defmacro defpackets (names slots &rest options)
  "Defines multiple identical packet types but with different names"
  `(progn
     ,@(mapcar (lambda (name)
		 `(progn
		    (defpacket ,name ,slots ,@options)
		    (def-ace-print-object-method ,name)))
	       names)))


;; 2.4.4.1 ACE_HEADER http://msdn.microsoft.com/en-us/library/cc230296.aspx
(defpacket ace-header 
  ((type :uint8 :initform 0 :initarg :type)
   (flags :uint8 :initform 0 :initarg :flags)
   (size :uint16 :initform 0 :initarg :size :documentation "Total size of ACE, including header"))
  (:packing 1))

(defenum *ace-types*
  ((:ACCESS-ALLOWED #x00
    "Access-allowed ACE that uses the ACCESS_ALLOWED_ACE (section 2.4.4.2) structure.")
   (:ACCESS-DENIED #x01
    "Access-denied ACE that uses the ACCESS_DENIED_ACE (section 2.4.4.4) structure.")
   (:SYSTEM-AUDIT #x02
    "System-audit ACE that uses the SYSTEM_AUDIT_ACE (section 2.4.4.10) structure.")
   (:SYSTEM-ALARM #x03
    "Reserved for future use.")
   (:ACCESS-ALLOWED-COMPOUND #x04
    "Reserved for future use.")
   (:ACCESS-ALLOWED-OBJECT #x05
    "Object-specific access-allowed ACE that uses the ACCESS_ALLOWED_OBJECT_ACE (section 2.4.4.3) structure.<31>")
   (:ACCESS-DENIED-OBJECT #x06
    "Object-specific access-denied ACE that uses the ACCESS_DENIED_OBJECT_ACE (section 2.4.4.5) structure.<32>")
   (:SYSTEM-AUDIT-OBJECT #x07
    "Object-specific system-audit ACE that uses the SYSTEM_AUDIT_OBJECT_ACE (section 2.4.4.11) structure.<33>")
   (:SYSTEM-ALARM-OBJECT #x08
    "Reserved for future use.")
   (:ACCESS-ALLOWED-CALLBACK #x09
    "Access-allowed callback ACE that uses the ACCESS_ALLOWED_CALLBACK_ACE (section 2.4.4.6) structure.<34>")
   (:ACCESS-DENIED-CALLBACK #x0A
    "Access-denied callback ACE that uses the ACCESS_DENIED_CALLBACK_ACE (section 2.4.4.7) structure.<35>")
   (:ACCESS-ALLOWED-CALLBACK-OBJECT #x0B
    "Object-specific access-allowed callback ACE that uses the ACCESS_ALLOWED_CALLBACK_OBJECT_ACE (section 2.4.4.8) structure.<36>")
   (:ACCESS-DENIED-CALLBACK-OBJECT #x0C
    "Object-specific access-denied callback ACE that uses the ACCESS_DENIED_CALLBACK_OBJECT_ACE (section 2.4.4.9) structure.<37>")
   (:SYSTEM-AUDIT-CALLBACK #x0D
    "System-audit callback ACE that uses the SYSTEM_AUDIT_CALLBACK_ACE (section 2.4.4.12) structure.<38>")
   (:SYSTEM-ALARM-CALLBACK #x0E
    "Reserved for future use.")
   (:SYSTEM-AUDIT-CALLBACK-OBJECT #x0F
    "Object-specific system-audit callback ACE that uses the SYSTEM_AUDIT_CALLBACK_OBJECT_ACE (section 2.4.4.14) structure.")
   (:SYSTEM-ALARM-CALLBACK-OBJECT #x10
    "Reserved for future use.")
   (:SYSTEM-MANDATORY-LABEL #x11
    "Mandatory label ACE that uses the SYSTEM_MANDATORY_LABEL_ACE (section 2.4.4.13) structure.")
   (:SYSTEM-RESOURCE-ATTRIBUTE #x12
    "Resource attribute ACE that uses the SYSTEM_RESOURCE_ATTRIBUTE_ACE (section 2.4.4.15)")
   (:SYSTEM-SCOPED-POLICY-ID #x13
    "A central policy ID ACE that uses the SYSTEM_SCOPED_POLICY_ID_ACE (section 2.4.4.16)")))

(defflags *ace-flags*
  ((:CONTAINER-INHERIT-ACE 1
   "Child objects that are containers, such as directories, inherit the ACE as an effective ACE. The inherited ACE is inheritable unless the NO_PROPAGATE_INHERIT_ACE bit flag is also set.")
   (:FAILED-ACCESS-ACE-FLAG 7
    "Used with system-audit ACEs in a system access control list (SACL) to generate audit messages for failed access attempts.")
   (:INHERIT-ONLY-ACE 3
    "Indicates an inherit-only ACE, which does not control access to the object to which it is attached. If this flag is not set, the ACE is an effective ACE that controls access to the object to which it is attached.
Both effective and inherit-only ACEs can be inherited depending on the state of the other inheritance flags.")
   (:INHERITED-ACE 4
    "Indicates that the ACE was inherited. The system sets this bit when it propagates an inherited ACE to a child object.<40>")
   (:NO-PROPAGATE-INHERIT-ACE 2
    "If the ACE is inherited by a child object, the system clears the OBJECT_INHERIT_ACE and CONTAINER_INHERIT_ACE flags in the inherited ACE. This prevents the ACE from being inherited by subsequent generations of objects.")
   (:OBJECT-INHERIT-ACE 0
    "Noncontainer child objects inherit the ACE as an effective ACE.
For child objects that are containers, the ACE is inherited as an inherit-only ACE unless the NO_PROPAGATE_INHERIT_ACE bit flag is also set.")
   (:SUCCESSFUL-ACCESS-ACE-FLAG 6
    "Used with system-audit ACEs in a SACL to generate audit messages for successful access attempts.")))

(defun pack-ace-flags (&rest flags)
  (pack-flags flags *ace-flags*))

(defun unpack-ace-flags (number)
  (unpack-flags number *ace-flags*))

(defun make-ace-header (type flags size)
  (declare (list flags))
  (make-instance 'ace-header 
                 :size size
                 :type type
                 :flags (apply #'pack-ace-flags flags)))

;; ----------- access allowed type aces -------------------

;; 2.4.4.2 ACCESS_ALLOWED_ACE http://msdn.microsoft.com/en-us/library/cc230286.aspx
;; 2.4.4.4 ACCESS_DENIED_ACE http://msdn.microsoft.com/en-us/library/cc230291.aspx
;; 2.4.4.10 SYSTEM_AUDIT_ACE http://msdn.microsoft.com/en-us/library/cc230376.aspx
;; 2.4.4.13 SYSTEM_MANDATORY_LABEL_ACE http://msdn.microsoft.com/en-us/library/cc230379.aspx
;; 2.4.4.16 SYSTEM_SCOPED_POLICY_ID_ACE http://msdn.microsoft.com/en-us/library/hh877846.aspx
(defpackets (access-allowed-ace access-denied-ace system-audit-ace system-mandatory-label-ace system-scoped-policy-id-ace)
  ((header ace-header :initform nil :initarg :header)
   (mask :uint32 :initform 0 :initarg :access-mask :accessor ace-access-mask)
   ;; payload 
   (sid (:uint8 0) :initform nil :initarg :sid :accessor ace-sid))
  (:packing 1))

(defun %make-access-allowed-ace (header mask sid ace-type-name)
  (declare (list mask))
  (make-instance ace-type-name
		 :header header
		 :sid sid
		 :access-mask (apply #'pack-access-mask mask)))

(defun %pack-access-allowed-ace (ace-flags mask sid ace-type ace-type-name)
  "Pack an ACCESS_ALLOWED ACE"
  (let ((packed-sid (pack-sid sid)))
    (usb8 (pack (%make-access-allowed-ace 
		 (make-ace-header ace-type
				  ace-flags 
				  (+ (type-size 'access-allowed-ace) 
				     (length packed-sid)))
		 mask
		 nil
		 ace-type-name)
		ace-type-name)
	  packed-sid)))


(defun %unpack-access-allowed-ace (buffer ace-type-name)
  (multiple-value-bind (ace payload) (unpack buffer ace-type-name)
    (setf (slot-value (slot-value ace 'header) 'flags)
	  (unpack-ace-flags (slot-value (slot-value ace 'header) 'flags))
	  (slot-value ace 'sid) 
	  (unpack-sid payload)
	  (slot-value ace 'mask) 
	  (unpack-access-mask (slot-value ace 'mask)))
    ace))

(defmacro def-access-allowed-pack/unpack (ace-type ace-type-name)
  `(progn
     (defun ,(intern (format nil "PACK-~A" ace-type-name)) (ace-flags mask sid)
       (%pack-access-allowed-ace ace-flags mask sid 
				 ,(enum ace-type *ace-types*)
				 ',ace-type-name))
     (defun ,(intern (format nil "UNPACK-~A" ace-type-name)) (buffer)
       (%unpack-access-allowed-ace buffer ',ace-type-name))))

(def-access-allowed-pack/unpack :access-allowed access-allowed-ace)
(def-access-allowed-pack/unpack :access-denied access-denied-ace)
(def-access-allowed-pack/unpack :system-audit system-audit-ace)
(def-access-allowed-pack/unpack :system-mandatory-label system-mandatory-label-ace)
(def-access-allowed-pack/unpack :system-scoped-policy-id system-scoped-policy-id-ace)

;; 2.4.4.3 ACCESS_ALLOWED_OBJECT_ACE http://msdn.microsoft.com/en-us/library/cc230289.aspx
;; 2.4.4.5 ACCESS_DENIED_OBJECT_ACE http://msdn.microsoft.com/en-us/library/gg750297.aspx
;; 2.4.4.11 SYSTEM_AUDIT_OBJECT_ACE http://msdn.microsoft.com/en-us/library/gg750298.aspx
(defpackets (access-allowed-object-ace access-denied-object-ace system-audit-object-ace)
  ((header ace-header :initform nil :initarg :header)
   (mask :uint32 :initform 0 :initarg :access-mask :accessor ace-access-mask)
   (flags :uint32 :initform 0 :initarg :flags)
   (object-type guid :initform nil :initarg :object-type :accessor ace-object)
   (inherited-object-type guid :initform nil :initarg :inherited-object-type :accessor ace-inherited-object)
   ;; payload 
   (sid (:uint8 0) :initform nil :initarg :sid))
  (:packing 1))

(defflags *access-allowed-object-ace-mask*
  ((:ADS-RIGHT-DS-CONTROL-ACCESS 8
   "The ObjectType GUID identifies an extended access right.")
   (:ADS-RIGHT-DS-CREATE-CHILD 0
    "The ObjectType GUID identifies a type of child object. The ACE controls the trustee's right to create this type of child object.")
   (:ADS-RIGHT-DS-DELETE-CHILD 1
    "The ObjectType GUID identifies a type of child object. The ACE controls the trustee's right to delete this type of child object.")
   (:ADS-RIGHT-DS-READ-PROP 4
    "The ObjectType GUID identifies a property set or property of the object. The ACE controls the trustee's right to read the property or property set.")
   (:ADS-RIGHT-DS-WRITE-PROP 5
    "The ObjectType GUID identifies a property set or property of the object. The ACE controls the trustee's right to write the property or property set.")
   (:ADS-RIGHT-DS-SELF 3
    "The ObjectType GUID identifies a validated write.")))

(defenum *object-specific-rights*
  ((:File-All-Access  #x001F01FF "FA File")
   (:File-Execute #x001200A0 "FX File")
   (:File-Write #x00100116 "FW File")
   (:File-Read #x00120089 "FR File")
   (:Key-All-Access #x00000019 "KA Registry-key")
   (:Key-Read #x0000003F "KR Registry-key")
   (:Key-Execute #x00000019 "KX Registry key")
   (:Key-Write #x00000006 "KW Registry key")
   (:Control-Access #x00000100 "CA Driectory")
   (:List-Object #x00000080 "LO Directory")
   (:Delete-Tree #x00000040 "DT Directrory")
   (:Write-Property #x00000020 "WP Diretryo")
   (:Read-Property #x00000010 "RP Directryo")
   (:Self-Write #x00000008 "SW Directryo")
   (:List-Children #x00000004 "LC Directory")
   (:Delete-Child #x00000002 "DC Directory")
   (:Create-Child #x00000001 "CC Directryo")))

(defun pack-access-mask-ace (&rest flags)
  "Pack ACCESS_MASK for an ACE. These are an extension of the base ACCESS_MASK flags"
  (pack-flags flags (append *access-allowed-object-ace-mask* *access-mask*)))

(defun unpack-access-mask-ace (number)
  "Unpack an ACCESS_MASK for an ACE."
  (unpack-flags number (append *access-allowed-object-ace-mask* *access-mask*)))

(defflags *access-allowed-object-ace-flags*
  ((:ACE-OBJECT-TYPE-PRESENT 0
    "ObjectType is valid.")
   (:ACE-INHERITED-OBJECT-TYPE-PRESENT 1
    "InheritedObjectType is valid. If this value is not specified, all types of child objects can inherit the ACE.")))

(defun pack-access-object-ace-flags (flags)
  (pack-flags flags *access-allowed-object-ace-flags*))

(defun unpack-access-object-ace-flags (number)
  (unpack-flags number *access-allowed-object-ace-flags*))


(defun %make-access-allowed-object-ace (header mask object-type inherited-object-type sid flags ace-type-name)
  (declare (list mask flags))
  (make-instance ace-type-name
		 :header header
		 :sid sid
		 :inherited-object-type inherited-object-type
		 :object-type object-type
		 :flags (pack-access-object-ace-flags flags)
		 :access-mask (apply #'pack-access-mask-ace mask)))

(defun %pack-access-allowed-object-ace (ace-flags mask object-type inherited-object-type sid flags ace-type ace-type-name)
  (let ((packed-sid (pack-sid sid)))
    (usb8 (pack (%make-access-allowed-object-ace 
		 (make-ace-header ace-type
				  ace-flags
				  (+ (type-size 'access-allowed-object-ace)
				     (length packed-sid)))
		 mask
		 object-type
		 inherited-object-type
		 nil
		 flags
		 ace-type-name)
		ace-type-name)
	  packed-sid)))

(defun %unpack-access-allowed-object-ace (buffer ace-type-name)
  (multiple-value-bind (ace payload) (unpack buffer ace-type-name)
    (setf (slot-value (slot-value ace 'header) 'flags)
	  (unpack-ace-flags (slot-value (slot-value ace 'header) 'flags))
	  (slot-value ace 'sid) 
	  (unpack-sid payload)
	  (slot-value ace 'mask) 
	  (unpack-access-mask-ace (slot-value ace 'mask)))
    ace))

(defmacro def-access-allowed-object-pack/unpack (ace-type ace-type-name)
  `(progn
     (defun ,(intern (format nil "PACK-~A" ace-type-name)) (ace-flags mask object-type inherited-object-type sid flags)
       (%pack-access-allowed-object-ace ace-flags mask object-type inherited-object-type sid flags 
					,(enum ace-type *ace-types*)
					',ace-type-name))
     (defun ,(intern (format nil "UNPACK-~A" ace-type-name)) (buffer)
       (%unpack-access-allowed-object-ace buffer ',ace-type-name))))

(def-access-allowed-object-pack/unpack :access-allowed-object access-allowed-object-ace)
(def-access-allowed-object-pack/unpack :access-denied-object access-denied-object-ace)
(def-access-allowed-object-pack/unpack :system-audit-object system-audit-object-ace)


;; 2.4.4.6 ACCESS_ALLOWED_CALLBACK_ACE http://msdn.microsoft.com/en-us/library/cc230287.aspx
;; 2.4.4.7 ACCESS_DENIED_CALLBACK_ACE http://msdn.microsoft.com/en-us/library/cc230292.aspx
;; 2.4.4.12 SYSTEM_AUDIT_CALLBACK_ACE http://msdn.microsoft.com/en-us/library/cc230377.aspx
(defpackets (access-allowed-callback-ace access-denied-callback-ace system-audit-callback-ace)
  ((header ace-header :initform nil :initarg :header)
   (mask :uint32 :initform 0 :initarg :access-mask :accessor ace-access-mask)
   (sid (:uint8 0) :initform nil :initarg :sid :accessor ace-sid)
   (data (:uint8 0) :initform nil :initarg :data :accessor ace-conditional-expression))
  (:packing 1))

(defun %make-access-allowed-callback-ace (header mask sid data ace-type-name)
  (declare (list mask))
  (make-instance ace-type-name
		 :header header
		 :access-mask (apply #'pack-access-mask mask)
		 :sid sid
		 :data data))

(defun %pack-access-allowed-callback-ace (ace-flags mask sid data ace-type ace-type-name)
  (let ((packed-sid (pack-sid sid)))
    (usb8 (pack (%make-access-allowed-callback-ace 
		 (make-ace-header ace-type 
				  ace-flags 
				  (+ (type-size 'access-allowed-callback-ace)
				     (length packed-sid)
				     (length data)))
		 mask
		 nil
		 nil
		 ace-type-name)
		ace-type-name)
	  packed-sid
	  (pack-conditional-expression data))))
		 
(defun %unpack-access-allowed-callback-ace (buffer ace-type-name)
  (multiple-value-bind (ace payload) (unpack buffer ace-type-name)
    (multiple-value-bind (sid payload) (unpack-sid payload)
      (setf (slot-value (slot-value ace 'header) 'flags)
	    (unpack-ace-flags (slot-value (slot-value ace 'header) 'flags))
	    (slot-value ace 'sid) 
	    sid
	    (slot-value ace 'mask)
	    (unpack-access-mask (slot-value ace 'mask))
	    (slot-value ace 'data) 
	    (tokens-conditional-expression (unpack-conditional-expression payload)))
      ace)))

(defmacro def-access-allowed-callback-pack/unpack (ace-type ace-type-name)
  `(progn
     (defun ,(intern (format nil "PACK-~A" ace-type-name)) (ace-flags mask sid data)
       (%pack-access-allowed-callback-ace ace-flags mask sid data
					  ,(enum ace-type *ace-types*)
					  ',ace-type-name))
     (defun ,(intern (format nil "UNPACK-~A" ace-type-name)) (buffer)
       (%unpack-access-allowed-callback-ace buffer ',ace-type-name))))

(def-access-allowed-callback-pack/unpack :access-allowed-callback access-allowed-callback-ace)
(def-access-allowed-callback-pack/unpack :access-denied-callback access-denied-callback-ace)
(def-access-allowed-callback-pack/unpack :system-audit-callback system-audit-callback-ace)


;; 2.4.4.15 SYSTEM_RESOURCE_ATTRIBUTE_ACE http://msdn.microsoft.com/en-us/library/hh877837.aspx
(defpacket system-resource-attribute-ace
  ((header ace-header :initform nil :initarg :header)
   (mask :uint32 :initform 0 :initarg :access-mask :accessor ace-access-mask)
   (sid (:uint8 0) :initform nil :initarg :sid :accessor ace-sid)
   (claim (:uint8 0) :initform nil :initarg :claim))
  (:packing 1))


(defun make-system-resource-attribute-ace (header mask claim)
  (declare (list mask))
  (make-instance 'system-resource-attribute-ace
                 :header header
                 :access-mask (apply #'pack-access-mask mask)
                 :sid (wellknown-sid :everyone)
                 :claim claim))

(defun pack-system-resource-attribute-ace (ace-flags mask claim)
  (let ((packed-sid (pack-sid :everyone))
        (packed-claim (pack-claim-attribute* claim)))
    (usb8 (pack (make-instance 'system-resource-attribute-ace 
                               :header
                               (make-ace-header (enum :system-resource-attribute *ace-types*)
                                                ace-flags 
                                                (+ (type-size 'system-resource-attribute-ace)
                                                   (length packed-sid)
                                                   (length packed-claim)))
                               :access-mask 
                               mask)
                'system-resource-attribute-ace)
          packed-sid
          packed-claim)))

(defun unpack-system-resource-attribute-ace (buffer)
  (multiple-value-bind (ace payload) (unpack buffer 'system-resource-attribute-ace)
    (multiple-value-bind (sid payload) (unpack-sid payload)
      (setf (slot-value (slot-value ace 'header) 'flags)
            (unpack-ace-flags (slot-value (slot-value ace 'header) 'flags))
            (slot-value ace 'sid) 
            sid
            (slot-value ace 'mask)
            (unpack-access-mask (slot-value ace 'mask))
            (slot-value ace 'claim) 
            (unpack-claim-attribute payload))
      ace)))


;; 2.4.4.8 ACCESS_ALLOWED_CALLBACK_OBJECT_ACE http://msdn.microsoft.com/en-us/library/cc230288.aspx
;; 2.4.4.9 ACCESS_DENIED_CALLBACK_OBJECT_ACE http://msdn.microsoft.com/en-us/library/cc230293.aspx
;; 2.4.4.14 SYSTEM_AUDIT_CALLBACK_OBJECT_ACE http://msdn.microsoft.com/en-us/library/cc230378.aspx
(defpackets (access-allowed-callback-object-ace access-denied-callback-object-ace system-audit-callback-object-ace)
  ((header ace-header :initform nil :initarg :header)
   (mask :uint32 :initform 0 :initarg :access-mask :accessor ace-access-mask)
   (flags :uint32 :initform 0 :initarg :flags )
   (object-type guid :initform nil :initarg :object-type :accessor ace-object)
   (inherited-object-type guid :initform nil :initarg :inherited-object-type :accessor ace-inherited-object)
   (sid (:uint8 0) :initform nil :initarg :sid :accessor ace-sid)
   (data (:uint8 0) :initform nil :initarg :data :accessor ace-conditional-expression))
  (:packing 1))

(defun %make-access-allowed-callback-object-ace (header mask flags object-type inherited-object-type sid data ace-type-name)
  (declare (list mask flags))
  (make-instance ace-type-name
		 :header header
		 :access-mask (apply #'pack-access-mask mask)
		 :flags (pack-access-object-ace-flags flags)
		 :object-type object-type
		 :inherited-object-type inherited-object-type
		 :sid sid
		 :data data))

(defun %pack-access-allowed-callback-object-ace (ace-flags mask flags object-type inherited-object-type sid data ace-type ace-type-name)
  (let ((packed-sid (pack-sid sid)))
    (usb8 (pack (%make-access-allowed-callback-object-ace 
		 (make-ace-header ace-type
				  ace-flags
				  (+ #.(enum :access-allowed-callback-object *ace-types*)
				     (length packed-sid)
				     (length data)))
		 mask
		 flags
		 object-type
		 inherited-object-type
		 nil 
		 nil
		 ace-type-name)
		ace-type-name)
	  packed-sid
	  (pack-conditional-expression data))))

(defun %unpack-access-allowed-callback-object-ace (buffer ace-type-name)
  (multiple-value-bind (ace payload) (unpack buffer ace-type-name)
    (multiple-value-bind (sid payload) (unpack-sid payload)
      (setf (slot-value (slot-value ace 'header) 'flags)
	    (unpack-ace-flags (slot-value (slot-value ace 'header) 'flags))
	    (slot-value ace 'sid) 
	    sid
	    (slot-value ace 'mask)
	    (unpack-access-mask (slot-value ace 'mask))
	    (slot-value ace 'data) 
	    (tokens-conditional-expression (unpack-conditional-expression payload)))
      ace)))

(defmacro def-access-allowed-callback-object-pack/unpack (ace-type ace-type-name)
  `(progn
     (defun ,(intern (format nil "PACK-~A" ace-type-name)) (ace-flags mask flags object-type inherited-object-type sid data)
       (%pack-access-allowed-callback-object-ace ace-flags mask flags object-type inherited-object-type sid data
						 ,(enum ace-type *ace-types*)
						 ',ace-type-name))
     (defun ,(intern (format nil "UNPACK-~A" ace-type-name)) (buffer)
       (%unpack-access-allowed-callback-object-ace buffer ',ace-type-name))))

(def-access-allowed-callback-object-pack/unpack :access-allowed-callback-object access-allowed-callback-object-ace)
(def-access-allowed-callback-object-pack/unpack :access-denied-callback-object access-denied-callback-object-ace)
(def-access-allowed-callback-object-pack/unpack :system-audit-callback-object system-audit-callback-object-ace)

(defun object-ace-p (ace)
  (let ((name (type-of ace)))
    (member name '(access-allowed-object-ace access-denied-object-ace
		   access-audit-object-ace system-audit-object-ace
		   access-allowed-callback-object-ace))))

(defun conditional-ace-p (ace)
  (let ((name (type-of ace)))
    (member name '(ACCESS-ALLOWED-CALLBACK-ace ACCESS-ALLOWED-CALLBACK-OBJECT-ace
		   ACCESS-DENIED-CALLBACK-ace ACCESS-DENIED-CALLBACK-OBJECT-ace
		   SYSTEM-AUDIT-CALLBACK-ace SYSTEM-AUDIT-CALLBACK-OBJECT-ace))))

(defun resource-ace-p (ace)
  (eq (type-of ace) 'system-resource-attribute-ace))

;; Public API functions 

(defparameter *ace-type-names*
  '((:access-allowed access-allowed-ace)
    (:access-denied access-denied-ace)
    (:access-audit access-audit-ace)
    (:access-allowed-object access-allowed-object-ace)
    (:access-denied-object access-denied-object-ace)
    (:access-audit-object access-audit-object-ace)
    (:system-audit-object system-audit-object-ace)
    (:system-mandatory-label system-mandataory-label-ace)
    (:system-scoped-policy-id system-scoped-policy-id-ace)
    (:access-allowed-callback access-allowed-callback-ace)
    (:access-denied-callback access-denied-callback-ace)
    (:access-allowed-callback-object access-allowed-calllback-object-ace)
    (:system-audit-callback system-audit-callback-ace)))

(defun make-ace (ace-type ace-flags mask sid &key object-type inherited-object-type flags conditional-expression)
  "Make an ACE for use within Lisp (rather than for getting packed)."
  (declare (keyword ace-type) (list ace-flags mask))
  (when (symbolp sid)
    (setf sid (wellknown-sid sid)))
  (apply #'make-instance 
	 (second (let ((ace-class-name (find ace-type *ace-type-names* :key #'car)))
			   (if ace-class-name
				   ace-class-name
				   (error "Invalid ACE type ~S. Wanted one of ~S"
						  ace-type (mapcar #'car *ace-type-names*)))))
	 :header (make-instance 'ace-header :flags ace-flags)
	 :access-mask mask
	 :sid sid
	 (append 
	  (when object-type
	    (list :object-type object-type))
	  (when inherited-object-type 
	    (list :inherited-object-type inherited-object-type))
	  (when flags
	    (list :flags flags))
	  (when conditional-expression
	    (list :data conditional-expression)))))

(defun pack-ace (ace-type ace-flags mask sid &key object-type inherited-object-type flags conditional-expression)
  "Pack an ACE object.

ACE-TYPE must be a keyword from the *ace-types* enumeration.

ACE-FLAGS must be a list of *ace-flags* flags.

MASK must be a list of *access-mask* flags.

SID should be a SID object.

For the ACE types which require them, you should provide the GUIDs OBJECT-TYPE and INHERITED-OBJECT-TYPE, the FLAGS from *access-allowed-object-ace-flags*. 

For callback-type ACEs (conditional ACEs) you should provide a DATA array which was generated by a call to PACK-CONDITIONAL-EXPRESSION."
  (when (symbolp sid)
    (setf sid (wellknown-sid sid)))
  (let ((data (conditional-expression-tokens conditional-expression)))
    (ecase ace-type
      (:access-allowed (pack-access-allowed-ace ace-flags mask sid))
      (:access-denied (pack-access-denied-ace ace-flags mask sid))
      (:system-audit (pack-system-audit-ace ace-flags mask sid))
      (:access-allowed-object (pack-access-allowed-object-ace ace-flags mask object-type inherited-object-type sid flags))
      (:access-denied-object (pack-access-denied-object-ace ace-flags mask object-type inherited-object-type sid flags))
      (:system-audit-object (pack-system-audit-object-ace ace-flags mask object-type inherited-object-type sid flags))
      (:access-allowed-callback (pack-access-allowed-callback-ace ace-flags mask sid data))
      (:access-denied-callback (pack-access-denied-callback-ace ace-flags mask sid data))
      (:access-allowed-callback-object (pack-access-allowed-callback-object-ace ace-flags mask flags object-type inherited-object-type sid data))
      (:access-denied-callback-object (pack-access-denied-callback-object-ace ace-flags mask flags object-type inherited-object-type sid data))
      (:system-audit-callback (pack-system-audit-callback-ace ace-flags mask sid data))
      (:system-audit-callback-object (pack-system-audit-callback-object-ace ace-flags mask flags object-type inherited-object-type sid data))
      (:system-mandatory-label (pack-system-mandatory-label-ace ace-flags mask sid))
      (:system-resource-attribute (pack-system-resource-attribute-ace ace-flags mask sid data))
      (:system-scoped-policy-id (pack-system-scoped-policy-id-ace ace-flags mask sid)))))

(defun pack-ace* (ace)
  (let ((ace-type (enum-id (slot-value (slot-value ace 'header) 'type) *ace-types*)))
    (pack-ace ace-type 
              (slot-value (slot-value ace 'header) 'flags)
              (slot-value ace 'mask)
              (slot-value ace 'sid)
              :object-type (when (object-ace-p ace) (slot-value ace 'object-type))
              :inherited-object-type (when (object-ace-p ace) (slot-value ace 'inherited-object-type))
              :flags (when (object-ace-p ace) (slot-value ace 'flags))
              :conditional-expression (when (conditional-ace-p ace) (slot-value ace 'data)))))
    
(defun unpack-ace (buffer)
  "Unpack an ACE."
  (multiple-value-bind (header payload) (unpack buffer 'ace-header)
    (declare (ignore payload))
    ;; dispatch to the appropriate unpacker depending on the type
    (let ((ace-type (enum-id (slot-value header 'type)
			     *ace-types*)))
      (ecase ace-type
	(:access-allowed (unpack-access-allowed-ace buffer))
	(:access-denied (unpack-access-denied-ace buffer))
	(:system-audit (unpack-system-audit-ace buffer))
	(:access-allowed-object (unpack-access-allowed-object-ace buffer))
	(:access-denied-object (unpack-access-denied-object-ace buffer))
	(:system-audit-object (unpack-system-audit-object-ace buffer))
	(:access-allowed-callback (unpack-access-allowed-callback-ace buffer))
	(:access-denied-callback (unpack-access-denied-callback-ace buffer))
	(:access-allowed-callback-object (unpack-access-allowed-callback-object-ace buffer))
	(:access-denied-callback-object (unpack-access-denied-callback-object-ace buffer))
	(:system-audit-callback (unpack-system-audit-callback-ace buffer))
	(:system-audit-callback-object (unpack-system-audit-callback-object-ace buffer))
	(:system-mandatory-label (unpack-system-mandatory-label-ace buffer))
	(:system-resource-attribute (unpack-system-resource-attribute-ace buffer))
	(:system-scoped-policy-id (unpack-system-scoped-policy-id-ace buffer))))))



;; ----------- conditional ACE data ------------


;; 2.4.4.17 Conditional ACEs http://msdn.microsoft.com/en-us/library/hh877827.aspx
(defparameter *ace-condition-signature* #(#x61 #x72 #x74 #x78))

;; 2.4.4.17.5 Literal Tokens http://msdn.microsoft.com/en-us/library/hh877858.aspx
(defenum *ace-conditional-literals*
  ((:Invalid #x00
    "Padding value.")
   (:int8 #x01
    "1 QWORD, least significant byte first, for the value, 2's complement, -128 to +127.
1 BYTE for sign. (possible values for sign in the following table) .
1 BYTE for base. (possible values for base in the following table).")
   (:int16 #x02
    "1 QWORD, least significant byte first, 2's complement, -32768 to +32767.
1 BYTE for sign.
1 BYTE for base.")
   (:int32 #x03
    "1 QWORD, least significant byte first, 2's complement.
1 BYTE for sign.
1 BYTE for base.")
   (:int64 #x04
"1 QWORD, least significant byte first, 2's complement.
1 BYTE for sign.
1 BYTE for base.")
   (:String #x10
"1 DWORD for the length in bytes.
1 WORD for each Unicode character. Characters are stored LSB first. Strings are not null-terminated.")
   (:Octet-String #x18
"Custom data is represented as a contiguous sequence of bytes.
1 DWORD for the length in bytes.
1 BYTE for each data octet.")
   (:Composite #x50
"1 DWORD that specifies the entire length in bytes of the entire set of elements.
List type--can be heterogeneous. Elements are stored in contiguous fashion according to the built-in data type storage rules.")
   (:SID #x51
"1 DWORD that specifies the entire length in bytes of the SID.
SID in binary representation (as specified in section 2.4.2.2.)")))

(defenum *ace-conditional-base*
  ((:Octal #x01 "Octal")
   (:Decimal #x02 "Decimal")
   (:Hexadecimal #x03 "Hexadecimal")))

(defenum *ace-conditional-sign*
  ((:plus #x01 "Plus sign in condition.")
   (:minus #x02 "Minus sign in condition.")
   (:None #x03 "No sign in condition.")))

(defun ace-condition-sign (type)
  (enum type *ace-conditional-sign*))


(defun pack-conditional-literal (token-type token-data &optional sign-code)
  "Returns the packed buffer"
  (ecase token-type
    (:invalid (usb8* 0))
    ((:int8 :int16 :int32 :int64)
     (usb8 (pack (enum token-type *ace-conditional-literals*) :uint8)
	   (pack token-data :int64)
	   (pack (if sign-code 
		     (ace-condition-sign :sign-code)
		     (ace-condition-sign :none))
		 :uint8)
	   (pack #.(enum :decimal *ace-conditional-base*) :uint8)))
    (:string
     (let ((val (pack token-data :wstring)))
       (usb8 (pack #.(enum :string *ace-conditional-literals*) :uint8)
	     (pack (length val) :uint32)
	     val)))
    (:octet-string 
     (usb8 (pack #.(enum :octet-string *ace-conditional-literals*) :uint8)
	   (pack (length token-data) :uint32)
	   token-data))
    (:composite 
     ;; token-data should be a list of (token-type value) pairs
     (let ((vals
	    (apply #'usb8
		   (mapcar (lambda (pair)
			     (destructuring-bind (token-type token-data) pair
			       (pack-conditional-literal token-type token-data)))
			   token-data))))
       (usb8 (pack #.(enum :composite *ace-conditional-literals*) :uint8)
	     (pack (length vals) :uint32)
	     vals)))	      
    (:sid 
     (let ((packed-sid (pack-sid token-data)))
       (usb8 (pack #.(enum :sid *ace-conditional-literals*) :uint8)
	     (pack (length packed-sid) :uint32)
	     packed-sid)))))

(defun unpack-conditional-literal (buffer)
  "Unpacks a conditional literal value. returns the value and the remainder of the buffer"
  (let ((token-type (enum-id (elt buffer 0) *ace-conditional-literals*)))
    (unless token-type (error "No known type ~S" (elt buffer 0)))
    (ecase token-type
      (:invalid 
;;       (values (list :invalid) (subseq buffer 1)))
       (values nil nil))
      ((:int8 :int16 :int32 :int64)
       (values (list token-type (unpack (subseq* buffer 1 8) :int64))
	       (subseq buffer 11)))
      (:string
       (let ((length (unpack (subseq buffer 1 5) :uint32)))
	 (values (list :string (unpack (subseq* buffer 5 length) :wstring))
		 (subseq buffer (+ length 5)))))
      (:octet-string 
       (let ((length (unpack (subseq buffer 1 5) :uint32)))
	 (values (list :octet-string (subseq* buffer 5 length))
		 (subseq buffer (+ 5 length)))))
      (:composite 
       (let ((length (unpack (subseq buffer 1 5) :uint32)))
	 (do ((buffer2 (subseq buffer 5))
	      (offset 0)
	      (vals nil))
	     ((>= offset length) 
	      (values (list :composite (nreverse vals))
		      (subseq buffer (+ 5 length))))
	   (multiple-value-bind (val payload) (unpack-conditional-literal buffer2)
	     (push val vals)
	     (incf offset (- (length buffer2) (length payload)))
	     (setf buffer2 payload)))))
      (:sid 
       (let ((length (unpack (subseq buffer 1 5) :uint32)))
	 (values (list :sid (unpack-sid (subseq* buffer 5 length)))
		 (subseq buffer (+ length 5))))))))
           
;; 2.4.4.17.6 Relational Operator Tokens http://msdn.microsoft.com/en-us/library/hh877828.aspx
(defenum *unary-relational-operators*
  ((:Member-of #x89
   "SDDL Form: Member_ofoperand
Return TRUE if SIDs[] array (section 2.5.2) contains all of the SIDs in the operand; FALSE otherwise.")
   (:Device-Member-of #x8a
    "SDDL Form: Device_Member_ofoperand
Return TRUE if DeviceSIDs[] array (section 2.5.2) contains all of the SIDs in the operand; FALSE otherwise.")
   (:Member-of-Any #x8b
    "SDDL Form: Member_of_Anyoperand
Return TRUE if SIDs[] array (section 2.5.2) contains any of the SIDs in the operand; FALSE otherwise.")
   (:Device-Member-of-Any #x8c
"SDDL Form: Device_Member_of_Anyoperand
Return TRUE if DeviceSIDs[] array (section 2.5.2) contains any of the SIDs in the operand; FALSE otherwise.")
   (:Not-Member-of #x90
"SDDL Form: Not_Member_ofoperand Logical inverse of Member_of.")
   (:Not-Device-Member-of #x91
"SDDL Form: Not_Device_Member_ofoperand Logical inverse of Device_Member_of.")
   (:Not-Member-of-Any #x92
"SDDL Form: Not_Member_of_Anyoperand Logical inverse of Not_Member_of_Any.")
   (:Not-Device-Member-of-Any #x93
    "SDDL Form: Not_Device_Member_of_Anyoperand Logical inverse of Device_Member_of_Any.")))

(defenum *binary-relational-operators*
  ((:equals #x80
"SDDL form: (LHS == RHS)
MUST evaluate to TRUE if the argument on the RHS evaluates to the exact value (single or set value) of the argument on the LHS; otherwise, FALSE.")
   (:not-equals #x81
"SDDL form: (LHS != RHS)
MUST evaluate to FALSE if the argument on the RHS evaluates to the exact value of the argument on LHS; otherwise, TRUE.")
   (:less-than #x82
"SDDL form: (LHS < RHS)
MUST evaluate to TRUE if the argument on the LHS is less than the argument on the RHS; otherwise, FALSE.")
   (:less-than-equals #x83
"SDDL form: (LHS <= RHS)
MUST evaluate to TRUE if the argument on the LHS is less than, or equal to, the argument on the RHS; otherwise, FALSE.")
   (:greater-than #x84
"SDDL form: (LHS > RHS)
MUST evaluate to TRUE if the argument on the LHS is greater than the argument on the RHS; otherwise, FALSE.")
   (:greater-than-equals #x85
"SDDL form: (LHS >= RHS)
MUST evaluate to TRUE if the argument on the LHS is greater than, or equal to, the argument on the RHS; otherwise, FALSE.")
   (:Contains #x86
"SDDL Form: LHSContainsRHS
LHS MUST be an attribute name in simple or @Prefixed form.
RHS MUST be a set of one or more literals, or an attribute name in @Prefixed form.
MUST evaluate to TRUE if the value(s) for the specified LHS includes value(s) identical to each of the value(s) specified by the RHS; otherwise, FALSE.<52>")
   (:Any-of #x88
"SDDL Form: LHSAny_ofRHS
LHS MUST be an attribute name in simple or @Prefixed form.RHS MUST be a set of one or more literals, or an attribute name in @Prefixed form.
MUST evaluate to TRUE if the RHS value set is a superset of the value(s) of the specified LHS; otherwise, FALSE. RHS can be either a set or a single value.<53>")
   (:Not-Contains #x8e
"SDDL Form: LHSNot_ContainsRHS
Logical inverse of Contains.")
   (:Not-Any-of #x8f
"Form: LHSNot_Any_ofRHS
Logical inverse of Any_of.")))

;; 2.4.4.17.7 Logical Operator Tokens http://msdn.microsoft.com/en-us/library/hh877842.aspx
(defenum *unary-logical-operators*
  ((:Exists #x87
"SDDL Form: Existsoperand
If the type of the operand is \"Local Attribute\"
  If the value is non-null return TRUE
  Else return FALSE
Else if the type of the operand is \"Resource Attribute\"
  Return  TRUE if value is non-null; FALSE otherwise.
Else return Error")
   (:Not-Exists #x8d
"SDDL Form: Not_Existsoperand Logical inverse of Exists.")
   (:NOT #xa2
"If the logical value of the operand is TRUE 
  Return FALSE
If the logical value of the operand is FALSE
  Return TRUE
If the logical value of the operand is UNKNOWN
  Return UNKNOWN")))

(defenum *binary-logical-operators*
  ((:AND #xa0
"SDDL Form: LHS&&RHS If the logical value of either operand is FALSE
  Return FALSE
Else if the logical value of either operand is UNKNOWN 
  Return UNKNOWN
Else Return TRUE")
   (:OR #xa1
"SDDL Form: LHS||RHS
If the logical value of either operand is TRUE
  Return TRUE
Else if the logical value of either operand is UNKNOWN 
  Return UNKNOWN
Else Return FALSE")))


(defparameter *ace-conditional-operators*
  (append *unary-relational-operators*
	  *binary-relational-operators*
	  *unary-logical-operators*
	  *binary-logical-operators*))

;; 2.4.4.17.8 Attribute Tokens http://msdn.microsoft.com/en-us/library/hh877832.aspx
(defenum *attribute-tokens*
  ((:Local #xf8
"Encoding same as Unicode string.
Lookup based on string name.")
   (:User #xf9
"Encoding same as Unicode String.
Lookup based on string name.")
   (:Resource #xfa
"Encoding same as Unicode String.
Lookup based on string name.")
   (:Device #xfb
"Encoding same as Unicode String.
Lookup based on string name.")))

(defun pack-attribute-token (attribute-type string)
  "Pack an attribute token"
  (usb8 (pack (enum attribute-type *attribute-tokens*) :uint8)
	(pack (* (length string) 2) :int32)
	(pack string :wstring)))
  
(defun unpack-attribute-token (buffer)
  "unpack an attrbiute token"
  (multiple-value-bind (attribute-type payload1) (unpack buffer :uint8)
    (multiple-value-bind (length payload2) (unpack payload1 :uint32)
      (multiple-value-bind (string payload3) (unpack (subseq payload2 0 length) :wstring)
	(declare (ignore payload3))
	(values (list (enum-id attribute-type *attribute-tokens*) string)
		(subseq payload2 length))))))


;; --------

(defun pack-conditional-token (token &optional value)
  ;; if it is an operator then just pack its code
  ;; if it is a literal then pack its value
  ;; if it is an attribute then pack that
  (cond
    ((assoc token *ace-conditional-operators*)
     (pack (enum token *ace-conditional-operators*) :uint8))
    ((assoc token *ace-conditional-literals*)
     (pack-conditional-literal token value))
    ((assoc token *attribute-tokens*)
     (pack-attribute-token token value))
    (t (error "Must be an operator, a literal or an attribute"))))

(defun unpack-conditional-token (buffer)
  (multiple-value-bind (token-code payload) (unpack buffer :uint8)
    ;; if it is an operator then return that and the payload
    (let ((x (find token-code *ace-conditional-operators* :key #'second)))
      (if x
	  (values (first x) payload)
	  ;; if it is a literal then unpack the literal
	  (let ((x (find token-code *ace-conditional-literals* :key #'second)))
	    (if x 
		(unpack-conditional-literal buffer)
		;; if it is an attribute then unpack the attribute
		(let ((x (find token-code *attribute-tokens* :key #'second)))
		  (if x 
		      (unpack-attribute-token buffer)
		      (error "Unknown token-code ~A" token-code)))))))))

;; Public API functions 
(defun conditional-expression-tokens (expression)
  "Generate a flat list of expression tokens from an S-expression"
  (cond
    ((null expression) nil)
    ((integerp expression) (list (list :int64 expression)))
    ((atom expression) (error "Unknown expression ~S" expression))
    ((assoc (car expression) (append *unary-logical-operators* *unary-relational-operators*))
     ;; (<operator> <arg>)
     (append (conditional-expression-tokens (cadr expression))
	     (list (car expression))))
    ((assoc (car expression) (append *binary-logical-operators* *binary-relational-operators*))
     ;; (<operator> <arg1> <arg2>)
     (append (conditional-expression-tokens (cadr expression))
	     (conditional-expression-tokens (caddr expression))					  
	     (list (car expression))))
    ((assoc (car expression) *attribute-tokens*)
     ;; (<attribute-type> <value>)
     (list expression))
    ((assoc (car expression) *ace-conditional-literals*)
     ;; (<literal type> <value>)
     (list expression))
    (t (Error "Unknown expression ~S" expression))))

(defun tokens-conditional-expression (tokens)
  "Converts a list of tokens into an s-expression"
  (do ((stack nil)
       (tokens tokens))
      ((null tokens) (car stack))
    (let ((token (pop tokens)))
      (cond
	((and (atom token) (assoc token (append *binary-relational-operators* *binary-logical-operators*)))
	 ;;; :and :equals ...
     (let ((x (pop stack))
           (y (pop stack)))
       (push (list token y x) stack)))
	((and (atom token) (assoc token (append *unary-relational-operators* *unary-logical-operators*)))
	 ;;; :and :equals ...
	 (push (list token (pop stack)) stack))
	((assoc (car token) *attribute-tokens*)
	 ;; attbiute
	 (push token stack))
	((assoc (car token) *ace-conditional-literals*)
	 ;; literal token. just push onto the form
	 (push token stack))
	(t (error "Unknown token ~S" token))))))

(defun pack-conditional-expression (tokens)
  "Pack a series of expression tokens together"
  (do ((buffer (usb8 *ace-condition-signature*))
       (tokens tokens (cdr tokens)))
      ((null tokens) 
       ;; ensure the buffer is padded to a length multiple of 4
       (pad* buffer 4))
    (let ((token (car tokens)))
      (setf buffer 
	    (usb8 buffer
		  (if (atom token)
		      (pack-conditional-token token)		
		      (apply #'pack-conditional-token (car token) (cdr token))))))))

(defun unpack-conditional-expression (buffer)
  "Extract the expresion tokens"
  (do ((buffer (subseq buffer 4))
       (vals nil))
      ((= (length buffer) 0) (nreverse vals))
    (multiple-value-bind (val payload) (unpack-conditional-token buffer)
      (when val
	(push val vals))
      (setf buffer payload))))


(defun ace-flags (ace)
  (slot-value (slot-value ace 'header) 'flags))
(defun (setf ace-flags) (value ace)
  (setf (slot-value (slot-value ace 'header) 'flags)
        value))






;; -------------- test suite below -------------------------











(defun test-expression (expr)
  (let ((buffer (pack-conditional-expression
                 (conditional-expression-tokens expr))))
    (hd buffer)
    (tokens-conditional-expression
     (unpack-conditional-expression buffer))))

;; 2.4.4.17.9 Examples: Conditional Expression Binary Representation
;; http://msdn.microsoft.com/en-us/library/hh877829.aspx
(defun test-expression-1 ()
  (test-expression '(:equals (:local "Title") (:string "VP"))))

(defun test-expression-2 ()
  (test-expression 
   '(:and (:or (:equals (:user "smartcard") (:int64 1))
               (:equals (:device "managed") (:int64 1)))
          (:any-of (:resource "Dept")
                   (:composite ((:string "sales") (:string "HR")))))))

(defun test-expression-3 ()
  (test-expression 
   `(:or (:greater-than-equals (:user "clearanceLevel") (:resource "requiredClearance"))
         (:member-of (:composite ((:sid ,(wellknown-sid :builtin-administrators))))))))


(defun prefix-postfix (form)
  (if (atom form) 
      (list form)
      (append (mapcan #'prefix-postfix (cdr form))
	      (list (car form)))))

  
