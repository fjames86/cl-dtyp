
;;;; MS-DTYP Microsoft Data Types

;;; 
;;; Defines the data types commonly used in Microsoft protocols.
;;;
;;; Copyright (C) Frank James, July 2014
;;;

(in-package :cl-dtyp)

;; 2.5.1.1 Syntax http://msdn.microsoft.com/en-us/library/cc230374.aspx
(defparameter *sddl-alias*
  '((:DOMAIN-ADMINS "DA")
    (:DOMAIN-GUESTS "DG")
    (:DOMAIN-USERS "DU")
    (:ENTERPRISE-DOMAIN-CONTROLLERS "ED")
    (:DOMAIN-DOMAIN-CONTROLLERS "DD")
    (:DOMAIN-COMPUTERS "DC")
    (:BUILTIN-ADMINISTRATORS "BA")
    (:BUILTIN-GUESTS "BG")
    (:BUILTIN-USERS "BU")
    (:ADMINISTRATOR "LA")
    (:GUEST "LG")
    (:ACCOUNT-OPERATORS "AO")
    (:BACKUP-OPERATORS "BO")
    (:PRINTER-OPERATORS "PO")
    (:SERVER-OPERATORS "SO")
    (:AUTHENTICATED-USERS "AU")
    (:PRINCIPAL-SELF "PS")
    (:CREATOR-OWNER "CO")
    (:CREATOR-GROUP "CG")
    (:LOCAL-SYSTEM "SY")
    (:POWER-USERS "PU")
    (:EVERYONE "WD")
    (:REPLICATOR "RE")
    (:INTERACTIVE "IU")
    (:NETWORK "NU")
    (:SERVICE "SU")
    (:RESTRICTED-CODE "RC")
    (:WRITE-RESTRICTED_CODE "WR")
    (:ANONYMOUS "AN")
    (:SCHEMA-ADMINISTRATORS "SA")
    (:CERT-PUBLISHERS "CA")
    (:RAS-SERVERS "RS")
    (:ENTERPRISE-ADMINS "EA")
    (:GROUP-POLICY-CREATOR-OWNER "PA")
    (:ALIAS-PREW2KCOMPACC "RU")
    (:LOCAL-SERVICE "LS")
    (:NETWORK-SERVICE "NS")
    (:REMOTE-DESKTOP "RD")
    (:NETWORK-CONFIGURATION-OPS "NO")
    (:PERFMON-USERS "MU")
    (:PERFLOG-USERS "LU")
    (:IIS-USERS "IS")
    (:CRYPTO-OPERATORS "CY")
    (:OWNER-RIGHTS "OW")
    (:EVENT-LOG-READERS "ER")
    (:ENTERPRISE-RO-DCS "RO")
    (:CERTSVC-DCOM-ACCESS "CD")
    (:ALL-APP-PACKAGES "AC")
    (:REMOTE-ACCESS-SERVERS "RA")
    (:RDS-ENDPOINT-SERVERS "ES")
    (:RDS-MANAGEMENT-SERVERS "MS")
    (:USER-MODE-DRIVERS "UD")
    (:HYPER-V-ADMINS "HA")
    (:CLONEABLE-CONTROLLERS "CN")
    (:ACCESS-CONTROL-ASSISTANCE-OPS "AA")
    (:REMOTE-MANAGEMENT-USERS "RM")
    (:ML-LOW "LW")
    (:ML-MEDIUM "ME")
    (:ML-MEDIUM-PLUS "MP")
    (:ML-HIGH "HI")
    (:ML-SYSTEM "SI")))

(defparameter *sddl-ace-types*
  '((:Access-Allowed "A")
    (:Access-Denied "D")
    (:system-Audit "AU")
    (:Access-Allowed-object "OA")
    (:Access-Denied-object "OD")
    (:system-audit-Object "OU")
    (:system-Mandatory-Label "ML")
    (:system-scoped-policy-id "SP")
    (:Access-Allowed-Callback "XA" #x9)
    (:Access-Denied-Callback "XD" #xA)
    (:Access-Allowed-Callback-object "XU" #xB)
    (:system-Audit-Callback "ZA" #xD)
    (:system-resource-attribute "RA")
    (:system-scoped-policy-id "SP")))

(defparameter *sddl-ace-type-names*
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
    (:system-audit-callback system-audit-callback-ace)
    (:system-resource-attribute system-resource-attribute-ace)
    (:system-scoped-policy-id system-scoped-policy-id-ace)))

(defparameter *sddl-access-mask-names*
  '((:generic-read "GR")
    (:generic-write "GW")
    (:generic-execute "GX")
    (:generic-all "GA")
    (:maximum-allowed "MA")
    (:access-system-security "AS")
    (:synchronize "SY")
    (:write-owner "WO")
    (:write-dacl "WD")
    (:read-control "RC")
    (:delete "DE")
    (:file-all-access "FA")
    (:file-execute "FX")
    (:file-write "FW")
    (:file-read "FR")
    (:key-all-access "KA")
    (:key-read "KR")
    (:key-execute "KX")
    (:key-write "KW")
    (:control-access "CR")
    (:list-object "LO")
    (:delete-tree "DT")
    (:write-property "WP")
    (:read-property "RP")
    (:self-write "SW")
    (:list-children "LC")
    (:delete-child "DC")
    (:create-child "CC")))

(defun ace-type (ace)
  "Get the ace-type (keyword) from an ACE instance."
  (first (find (type-of ace) *sddl-ace-type-names* :key #'second)))
(defun ace-type-name (ace)
;;  (warn "This funciton is deprecated - use ACE-TYPE instead")
  (ace-type ace))
(defun ace-type-name-kw (ace)
;;  (warn "This funciton is deprecated - use ACE-TYPE instead")
  (ace-type ace))


(defun sddl-sid-string (sid)
  (if sid
      (let ((str (sid-string sid)))
        (or (let ((kw (wellknown-sid-by-string str)))
              (when kw (second (assoc kw *sddl-alias*))))
            (format nil "SID(~A)" str)))
      ""))

(defun sddl-string-sid (string)
  (let ((wk (first (find string *sddl-alias* :key #'second :test #'string-equal))))
    (if wk
        (wellknown-sid wk)
        (string-sid string))))
        
(defun sddl-control-code-string (control-code)
  (let ((flags (unpack-flags control-code *sd-control-codes*)))
    (cond
      ((intersection '(:sacl-protected :dacl-protected) flags)
       "P")
      ((intersection '(:sacl-computed-inheritance-required :dacl-computed-inheritance-required) flags)
       "AR")
      ((intersection '(:sacl-autoinherited :dacl-autoinherited) flags)
       "AI")
      (t ""))))
    
(defun ace-string (ace)
  "Format an ACE as a string."
  (with-output-to-string (s)
    ;; ace-type
    (format s "~A;" (second (find (ace-type-name-kw ace) *sddl-ace-types* :key #'car)))
    ;; ace-flags
    (dolist (flag (slot-value (slot-value ace 'header) 'flags))
      (format s "~A"
	      (ecase flag
		(:CONTAINER-INHERIT-ACE "CI") 
		(:FAILED-ACCESS-ACE-FLAG "FA")
		(:INHERIT-ONLY-ACE "IO") 
		(:INHERITED-ACE "ID")
		(:NO-PROPAGATE-INHERIT-ACE "NP") 
		(:OBJECT-INHERIT-ACE "OI") 
		(:SUCCESSFUL-ACCESS-ACE-FLAG "SA"))))
    (format s ";")
    ;; access-mask 
    (dolist (mask (slot-value ace 'mask))      
      (format s "~A" (second (find mask *sddl-access-mask-names* :key #'car))))
;;    (format s "0x~X" (apply #'pack-access-mask (slot-value ace 'mask)))
    (format s ";")
    ;; object-type
    (when (and (object-ace-p ace) (slot-value ace 'object-type))
      (format s "~A" (guid-string (slot-value ace 'object-type))))
    (format s ";")
    ;; inherited-object-type
    (when (and (object-ace-p ace) (slot-value ace 'inherited-object-type))
      (format s "~A" (guid-string (slot-value ace 'inherited-object-type))))
    (format s ";")
    ;; sid
    (format s "~A" (sddl-sid-string (slot-value ace 'sid)))
    ;; if conditional then write the condituional expression
    (when (conditional-ace-p ace)
      (format s ";(")
      (conditional-expression-string (slot-value ace 'data) s)
      (format s ")"))))
    	
(defun acl-string (control-codes acl)
  (with-output-to-string (s)
    ;; flags    
    (when (intersection '(:sacl-protected :dacl-protected) control-codes)
      (format s "P"))
    (when (intersection '(:sacl-computed-inheritance-required :dacl-computed-inheritance-required) control-codes)
      (format s "AR"))
    (when (intersection '(:sacl-autoinherited :dacl-autoinherited) control-codes)
      (format s "AI"))
    
    ;; aces
    (dolist (ace acl)
      (format s "(~A)" (ace-string ace)))))

(defparameter *sddl-operator-strings* 
  '((:and "&&")
    (:or "||")
    (:not "!")
    (:member-of "Member_of")
    (:device-member-of "Device_Member_of")
    (:member-of-any "Member_of_Any")
    (:device-member-of-any "Device_Member_of_Any")
    (:not-member-of "Not_Member_of")
    (:not-device-member-of "Not_Device_Member_of")
    (:not-member-of-any "Not_Member_of_Any")
    (:not-device-member-of-any "Not_Device_Member_of_Any")
    (:equals "==")
    (:not-equals "!=")
    (:less-than "<")
    (:less-than-equals "<=")
    (:greater-than ">")
    (:greater-than-equals ">=")
    (:contains "Contains")
    (:any-of "Any_of")
    (:not-contains "Not_Contains")
    (:not-any-of "Not_Any_of")))

(defun conditional-expression-string (expression stream)
  (labels ((print-literal (type val)
             (ecase type
               ((:int8 :int16 :int32 :int64)
                (format stream "0x~X" val))
               (:string 
                (format stream "\"~A\"" val))
               (:octet-string 
                (format stream "#")
                (dotimes (i (length val))
                  (format stream "~2,'0X" (elt val i))))
               (:composite 
                (format stream "{")
                (let ((printed nil))
                  (dolist (x val)
                    (when printed (format stream ","))
                    (print-literal (car x) (cadr x))
                    (setf printed t)))
                (format stream "}"))
               (:sid 
                (format stream "SID(~A)" (sid-string val))))))
    (let ((expr (car expression)))
      (cond 
        ;; literals
        ((assoc expr *ace-conditional-literals*)
         (print-literal expr (cadr expression))
         (format stream " "))
        ;; binary operators
        ((assoc expr (append *binary-relational-operators* *binary-logical-operators*))
         (conditional-expression-string (cadr expression) stream)
         (format stream "~A " (second (assoc expr *sddl-operator-strings*)))
         (conditional-expression-string (caddr expression) stream))
        ;; unary operators
        ((assoc expr (append *unary-relational-operators* *unary-logical-operators*))
         (format stream "~A " (second (assoc expr *sddl-operator-strings*)))
         (conditional-expression-string (cadr expression) stream))
        ;; attributes
        ((assoc expr *attribute-tokens*)
         (format stream "@~A.~A "
                 (ecase expr
                   (:user "User")
                   (:local "Local")
                   (:resource "Resource")
                   (:device "Device"))
                 (cadr expression)))))))
			    


(defun security-descriptor-string (sd)
  "Convert a SECURITY-DESCRIPTOR object to its SDDL representation"
  (with-output-to-string (s)
    (format s "O:~A;G:~A;D:~A;S:~A" 
            (sddl-sid-string (slot-value sd 'owner-sid))
            (sddl-sid-string (slot-value sd 'group-sid))
            (acl-string (slot-value sd 'control) (slot-value sd 'dacl))
            (acl-string (slot-value sd 'control) (slot-value sd 'sacl)))))


;; parse ACEs

(defun parse-ace-type (string)
  (let ((ace-type (first (find string *sddl-ace-types* :key #'second :test #'string-equal))))
    (unless ace-type (error "Unknown ace type string ~S" string))
    ace-type))

(defun parse-ace-flags (string)
  (do ((flags nil)
       (offset 0 (+ offset 2))
       (len (length string)))
      ((>= offset len) flags)
    (let ((str (subseq* string offset 2)))
      (push 
       (cond
         ((string-equal str "CI") :CONTAINER-INHERIT-ACE)
         ((string-equal str "FA") :FAILED-ACCESS-ACE-FLAG)
         ((string-equal str "IO") :INHERIT-ONLY-ACE)
         ((string-equal str "ID") :INHERITED-ACE)
         ((string-equal str "NP") :NO-PROPAGATE-INHERIT-ACE)
         ((string-equal str "OI") :OBJECT-INHERIT-ACE) 
         ((string-equal str "SA") :SUCCESSFUL-ACCESS-ACE-FLAG)
         (t (error "Unknown flag string ~A" str)))
       flags))))

(defun parse-ace-rights (string)
  (do ((flags nil)
       (offset 0 (+ offset 2))
       (len (length string)))
      ((>= offset len) flags)
    (let ((str (subseq* string offset 2)))
      (let ((mask (first (find str *sddl-access-mask-names* :key #'second :test #'string-equal))))
        (unless mask (error "Unknown mask string ~S" str))
        (push mask flags)))))

;; this is a massive ball ache... do we need a full on yacc parser?
(defun string-conditional-expression (string)
  (declare (ignore string))
  (error "FIXME: please implement a conditional expression parser"))


(defun string-ace (string)
  "Parse an ACE from a string in SDDL format."
  (let ((offset 0)
        type flags mask object-type inherited-object-type sid cond-expr)
    (labels ((pos (fn)
               (let ((p (position #\; string :start offset)))
                 (let ((str (subseq string offset p)))
                   (prog1
                       (if (string= str "")
                           nil
                           (funcall fn str))
                     (if p
                         (setf offset (1+ p))
                         (setf offset nil)))))))
      (setf type (pos #'parse-ace-type))
      (setf flags (pos #'parse-ace-flags))
      (setf mask (pos #'parse-ace-rights))
      (setf object-type (pos #'string-guid))
      (setf inherited-object-type (pos #'string-guid))
      (setf sid (pos #'sddl-string-sid))
      (when offset
        (setf cond-expr (pos #'string-conditional-expression)))
      (make-ace type flags mask sid 
                :object-type object-type 
                :inherited-object-type inherited-object-type
                :conditional-expression cond-expr))))
      
(defun sddl-get-ace-offset (string start)
  (do ((offset start (1+ offset))
       (count 1)
       (len (length string)))
      ((= count 0) offset)
    (if (= offset len) (return-from sddl-get-ace-offset nil))
    (let ((c (char string offset)))
      (cond
        ((char= c #\()
         (incf count))
        ((char= c #\))
         (decf count))))))

(defun sddl-string-acl (string)
  (do ((aces nil)
       (offset 0)
       (len (length string)))
      ((>= offset len) (nreverse aces))
    (let ((c (char string offset)))
      (if (char= c #\()
          ;; start of ace. find end
          (let ((end (sddl-get-ace-offset string (1+ offset))))
            (push (string-ace (subseq string (1+ offset) (1- end))) aces)
            (setf offset end))
          ;; keep moving
          (incf offset)))))

(defun string-security-descriptor (string)
  (multiple-value-bind (match matches) (cl-ppcre:scan-to-strings "O:(.*);G:(.*);D:(.*);S:(.*)" string)
    (unless match (error "Unable to parse ~S" string))
    (make-security-descriptor nil 
                              (sddl-string-sid (elt matches 0))
                              (sddl-string-sid (elt matches 1))
                              (sddl-string-acl (elt matches 3))
                              (sddl-string-acl (elt matches 2)))))

