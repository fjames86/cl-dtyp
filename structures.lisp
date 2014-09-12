
;;;; MS-DTYP Microsoft Data Types

;;; 
;;; Defines the data types commonly used in Microsoft protocols.
;;;
;;; Copyright (C) Frank James, July 2014
;;;



(in-package :cl-dtyp)


;; 2.3.4.2 GUID--Packet Representation http://msdn.microsoft.com/en-us/library/dd302644.aspx
(defpacket guid
  ((data1 :uint32 :initform 0 :initarg :data1)
   (data2 :uint16 :initform 0 :initarg :data2)
   (data3 :uint16 :initform 0 :initarg :data3)
   (data4 :uint64 :initform 0 :initarg :data4))
  (:packing 1))

(defun guid-string (guid)
  "Convert a GUID to a string"
  (declare (guid guid))
  (let ((d4 (slot-value guid 'data4)))
    (format nil "~8,'0X-~4,'0X-~4,'0X-~4,'0X-~12,'0X"
            (slot-value guid 'data1)
            (slot-value guid 'data2)
            (slot-value guid 'data3)
            (ash d4 -48)  
            (logand d4 #.(lognot (ash (- (expt 2 16) 1) 48))))))

(let ((scanner (cl-ppcre:create-scanner 
                "{?([0-9a-fA-F]+)-([0-9a-fA-F]+)-([0-9a-fA-F]+)-([0-9a-fA-F]+)-([0-9a-fA-F]+)}?")))
  (defun string-guid (string)
    "Parse a string to a GUID"
    (declare (string string))
    (multiple-value-bind (start-index end-index starts ends) (cl-ppcre:scan scanner string)
    (declare (ignore start-index end-index))
    (if (= (length starts) 5)
        (make-instance 'guid
                       :data1 
                       (parse-integer (subseq string (svref starts 0) (svref ends 0)) :radix 16)
                       :data2 
                       (parse-integer (subseq string (svref starts 1) (svref ends 1)) :radix 16)
                       :data3 
                       (parse-integer (subseq string (svref starts 2) (svref ends 2)) :radix 16)
                       :data4 
                       (logior 
                        (ash (parse-integer (subseq string (svref starts 3) (svref ends 3)) :radix 16)
                             48)
                        (parse-integer (subseq string (svref starts 4) (svref ends 4)) :radix 16)))
        (error "Unable to parse GUID ~S" string)))))

(defmethod print-object ((guid guid) stream)
  (print-unreadable-object (guid stream :type t)
    (format stream "~A " (guid-string guid))))

(defun guid= (guid1 guid2)
  "GUID comparison"
  (declare (guid guid1 guid2))
  (and (= (slot-value guid1 'data1) (slot-value guid2 'data1))
       (= (slot-value guid1 'data2) (slot-value guid2 'data2))
       (= (slot-value guid1 'data3) (slot-value guid2 'data3))
       (= (slot-value guid1 'data4) (slot-value guid2 'data4))))

(defun pack-guid (guid)
  "Pack a GUID to a buffer"
  (declare (guid guid))
  (pack guid 'guid))

(defun unpack-guid (buffer)
  "Extract a GUID from a buffer"
  (declare (array buffer))
  (unpack buffer 'guid))

(defun make-guid (data1 data2 data3 data4)
  "Make a GUID from 4 integers. 
DATA1 should be 32-bit. DATA2 and DATA3 should be 16-bit. DATA4 may be a 64-bit integer or an array of 8 8-bit integers."
  (declare (integer data1 data2 data3))
  (make-instance 'guid 
		 :data1 data1
		 :data2 data2
		 :data3 data3
		 :data4 (if (integerp data4)
			    data4
			    (unpack data4 :uint64))))

;; 2.4.1 SID_IDENTIFIER_AUTHORITY http://msdn.microsoft.com/en-us/library/dd302645.aspx
(defpacket sid-identifier-authority
  ((reserved (:uint8 5) :initform nil)
   (authority :uint8 :initform 0 :initarg :authority))
  (:packing 1))

(defenum *identifier-authorities*
  ((:NULL-SID-AUTHORITY #x00
    "Specifies the NULL SID authority. It defines only the NULL well-known-SID: S-1-0-0.")
   (:WORLD-SID-AUTHORITY #x01
    "Specifies the World SID authority. It only defines the Everyone well-known-SID: S-1-1-0.")
   (:LOCAL-SID-AUTHORITY #x02
    "Specifies the Local SID authority. It defines only the Local well-known-SID: S-1-2-0.")
   (:CREATOR-SID-AUTHORITY #x03
    "Specifies the Creator SID authority. It defines the Creator Owner, Creator Group, and Creator Owner Server well-known-SIDs: S-1-3-0, S-1-3-1, and S-1-3-2. These SIDs are used as placeholders in an access control list (ACL) and are replaced by the user, group, and machine SIDs of the security principal.")
   (:NON-UNIQUE-AUTHORITY #x04
    "Not used.")
   (:SECURITY-NT-AUTHORITY #x05
    "Specifies the Windows NT security subsystem SID authority. It defines all other SIDs in the forest.")
   (:SECURITY-APP-PACKAGE-AUTHORITY #x0F
    "Specifies the application package authority. It defines application capability SIDs.")
   (:SECURITY-MANDATORY-LABEL-AUTHORITY #x10
    "Specifies the Mandatory label authority. It defines the integrity level SIDs.")
   (:SECURITY-SCOPED-POLICY-ID-AUTHORITY #x11
    "Specifies the Scoped Policy Authority. It defines all other scoped policy SIDs in the forest.<3>")
   (:SECURITY-AUTHENTICATION-AUTHORITY #x12
    "Specifies the authentication authority asserting the client's identity. It defines only the following well-known SIDs: S-1-18-1, and S-1-18-2.<4>")))

;; 2.4.2 SID http://msdn.microsoft.com/en-us/library/cc230371.aspx
(defpacket sid 
  ((revision :uint8 :initform 1)
   (subauthority-count :uint8 :initform 0 :initarg :subauthority-count)
   (identifier-authority sid-identifier-authority :initform nil :initarg :identifier-authority)
   ;; payload
   (subauthority (:uint32 0) :initform nil :initarg :subauthority))
  (:packing 1))

(defun make-sid (subauthority &key (identifier-authority #.(enum :security-nt-authority *identifier-authorities*)))
  "Make a SID object. SUBAUTHORITY should be a list of (integer) authorities."
  (make-instance 'sid
                 :identifier-authority (make-instance 'sid-identifier-authority 
                                                      :authority identifier-authority)
                 :subauthority-count (length subauthority)
                 :subauthority subauthority))

(defun sid= (&rest sids)
  "SID comparison operator."
  (when (apply #'= 
               (mapcar (lambda (sid) 
                         (declare (sid sid))
                         (slot-value sid 'subauthority-count)) 
                       sids))
    (apply #'every 
           #'= 
           (mapcar (lambda (sid) 
                     (slot-value sid 'subauthority))
                   sids))))
           


(defun sid-string (sid)
  "Convert a SID to a string"
  (declare (sid sid))
  (with-output-to-string (s)
    (format s "S-~D-" (slot-value sid 'revision))
    (let ((auth (slot-value (slot-value sid 'identifier-authority)
                            'authority)))
      (if (< auth #.(expt 2 32))
          (format s "~D" auth)
          (format s "0x~X" auth)))
    (let ((sauths (slot-value sid 'subauthority))
          (count (slot-value sid 'subauthority-count)))
      (dotimes (i count)
        (when (= i 0) (format s "-"))
        (format s "~D" (elt sauths i))
        (unless (= i (1- count)) (format s "-"))))))

(defun string-sid (string)
  "Parse a string to a SID"
  (declare (string string))
  (let ((offset 0)
        (auth 0)
        (sauths nil))
    (multiple-value-bind (start end starts ends) (cl-ppcre:scan "S-1-([0-9])" string)
      (declare (ignore start))
      (setf offset end)
      (setf auth (parse-integer (subseq string (elt starts 0) (elt ends 0)))))
    (do ((done nil))
        (done)
      (multiple-value-bind (start end starts ends) (cl-ppcre:scan "(-[0-9]+)" string :start offset)        
        (if start             
            (setf offset 
                  end
                  sauths 
                  (append sauths 
                          (list (parse-integer (subseq string 
                                                       (1+ (elt starts 0)) 
                                                       (elt ends 0))))))
            (setf done t))))
    (make-sid sauths
              :identifier-authority auth)))

(defmethod print-object ((sid sid) stream)
  (print-unreadable-object (sid stream :type t)
    (format stream "~A" (sid-string sid))))

(defun pack-sid (sid)
  "Pack a SID to an array"
  (when (symbolp sid)
    (setf sid (wellknown-sid sid)))
  (apply #'usb8 
         (pack sid 'sid)
         (let ((sauths (slot-value sid 'subauthority)))
           (loop :for i :below (slot-value sid 'subauthority-count) :collect 
              (pack (elt sauths i) :uint32)))))


(defun unpack-sid (buffer)
  "Extract a SID from a buffer"
  (declare (array buffer))
  (multiple-value-bind (sid payload) (unpack buffer 'sid)
    (setf (slot-value sid 'subauthority)
          (let ((sauths (make-array (slot-value sid 'subauthority-count))))
            (dotimes (i (slot-value sid 'subauthority-count))
              (setf (elt sauths i)
                    (unpack (subseq* payload (* i 4) 4) :uint32)))
            sauths))
    (values sid
	    (subseq payload (* (slot-value sid 'subauthority-count) 4)))))

(defparameter *wellknown-sids*
  `((:NULL "S-1-0-0" "No Security principal.")
    (:EVERYONE "S-1-1-0" "A group that includes all users.")
    (:LOCAL "S-1-2-0" "A group that includes all users who have logged on locally.")
    (:CONSOLE-LOGON "S-1-2-1"
     "A group that includes users who are logged on to the physical console. This SID can be used to implement security policies that grant different rights based on whether a user has been granted physical access to the console.<5>")
    (:CREATOR-OWNER "S-1-3-0"
"A placeholder in an inheritable access control entry (ACE). When the ACE is inherited, the system replaces this SID with the SID for the object's creator.")
    (:CREATOR-GROUP "S-1-3-1"
"A placeholder in an inheritable ACE. When the ACE is inherited, the system replaces this SID with the SID for the primary group of the object's creator.")
    (:OWNER-SERVER "S-1-3-2"
"A placeholder in an inheritable ACE. When the ACE is inherited, the system replaces this SID with the SID for the object's owner server.<6>")
    (:GROUP-SERVER "S-1-3-3"
"A placeholder in an inheritable ACE. When the ACE is inherited, the system replaces this SID with the SID for the object's group server.<7>")
    (:OWNER-RIGHTS "S-1-3-4"
"A group that represents the current owner of the object. When an ACE that carries this SID is applied to an object, the system ignores the implicit READ_CONTROL and WRITE_DAC permissions for the object owner.")
    (:NT-AUTHORITY "S-1-5"
"A SID containing only the SECURITY_NT_AUTHORITY identifier authority.")
    (:DIALUP "S-1-5-1"
"A group that includes all users who have logged on through a dial-up connection.")
    (:NETWORK "S-1-5-2"
"A group that includes all users who have logged on through a network connection.")
    (:BATCH "S-1-5-3"
"A group that includes all users who have logged on through a batch queue facility.")
    (:INTERACTIVE "S-1-5-4"
"A group that includes all users who have logged on interactively.")
    (:LOGON-ID "S-1-5-5-~D-~D" ;; "S-1-5-5-x-y"
"A logon session. The X and Y values for these SIDs are different for each logon session and are recycled when the operating system is restarted.")
    (:SERVICE "S-1-5-6"
"A group that includes all security principals that have logged on as a service.")
    (:ANONYMOUS "S-1-5-7"
"A group that represents an anonymous logon.")
    (:PROXY "S-1-5-8"
"Identifies a SECURITY_NT_AUTHORITY Proxy.<8>")
    (:ENTERPRISE-DOMAIN-CONTROLLERS "S-1-5-9"
"A group that includes all domain controllers in a forest that uses an Active Directory directory service.")
    (:PRINCIPAL-SELF "S-1-5-10"
"A placeholder in an inheritable ACE on an account object or group object in Active Directory. When the ACE is inherited, the system replaces this SID with the SID for the security principal that holds the account.")
    (:AUTHENTICATED-USERS "S-1-5-11"
"A group that includes all users whose identities were authenticated when they logged on.")
    (:RESTRICTED-CODE "S-1-5-12"
"This SID is used to control access by untrusted code. ACL validation against tokens with RC consists of two checks, one against the token's normal list of SIDs and one against a second list (typically containing RC - the \"RESTRICTED_CODE\" token - and a subset of the original token SIDs). Access is granted only if a token passes both tests. Any ACL that specifies RC must also specify WD - the \"EVERYONE\" token. When RC is paired with WD in an ACL, a superset of \"EVERYONE\", including untrusted code, is described.")
    (:TERMINAL-SERVER-USER "S-1-5-13"
"A group that includes all users who have logged on to a Terminal Services server.")
    (:REMOTE-INTERACTIVE-LOGON "S-1-5-14"
"A group that includes all users who have logged on through a terminal services logon.")
    (:THIS-ORGANIZATION "S-1-5-15"
"A group that includes all users from the same organization. If this SID is present, the OTHER_ORGANIZATION SID MUST NOT be present.<9>")
    (:IUSR "S-1-5-17"
"An account that is used by the default Internet Information Services (IIS) user.")
    (:LOCAL-SYSTEM "S-1-5-18"
"An account that is used by the operating system.")
    (:LOCAL-SERVICE "S-1-5-19"
"A local service account.")
    (:NETWORK_SERVICE "S-1-5-20"
"A network service account.")
    (:ENTERPRISE-READONLY-DOMAIN-CONTROLLERS "S-1-5-21-~D-498" ;;"S-1-5-21-<root domain>-498"
"A universal group containing all read-only domain controllers in a forest.")
    (:COMPOUNDED-AUTHENTICATION "S-1-5-21-0-0-0-496"
"Device identity is included in the Kerberos service ticket. If a forest boundary was crossed, then claims transformation occurred.<10>")
    (:CLAIMS-VALID "S-1-5-21-0-0-0-497"
"Claims were queried for in the account's domain, and if a forest boundary was crossed, then claims transformation occurred.<11>")
    (:ADMINISTRATOR "S-1-5-21-~D-500" ;;"S-1-5-21-<machine>-500"
"A user account for the system administrator. By default, it is the only user account that is given full control over the system.")
    (:GUEST "S-1-5-21-~D-501" ;; "S-1-5-21-<machine>-501"
"A user account for people who do not have individual accounts. This user account does not require a password. By default, the Guest account is disabled.")
    (:DOMAIN-ADMINS "S-1-5-21-~D-512" ;; "S-1-5-21-<domain>-512"
"A global group whose members are authorized to administer the domain. By default, the DOMAIN_ADMINS group is a member of the Administrators group on all computers that have joined a domain, including the domain controllers. DOMAIN_ADMINS is the default owner of any object that is created by any member of the group.")
    (:DOMAIN-USERS "S-1-5-21-~D-513" ;; "S-1-5-21-<domain>-513"
"A global group that includes all user accounts in a domain.")
    (:DOMAIN-GUESTS "S-1-5-21-~D-514" ;; "S-1-5-21-<domain>-514"
"A global group that has only one member, which is the built-in Guest account of the domain.")
    (:DOMAIN-COMPUTERS "S-1-5-21-~D-515" ;; "S-1-5-21-<domain>-515"
"A global group that includes all clients and servers that have joined the domain.")
    (:DOMAIN-DOMAIN-CONTROLLERS "S-1-5-21-~D-516" ;; "S-1-5-21-<domain>-516"
"A global group that includes all domain controllers in the domain.")
    (:CERT-PUBLISHERS "S-1-5-21-~D-517" ;; "S-1-5-21-<domain>-517"
"A global group that includes all computers that are running an enterprise certification authority. Cert Publishers are authorized to publish certificates for User objects in Active Directory.")
    (:SCHEMA-ADMINISTRATORS "S-1-5-21-~D-518" ;; "S-1-5-21-<root-domain>-518"
"A universal group in a native-mode domain, or a global group in a mixed-mode domain. The group is authorized to make schema changes in Active Directory.")
    (:ENTERPRISE-ADMINS "S-1-5-21-~D-519" ;; "S-1-5-21-<root-domain>-519"
"A universal group in a native-mode domain, or a global group in a mixed-mode domain. The group is authorized to make forestwide changes in Active Directory, such as adding child domains.")
    (:GROUP-POLICY-CREATOR-OWNERS "S-1-5-21-~D-520" ;; "S-1-5-21-<domain>-520"
"A global group that is authorized to create new Group Policy Objects in Active Directory.")
    (:READONLY-DOMAIN-CONTROLLERS "S-1-5-21-~D-521" ;; "S-1-5-21-<domain>-521"
"A global group that includes all read-only domain controllers.")
    (:CLONEABLE-CONTROLLERS "S-1-5-21-~D-522" ;; "S-1-5-21-<domain>-522"
"A global group that includes all domain controllers in the domain that may be cloned.")
    (:PROTECTED-USERS "S-1-5-21-~D-525" ;; "S-1-5-21-<domain>-525"
"A global group that are afforded additional protections against authentication security threats. <12> For more information, see [MS-APDS] and [MS-KILE].")
    (:RAS-SERVERS "S-1-5-21-~D-553" ;; "S-1-5-21-<domain>-553"
"A domain local group for Remote Access Services (RAS) servers. Servers in this group have Read Account Restrictions and Read Logon Information access to User objects in the Active Directory domain local group.")
    (:BUILTIN-ADMINISTRATORS "S-1-5-32-544"
"A built-in group. After the initial installation of the operating system, the only member of the group is the Administrator account. When a computer joins a domain, the Domain Administrators group is added to the Administrators group. When a server becomes a domain controller, the Enterprise Administrators group also is added to the Administrators group.")
    (:BUILTIN-USERS "S-1-5-32-545"
"A built-in group. After the initial installation of the operating system, the only member is the Authenticated Users group. When a computer joins a domain, the Domain Users group is added to the Users group on the computer.")
    (:BUILTIN-GUESTS "S-1-5-32-546"
"A built-in group. The Guests group allows users to log on with limited privileges to a computer's built-in Guest account.")
    (:POWER-USERS "S-1-5-32-547"
"A built-in group. Power users can perform the following actions:
Create local users and groups.
Modify and delete accounts that they have created.
Remove users from the Power Users, Users, and Guests groups.
Install programs.
Create, manage, and delete local printers.
Create and delete file shares.")
    (:ACCOUNT-OPERATORS "S-1-5-32-548"
"A built-in group that exists only on domain controllers. Account Operators have permission to create, modify, and delete accounts for users, groups, and computers in all containers and organizational units of Active Directory except the Built-in container and the Domain Controllers OU. Account Operators do not have permission to modify the Administrators and Domain Administrators groups, nor do they have permission to modify the accounts for members of those groups.")
    (:SERVER-OPERATORS "S-1-5-32-549"
"A built-in group that exists only on domain controllers. Server Operators can perform the following actions:
Log on to a server interactively.
Create and delete network shares.
Start and stop services.
Back up and restore files.
Format the hard disk of a computer.
Shut down the computer.")
    (:PRINTER-OPERATORS "S-1-5-32-550"
"A built-in group that exists only on domain controllers. Print Operators can manage printers and document queues.")
    (:BACKUP-OPERATORS "S-1-5-32-551"
"A built-in group. Backup Operators can back up and restore all files on a computer, regardless of the permissions that protect those files.")
    (:REPLICATOR "S-1-5-32-552"
"A built-in group that is used by the File Replication Service (FRS) on domain controllers.")
    (:ALIAS-PREW2KCOMPACC "S-1-5-32-554"
"A backward compatibility group that allows read access on all users and groups in the domain.<13>")
    (:REMOTE-DESKTOP "S-1-5-32-555"
"An alias. Members of this group are granted the right to log on remotely.<14>")
    (:NETWORK-CONFIGURATION-OPS "S-1-5-32-556"
"An alias. Members of this group can have some administrative privileges to manage configuration of networking features.<15>")
    (:INCOMING-FOREST-TRUST-BUILDERS "S-1-5-32-557"
"An alias. Members of this group can create incoming, one-way trusts to this forest.<16>")
    (:PERFMON-USERS "S-1-5-32-558"
"An alias. Members of this group have remote access to monitor this computer.<17>")
    (:PERFLOG-USERS "S-1-5-32-559"
"An alias. Members of this group have remote access to schedule the logging of performance counters on this computer.<18>")
    (:WINDOWS-AUTHORIZATION-ACCESS-GROUP "S-1-5-32-560"
"An alias. Members of this group have access to the computed tokenGroupsGlobalAndUniversal attribute on User objects.<19>")
    (:TERMINAL-SERVER-LICENSE-SERVERS "S-1-5-32-561"
"An alias. A group for Terminal Server License Servers.<20>")
    (:DISTRIBUTED-COM-USERS "S-1-5-32-562"
"An alias. A group for COM to provide computer-wide access controls that govern access to all call, activation, or launch requests on the computer.<21>")
    (:IIS-IUSRS "S-1-5-32-568"
"A built-in group account for IIS users.")
    (:CRYPTOGRAPHIC-OPERATORS "S-1-5-32-569"
"A built-in group account for cryptographic operators.<22>")
    (:EVENT-LOG-READERS "S-1-5-32-573"
"A built-in local group. Members of this group can read event logs from the local machine.<23>")
    (:CERTIFICATE-SERVICE-DCOM_ACCESS "S-1-5-32-574"
"A built-in local group. Members of this group are allowed to connect to Certification Authorities in the enterprise.<24>")
    (:RDS-REMOTE-ACCESS-SERVERS "S-1-5-32-575"
"A group that allows members use of Remote Application Services resources.")
    (:RDS-ENDPOINT-SERVERS "S-1-5-32-576"
"A group that enables member servers to run virtual machines and host sessions.")
    (:RDS-MANAGEMENT-SERVERS "S-1-5-32-577"
"A group that allows members to access WMI resources over management protocols (such as WS-Management via the Windows Remote Management service).")
    (:HYPER-V-ADMINS "S-1-5-32-578"
"A group that gives members access to all administrative features of Hyper-V.")
    (:ACCESS-CONTROL-ASSISTANCE-OPS "S-1-5-32-579"
"A local group that allows members to remotely query authorization attributes and permissions for resources on the local computer.")
    (:REMOTE-MANAGEMENT-USERS "S-1-5-32-580"
"Members of this group can access Windows Management Instrumentation (WMI) resources over management protocols (such as WS-Management [DMTF-DSP0226]). This applies only to WMI namespaces that grant access to the user.")
    (:WRITE-RESTRICTED-CODE "S-1-5-33"
"A SID that allows objects to have an ACL that lets any service process with a write-restricted token to write to the object.")
    (:NTLM-AUTHENTICATION "S-1-5-64-10"
"A SID that is used when the NTLM authentication package authenticated the client.")
    (:SCHANNEL-AUTHENTICATION "S-1-5-64-14"
"A SID that is used when the SChannel authentication package authenticated the client.")
    (:DIGEST-AUTHENTICATION "S-1-5-64-21"
"A SID that is used when the Digest authentication package authenticated the client.")
    (:THIS-ORGANIZATION-CERTIFICATE "S-1-5-65-1"
"A SID that indicates that the client's Kerberos service ticket's PAC contained a NTLM_SUPPLEMENTAL_CREDENTIAL structure (as specified in [MS-PAC] section 2.6.4). If the OTHER_ORGANIZATION SID is present, then this SID MUST NOT be present. <25>")
    (:NT-SERVICE "S-1-5-80"
"An NT Service account prefix.")
    (:USER-MODE-DRIVERS "S-1-5-84-0-0-0-0-0"
"Identifies a user-mode driver process.")
    (:LOCAL-ACCOUNT "S-1-5-113"
"A group that includes all users who are local accounts.<26>")
    (:LOCAL-ACCOUNT-AND-MEMBER-OF-ADMINISTRATORS_GROUP "S-1-5-114"
"A group that includes all users who are local accounts and members of the administrators group.<27>")
    (:OTHER-ORGANIZATION "S-1-5-1000"
"A group that includes all users and computers from another organization. If this SID is present, THIS_ORGANIZATION SID MUST NOT be present.<28>")
    (:ALL-APP-PACKAGES "S-1-15-2-1"
"All applications running in an app package context.")
    (:ML-UNTRUSTED "S-1-16-0"
"An untrusted integrity level.")
    (:ML-LOW "S-1-16-4096"
"A low integrity level.")
    (:ML-MEDIUM "S-1-16-8192"
"A medium integrity level.")
    (:ML-MEDIUM-PLUS "S-1-16-8448"
"A medium-plus integrity level.")
    (:ML-HIGH "S-1-16-12288"
"A high integrity level.")
    (:ML-SYSTEM "S-1-16-16384"
"A system integrity level.")
    (:ML-PROTECTED-PROCESS "S-1-16-20480"
"A protected-process integrity level.")
    (:AUTHENTICATION-AUTHORITY-ASSERTED-IDENTITY "S-1-18-1"
"A SID that means the client's identity is asserted by an authentication authority based on proof of possession of client credentials.<29>")
    (:SERVICE-ASSERTED-IDENTITY "S-1-18-2"
"A SID that means the client's identity is asserted by a service.<30>")))


(defun wellknown-sid (name &rest args)
  (let ((fmt (second (find name *wellknown-sids* :key #'car))))
    (unless fmt (error "Wellknown SID ~S not found" name))
    (string-sid (apply #'format nil fmt args))))

(defun wellknown-sid-by-string (string &rest args)
  (dolist (wellknown *wellknown-sids*)
    (destructuring-bind (kw str doc) wellknown
      (declare (ignore doc))
      (handler-case 
          (when (string= string (apply #'format nil str args))
            (return-from wellknown-sid-by-string kw))
        (error (err) 
          (declare (ignore err))
          nil))))
  nil)
      
       

;; 2.4.3 ACCESS_MASK http://msdn.microsoft.com/en-us/library/cc230294.aspx

(defflags *access-mask*
  ((:GENERIC-READ 31
   "When used in an Access Request operation: When read access to an object is requested, this bit is translated to a combination of bits. These are most often set in the lower 16 bits of the ACCESS_MASK. (Individual protocol specifications MAY specify a different configuration.) The bits that are set are implementation dependent. During this translation, the GR bit is cleared. The resulting ACCESS_MASK bits are the actual permissions that are checked against the ACE structures in the security descriptor that attached to the object.
When used to set the Security Descriptor on an object: When the GR bit is set in an ACE that is to be attached to an object, it is translated into a combination of bits, which are usually set in the lower 16 bits of the ACCESS_MASK. (Individual protocol specifications MAY specify a different configuration.) The bits that are set are implementation dependent. During this translation, the GR bit is cleared. The resulting ACCESS_MASK bits are the actual permissions that are granted by this ACE.")
   (:GENERIC-WRITE 26
   "When used in an Access Request operation: When write access to an object is requested, this bit is translated to a combination of bits, which are usually set in the lower 16 bits of the ACCESS_MASK. (Individual protocol specifications MAY specify a different configuration.) The bits that are set are implementation dependent. During this translation, the GW bit is cleared. The resulting ACCESS_MASK bits are the actual permissions that are checked against the ACE structures in the security descriptor that attached to the object.
When used to set the Security Descriptor on an object: When the GW bit is set in an ACE that is to be attached to an object, it is translated into a combination of bits, which are usually set in the lower 16 bits of the ACCESS_MASK. (Individual protocol specifications MAY specify a different configuration.) The bits that are set are implementation dependent. During this translation, the GW bit is cleared. The resulting ACCESS_MASK bits are the actual permissions that are granted by this ACE.")
   (:GENERIC-EXECUTE 29
    "When used in an Access Request operation: When execute access to an object is requested, this bit is translated to a combination of bits, which are usually set in the lower 16 bits of the ACCESS_MASK. (Individual protocol specifications MAY specify a different configuration.) The bits that are set are implementation dependent. During this translation, the GX bit is cleared. The resulting ACCESS_MASK bits are the actual permissions that are checked against the ACE structures in the security descriptor that attached to the object.
When used to set the Security Descriptor on an object: When the GX bit is set in an ACE that is to be attached to an object, it is translated into a combination of bits, which are usually set in the lower 16 bits of the ACCESS_MASK. (Individual protocol specifications MAY specify a different configuration.) The bits that are set are implementation dependent. During this translation, the GX bit is cleared. The resulting ACCESS_MASK bits are the actual permissions that are granted by this ACE.")
   (:GENERIC-ALL 28
    "When used in an Access Request operation: When all access permissions to an object are requested, this bit is translated to a combination of bits, which are usually set in the lower 16 bits of the ACCESS_MASK. (Individual protocol specifications MAY specify a different configuration.) Objects are free to include bits from the upper 16 bits in that translation as required by the objects semantics. The bits that are set are implementation dependent. During this translation, the GA bit is cleared. The resulting ACCESS_MASK bits are the actual permissions that are checked against the ACE structures in the security descriptor that attached to the object.
When used to set the Security Descriptor on an object: When the GA bit is set in an ACE that is to be attached to an object, it is translated into a combination of bits, which are usually set in the lower 16 bits of the ACCESS_MASK. (Individual protocol specifications MAY specify a different configuration.) Objects are free to include bits from the upper 16 bits in that translation, if required by the objects semantics. The bits that are set are implementation dependent. During this translation, the GA bit is cleared. The resulting ACCESS_MASK bits are the actual permissions that are granted by this ACE.")
   (:MAXIMUM-ALLOWED 25
    "When used in an Access Request operation: When requested, this bit grants the requestor the maximum permissions allowed to the object through the Access Check Algorithm. This bit can only be requested; it cannot be set in an ACE.
When used to set the Security Descriptor on an object: Specifying the Maximum Allowed bit in the SECURITY_DESCRIPTOR has no meaning. The MA bit SHOULD NOT be set and SHOULD be ignored when part of a SECURITY_DESCRIPTOR structure.")
   (:ACCESS-SYSTEM-SECURITY 24
    "When used in an Access Request operation: When requested, this bit grants the requestor the right to change the SACL of an object. This bit MUST NOT be set in an ACE that is part of a DACL. When set in an ACE that is part of a SACL, this bit controls auditing of accesses to the SACL itself.")
   (:SYNCHRONIZE 20
    "Specifies access to the object sufficient to synchronize or wait on the object.")
   (:WRITE-OWNER 19
    "Specifies access to change the owner of the object as listed in the security descriptor.")
   (:WRITE-DACL 18
    "Specifies access to change the discretionary access control list of the security descriptor of an object.")
   (:READ-CONTROL 17
    "Specifies access to read the security descriptor of an object.")
   (:DELETE 16
    "Specifies access to delete an object.")))

(defun pack-access-mask (&rest flags)
  (pack-flags flags *access-mask*))

(defun unpack-access-mask (number)
  (unpack-flags number *access-mask*))



;; 2.4.5 ACL http://msdn.microsoft.com/en-us/library/cc230297.aspx
(defpacket acl 
  ((revision :uint8 :initform 2 :initarg :revision)
   (reverved :uint8 :initform 0)
   (size :uint16 :initform 0 :initarg :size)
   (count :uint16 :initform 0 :initarg :count)
   (reserved2 :uint16 :initform 0)
   ;; payload 
   (aces (:uint8 0) :initform nil :accessor acl-aces))
  (:packing 1))

(defmethod print-object ((acl acl) stream)
  (print-unreadable-object (acl stream :type t)
    (format stream ":ACE-COUNT ~D" (slot-value acl 'count))))

(defun pack-acl (aces)
  "Generate a packed ACL from a list of ACEs."
  (let ((ace-buffer (apply #'usb8 (mapcar #'pack-ace* aces))))
    (usb8 
     (pack (make-instance 'acl 
                          :count (length aces)
                          :size (+ (type-size 'acl) (length ace-buffer)))
           'acl)
     ace-buffer)))

(defun unpack-acl (buffer)
  "Unpack an ACL from the buffer. Returns the list of ACEs."
  (multiple-value-bind (acl payload) (unpack buffer 'acl)
    (setf (slot-value acl 'aces) nil)
    (dotimes (i (slot-value acl 'count))
      (multiple-value-bind (ace payload2) (unpack-ace payload)
	(push ace (slot-value acl 'aces))
	(setf payload payload2)))
    (slot-value acl 'aces)))

;; 2.4.6 SECURITY_DESCRIPTOR http://msdn.microsoft.com/en-us/library/cc230366.aspx
(defpacket security-descriptor 
  ((revision :uint8 :initform 1)
   (sbz1 :uint8 :initform 0)
   (control :uint16 :initform 0 :initarg :control)
   (offset-owner :uint32 :initform 0 :initarg :offset-owner)
   (offset-group :uint32 :initform 0 :initarg :offset-group)
   (offset-sacl :uint32 :initform 0 :initarg :offset-sacl)
   (offset-dacl :uint32 :initform 0 :initarg :offset-dacl)
   ;; payload
   (owner-sid (:uint8 0) :initform nil :initarg :owner-sid)
   (group-sid (:uint8 0) :initform nil :initarg :group-sid)
   (sacl (:uint8 0) :initform nil :initarg :sacl)
   (dacl (:uint8 0) :initform nil :initarg :dacl))
  (:packing 1))

(defmethod print-object ((sd security-descriptor) stream)
  (print-unreadable-object (sd stream :type t)
    (format stream "~A" (security-descriptor-string sd))))


(defflags *sd-control-codes*
  ((:self-relative 0
"SR Set when the security descriptor is in self-relative format. Cleared when the security descriptor is in absolute format.")
   (:rm-control-valid 1
"RM Set to 0x1 when the Sbz1 field is to be interpreted as resource manager control bits.")
   (:sacl-protected 2
"PS Set when the SACL should be protected from inherit operations.")
   (:dacl-protected 3
"PD Set when the DACL should be protected from inherit operations.")
   (:sacl-autoinherited 4
"SI Set when the SACL was created through inheritance.")
   (:dacl-authoinherited 5
"DI Set when the DACL was created through inheritance.")
   (:sacl-computed-inheritance-required 6
"SC Set when the SACL is to be computed through inheritance. When both SC and SI are set, the resulting security descriptor should set SI; the SC setting is not preserved.")
   (:dacl-computed-inheritance-required 7
"DC Set when the DACL is to be computed through inheritance. When both DC and DI are set, the resulting security descriptor should set DI; the DC setting is not preserved.")
   (:DACL-Trusted 8
"DT Set when the ACL that is pointed to by the DACL field was provided by a trusted source and does not require any editing of compound ACEs.")
   (:Server-Security 9
"SS Set when the caller wants the system to create a Server ACL based on the input ACL, regardless of its source (explicit or defaulting).")
   (:SACL-Defaulted 10
"SD Set when the SACL was established by default means.")
   (:SACL-Present 11
"SP Set when the SACL is present on the object.")
   (:DACL-Defaulted 12
"DD Set when the DACL was established by default means.")
   (:DACL-Present 13 
"DP Set when the DACL is present on the object.")
   (:Group-Defaulted 14
"GD Set when the group was established by default means.")
   (:Owner-Defaulted 15
"OD Set when the owner was established by default means.")))
   
(defun make-security-descriptor (control-codes owner group sacl dacl)
  "Make a SECURITY_DESCRIPTOR object."
  (declare (list control-codes sacl dacl))
  (when (symbolp owner) (setf owner (wellknown-sid owner)))
  (when (symbolp group) (setf group (wellknown-sid group)))
  (make-instance 'security-descriptor 
		 :control control-codes
		 :owner-sid owner
		 :group-sid group
		 :sacl sacl
		 :dacl dacl))

(defun copy-security-descriptor (security-descriptor)
  (make-security-descriptor (slot-value security-descriptor 'control-codes)
                            (slot-value security-descriptor 'owner-sid)
                            (slot-value security-descriptor 'group-sid)
                            (slot-value security-descriptor 'sacl)
                            (slot-value security-descriptor 'dacl)))

(defun pack-security-descriptor (control-codes owner-sid group-sid sacl dacl)
  "Create an pack a security descriptor object."
  (when (symbolp owner-sid)
    (setf owner-sid (wellknown-sid owner-sid)))
  (when (symbolp group-sid)
    (setf group-sid (wellknown-sid group-sid)))
  (let ((packed-owner (pack-sid owner-sid))
	(packed-group (pack-sid group-sid))
	(packed-sacl (pack-acl sacl))
	(packed-dacl (pack-acl dacl))
	(size (type-size 'security-descriptor)))
    (let* ((offset-owner size)
	   (offset-group (+ offset-owner (length packed-owner)))
	   (offset-sacl (+ offset-group (length packed-group)))
	   (offset-dacl (+ offset-sacl (length packed-sacl))))
      (usb8 (pack (make-instance 'security-descriptor 
				 :control (pack-flags control-codes *sd-control-codes*)
				 :offset-owner offset-owner
				 :offset-group offset-group
				 :offset-sacl offset-sacl
				 :offset-dacl offset-dacl)
		  'security-descriptor)
	    packed-owner
	    packed-group
	    packed-sacl
	    packed-dacl))))

(defun pack-security-descriptor* (sd)
  "Pack a security descriptor object."
  (pack-security-descriptor (slot-value sd 'control)
                            (slot-value sd 'owner-sid)
                            (slot-value sd 'group-sid)
                            (slot-value sd 'sacl)
                            (slot-value sd 'dacl)))


(defun unpack-security-descriptor (buffer)
  "Unpack a security descriptor object from a buffer."
  (declare (array buffer))
  (let ((size (type-size 'security-descriptor)))
    (multiple-value-bind (sd payload) (unpack buffer 'security-descriptor)
      (setf (slot-value sd 'control)
	    (unpack-flags (slot-value sd 'control) *sd-control-codes*)
	    (slot-value sd 'owner-sid) 
	    (unpack-sid (subseq payload (- (slot-value sd 'offset-owner) size)))
	    (slot-value sd 'group-sid) 
	    (unpack-sid (subseq payload (- (slot-value sd 'offset-group) size)))
	    (slot-value sd 'sacl) 
	    (unpack-acl (subseq payload (- (slot-value sd 'offset-sacl) size)))
	    (slot-value sd 'dacl) 
	    (unpack-acl (subseq payload (- (slot-value sd 'offset-dacl) size))))
      sd)))
      


;; 2.4.7 SECURITY_INFORMATION http://msdn.microsoft.com/en-us/library/cc230369.aspx
(defflags *security-information*
  ((:OWNER-SECURITY-INFORMATION 0
"The owner identifier of the object is being referenced.")
   (:GROUP-SECURITY-INFORMATION 1
"The primary group identifier of the object is being referenced.")
   (:DACL-SECURITY-INFORMATION 2
"The DACL of the object is being referenced.")
   (:SACL-SECURITY-INFORMATION 3
"The SACL of the object is being referenced.")
   (:LABEL-SECURITY-INFORMATION 4
"The mandatory integrity label is being referenced.")
   (:UNPROTECTED-SACL-SECURITY-INFORMATION 28
"The SACL inherits access control entries (ACEs) from the parent object.")
   (:UNPROTECTED-DACL-SECURITY-INFORMATION 29
"The DACL inherits ACEs from the parent object.")
   (:PROTECTED-SACL-SECURITY-INFORMATION 30
"The SACL cannot inherit ACEs.")
   (:PROTECTED-DACL-SECURITY-INFORMATION 31
"The DACL cannot inherit ACEs.")
   (:ATTRIBUTE-SECURITY-INFORMATION 5
"A SYSTEM_RESOURCE_ATTRIBUTE_ACE (section 2.4.4.15) is being referenced.")
   (:SCOPE-SECURITY-INFORMATION 6
"A SYSTEM_SCOPED_POLICY_ID_ACE (section 2.4.4.16) is being referenced.")
   (:BACKUP-SECURITY-INFORMATION 16
"The security descriptor is being accessed for use in a backup operation.")))

(defun pack-security-information (&rest flags)
  (pack-flags flags *security-information*))

(defun unpack-security-information (number)
  (unpack-flags number *security-information*))

;; 2.4.8 TOKEN_MANDATORY_POLICY http://msdn.microsoft.com/en-us/library/gg465319.aspx

(defenum *token-mandatory-policy*
  ((:TOKEN-MANDATORY-POLICY-OFF #x00000000
"No mandatory integrity policy is enforced for the token.")
   (:TOKEN-MANDATORY-POLICY-NO-WRITE-UP #x00000001
"A process associated with the token cannot write to objects that have a greater mandatory integrity level.")
   (:TOKEN-MANDATORY-POLICY-NEW-PROCESS-MIN #x00000002
"A process created with the token has an integrity level that is the lesser of the parent-process integrity level and the executable-file integrity level.")))

;; 2.4.9 MANDATORY_INFORMATION http://msdn.microsoft.com/en-us/library/gg465312.aspx
(defpacket mandatory-information 
  ((access-mask :uint32 :initform 0 :initarg :access-mask)
   (write-allowed :bool :initform 0 :initarg :write-allowed)
   (read-allowed :bool :initform 0 :initarg :read-allowed)
   (execute-allowed :bool :initform 0 :initarg :execute-allowed)
   (token-mandatory-policy :uint32 :initform 0 :initarg :token-mandatory-policy))
  (:packing 1))

;; 2.4.10.1 CLAIM_SECURITY_ATTRIBUTE_RELATIVE_V1 http://msdn.microsoft.com/en-us/library/hh877833.aspx
(defpacket claim-security-attribute-relative-v1 
  ((name-offset :uint32 :initform 0 :initarg :name-offset)
   (values-type :uint16 :initform 0 :initarg :values-type)
   (reserved :uint16 :initform 0)
   (flags :uint32 :initform 0 :initarg :flags)
   (value-count :uint32 :initform 0 :initarg :value-count)
   ;; payload
   (name (:uint8 0) :initform nil :initarg :name )
   (values (:uint8 0) :initform nil :initarg :values))
  (:packing 1))

(defmethod print-object ((claim claim-security-attribute-relative-v1) stream)
  (print-unreadable-object (claim stream :type t)
    (format stream ":NAME ~A" (slot-value claim 'name))))

(defenum *value-types*
  ((:INT64 #x0001
"Values member refers to an array of offsets to LONG64 value(s).")
   (:UINT64 #x0002
"Values member refers to an array of offsets to ULONG64 value(s).")
   (:STRING #x0003
"Values member refers to an array of offsets to Unicode character string value(s).")
   (:SID #x0005
"The Values member refers to an array of offsets to CLAIM_SECURITY_ATTRIBUTE_OCTET_STRING_RELATIVE value(s) where the OctetString value is a SID string.")
   (:BOOLEAN #x0006
"The Values member refers to an array of offsets to ULONG64 values where each element indicates a Boolean value. The value 1 indicates TRUE, and the value 0 indicates FALSE.")
   (:OCTET-STRING #x0010
"Values member contains an array of CLAIM_SECURITY_ATTRIBUTE_OCTET_STRING_RELATIVE value(s) as specified in section 2.4.10.2.")))

(defflags *claim-security-attribute-flags*
  ((:FCI-CLAIM-SECURITY-ATTRIBUTE_MANUAL 16
"The CLAIM_SECURITY_ATTRIBUTE has been manually assigned.")
   (:FCI-CLAIM-SECURITY-ATTRIBUTE-POLICY-DERIVED 17
"The CLAIM_SECURITY_ATTRIBUTE has been determined by a central policy.")
   (:CLAIM-SECURITY-ATTRIBUTE-NON-INHERITABLE 0
"This claim security attribute is not inherited across processes.")
   (:CLAIM-SECURITY-ATTRIBUTE-VALUE-CASE-SENSITIVE 1
"The value of the claim security attribute is case sensitive. This flag is valid for values that contain string types.")
   (:CLAIM-SECURITY-ATTRIBUTE-USE-FOR-DENY-ONLY 2
"Reserved for future use.")
   (:CLAIM-SECURITY-ATTRIBUTE-DISABLED-BY-DEFAULT 3
"The claim security attribute is disabled by default.")
   (:CLAIM-SECURITY-ATTRIBUTE-DISABLED 4
"Reserved for future use.")
   (:CLAIM-SECURITY-ATTRIBUTE-MANDATORY 5
"The claim security attribute is mandatory.")))

(defun make-claim-attribute (name value-type values &key flags)
  (make-instance 'claim-security-attribute-relative-v1 
                 :value-count (length values)
                 :values-type value-type
                 :flags flags
                 :values values
                 :name name))

(defun pack-claim-attribute (name value-type values &key flags)
  (let ((packed-values 
         (mapcar (lambda (value)
                   (ecase value-type
                     (:int64 (pack value :int64))
                     (:uint64 (pack value :uint64))
                     (:string (pack value :wstring))
                     (:sid (pack-octet-string (pack-sid value)))
                     (:boolean (if value (pack 1 :uint64) (pack 0 :uint64)))
                     (:octet-string (pack-octet-string value))))
                 values))
        (packed-name (usb8 (pack name :wstring) 
                           #(0 0))))
    (let ((packed-value-offsets 
           (do ((offset (+ (type-size 'claim-security-attribute-relative-v1)
                           (* (type-size :uint32) (length values))
                           (length packed-name)))
                (offsets nil)
                (packed-values packed-values (cdr packed-values)))
               ((null packed-values) (apply #'usb8 (nreverse offsets)))
             (push (pack offset :uint32) offsets)
             (incf offset (length (car packed-values))))))
      (apply #'usb8
             (pack (make-instance 'claim-security-attribute-relative-v1 
                                  :name-offset (+ (type-size 'claim-security-attribute-relative-v1)
                                                  (* (type-size :uint32) (length values)))
                                  :flags (pack-flags flags *claim-security-attribute-flags*)
                                  :value-count (length values)
                                  :values-type (enum value-type *value-types*))
                   'claim-security-attribute-relative-v1)
             packed-value-offsets
             packed-name
             packed-values))))
                    
(defun pack-claim-attribute* (claim)
  (pack-claim-attribute (slot-value claim 'name)
                        (slot-value claim 'values-type)
                        (slot-value claim 'values)
                        :flags (slot-value claim 'flags)))

(defun unpack-claim-attribute (buffer)
  (declare (array buffer))
  (multiple-value-bind (claim payload) (unpack buffer 'claim-security-attribute-relative-v1)
    (let ((offsets (loop :for i :below (slot-value claim 'value-count)
                      :collect (- (unpack (subseq payload (* i #.(type-size :uint32)))
                                          :uint32)
                                  (type-size 'claim-security-attribute-relative-v1)))))
      (setf (slot-value claim 'name) 
            (unpack (subseq payload 
                            (- (slot-value claim 'name-offset) 
                               (type-size 'claim-security-attribute-relative-v1)))
                    :wstring)
            (slot-value claim 'flags)
            (unpack-flags (slot-value claim 'flags) *claim-security-attribute-flags*)
            (slot-value claim 'values)
            (mapcar (lambda (offset)
                      (ecase (enum-id (slot-value claim 'values-type) *value-types*)
                        (:int64 (unpack (subseq payload offset) :int64))
                        (:uint64 (unpack (subseq payload offset) :uint64))
                        (:string (unpack (subseq payload offset) :wstring))
                        (:sid (unpack-sid (unpack-octet-string (subseq payload offset))))
                        (:octet-string (unpack-octet-string (subseq payload offset)))))
                    offsets))
      claim)))

(defun claim-attribute-name (claim)
  (slot-value claim 'name))

;; 2.4.10.2 CLAIM_SECURITY_ATTRIBUTE_OCTET_STRING_RELATIVE http://msdn.microsoft.com/en-us/library/hh877847.aspx
(defpacket claim-security-attribute-octet-string-relative 
  ((length :uint32 :initform 0 :initarg :length)
   (octet-string (:uint8 0) :initform nil :initarg :octet-string))
  (:packing 1))

(defun pack-octet-string (octets)
  (declare (array octets))
  (usb8 
   (pack (make-instance 'claim-security-attribute-octet-string-relative
                        :length (length octets))
         'claim-security-attribute-octet-string-relative)
   octets))

(defun unpack-octet-string (buffer)
  (declare (array buffer))
  ;; the structure is so simple we can probabyl just ignore the intermediate value 
  (let ((length (unpack buffer :uint32)))
    (subseq* buffer 4 length)))

