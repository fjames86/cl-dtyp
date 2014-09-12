
;;;; MS-DTYP Microsoft Data Types

;;; 
;;; Defines the data types commonly used in Microsoft protocols.
;;;
;;; Copyright (C) Frank James, July 2014
;;;


(in-package :cl-dtyp)

;; need a definition of a "domain" 
;; is it a list of groups, a list of users, a list of SIDs?

;; a domain contains a list of users and groups
;; a group contains a list of users
(defun make-domain ()
  (list nil nil))

(defun domain-users (domain)
  (first domain))

(defun domain-groups (domain)
  (second domain))

(defun add-user (domain user)
  (push user (first domain)))

(defun create-group (domain group)
  (push (list group nil) (second domain)))

(defun group-sid (group)
  (first group))

(defun group-users (group)
  (second group))

(defun add-user-group (domain group user)
  (let ((g (find group (domain-groups domain) :key #'group-sid :test #'sid=)))
    (unless g (error "No group with SID ~S" group))
    (push user (second g))))

(defun user-groups (domain user)
  (mapcan (lambda (group)
            (when (member user (group-users group) :test #'sid=)
              (list (group-sid group))))
          (domain-groups domain)))

(defparameter *my-domain* (make-domain))
(defparameter *my-user* (make-sid '(123)))
(defparameter *my-group* (make-sid '(666)))
(add-user *my-domain* *my-user*)
(create-group *my-domain* *my-group*)
(add-user-group *my-domain* *my-group* *my-user*)



;; ---------------- tokens -----------

(defun make-mapping (sid privileges)
  (list sid privileges))

(defun mapping-sid (mapping)
  (first mapping))

(defun mapping-privileges (mapping)
  (second mapping))

(defun make-token (user-sid owner-sid 
                   &key sids user-claims local-claims privileges device-sids device-claims default-dacl)
  (list (cons :user-sid user-sid)
        (cons :owner-sid owner-sid)
        (cons :sids (append (list user-sid owner-sid) sids))
        (cons :user-claims user-claims)
        (cons :local-claims local-claims)
        (cons :privileges privileges)
        (cons :device-sids device-sids)
        (cons :device-claims device-claims)
        (cons :default-dacl default-dacl)))
  
(defun token-sids (token)
  (cdr (assoc :sids token)))

(defun token-privileges (token)
  (cdr (assoc :privileges token)))

(defun (setf token-privileges) (value token)
  (setf (cdr (assoc :privileges token))
        value))

(defun token-user (token)
  (cdr (assoc :user-sid token)))
(defun token-owner (token)
  (cdr (assoc :owner-sid token)))
(defun token-user-claims (token)
  (cdr (assoc :user-claims token)))
(defun token-local-claims (token)
  (cdr (assoc :local-claims token)))
(defun token-device-claims (token)
  (cdr (assoc :device-claims token)))

;; --------- luids -------------

(defparameter *luid-seqno* 0)
(defun make-luid ()
  (incf *luid-seqno*)
  *luid-seqno*)

(defparameter *wellknown-luids*
  (mapcar (lambda (name)
            (cons name (make-luid)))
          '(:Create-Token
            :Assign-Primary-Token
            :Lock-Memory
            :Increase-Quota
            :Unsolicited-Input
            :Machine-Account
            :Tcb
            :Security
            :Take-Ownership
            :Load-Driver
            :System-Profile
            :Systemtime
            :Profile-Single-Process
            :Increase-Base-Priority
            :Create-Pagefile
            :Create-Permanent
            :Backup
            :Restore
            :Shutdown
            :Debug
            :Audit
            :System-Environment
            :Change-Notify
            :Remote-Shutdown
            :Undock
            :Sync-Agent
            :Enable-Delegation
            :Manage-Volume
            :Impersonate
            :Create-Global
            :Trusted-Cred-Man-Access
            :Relabel
            :Increase-Working-Set
            :Time-Zone
            :Create-Symbolic-Link)))

(defun luid (name)
  (cdr (assoc name *wellknown-luids*)))




;; 2.5.2.1.1 GatherGroupMembershipForSystem http://msdn.microsoft.com/en-us/library/jj663177.aspx
(defun group-membership-from-domain (domain initial-sids)
  "Returns a list of SIDs"
  (let ((additional-sids nil))
    (dolist (group (domain-groups domain))
      (dolist (user (group-users group))
        (dolist (sid initial-sids)
          (when (sid= sid user)
            (push (group-sid group) additional-sids)))))
    (append initial-sids additional-sids)))
   
;; 2.5.2.1.2 AddPrivilegesToToken http://msdn.microsoft.com/en-us/library/jj663155.aspx
(defun add-privileges-to-token (token privilege-mapping)
  (dolist (sid (token-sids token))
    (dolist (mapping privilege-mapping)
      (when (sid= (mapping-sid mapping) sid)
        (setf (token-privileges token)
              (union (token-privileges token) (mapping-privileges mapping)))))))


;; 2.5.3.1.1 SidInToken http://msdn.microsoft.com/en-us/library/ff842414.aspx
(defun sid-in-token (token sid-to-test &optional principal-self-substitute)
  (let ((sids (token-sids token)))
    (when (sid= sid-to-test (wellknown-sid :principal-self))
      (unless principal-self-substitute 
        (error "Must provide a principal-self-substitute when using :principal-self as sid-to-test"))
      (setf sid-to-test principal-self-substitute))
    (some (lambda (sid) 
            (sid= sid sid-to-test))
          sids)))

;; 2.5.3.1.2 SidDominates http://msdn.microsoft.com/en-us/library/ff842412.aspx
(defun sid-dominates (sid1 sid2)
  (declare (sid sid1 sid2))
  (cond
    ((sid= sid1 sid2)
     t)
    ((> (slot-value sid2 'subauthority-count) (slot-value sid1 'subauthority-count))
     nil)
    (t 
     (some (lambda (s1 s2)
             (>= s1 s2))
           (slot-value sid1 'subauthority)
           (slot-value sid2 'subauthority)))))
          
;; 2.5.3.1.3 GetScopedPolicySid http://msdn.microsoft.com/en-us/library/hh877831.aspx
(defun get-scoped-policy-sid (sacl)
  (let ((ace 
         (find-if (lambda (ace)
                    (and (eq (ace-type-name-kw ace) :SYSTEM-SCOPED-POLICY-ID)
                         (not (member :INHERIT-ONLY-ACE (slot-value (slot-value ace 'header) 'ace-flags)))))
                  sacl)))
    (when ace 
      (slot-value ace 'sid))))
          
;; 2.5.3.1.4 GetCentralizedAccessPolicy http://msdn.microsoft.com/en-us/library/hh877839.aspx
;; wtf does this function do?
(defun get-centralized-access-policy (sacl)
  (declare (ignore sacl))
  (warn "WTF does this function do?")
  nil)
    
;; 2.5.3.1.5 EvaluateAceCondition http://msdn.microsoft.com/en-us/library/hh877855.aspx
(defun evaluate-ace-condition (token sacl expr)
  (error "FIXME"))

;; 2.5.3.1.6 LookupAttributeInToken http://msdn.microsoft.com/en-us/library/hh877850.aspx

;; these are the attribute type codes
;;(defparameter *attribute-sources* 
;;  '((:local-claims . #xf8)
;;    (:user-claims . #xf9)
;;    (:device-claims . #xfb)))

(defun lookup-attribute-in-token (name token &optional source)
  (let ((attributes
         (case source
           (:local-claims
            (token-local-claims token))
           (:user-claims
            (token-user-claims token))
           (:device-claims
            (token-device-claims token)))))
    (find name attributes :key #'claim-attribute-name :test #'string-equal)))


;; 2.5.3.1.7 LookupAttributeInSacl http://msdn.microsoft.com/en-us/library/hh877826.aspx
(defun lookup-attribute-in-sacl (name sacl)
  (let ((ace 
         (find-if (lambda (ace)
                    (and (eq (ace-type-name-kw ace) :SYSTEM-RESOURCE-ATTRIBUTE)
                         (string-equal (slot-value ace 'name) name)))
                  sacl)))
    (when ace
      (slot-value ace 'claim))))
           
;; 2.5.3.2 Access Check Algorithm Pseudocode http://msdn.microsoft.com/en-us/library/cc230290.aspx
(defun remove* (items list &rest args)
  (mapcan (lambda (item)
            (unless (apply #'member item items args)
              (list item)))
          list))

(defun make-object-tree-node (guid access level)
  (list guid access level))
(defun object-tree-node-access (node)
  (second node))
(defun (setf object-tree-node-access) (value node)
  (setf (second node) value))
(defun object-tree-node-guid (node)
  (first node))

(defun evaluate-token-against-descriptor (token security-descriptor access-mask object-tree sid)
  "OBJECT-TREE is a list of forms (<guid> <mask> <level>)"
  (let ((dacl (slot-value security-descriptor 'dacl))
        (sacl (slot-value security-descriptor 'sacl))
        (remaining-access access-mask)
        (allowed-access nil)
        (denied-access nil)
        (max-allowed-mode nil)
        (local-tree nil))
    (when (and (member :access-system-security remaining-access)
               (member (luid :Security) (token-privileges token)))
      (setf remaining-access (remove :access-system-security remaining-access)))
    (when (and (member :write-access remaining-access)
               (member (luid :Take-Ownership) (token-privileges token)))
      (setf remaining-access (remove :write-owner remaining-access)))
    (when (sid-in-token token (slot-value security-descriptor 'owner-sid) sid)
      (unless (some (lambda (ace)
                      (sid= (slot-value ace 'sid) 
                            (slot-value security-descriptor 'owner-sid)))
                    dacl)
        (setf remaining-access (remove :read-control remaining-access)
              remaining-access (remove :write-dac remaining-access))))
    (when (member :maximum-allowed remaining-access)
      (setf max-allowed-mode t))
    (when object-tree 
      (setf local-tree object-tree)
      (dolist (node local-tree)
        (setf (second node) remaining-access)))
    (dolist (ace dacl)
      (when (member :inherit-only (ace-flags ace))
        (case (ace-type ace)
          (:access-allowed 
           (when (sid-in-token token (ace-sid ace) sid)
             (if max-allowed-mode
                 (setf allowed-access (union allowed-access (ace-access-mask ace)))
                 (progn
                   (setf remaining-access (remove* (ace-access-mask ace) remaining-access))
                   (dolist (node local-tree)
                     (setf (object-tree-node-access node) 
                           (remove* (ace-access-mask ace) (object-tree-node-access node))))))))
          (:access-denied
           (when (sid-in-token token (ace-sid ace) sid)
             (if max-allowed-mode
                 (setf denied-access (union denied-access (ace-access-mask ace)))
                 (when (intersection remaining-access (ace-access-mask ace))
                   (return-from evaluate-token-against-descriptor (values nil nil))))))
          (:access-allowed-object 
           (when (sid-in-token token (ace-sid ace) sid)
             (let ((n (find (ace-object ace) local-tree :key #'object-tree-node-guid :test #'guid=)))
               (when n
                 (setf (object-tree-node-access n) 
                       (remove* (ace-access-mask ace) (object-tree-node-access n) ))
                 (dolist (ns local-tree)
                   (when (guid= (ace-inherited-object ace) (ace-object ace))
                     ;; ace is a direct descendent of ns
                     (setf (object-tree-node-access ns) 
                           (remove* (ace-access-mask ace) (object-tree-node-access ns)))))
                 (let ((g (ace-inherited-object ace)))
                   (when g
                     (dolist (np local-tree)
                       (when (guid= g (object-tree-node-guid np))
                         ;; ace is a direct ancestor of np
                         (setf (object-tree-node-access np)
                               (union (object-tree-node-access np) (object-tree-node-access n)))))))))))
          (:access-denied-object 
           (when (sid-in-token token (ace-sid ace) sid)
             (let ((node (find (ace-object ace) local-tree :key #'object-tree-node-guid :test #'guid=)))
               (when (and node (intersection (object-tree-node-access node) (ace-access-mask ace)))
                 (return-from evaluate-token-against-descriptor (values nil nil))))))
          (:access-allowed-callback
           (when (evaluate-ace-condition token sacl (ace-conditional-expression ace))
             (when (sid-in-token token (ace-sid ace) sid)
               (let ((n (car object-tree)))
                 (dolist (node (cdr object-tree))
                   (setf (object-tree-node-access node) 
                         (union (object-tree-node-access node) (object-tree-node-access n)))))))))))
    (let ((granted-access (union allowed-access (remove* denied-access (mapcar #'car *access-mask*)))))
      (when max-allowed-mode
        (return-from evaluate-token-against-descriptor (values granted-access t)))
      (if (null remaining-access)
          (return-from evaluate-token-against-descriptor (values nil t))))
    (values nil nil)))
          
                 
               
                           
          
        

    
(defun centralized-access-policy-rules (&rest wtf)
  (declare (ignore wtf))
  nil)

;; this is absolutely horrendous. what does it all mean? the 
;; algorithm specified is not very clear
(defun access-check (token security-descriptor access-mask object-tree)
  (let ((effective-access nil)
        (staged-access nil)
        (granted-access nil)
;;        (denied-access nil)
 ;;       (max-allowed-mode nil)
        (remaining-access nil)
        (sacl (slot-value security-descriptor 'sacl))
        (desired-access access-mask)
        (capr-security-descriptor nil))
    (multiple-value-bind (granted allowed) 
        (evaluate-token-against-descriptor token security-descriptor access-mask object-tree nil)
      (setf granted-access granted)
      (unless allowed (return-from access-check (values nil nil)))
      (let ((centralized-access-policy (get-centralized-access-policy sacl)))
        (cond 
          (centralized-access-policy 
           (setf effective-access desired-access
                 staged-access desired-access)
           (dolist (rule (centralized-access-policy-rules centralized-access-policy))
             ;; this is probably not quite correct... what is a rule?
             (when (evaluate-ace-condition token sacl rule)
               (setf capr-security-descriptor 
                     (copy-security-descriptor security-descriptor))
               (setf (slot-value capr-security-descriptor 'dacl)
                     (slot-value (slot-value (slot-value rule 
                                                         'effective-condition) 
                                             'access-condition) 
                                 'dacl))
               (let ((eff (evaluate-token-against-descriptor 
                           token
                           capr-security-descriptor
                           desired-access
                           nil
                           nil)))
                 (setf effective-access 
                       (union effective-access eff)))

               (unless effective-access 
                 (return-from access-check (values nil nil)))))
           ;; there is a bit here about logging differences... prob don't need it
;;           (setf allowed-access (union allowed-access effective-access))
           (setf remaining-access (union desired-access 
                                         (remove* effective-access 
                                                  (mapcar #'car *access-mask*))))
           (dolist (node object-tree)
             (setf (object-tree-node-access node) remaining-access)))
          (t (return-from access-check (values granted-access t))))
;;        (when max-allowed-mode 
  ;;        (setf granted-access
    ;;            (remove* denied-access 
      ;;                   (mapcar #'car *access-mask*)))
        ;;  (return-from access-check (values granted-access t)))
        (if remaining-access 
            (return-from access-check (values granted-access t))
            (return-from access-check (values nil nil)))))))
        
          
                  
                          
                                
              
        
    
  
