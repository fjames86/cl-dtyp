cl-dtyp
=======

Common Lisp implementation of Windows Security Model (MS-DTYP).


This package implements the data structures and algorithms described 
in the MS-DTYP specification, http://msdn.microsoft.com/en-us/library/cc230273.aspx. 

These are primarily related to the Windows security model.

All structures have associated binary serialization functions so that they 
can be sent "over the wire" if required.

1. Overview
------------

All structures have a pack- and unpack- function for binary serialization. 

2. ACE structures
------------------

The Windows security model uses various types of ACE to form an ACL. Although
there are several types, they are mostly similar and are variants of either
an access-allowed or access-denied ACE. 

The functions make-ace, pack-ace, pack-ace* and unpack-ace can be used to operate
on them.

2.1 Conditional ACEs
----------------------

Some types of ace ("callback" ACEs) can be given a conditional expression
to determine when they apply. This information is entered as a Lisp expression
and has a well-defined binary representation. The serialization is handled 
automatically by the Lisp functions. 

There are some examples in the Microsoft specification and these have been
implemented. 

2.1.1 Example 
--------------

Three examples below take a Lisp input, pack and unpack to get back the 
same original expression.

```
(test-expression-1)
(:EQUALS (:LOCAL "Title") (:STRING "VP"))

(test-expression-2)
(:AND
 (:OR (:EQUALS (:USER "smartcard") (:INT64 1))
  (:EQUALS (:DEVICE "managed") (:INT64 1)))
 (:ANY-OF (:RESOURCE "Dept") (:COMPOSITE ((:STRING "sales") (:STRING "HR")))))

(test-expression-3)
(:OR
 (:GREATER-THAN-EQUALS (:USER "clearanceLevel")
  (:RESOURCE "requiredClearance"))
 (:MEMBER-OF (:COMPOSITE ((:SID #<SID S-1-5-32-544>)))))
```

3. Security descriptors
-------------------------

A security descriptor contains 2 sids (the owner and group) and 2 ACLs,
the Discretionary ACL (DACL) and System ACL (SACL). The DACL is used to determine
access to the object, whereas the SACL is used for advanced system purposes 
(such as auditing accesses to the object). The SACL can typically be ignored.


3.1 SDDL
--------

Security descriptors can be encoded in a string representation using 
Security Descriptor Description Language (SDDL). With the exception of 
conditional expressions, an SDDL parser has been implemented to read 
in and print out security descriptors in SDDL format.

[ Note: reading of SDDL conditional expressions is not implemented ]

4. Algorithms 
-------------

Various algorithms are described to allow validating access to objects 
protected with a security descritor. The primary function is access-check.

At present the algorithm functionality is not complete (i.e. they don't work). This is mainly because the algorithm descriptions in the MS-DTYP documentation are not clear.


5. Notes
---------

This package was written as a largely educational exercise, to understand
the Windows security model, but it's possible the functionality could be more
generally useful at some point in the future. 

Documentation is poor so I should write it down before I forget what it all does. Also the algorithms section is incomplete (and unlikely to be finished in the near-future). Also there are probably bugs galore.


6. Examples
------------

```
;; parse a SID string into a SID object
CL-DTYP> (string-sid "S-1-2-3")
#<SID S-1-2-3>

;; get a wellknown sid
CL-DTYP> (wellknown-sid :everyone)
#<SID S-1-1-0>

;; pack/unpack an ACE
CL-DTYP> (pack-ace :access-allowed nil nil :everyone)
#(0 0 20 0 0 0 0 0 1 1 0 0 0 0 0 1 0 0 0 0)
CL-DTYP> (unpack-ace (pack-ace :access-allowed nil nil :everyone))
#<ACCESS-ALLOWED-ACE A;;;;;WD>

;; security descriptor
CL-DTYP> (make-security-descriptor nil :builtin-administrators :everyone nil (list (make-ace :access-allowed nil nil :everyone)))
#<SECURITY-DESCRIPTOR O:BA;G:WD;D:(A;;;;;WD);S:>

;; conditional aces
CL-DTYP> (pack-ace :access-allowed-callback nil nil :everyone :conditional-expression '(:not (:local "frank")))
#(9 0 22 0 0 0 0 0 1 1 0 0 0 0 0 1 0 0 0 0 97 114 116 120 248 10 0 0 0 102 0
  114 0 97 0 110 0 107 0 162)
CL-DTYP> (unpack-ace (pack-ace :access-allowed-callback nil nil :everyone :conditional-expression '(:not (:local "frank"))))
#<ACCESS-ALLOWED-CALLBACK-ACE XA;;;;;WD;(! @Local.frank )>
```

Frank James 
August 2014.

