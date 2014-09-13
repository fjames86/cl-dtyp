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
(such as auditing accesses to the object). The SACL can normally be ignored.


3.1 SDDL
--------

Security descriptors can be encoded in a string representation using 
Security Descriptor Description Language (SDDL). With the exception of 
conditional expressions, an SDDL parser has been implemented to read 
in and print out security descriptors in SDDL format. 

4. Algorithms 
-------------

Various algorithms are described to allow validating access to objects 
protected with a security descritor. The primary function is access-check.

At present the algorithm functionality is not complete (i.e. they don't work).


5. Notes
---------

This package was written as a largely educational exercise, to understand
the Windows security model, but its possible the functionality could be more
generally useful at some point in the future. 

6. Examples
------------

```
;; parse a SID string into a SID object
(string-sid "S-1-2-3")
-> #<SID S-1-2-3>

;; 


Frank James 
August 2014.

