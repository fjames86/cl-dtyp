
(asdf:defsystem :cl-dtyp
  :name "CL-DTYP"
  :author "Frank James <frank.a.james@gmail.com>"
  :description "Microsoft Data Types"
  :license "BSD"
  :components
  ((:file "package")
   (:file "structures" :depends-on ("package"))
   (:file "aces" :depends-on ("structures"))
   (:file "sddl" :depends-on ("aces"))
   (:file "algorithms" :depends-on ("sddl"))
   (:file "ntstatus" :depends-on ("package")))
  :depends-on (:packet :cl-ppcre))





