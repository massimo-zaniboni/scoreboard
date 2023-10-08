(in-package :asdf-user)

(defsystem "scoreboard"
  :description "Show the scoreboard of a turn based games where players can make points at every turn, as a linear graph."
  :version "0.1"
  :author "Massimo Zaniboni <mzan@dokmelody.org>"
  :licence "LGPLv3+"
  :depends-on (
    #:trivia
    #:alexandria
    #:serapeum
    #:trivial-types
    #:defstar
    #:str
    #:parse-float
    #:iterate
    #:let-plus
    #:array-operations
    #:bordeaux-threads
    #:esrap
    #:for
    #:eazy-gnuplot
    #:cmd
    )
  :pathname "src/"
  :serial t
  :components
    ((:file "package")
     (:file "main"))
  :build-operation "program-op"
  :build-pathname "scoreboard"
  :entry-point "scoreboard:main")
