;;; decision 3: luv ASDF systems the game depends on
(defsystem "luv-in-space"
  :description "Luv in Space: a hard sci-fi spaceflight game on luv."
  :version "0.0.1"
  :depends-on ("luv")
  :serial t
  :pathname "src/"
  :components ((:file "package")
               (:file "main")))

(defsystem "luv-in-space/program"
  :description "The standalone luv-in-space executable."
  :depends-on ("luv-in-space")
  :build-operation "program-op"
  :build-pathname "build/luv-in-space"
  :entry-point "luv-in-space:main")
