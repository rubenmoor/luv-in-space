;;; decision 3: luv ASDF systems the game depends on
(defsystem "luv-in-space"
  :description "Luv in Space: a hard sci-fi spaceflight game on luv."
  :version "0.0.1"
  :depends-on ("luv")
  :serial t
  :pathname "src/"
  :components ((:file "package")
               (:file "math")
               (:file "units")
               (:file "bodies")
               (:file "orbits")
               (:file "physics")
               (:file "ship")
               (:file "places")
               (:file "controls")
               (:file "clock")
               (:file "hazards")
               (:file "prediction")
               (:file "autopilot")
               (:file "flights")
               (:file "shaders")
               (:file "assets")
               (:file "gpu")
               (:file "camera")
               (:file "stars")
               (:file "impostors")
               (:file "meshes")
               (:file "ribbons")
               (:file "post")
               (:file "overlay")
               (:file "render")
               (:file "input")
               (:file "main")
               (:file "capture")))

(defsystem "luv-in-space/program"
  :description "The standalone luv-in-space executable."
  :depends-on ("luv-in-space")
  :build-operation "program-op"
  :build-pathname "build/luv-in-space"
  :entry-point "luv-in-space:main")

(defsystem "luv-in-space/tests"
  :description "Tests of luv-in-space, run by scripts/test."
  :depends-on ("luv-in-space" "luv/test-support" "parachute")
  :serial t
  :pathname "tests/"
  :components ((:file "package")
               (:file "math")
               (:file "units")
               (:file "bodies")
               (:file "orbits")
               (:file "physics")
               (:file "ship")
               (:file "places")
               (:file "controls")
               (:file "clock")
               (:file "hazards")
               (:file "prediction")
               (:file "autopilot")
               (:file "flights")
               (:file "camera")
               (:file "assets")
               (:file "stars")
               (:file "impostors")
               (:file "meshes")
               (:file "ribbons")
               (:file "post")
               (:file "overlay")
               (:file "render")
               (:file "input")
               (:file "capture"))
  :perform (test-op (operation component)
             (declare (ignore operation component))
             (uiop:symbol-call '#:luv.test-support '#:test-package
                               '#:luv-in-space.tests)))

;;; decision 123: Milestone 3's flight scripts run in scripts/test-slow, not scripts/test
(defsystem "luv-in-space/slow-tests"
  :description "The slow tests of luv-in-space, run by scripts/test-slow."
  :depends-on ("luv-in-space/tests")
  :pathname "tests/"
  :components ((:file "slow-flights"))
  :perform (test-op (operation component)
             (declare (ignore operation component))
             (uiop:symbol-call '#:luv.test-support '#:test-package
                               '#:luv-in-space.slow-tests)))
