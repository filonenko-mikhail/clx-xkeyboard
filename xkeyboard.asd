;;;; clx-xkeyboard.asd

(asdf:defsystem :xkeyboard
  :serial t
  :description "XKeyboard is X11 extension for clx of the same name."
  :author "Michael Filonenko <filonenko.mikhail at gmail.com>, Eric Wolf <eric at boese-wolf.eu>"
  :license "MIT"
  :version "0.1"
  :depends-on (:clx)
  :components ((:file "keysymdef")
               (:file "xkeyboard" :depends-on ("keysymdef"))))


(asdf:defsystem :xkeyboard-test
  :serial t
  :description "TEst code for XKeyboard"
  :author "Eric Wolf <eric at boese-wolf.eu>"
  :license "MIT"
  :version "0.1"
  :depends-on (:xkeyboard)
  :components ((:module test
                :components ((:file "simple-key-event-viewer")))))

