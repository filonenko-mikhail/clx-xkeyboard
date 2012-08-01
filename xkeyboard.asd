;;;; clx-xkeyboard.asd

(asdf:defsystem :xkeyboard
  :serial t
  :description "XKeyboard is X11 extension for clx of the same name."
  :author "Michael Filonenko <filonenko.mikhail at gmail.com>, Eric Wolf"
  :license "MIT"
  :version "0.1"
  :depends-on (:clx)
  :components ((:file "keysymdef")
               (:file "xkeyboard" :depends-on ("keysymdef"))))

