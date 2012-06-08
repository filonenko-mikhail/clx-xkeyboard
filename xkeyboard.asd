;;;; clx-xkeyboard.asd

(asdf:defsystem :xkeyboard
  :serial t
  :description "XKeyboard is X11 extension for clx of the same name."
  :author "Michael Filonenko"
  :license "MIT"
  :depends-on (:clx)
  :components ((:file "package")
               (:file "xkeyboard")))

