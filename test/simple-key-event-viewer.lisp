(declaim (optimize (speed 0) (safety 3) (debug 3) (compilation-speed 0)))
;;;; Open an X-window and emit detail for key-events to the console
;;;; Button2 (usually middle mouse butten) closes window and ends
;;;; application.

(asdf:compute-source-registry)

(asdf:load-system :xkeyboard)

(defun print-key-list (&rest args)
  (fresh-line)
  (format t "~{   ~a: ~a~%~}" args))

(defun print-key-event (mapping type code sequence time
                        root event-window child
                        root-x root-y x y state same-screen-p)
  (format t "~%~%~a~%" type)
  (print-key-list
   :code code
   :sequence sequence
   :time time
   :root-window root
   :event-window event-window
   :child-window child
   :root-x root-x
   :root-y root-y
   :x x
   :y y
   :state state
   :same-screen-p same-screen-p)
  (let* ((symmap (svref (xlib::client-mapping-symmaps mapping) code))
         (effective-group (xlib::effective-group
                           (xlib::corestate->group state)
                           (xlib::client-keysymmap-num-groups symmap)
                           (xlib::client-keysymmap-groups-wrap symmap)
                           (xlib::client-keysymmap-redirect-group symmap)))
         (keytype (svref (xlib::client-keysymmap-keytypes symmap) effective-group)))
    (format t "    state(binary): ~b~%" state)
    (format t "    group: ~a~%" (xlib::corestate->group state))
    (format t "    effective group: ~a~%" effective-group)
    (format t "    shiftlevel: ~a~%" (xlib::shiftlevel/leftover-modifiers
                                      keytype (xlib::corestate->mask state)))
    (multiple-value-bind (keysym lm) (xlib::keyevent->keysym mapping code state)
      (format t "   Keysym: ~x~%   Symbolic Keysym: ~a~%   Character: ~a~%   LM: ~a~%   Character (after LM processing): ~a~%"
              keysym
              (or (cadr (assoc :sym (gethash keysym xlib::+xkbkeysymdb+))) 'unknown)
              (xlib::xkb/keysym->character keysym xlib::+xkbkeysymdb+)
              lm
              (multiple-value-bind (keysym character)
                  (xlib::process-leftover-modifiers keysym lm xlib::+xkbkeysymdb+)
                (declare (ignore keysym))
                character)))))

(defun key-event-viewer (display mapping)
  (let* ((screen (first (xlib:display-roots display)))
         (black (xlib:screen-black-pixel screen))
         (white (xlib:screen-white-pixel screen))
         (rootwindow (xlib:screen-root screen))
         (grackon (xlib:create-gcontext :drawable rootwindow :foreground white
                                        :background black))
         (window (xlib:create-window
                  :parent rootwindow
                  :x 0
                  :y 0
                  :width 100
                  :height 100
                  :background black
                  :event-mask (xlib:make-event-mask :exposure :key-press
                                                    :key-release :button-press
                                                    :button-release))))
    (xlib:map-window window)
    (xlib:event-case (display :force-output-p t :discard-p t)
      (:exposure (count)
                 (when (zerop count)
                   (xlib:draw-glyphs window
                                     grackon
                                     20
                                     50
                                     "Hello World!"))
                 nil)
      (:key-press (code sequence time root event-window
                        child root-x root-y x y state same-screen-p)
                  (print-key-event mapping :key-press
                                   code sequence time root
                                   event-window child root-x root-y
                                   x y state same-screen-p)
                  nil)
      (:key-release (code sequence time root event-window
                          child root-x root-y x y state same-screen-p)
                    (print-key-event mapping :key-release
                                     code sequence time root
                                     event-window child root-x root-y
                                     x y state same-screen-p)
                    nil)
      (:button-release (code)
                       (if (eql code 2)
                           (xlib:close-display display))
                       (eql code 2)))))

(defun simple-key-event-viewer ()
  (let ((display (xlib:open-default-display)))
    (xlib:enable-xkeyboard display)
    (key-event-viewer display (xlib::transform-xkb-keymap-to-client-mapping
                               (xlib::get-map display xlib::+use-core-kbd+
                                              (logior xlib::+KeyTypes+
                                                      xlib::+KeySyms+))))
    (xlib:close-display display)))
