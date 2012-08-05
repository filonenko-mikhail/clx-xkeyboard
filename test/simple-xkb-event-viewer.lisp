(declaim (optimize (speed 0) (safety 3) (debug 3) (compilation-speed 0)))
;;;; Open an X-window and emit detail for key-events to the console
;;;; Button2 (usually middle mouse butten) closes window and ends
;;;; application.


(defpackage xkb-test
  (:use cl)
  (:export simple-xkb-event-viewer))

(in-package :xkb-test)

(defun dupl (list)
  (loop for a in list
        append `(',a ,a)))

(defmacro print-key-list-dupled (&rest args)
  `(print-key-list ,@(dupl args)))

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

(defun xkb-event-viewer (display mapping)
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
                                     "Hello World! Press middle mouse button to exit."))
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
                       (eql code 2))
      (:new-keyboard-notify (sequence time device-id old-device-id
                                      min-key-code max-key-code
                                      old-min-key-code old-max-key-code
                                      request-major request-minor changed)
                            (format t "NEW KEYBOARD NOTIFY~%")
                            (print-key-list-dupled sequence time
                                                   device-id old-device-id
                                                   min-key-code max-key-code
                                                   old-min-key-code old-max-key-code
                                                   request-major request-minor
                                                   changed)
                            (format t "END NEW KEYBOARD NOTIFY~%~%")
                            nil)
      (:xkb/map-notify (sequence time device-id
                        ptr-btn-actions changed
                        min-key-code max-key-code
                        first-type n-types
                        first-key-sym n-key-syms
                        first-key-action n-key-actions
                        first-key-behavior n-key-behaviors
                        first-key-explicit n-key-explicits
                        first-mod-map-key n-mod-map-keys
                        first-vmod-map-key n-vmod-map-keys
                        virtual-mods)
                       (format t "XKB MAP NOTIFY~%")
                       (print-key-list-dupled sequence time device-id
                                              ptr-btn-actions changed
                                              min-key-code max-key-code
                                              first-type n-types
                                              first-key-sym n-key-syms
                                              first-key-action n-key-actions
                                              first-key-behavior n-key-behaviors
                                              first-key-explicit n-key-explicits
                                              first-mod-map-key n-mod-map-keys
                                              first-vmod-map-key n-vmod-map-keys
                                              virtual-mods)
                       (format t "END XKB MAP NOTIFY~%~%")
                       nil)
      (:state-notify (sequence time device-id
                               mods base-mods latched-mods
                               locked-mods group base-group
                               latched-group locked-group
                               compat-state grab-mods
                               compat-grab-mods lookup-mods
                               compat-lookup-mods ptr-btn-state
                               changed keycode event-type
                               request-major request-minor)
                     (format t "STATE NOTIFY~%")
                     (print-key-list-dupled sequence time device-id
                                            mods base-mods latched-mods
                                            locked-mods group base-group
                                            latched-group locked-group
                                            compat-state grab-mods
                                            compat-grab-mods lookup-mods
                                            compat-lookup-mods ptr-btn-state
                                            changed keycode event-type
                                            request-major request-minor)
                     (format t "END STATE NOTIFY~%~%")
                     nil)
      (:controls-notify (sequence time device-id
                                  num-groups changed-control
                                  enabled-controls
                                  enabled-control-changes
                                  keycode event-type
                                  request-major request-minor)
                        (format t "CONTROLS NOTIFY~%")
                        (print-key-list-dupled sequence time device-id
                                               num-groups changed-control
                                               enabled-controls
                                               enabled-control-changes
                                               keycode event-type
                                               request-major request-minor)
                        (format t "END CONTROLS NOTIFY~%~%")
                        nil)
      (:indicator-state-notify (sequence time device-id state state-changed)
                               (format t "INDICATOR STATE NOTIFY~%")
                               (print-key-list-dupled sequence time device-id
                                                      state state-changed)
                               (format t "END INDICATOR STATE NOTIFY~%~%")
                               nil)
      (:indicator-map-notify (sequence time device-id state map-changed)
                             (format t "INDICATOR MAP NOTIFY~%")
                             (print-key-list-dupled sequence time
                                                    device-id state map-changed)
                             (format t "END INDICATOR MAP NOTIFY~%~%")
                             nil)
      (:names-notify (sequence timp device-id changed first-type n-types
                               fist-level-name n-level-names n-radio-groups
                               n-key-aliases changed-group-names
                               changed-virtual-mods fist-key n-keys
                               changed-indicators)
                     (format t "NAMES NOTIFY~%")
                     (print-key-list-dupled sequence timp device-id changed first-type
                                            n-types fist-level-name n-level-names
                                            n-radio-groups n-key-aliases
                                            changed-group-names changed-virtual-mods
                                            fist-key n-keys changed-indicators)
                     (format t "END NAMES NOTIFY~%~%")
                     nil)
      (:compat-map-notify (sequence time device-id
                                    changed-groups first-si n-si
                                    n-total-si)
                          (format t "COMPAT MAP NOTIFY~%")
                          (print-key-list-dupled sequence time device-id
                                                 changed-groups first-si n-si
                                                 n-total-si)
                          (format t "END COMPAT MAP NOTIFY~%~%")
                          nil)
      (:bell-notify (sequence time device-id bell-class
                              bell-id percent pitch duration
                              name window event-only)
                    (format t "BELL NOTIFY~%")
                    (print-key-list-dupled sequence time device-id bell-class
                                           bell-id percent pitch duration
                                           name window event-only)
                    (format t "END BELL NOTIFY~%~%")
                    nil)
      (:action-message (sequence time device-id
                                 keycode press key-event-follows mods group
                                 message-byte1 message-byte2 message-byte3 message-byte4
                                 message-byte5 message-byte6 message-byte7 message-byte8)
                       (format t "ACTION MESSAGE~%")
                       (print-key-list-dupled sequence time device-id
                                              keycode press key-event-follows mods group
                                              message-byte1 message-byte2 message-byte3
                                              message-byte4 message-byte5 message-byte6
                                              message-byte7 message-byte8)
                       (format t "END ACTION MESSAGE~%~%")
                       nil)
      (:access-xnotify (sequence time device-id keycode
                                 detail slow-keys-delay debounce-delay)
                       (format t "ACCESS X NOTIFY~%")
                       (print-key-list-dupled sequence time device-id keycode
                                              detail slow-keys-delay debounce-delay)
                       (format t "END ACCESS X NOTIFY~%~%")
                       nil)
      (:extension-device-notify (sequence time device-id
                                          reason led-class
                                          led-id leds-defined
                                          led-state first-button
                                          n-buttons supported
                                          unsupported)
                                (format t "EXTENSION DEVICE NOTIFY~%")
                                (print-key-list-dupled sequence time device-id
                                                       reason led-class
                                                       led-id leds-defined
                                                       led-state first-button
                                                       n-buttons supported
                                                       unsupported)
                                (format t "END EXTENSION DEVICE NOTIFY~%~%")
       nil))))

(defun simple-xkb-event-viewer ()
  (let ((display (xlib:open-default-display)))
    (xlib:enable-xkeyboard display)
    (xlib::xkb/select-events display xlib::+use-core-kbd+
                             '(:new-keyboard-notify :full)
                             '(:xkb/map-notify :full)
                             '(:state-notify :full)
                             '(:controls-notify :full)
                             '(:indicator-state-notify :full)
                             '(:indicator-map-notify :full)
                             '(:names-notify :full)
                             '(:compat-map-notify :full)
                             '(:bell-notify :full)
                             '(:action-message :full)
                             '(:access-xnotify :full)
                             '(:extension-device-notify :full))
    (xkb-event-viewer display (xlib::transform-xkb-keymap-to-client-mapping
                               (xlib::get-map display xlib::+use-core-kbd+
                                              (logior xlib::+KeyTypes+
                                                      xlib::+KeySyms+))))
    (xlib:close-display display)))
