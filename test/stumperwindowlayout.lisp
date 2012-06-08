;;;; Save keyboard layout per window for stumpwm. 

(asdf:compute-source-registry)
(require :xkeyboard)

(in-package :stumpwm)

(defun get-current-layout (display &optional (device +use-core-kbd+))
  (xlib:device-state-locked-group (xlib:get-state display device)))

(defun window-focus-changed (window previous-window)
  (let ((current-layout (get-current-layout *display*)))
    (when previous-window
      (setf (getf (xlib:window-plist (window-xwin previous-window)) :keyboard-layout) current-layout)
      (when window
        (let ((window-layout (getf (xlib:window-plist (window-xwin window)) :keyboard-layout current-layout)))
          (when (not (equal current-layout window-layout))
            (xlib:lock-group *display* :group window-layout)))))))

(defun group-focus-changed (group previous-group)
  (let ((previous-window (stumpwm::group-current-window previous-group))
        (window (stumpwm::group-current-window group)))
    (format t "prev w ~a next w ~a~%" previous-window window)
    (focus-changed window previous-window)))


(defcommand enable-per-window-layout () ()
  (xlib:enable-xkeyboard *display*) ;; we need it because
  (xlib::initialize-extensions *display*) ;; stumpwm opens display before extension definition
  (add-hook *focus-group-hook* 'group-focus-changed)
  (add-hook *focus-window-hook* 'window-focus-changed))

(defcommand disable-per-window-layout () ()
  (remove-hook *focus-window-hook* 'window-focus-changed)
  (remove-hook *focus-group-hook* 'group-focus-changed))