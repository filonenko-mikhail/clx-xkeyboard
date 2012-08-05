(in-package :xlib)

(defgeneric mask-normalize (mask-type mask))

(defmacro define-xkb-mask-type (base-type name &rest keys)
  (let* ((mask-vector-symbol (xintern #\+ name "-VECTOR+"))
         (type-symbol (xintern name))
         (mask-class-symbol (xintern name "-CLASS")))
    `(progn
       (defconstant ,mask-vector-symbol
         '#(,@keys))
       (deftype ,mask-class-symbol ()         
         '(member ,@keys))
       (deftype ,type-symbol ()
         '(or ,base-type (clx-list ,mask-class-symbol) (eql :full)))
       (defmethod mask-normalize ((mask-type (eql ',type-symbol)) mask)
         (declare (type ,type-symbol mask))
         (declare (clx-values ,base-type))
         (let ((real-mask (or (when (eql mask :full)
                                (coerce ,mask-vector-symbol 'list))
                              mask)))
           (or (encode-mask ,mask-vector-symbol real-mask ',mask-class-symbol)
               (x-type-error mask ',type-symbol)))))))

(define-xkb-mask-type mask16 mappart
  :key-types
  :key-syms
  :modifier-map
  :explicit-components
  :key-actions
  :key-behaviors
  :virtual-mods
  :virtual-mod-map)

(define-xkb-mask-type mask16 eventtype 
  :new-keyboard-notify
  :xkb/map-notify
  :state-notify
  :controls-notify
  :indicator-state-notify
  :indicator-map-notify
  :names-notify
  :compat-map-notify
  :bell-notify
  :action-message
  :access-xnotify
  :extension-device-notify)

;;above macro-invocation will expand to 
;;(eval-when should not be needed, as the established methods
;;will only be needed in other compilation units)
;; (defconstant +eventtype-vector+
;;   '#(:new-keyboard-notify
;;            :xkb/map-notify
;;            :state-notify
;;            :controls-notify
;;            :indicator-state-notify
;;            :indicator-map-notify
;;            :names-notify
;;            :compat-map-notify
;;            :bell-notify
;;            :action-message
;;            :access-xnotify
;;            :extension-device-notify))

;; (deftype eventtype-class ()
;;   '(member :new-keyboard-notify
;;            :xkb/map-notify
;;            :state-notify
;;            :controls-notify
;;            :indicator-state-notify
;;            :indicator-map-notify
;;            :names-notify
;;            :compat-map-notify
;;            :bell-notify
;;            :action-message
;;            :access-xnotify
;;            :extension-device-notify))

;; (deftype eventtype ()
;;   '(or mask16 (clx-list eventtype-class)))

;; (defmethod mask-normalize ((mask-type (eql 'eventtype)) mask)
;;   (declare (type eventtype mask))
;;   (declare (clx-values mask16))
;;   (or (encode-mask +eventtype-vector+ mask 'eventtype-class)
;;       (x-type-error mask 'eventtype)))
