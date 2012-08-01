;;;; clx-xkeyboard.lisp

(in-package :xlib)

(pushnew :clx-ext-xkeyboard *features*)

(define-extension "XKEYBOARD"
  :events () 
  :errors (xkeyboard-error))

(export '(+use-core-kbd+
          +use-core-ptr+

          +xkbkeysymdb+

          xkeyboard-error
          make-device-state
          device-state-p
          copy-device-state
          device-state-device-id
          device-state-mods
          device-state-base-mods
          device-state-latched-mods
          device-state-locked-mods
          device-state-group
          device-state-locked-group
          device-state-base-group
          device-state-latched-group
          device-state-compat-state
          device-state-grab-mods
          device-state-compat-grab-mods
          device-state-lookup-mods
          device-state-compat-lookup-mods
          device-state-ptr-btn-state

          get-state
          latch-lock-state
          lock-group

          get-map
          transform-xkb-keymap-to-client-mapping
          keyevent->keysym
          xkb/keysym->character

          client-mapping
          process-leftover-modifiers
          client-mapping-symmaps
          effective-group
          corestate->group
          client-keysymmap-num-groups
          client-keysymmap-groups-wrap
          client-keysymmap-redirect-group
          client-keysymmap-keytypes
          shiftlevel/leftover-modifiers
          corestate->mask
          ))

(define-condition xkeyboard-error (request-error) ())

(define-error xkeyboard-error decode-core-error)


;;; Types
(defmacro define-card8-abrev (name)
  `(define-accessor ,name (8)
     ((index) `(read-card8 ,index))
     ((index thing) `(write-card8 ,index ,thing))))

(defmacro define-card16-abrev (name)
  `(define-accessor ,name (16)
     ((index) `(read-card16 ,index))
     ((index thing) `(write-card16 ,index ,thing))))

(defmacro define-card32-abrev (name)
  `(define-accessor ,name (32)
     ((index) `(read-card32 ,index))
     ((index thing) `(write-card32 ,index ,thing))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-card16-abrev devicespec)
  (define-card8-abrev keycode)
  (define-card8-abrev keymask)

  (define-card16-abrev butmask)
  (define-card16-abrev vmodmask)

  (define-card16-abrev gbndetailmask)
  (define-card16-abrev devfeature)
  (define-card16-abrev feature)
  (define-card16-abrev idresult)
  (define-card16-abrev vmod)
  (define-card16-abrev idspec) 
  (define-card16-abrev devicespec)
  (define-card16-abrev behavior) 
  (define-card16-abrev mapdetails)
  (define-card32-abrev indicator)
  (define-card32-abrev name-detail)
  (define-card8-abrev belldetail)
  (define-card8-abrev msgdetail)
  (define-card8-abrev imflags) 
  (define-card8-abrev explicit)
  (define-card16-abrev eventtype) 
  (define-card16-abrev bellclassspec)
  (define-card16-abrev nkndetail)
  (define-card16-abrev statepart)
  (define-card32-abrev control)
  (define-card8-abrev cmdetail)
  (define-card16-abrev xidetail) 

  (define-card8-abrev group)

  (define-card8-abrev groups)
  (define-card8-abrev immodswhich)
  (define-card8-abrev imgroupswhich)
  (define-card8-abrev imflag)
  (define-card8-abrev bellclassresult)
  (define-card16-abrev axoption)
  (define-card32-abrev boolctrl)
  (define-card16-abrev mappart)
  (define-card16-abrev setmapflags) 
  (define-card16-abrev ledclassspec)
  (define-card16-abrev axndetail)
  (define-card32-abrev namedetail)
  (define-card32-abrev perclientflag))

(deftype devicespec () 'card8)
(deftype keycode () 'card8)
(deftype keymask () 'card8)
(deftype butmask () 'card16)
(deftype vmodmask () 'card16)
(deftype gbndetailmask () 'card16)
(deftype devfeature () 'card16)
(deftype feature () 'card16)
(deftype idresult () 'card16)
(deftype vmod () 'card16)
(deftype idspec () 'card16)
(deftype devicespec () 'card16)
(deftype behavior () 'card16)
(deftype mapdetails () 'card16)
(deftype indicator () 'card32)
(deftype name-detail () 'card32)
(deftype belldetail () 'card8)
(deftype msgdetail () 'card8)
(deftype imflags () 'card8)
(deftype explicit () 'card8)
(deftype eventtype () 'card16)
(deftype bellclassspec () 'card16)
(deftype nkndetail () 'card16)
(deftype statepart () 'card16)
(deftype control () 'card32)
(deftype cmdetail () 'card8)
(deftype xidetail () 'card16)
(deftype group () 'card8)
(deftype groups () 'card8)
(deftype immodswhich () 'card8)
(deftype imgroupswhich () 'card8)
(deftype imflag () 'card8)
(deftype bellclassresult () 'card8)
(deftype axoption () 'card16)
(deftype boolctrl () 'card32)
(deftype mappart () 'card16)
(deftype setmapflags () 'card16)
(deftype ledclassspec () 'card16)
(deftype axndetail () 'card16)
(deftype namedetail () 'card32)
(deftype perclientflag () 'card32)

 ;;; XKeyboard opcodes
(defconstant +use-extension+ 0)
(defconstant +select-events+ 1)
(defconstant +bell+ 3)
(defconstant +get-state+ 4)
(defconstant +latch-lock-state+ 5)
(defconstant +get-controls+ 6)
(defconstant +set-controls+ 7)
(defconstant +get-map+ 8)
(defconstant +set-map+ 9)
(defconstant +get-compat-map+ 10)
(defconstant +set-compat-map+ 11)
(defconstant +get-indicator-state+ 12)
(defconstant +get-indicator-map+ 13)
(defconstant +set-indicator-map+ 14)
(defconstant +get-named-indicator+ 15)
(defconstant +set-named-indicator+ 16)
(defconstant +get-names+ 17)
(defconstant +set-names+ 18)
(defconstant +get-geometry+ 19)
(defconstant +set-geometry+ 20)
(defconstant +per-client-flags+ 21)
(defconstant +list-components+ 22)
(defconstant +get-kbd-by-name+ 23)
(defconstant +get-device-info+ 24)
(defconstant +set-device-info+ 25)
(defconstant +set-debugging-flags+ 101)

 ;;; Enum Constants
(defconstant +new-keyboard-notify+ #x0001)
(defconstant +map-notify+ #x0002)
(defconstant +state-notify+ #x0004)
(defconstant +controls-notify+ #x0008)
(defconstant +indicator-state-notify+ #x0010)
(defconstant +indicator-map-notify+ #x0020)
(defconstant +names-notify+ #x0040)
(defconstant +compat-map-notify+ #x0080)
(defconstant +bell-notify+ #x0100)
(defconstant +action-message+ #x0200)
(defconstant +access-xnotify+ #x0400)
(defconstant +extension-device-notify+ #x0800)

(defconstant +NKN_Keycodes+ #x01)
(defconstant +NKN_Geometry+ #x02)
(defconstant +NKN_DeviceID+ #x04)
(defconstant +AXN_SKPress+ #x01)
(defconstant +AXN_SKAccept+ #x02)
(defconstant +AXN_SKReject+ #x04)
(defconstant +AXN_SKRelease+ #x08)
(defconstant +AXN_BKAccept+ #x10)
(defconstant +AXN_BKReject+ #x20)
(defconstant +AXN_AXKWarning+ #x40)

;SETofKB_MAPPART
(defconstant +KeyTypes+ #x0001)
(defconstant +KeySyms+ #x0002)
(defconstant +ModifierMap+ #x0004)
(defconstant +ExplicitComponents+ #x0008)
(defconstant +KeyActions+ #x0010)
(defconstant +KeyBehaviors+ #x0020)
(defconstant +VirtualMods+ #x0040)
(defconstant +VirtualModMap+ #x0080)

;SETofKEYMASK clx never explicitly defines them
(defconstant +shift+ #x0001)
(defconstant +lock+ #x0002)
(defconstant +control+ #x0004)
(defconstant +mod1+ #x0008)
(defconstant +mod2+ #x0010)
(defconstant +mod3+ #x0020)
(defconstant +mod4+ #x0040)
(defconstant +mod5+ #x0080)

(defconstant +ModifierState+ #x0001)
(defconstant +ModifierBase+ #x0002)
(defconstant +ModifierLatch+ #x0004)
(defconstant +ModifierLock+ #x0008)
(defconstant +GroupState+ #x0010)
(defconstant +GroupBase+ #x0020)
(defconstant +GroupLatch+ #x0040)
(defconstant +GroupLock+ #x0080)
(defconstant +CompatState+ #x0100)
(defconstant +GrabMods+ #x0200)
(defconstant +CompatGrabMods+ #x0400)
(defconstant +LookupMods+ #x0800)
(defconstant +CompatLookupMods+ #x1000)
(defconstant +PointerButtons+ #x2000)
(defconstant +RepeatKeys+ #x00000001)
(defconstant +SlowKeys+ #x00000002)
(defconstant +BounceKeys+ #x00000004)
(defconstant +StickyKeys+ #x00000008)
(defconstant +MouseKeys+ #x00000010)
(defconstant +MouseKeysAccel+ #x00000020)
(defconstant +AccessXKeys+ #x00000040)
(defconstant +AccessXTimeoutMask+ #x00000080)
(defconstant +AccessXFeedbackMask+ #x00000100)
(defconstant +AudibleBellMask+ #x00000200)
(defconstant +Overlay1Mask+ #x00000400)
(defconstant +Overlay2Mask+ #x00000800)
(defconstant +IgnoreGroupLockMask+ #x00001000)
(defconstant +RepeatKeys+ #x00000001) 
(defconstant +SlowKeys+ #x00000002)
(defconstant +BounceKeys+ #x00000004)
(defconstant +StickyKeys+ #x00000008)
(defconstant +MouseKeys+ #x00000010)
(defconstant +MouseKeysAccel+ #x00000020)
(defconstant +AccessXKeys+ #x00000040)
(defconstant +AccessXTimeoutMask+ #x00000080)
(defconstant +AccessXFeedbackMask+ #x00000100)
(defconstant +AudibleBellMask+ #x00000200)
(defconstant +Overlay1Mask+ #x00000400)
(defconstant +Overlay2Mask+ #x00000800)
(defconstant +IgnoreGroupLockMask+ #x00001000)
(defconstant +GroupsWrap+ 27) 
(defconstant +InternalMods+ 28)
(defconstant +IgnoreLockMods+ 29)
(defconstant +PerKeyRepeat+ 30)
(defconstant +ControlsEnabled+ 31)
(defconstant +AX_SKPressFB+ #x0001) 
(defconstant +AX_SKAcceptFB+ #x0002)
(defconstant +AX_FeatureFB+ #x0004) 
(defconstant +AX_SlowWarnFB+ #x0008)
(defconstant +AX_IndicatorFB+ #x0010)
(defconstant +AX_StickyKeysFB+ #x0020)
(defconstant +AX_SKReleaseFB+ #x0100)
(defconstant +AX_SKRejectFB+ #x0200) 
(defconstant +AX_BKRejectFB+ #x0400)
(defconstant +AX_DumbBell+ #x0800)
(defconstant +AX_TwoKeys+ #x0040)
(defconstant +AX_LatchToLock+ #x0080)
(defconstant +AX_SKPressFB+ #x0001) 
(defconstant +AX_SKAcceptFB+ #x0002)
(defconstant +AX_FeatureFB+ #x0004)
(defconstant +AX_SlowWarnFB+ #x0008)
(defconstant +AX_IndicatorFB+ #x0010)
(defconstant +AX_StickyKeysFB+ #x0020)
(defconstant +AX_TwoKeys+ #x0040)
(defconstant +AX_LatchToLock+ #x0080)
(defconstant +AX_SKReleaseFB+ #x0100)
(defconstant +AX_SKRejectFB+ #x0200)
(defconstant +AX_BKRejectFB+ #x0400)
(defconstant +AX_DumbBell+ #x0800)

(defconstant +kbd-feedback-class+ 0)
(defconstant +led-feedback-class+ 4)
(defconstant +kbd-feedback-class+ 0)
(defconstant +led-feedback-class+ 4)

(defconstant +DfltXIClass+ #x0300)
(defconstant +AllXIClasses+ #x0500)
(defconstant +KbdFeedbackClass+ 0)
(defconstant +BellFeedbackClass+ 5)
(defconstant +KbdFeedbackClass+ 0)
(defconstant +BellFeedbackClass+ 5)
(defconstant +DfltXIClass+ #x0300)
(defconstant +DfltXIId+ #x0400)
(defconstant +DfltXIId+ #x0400)
(defconstant +XINone+ #xff00)
(defconstant +DfltXIId+ #x0400)
(defconstant +AllXIIds+ #x0500)

(defconstant +group1+ 0)
(defconstant +group2+ 1)
(defconstant +group3+ 2)
(defconstant +group4+ 3)
(defconstant +group1+ 0)
(defconstant +group2+ 1)
(defconstant +group3+ 2)
(defconstant +group4+ 3)

(defconstant +any-group+ 254)
(defconstant +all-groups+ 255)
(defconstant +wrap-into-range+ #x00)
(defconstant +clamp-into-range+ #x40)
(defconstant +redirect-into-range+ #x80)
;; (defconstant +virtual_modifier_15+ #x80)
;; (defconstant +virtual_modifier_14+ #x40)
;; (defconstant +virtual_modifier_13+ #x20)
;; (defconstant +virtual_modifier_12+ #x10)
;; (defconstant +virtual_modifier_11+ #x08)
;; (defconstant +virtual_modifier_10+ #x04)
;; (defconstant +virtual_modifier_9+ #x02)
;; (defconstant +virtual_modifier_8+ #x01)
;; (defconstant +virtual_modifier_7+ #x80)
;; (defconstant +virtual_modifier_6+ #x40)
;; (defconstant +virtual_modifier_5+ #x20)
;; (defconstant +virtual_modifier_4+ #x10)
;; (defconstant +virtual_modifier_3+ #x08)
;; (defconstant +virtual_modifier_2+ #x04)
;; (defconstant +virtual_modifier_1+ #x02)
;; (defconstant +virtual_modifier_0+ #x01)
;; (defconstant +virtual_modifier_15+ #x8000)
;; (defconstant +virtual_modifier_14+ #x4000)
;; (defconstant +virtual_modifier_13+ #x2000)
;; (defconstant +virtual_modifier_12+ #x1000)
;; (defconstant +virtual_modifier_11+ #x0800)
;; (defconstant +virtual_modifier_10+ #x0400)
;; (defconstant +virtual_modifier_9+ #x0200)
;; (defconstant +virtual_modifier_8+ #x0100)
;; (defconstant +virtual_modifier_7+ #x0080)
;; (defconstant +virtual_modifier_6+ #x0040)
;; (defconstant +virtual_modifier_5+ #x0020)
;; (defconstant +virtual_modifier_4+ #x0010)
;; (defconstant +virtual_modifier_3+ #x0008)
;; (defconstant +virtual_modifier_2+ #x0004)
;; (defconstant +virtual_modifier_1+ #x0002)
;; (defconstant +virtual_modifier_0+ #x0001)
(defconstant +explicit-vmod-map+ #x80)
(defconstant +explicit-behavior+ #x40)
(defconstant +explicit-auto-repeat+ #x20)
(defconstant +explicit-interpret+ #x10)
(defconstant +explicit-key-type4+ #x08)
(defconstant +explicit-key-type3+ #x04)
(defconstant +explicit-key-type2+ #x02)
(defconstant +explicit-key-type1+ #x01)

(defconstant +IM_NoExplicit+ #x80)
(defconstant +IM_NoAutomatic+ #x40)
(defconstant +IM_LEDDrivesKB+ #x20)
(defconstant +IM_UseCompat+ #x10)
(defconstant +IM_UseEffective+ #x08)
(defconstant +IM_UseLocked+ #x04)
(defconstant +IM_UseLatched+ #x02)
(defconstant +IM_UseBase+ #x01)
(defconstant +IM_UseCompat+ #x10)
(defconstant +IM_UseEffective+ #x08)
(defconstant +IM_UseLocked+ #x04)
(defconstant +IM_UseLatched+ #x02)
(defconstant +IM_UseBase+ #x01)
(defconstant +SymInterp+ #x01)
(defconstant +GroupCompat+ #x02)

(defconstant +keycodes-name+ #x0001)
(defconstant +geometry-name+ #x0002)
(defconstant +symbols-name+ #x0004)
(defconstant +phys-symbols-name+ #x0008)
(defconstant +types-name+ #x0010)
(defconstant +compat-name+ #x0020)
(defconstant +key-type-names+ #x0040)
(defconstant +ktlevel-names+ #x0080)
(defconstant +indicator-names+ #x0100)
(defconstant +key-names+ #x0200)
(defconstant +key-aliases+ #x0400)
(defconstant +virtual-mod-names+ #x0800)
(defconstant +group-names+ #x1000)
(defconstant +rgnames+ #x2000)

(defconstant +GBN_Types+ #x01)
(defconstant +GBN_CompatMap+ #x02)
(defconstant +GBN_ClientSymbols+ #x04)
(defconstant +GBN_ServerSymbols+ #x08)
(defconstant +GBN_IndicatorMaps+ #x10)
(defconstant +GBN_KeyNames+ #x20)
(defconstant +GBN_Geometry+ #x40)
(defconstant +GBN_OtherNames+ #x80)
(defconstant +XI_ButtonActions+ #x02)
(defconstant +XI_IndicatorNames+ #x04)
(defconstant +XI_IndicatorMaps+ #x08)
(defconstant +XI_IndicatorState+ #x10)
(defconstant +XI_Keyboards+ #x01)
(defconstant +XI_ButtonActions+ #x02)
(defconstant +XI_IndicatorNames+ #x04)
(defconstant +XI_IndicatorMaps+ #x08)
(defconstant +XI_IndicatorState+ #x10)
(defconstant +XI_Keyboards+ #x01)
(defconstant +XI_ButtonActions+ #x02)
(defconstant +XI_IndicatorNames+ #x04)
(defconstant +XI_IndicatorMaps+ #x08)
(defconstant +XI_IndicatorState+ #x10)
(defconstant +XI_UnsupportedFeature+ #x8000)
(defconstant +DetectableAutorepeat+ #x01)
(defconstant +GrabsUseXKBState+ #x02)
(defconstant +AutoResetControls+ #x04)
(defconstant +LookupStateWhenGrabbed+ #x08)
(defconstant +SendEventUsesXKBState+ #x10)
(defconstant +SetMapResizeTypes+ #x01)
(defconstant +SetMapRecomputeActions+ #x02)
(defconstant +OutlineDoodad+ 1)
(defconstant +SolidDoodad+ 2)
(defconstant +TextDoodad+ 3)
(defconstant +IndicatorDoodad+ 4)
(defconstant +LogoDoodad+ 5)

 ;;; The version we implement
(defconstant +major-version+ 1)
(defconstant +minor-version+ 0)

 ;;; KB_DEVICESPEC card8
 ;;; 0..255     input extension device id
(defconstant +use-core-kbd+ #x100)
(defconstant +use-core-ptr+ #x200)

 ;;; KB_BELLCLASSRESULT card8
;;KbdFeedbackClass , BellFeedbackClass , DfltXIClass ,
;;                  AllXIClasses
;;     0     KbdFeedbackClass
;;     5     BellFeedbackClass
(defconstant +kbd-feedback-class+ 0)
(defconstant +bell-feedback-class+ 5)

(defstruct moddef
  (mask 0 :type keymask)
  (real-mods 0 :type keymask)
  (vmods 0 :type vmodmask))

;; TODO
(defstruct action
  (type 0 :type card8))

(defmacro xkeyboard-opcode (display)
  `(extension-opcode ,display "XKEYBOARD"))

(defun enable-xkeyboard (display &optional (major +major-version+) (minor +minor-version+))
  (declare (type display display))
  (with-buffer-request-and-reply (display (xkeyboard-opcode display) nil)
                                 ((data +use-extension+)
                                  (card16 major)
                                  (card16 minor))
    (values (boolean-get 1)
            (card16-get 8))))

(export 'enable-xkeyboard)

;; (defun select-events (display))


(defun xkb-bell (display &key (device +use-core-kbd+)
                       bell-class id percent force-sound event-only pitch duration name window)
  (declare (type display display)
           (type devicespec device)
           (type window window))
  (with-buffer-request (display (xkeyboard-opcode display))
    (data +bell+)
    (devicespec device)
    (bellclassspec bell-class)
    (idspec id)
    (card8 percent)
    (boolean force-sound)
    (boolean event-only)
    (pad8 0)
    (int16 pitch)
    (int16 duration)
    (pad16 0)
    (keyword name)
    (window window)))

(defstruct device-state 
  (device-id 0 :type card8)
  (mods 0 :type keymask)
  (base-mods 0 :type keymask)
  (latched-mods 0 :type keymask)
  (locked-mods 0 :type keymask)
  (group 0 :type group)
  (locked-group 0 :type group)
  (base-group 0 :type int16)
  (latched-group 0 :type int16)
  (compat-state 0 :type keymask)
  (grab-mods 0 :type keymask)
  (compat-grab-mods 0 :type keymask)
  (lookup-mods 0 :type keymask)
  (compat-lookup-mods 0 :type keymask)
  (ptr-btn-state 0 :type butmask))

(defun get-state (display &optional (device +use-core-kbd+))
  (declare (type display display))
  (with-buffer-request-and-reply (display (xkeyboard-opcode display) nil)
                                 ((data +get-state+)
                                  (devicespec device)
                                  (pad16 0))
    (make-device-state
     :device-id (card8-get 1)
     :mods (keymask-get 8)
     :base-mods (keymask-get 9)
     :latched-mods (keymask-get 10)
     :locked-mods (keymask-get 11)
     :group (group-get 12)
     :locked-group (group-get 13)
     :base-group (int16-get 14)
     :latched-group (int16-get 16)
     :compat-state (keymask-get 18)
     :lookup-mods (keymask-get 19)
     :compat-lookup-mods (keymask-get 20)
     :ptr-btn-state (butmask-get 22))))

(defun latch-lock-state (display &key (device +use-core-kbd+)
                                   affect-mod-locks
                                   mod-locks
                                   lock-group
                                   group-lock
                                   affect-mod-latches
                                   mod-latches
                                   latch-group
                                   group-latch)
  (declare (type display display))
  (with-buffer-request (display (xkeyboard-opcode display))
    (data +latch-lock-state+)
    (devicespec device)
    (keymask affect-mod-locks)
    (keymask mod-locks)
    (boolean lock-group)
    (group group-lock)
    (keymask affect-mod-latches)
    (keymask mod-latches)
    (pad8 0)
    (boolean latch-group)
    (int16 group-latch)))

(defun lock-group (display &key (device +use-core-kbd+) group)
  (latch-lock-state display :device device
                            :affect-mod-locks 0
                            :mod-locks 0
                            :lock-group t
                            :group-lock group
                            :affect-mod-latches 0
                            :mod-latches 0
                            :latch-group nil
                            :group-latch 0))

(defstruct modmap
  (keycode 0 :type keycode)
  (mods 0 :type keymask))

 (defstruct vmodmap
  (keycode 0 :type keycode)
  (vmods 0 :type vmodmask))

(defstruct behaviormap ;defstruct introduces a new type, so we have to
                       ;distinguish it from the existing behavior.
  (keycode 0 :type keycode)
  (behavior 0 :type behavior))

(defstruct explicitmap ;same as behavior/behaviormap
  (keycode 0 :type keycode)
  (explicit 0 :type explicit))

(defstruct keytype-mapentry
  (active nil :type (member nil t))
  (mask 0 :type keymask)
  (level 0 :type card8)
  (mods 0 :type keymask)
  (vmods 0 :type vmodmask))

(defstruct keytype
  (mask 0 :type keymask)
  (mods 0 :type keymask)
  (vmods 0 :type vmodmask)
  (levels 0 :type card8)
  (map-entries 0 :type card8)
  (preserve-p nil :type (member nil t))
  (map nil :type list)
  (preserve nil :type list))

(defstruct keysymmap
  (kt-index (make-array 4 :element-type 'card8
                          :initial-element 0)
   :type (vector card8 4))
  (group 0 :type card8)
  (width 0 :type card8)
  (n 0 :type card16)
  (keysyms nil :type list))

(defstruct virtual-modifier-bindings
  (virtual-modifiers 0 :type vmodmask)
  (real-modifiers-per-virtual-modifier nil :type list))

(defstruct xkb-keymap-part
  (first 0 :type (or card8 keycode)) ;doesn't matter as card8 = keycode, but logically
  (n 0 :type card8)
  (total 0 :type card16)
  (list nil :type list))

(defstruct xkb-keymap
  (min-keycode 0 :type keycode)
  (max-keycode 0 :type keycode)
  (mappart-mask 0 :type mappart)
  (types nil :type (or null xkb-keymap-part))
  (syms nil :type (or null xkb-keymap-part))
  (actions nil :type (or null xkb-keymap-part))
  (behaviors nil :type (or null xkb-keymap-part))
  (explicits nil :type (or null xkb-keymap-part))
  (modmapkeys nil :type (or null xkb-keymap-part))
  (vmodmapkeys nil :type (or null xkb-keymap-part))
  (virtualmods nil :type (or null virtual-modifier-bindings)))

(defun contained-in-mask (const mask)
  (plusp (logand const mask)))

(defmacro moddef-get (indexsym)
  `(prog1 (make-moddef
           :mask (keymask-get ,indexsym)
           :real-mods (keymask-get (index-incf ,indexsym 1))
           :vmods (vmodmask-get (index-incf ,indexsym 1)))
     (index-incf ,indexsym 2)))

(defmacro modmap-get (indexsym)
  `(prog1 (make-modmap
           :keycode (keycode-get ,indexsym)
           :mods (keymask-get (index-incf ,indexsym 1)))
     (index-incf ,indexsym 1)))

(defmacro vmodmap-get (indexsym)
  `(prog1 (make-vmodmap
           :keycode (keycode-get ,indexsym)
           :vmods   (vmodmask-get (index-incf ,indexsym 2)))
     (index-incf ,indexsym 2)))

(defmacro behaviormap-get (indexsym)
  `(prog1 (make-behaviormap
           :keycode (keycode-get ,indexsym)
           :behavior (behavior-get (index-incf ,indexsym 1)))
     (index-incf ,indexsym 3)))

(defmacro explicitmap-get (indexsym)
  `(prog1 (make-explicitmap
           :keycode (keycode-get ,indexsym)
           :explicit (explicit-get (index-incf ,indexsym 1)))
     (index-incf ,indexsym 1)))

(defmacro keytype-mapentry-get (indexsym)
  `(prog1 (make-keytype-mapentry
           :active (boolean-get ,indexsym)
           :mask (keymask-get (index-incf ,indexsym 1))
           :level (card8-get (index-incf ,indexsym 1))
           :mods (keymask-get (index-incf ,indexsym 1))
           :vmods (vmodmask-get (index-incf ,indexsym 1)))
     (index-incf ,indexsym 4)))

(defmacro keytype-get (indexsym)
  (let ((n-map-entries-sym (gensym "n-map-entries"))
        (preserve-p-sym (gensym "preserve-p")))
    `(let (,n-map-entries-sym ,preserve-p-sym)
       (make-keytype
        :mask (keymask-get ,indexsym)
        :mods (keymask-get (index-incf ,indexsym 1))
        :vmods (vmodmask-get (index-incf ,indexsym 1))
        :levels (card8-get (index-incf ,indexsym 2))
        :map-entries (setf ,n-map-entries-sym (card8-get (index-incf ,indexsym 1)))
        :preserve-p (setf ,preserve-p-sym (boolean-get (index-incf ,indexsym 1)))
        :map (progn (index-incf ,indexsym 2)
                    (loop repeat ,n-map-entries-sym
                          collect (keytype-mapentry-get ,indexsym)))
        :preserve (when ,preserve-p-sym
                    (loop repeat ,n-map-entries-sym
                          collect (moddef-get ,indexsym)))))))

(defmacro keysymmap-get (indexsym)
  (let ((n-sym (gensym)))
    `(let (,n-sym)
       (make-keysymmap
        :kt-index (coerce (loop repeat 4
                               collect (prog1 (card8-get ,indexsym)
                                         (index-incf ,indexsym 1)))
                          '(vector card8 4))
        :group (prog1 (card8-get ,indexsym)
                 (index-incf ,indexsym 1))
        :width (prog1 (card8-get ,indexsym)
                 (index-incf ,indexsym 1))
        :n (prog1 (setf ,n-sym (card16-get ,indexsym)) ;depending on
                                                       ;side effects
                                                       ;in this way
                                                       ;makes me
                                                       ;feeling dirty
             (index-incf ,indexsym 2))
        :keysyms (loop repeat ,n-sym
                       collect (prog1 (card32-get ,indexsym)
                                 (index-incf ,indexsym 4)))))))

(defmacro xkb-keymap-get (indexsym)
  (let ((minKeycodeSym (gensym))
        (maxKeycodeSym (gensym))
        (mappartMaskSym (gensym))
        (firstTypeSym (gensym))
        (nTypesSym (gensym))
        (totalTypesSym (gensym))
        (firstKeysymSym (gensym))
        (totalSymsSym (gensym))
        (nKeysymsSym (gensym))
        (firstKeyactionSym (gensym))
        (totalActionsSym (gensym))
        (nKeyactionsSym (gensym))
        (firstKeybehaviorSym (gensym))
        (nKeybehaviorSym (gensym))
        (totalKeybehaviorsSym (gensym))
        (firstKeyexplicitSym (gensym))
        (nKeyexplicitSym (gensym))
        (totalKeyexplicitSym (gensym))
        (firstModMapKeySym (gensym))
        (nModMapKeySym (gensym))
        (totalModMapKeySym (gensym))
        (firstVModMapKeySym (gensym))
        (nVModMapKeySym (gensym))
        (totalVModMapKeySym (gensym))
        (virtualModsSym (gensym)))
    `(let ((,minKeycodeSym (card8-get ,indexsym))
           (,maxKeycodeSym (card8-get (index-incf ,indexsym 1)))
           (,mappartMaskSym (card16-get (index-incf ,indexsym 1)))
           (,firstTypeSym (card8-get (index-incf ,indexsym 2)))
           (,nTypesSym (card8-get (index-incf ,indexsym 1)))
           (,totalTypesSym (card8-get (index-incf ,indexsym 1)))
           (,firstKeysymSym (card8-get (index-incf ,indexsym 1)))
           (,totalSymsSym (card16-get (index-incf ,indexsym 1)))
           (,nKeysymsSym (card8-get (index-incf ,indexsym 2)))
           (,firstKeyactionSym (card8-get (index-incf ,indexsym 1)))
           (,totalActionsSym (card16-get (index-incf ,indexsym 1)))
           (,nKeyactionsSym (card8-get (index-incf ,indexsym 2)))
           (,firstKeybehaviorSym (card8-get (index-incf ,indexsym 1)))
           (,nKeybehaviorSym (card8-get (index-incf ,indexsym 1)))
           (,totalKeybehaviorsSym (card8-get (index-incf ,indexsym 1)))
           (,firstKeyexplicitSym (card8-get (index-incf ,indexsym 1)))
           (,nKeyexplicitSym (card8-get (index-incf ,indexsym 1)))
           (,totalKeyexplicitSym (card8-get (index-incf ,indexsym 1)))
           (,firstModMapKeySym (card8-get (index-incf ,indexsym 1)))
           (,nModMapKeySym (card8-get (index-incf ,indexsym 1)))
           (,totalModMapKeySym (card8-get (index-incf ,indexsym 1)))
           (,firstVModMapKeySym (card8-get (index-incf ,indexsym 1)))
           (,nVModMapKeySym (card8-get (index-incf ,indexsym 1)))
           (,totalVModMapKeySym (card8-get (index-incf ,indexsym 1)))
           (,virtualModsSym (card16-get (index-incf ,indexsym 2))))
       (declare (ignore ,nVModMapKeySym))
       (index-incf ,indexsym 2)
       (make-xkb-keymap
        :min-keycode ,minKeycodeSym
        :max-keycode ,maxKeycodeSym
        :mappart-mask ,mappartMaskSym
        :types (when (contained-in-mask +KEYTYPES+ ,mappartMaskSym)
                 (make-xkb-keymap-part
                  :first ,firstTypeSym
                  :n ,nTypesSym
                  :total ,totalTypesSym
                  :list (loop repeat ,nTypesSym
                              collect (keytype-get ,indexsym))))
        :syms (when (contained-in-mask +KEYSYMS+ ,mappartMaskSym)
                (make-xkb-keymap-part
                 :first ,firstKeysymSym
                 :n ,nKeysymsSym
                 :total ,totalSymsSym
                 :list (loop repeat ,nKeysymsSym
                             collect (keysymmap-get ,indexsym))))
        :actions (when (contained-in-mask +KEYACTIONS+ ,mappartMaskSym)
                   (make-xkb-keymap-part
                    :first ,firstKeyactionSym
                    :n ,nKeyactionsSym
                    :total ,totalActionsSym
                    :list (cons
                           (loop repeat ,nKeyactionsSym
                                 collect (prog1 (card8-get ,indexsym)
                                           (index-incf ,indexsym 1))
                                 finally (setf ,indexsym (lround ,indexsym)))
                           (loop repeat ,totalActionsSym
                                 collect (loop repeat 8
                                               collect (prog1 (card8-get ,indexsym)
                                                         (index-incf ,indexsym 1)))))))
        :behaviors (when (contained-in-mask +KEYBEHAVIORS+ ,mappartMaskSym)
                     (make-xkb-keymap-part
                      :first ,firstKeybehaviorSym
                      :n ,nKeybehaviorSym
                      :total ,totalKeybehaviorsSym
                      :list (loop repeat ,totalKeybehaviorsSym
                                  collect (behaviormap-get ,indexsym))))
        :virtualmods (when (contained-in-mask +VIRTUALMODS+ ,mappartMaskSym)
                       (make-virtual-modifier-bindings
                        :virtual-modifiers ,virtualModsSym
                        :real-modifiers-per-virtual-modifier
               (loop for i from 0 upto 15 when (= (ldb (byte 1 i) ,virtualModsSym) 1)
                     collect (prog1 (keymask-get ,indexsym)
                               (index-incf ,indexsym 1)))))
        :explicits (when (contained-in-mask +EXPLICITCOMPONENTS+ ,mappartMaskSYm)
                     (prog1 (make-xkb-keymap-part
                             :first ,firstKeyexplicitSym
                             :n ,nKeyexplicitSym
                             :total ,totalKeyexplicitSym
                             :list (loop repeat ,totalKeyexplicitSym
                                         collect (explicitmap-get ,indexsym)))
                       (setf ,indexsym (lround ,indexsym))))
        :modmapkeys (when (contained-in-mask +MODIFIERMAP+ ,mappartMaskSym)
                      (prog1 (make-xkb-keymap-part
                              :first ,firstModMapKeySym
                              :n ,nModMapKeySym
                              :total ,totalModMapKeySym
                              :list (loop repeat ,totalModMapKeySym
                                          collect (modmap-get ,indexsym)))
                        (setf ,indexsym (lround ,indexsym))))
        :vmodmapkeys (when (contained-in-mask +VIRTUALMODMAP+ ,mappartMaskSym)
                       (make-xkb-keymap-part
                        :first ,firstVModMapKeySym
                        :n 0
                        :total ,totalVModMapKeySym
                        :list (loop repeat ,totalVModMapKeySym
                                    collect (vmodmap-get ,indexsym))))))))

(defun get-map (display devicespec mappart-mask-full)
  (with-buffer-request-and-reply (display (xkeyboard-opcode display) nil)
      ((data +get-map+)
       (devicespec devicespec)
       (mappart mappart-mask-full) ;full
       (mappart 0) ;partial
       (card8 0)  ;firstType
       (card8 0)  ;nTypes
       (keycode 0)  ;firstKeySym
       (card8 0)  ;nKeySyms
       (keycode 0)  ;firstKeyAction
       (card8 0)  ;nKeyActions
       (keycode 0)  ;firstKeyBehavior
       (card8 0)  ;nKeyBehavior
       (vmodmask 0) ;virtualMods
       (keycode 0)  ;firstKeyExplicit
       (card8 0)  ;nKeyExplicit
       (keycode 0)  ;firstModMapKey
       (card8 0)  ;nModMapKeys
       (keycode 0)  ;firstVModMapKey
       (card8 0)  ;nVModMapKeys
       (pad16 nil))
    (let ((index 10))
      (xkb-keymap-get index))))

(defstruct client-keytype-mapentry
  (preserve 0 :type keymask)
  (active nil :type (member nil t))
  (level 0 :type card8))

(defstruct client-keytype
  (mask 0 :type keymask)
  (map (make-hash-table) :type hash-table))

(defstruct client-keysymmap
  (num-groups 0 :type card8)
  (groups-wrap nil :type symbol)
  (redirect-group 0 :type card8)
  (keysyms (make-array 0) :type vector)
  (width 0 :type card8)
  (keytypes (make-array 0) :type vector))

(defstruct client-mapping
  (symmaps (make-array 0) :type vector))


(defun construct-keytype-map-entry (entry preserve)
  (make-client-keytype-mapentry
   :preserve preserve
   :active (keytype-mapentry-active entry)
   :level (keytype-mapentry-level entry)))

(defun construct-type (type)
  (let ((result (make-client-keytype
                 :mask (keytype-mask type))))
    (loop for i from 0 below (keytype-map-entries type)
          for mapentry in (keytype-map type)
          for preserve = (if (keytype-preserve-p type)
                             (moddef-mask (nth i (keytype-preserve type))) 0)
          do (push (construct-keytype-map-entry mapentry preserve)
                   (gethash (keytype-mapentry-mask mapentry)
                            (client-keytype-map result))))
    result))

(defun construct-type-vector (typevector types)
  (loop for i from 0
        repeat (xkb-keymap-part-n types)
        do (setf (svref typevector (+ i (xkb-keymap-part-first types)))
                 (construct-type (nth i (xkb-keymap-part-list types))))))

(defun symbolize-groups-wrap (groups-wrap-number)
  (or (case groups-wrap-number
        (0 :wrap)
        (2 :clamp)
        (4 :redirect))
      (error "wrong-value-in-groups-wrap")))

(defun construct-symmap (symmap typevector)
  (let ((groupinfo (keysymmap-group symmap)))
    (make-client-keysymmap
     :num-groups (ldb (byte 4 0) groupinfo)
     :groups-wrap (symbolize-groups-wrap (ldb (byte 2 6) groupinfo))
     :redirect-group (ldb (byte 2 4) groupinfo)
     :keysyms (coerce (keysymmap-keysyms symmap) 'vector)
     :width (keysymmap-width symmap)
     :keytypes (coerce (loop for i from 0 below 4
                             collect (svref
                                      typevector
                                        ;svref better? but sbcl complains about types.
                                      (aref (keysymmap-kt-index symmap) i)))
                'vector))))

(defun transform-xkb-keymap-to-client-mapping (info)
  (let*
      ((types (xkb-keymap-types info))
       (symmaps (xkb-keymap-syms info))
       (symmaps-array (make-array (+ (xkb-keymap-part-first symmaps)
                                     (xkb-keymap-part-n symmaps))))
       (typevector (make-array (+ (xkb-keymap-part-first types)
                                  (xkb-keymap-part-n types)))))
    (construct-type-vector typevector types)
    (loop for i from 0
                repeat (xkb-keymap-part-n symmaps)
          do (setf (svref symmaps-array (+ (xkb-keymap-part-first symmaps) i))
                   (construct-symmap (nth i (xkb-keymap-part-list symmaps))
                                     typevector)))
    (make-client-mapping
     :symmaps symmaps-array)))

(defun corestate->group (corestate)
  (ldb (byte 2 13) corestate))

(defun corestate->mask (corestate)
  (ldb (byte 8 0) corestate))

(defun sanitize-redirect-group (num-groups redirect-group)
  (if (>= redirect-group num-groups)
      0 redirect-group))

(defun effective-group (event-group num-groups groups-wrap redirect-group)
  (cond
    ((= num-groups 0) 0)
    ((<= 0 event-group (1- num-groups)) event-group)
    (t
     (case groups-wrap
       (:wrap (mod event-group num-groups))
       (:clamp (1- num-groups))
       (:redirect (sanitize-redirect-group num-groups redirect-group))))))

(defun calculate-leftover-modifiers (mask entry keytype-mask)
  (logior (client-keytype-mapentry-preserve entry)
          (logand mask (lognot keytype-mask))))

(defun shiftlevel/leftover-modifiers (keytype mask)
  (let ((keytype-mask (client-keytype-mask keytype)))
    (loop for entry in (gethash (logand mask keytype-mask)
                                (client-keytype-map keytype))
          when (client-keytype-mapentry-active entry)
            return (values (client-keytype-mapentry-level entry)
                           (calculate-leftover-modifiers mask entry keytype-mask))
          finally (return (values 0 (logand mask (lognot keytype-mask)))))))

(defun keyevent->keysym (mapping keycode corestate)
  (let* ((group (corestate->group corestate))
         (mask (corestate->mask corestate))
         (symmap (svref (client-mapping-symmaps mapping) keycode))
         (effective-group (effective-group group
                                           (client-keysymmap-num-groups symmap)
                                           (client-keysymmap-groups-wrap symmap)
                                           (client-keysymmap-redirect-group symmap)))
         (keytype (svref (client-keysymmap-keytypes symmap)
                         effective-group)))
    (if (= (length (client-keysymmap-keysyms symmap)) 0)
        nil
        (multiple-value-bind (shiftlevel leftover-modifiers)
            (shiftlevel/leftover-modifiers keytype mask)
          (values
           (svref (client-keysymmap-keysyms symmap)
                  (+ (* effective-group (client-keysymmap-width symmap)) shiftlevel))
           leftover-modifiers)))))

(defun xkb/keysym->character (keysym keysymdb) ;keysymdb would be normally +xkbkeysymdb+
  (cond
    ((or (<= #x0020 keysym #x007E) (<= #x00A0 keysym #x00FF))
        (code-char keysym))
    ((<= #x01000100 keysym #x0110FFFF)
        (code-char (- keysym #x01000000)))
    (t
        (let ((codepoint (cadr (assoc :unicode (gethash keysym keysymdb)))))
          (if codepoint (code-char codepoint))))))

(defun upcase-keysym (keysym)
  ;;this is wrong, but ...
  ;;we only upcase a-z for the time being, if someone wants
  ;;better upcasing support, she or he should do it herself, himself
  ;;the f*** *p xkb specification talks about keysyms with symbolic names
  ;;in Appendix A, which (the symbolic names) never get defined in it
  ;;or in the core X protocol specification.
  (cond
    ((<= #x0061 keysym #x007A) (- keysym #x0020))
    (t keysym)))

(defun control-character (keysym keysymdb)
  (cond
    ((<= #x0041 keysym #x005F) (code-char (- keysym #x0041))) ;A-Z[\]^_
    ((<= #x0061 keysym #x007A) (code-char (- keysym #x0061))) ;a-z
    (t (xkb/keysym->character keysym keysymdb))))

(defun process-leftover-modifiers (keysym leftover-modifiers keysymdb)
  (cond
    ((contained-in-mask +control+ leftover-modifiers)
     (values keysym (string (control-character keysym keysymdb))))
    ((contained-in-mask +lock+ leftover-modifiers)
     (values (upcase-keysym keysym)
             (string (xkb/keysym->character (upcase-keysym keysym) keysymdb))))
    (t
     (values keysym
             (string (xkb/keysym->character keysym keysymdb))))))

;;; Local Variables:
;;; indent-tabs-mode: nil
;;; End:
