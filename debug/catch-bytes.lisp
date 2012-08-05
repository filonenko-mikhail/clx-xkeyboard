;;;; Here we overwrite some functions form xlib
;;;; so we can catch all bytes, that are communicated
;;;; over the connection to the x-server

(in-package :xlib)

;;;; we store for each fd a little state here.
;;;; (totalcount open-p count logfile-input logfile-output)
;;;; totalcount in  -- count of all bytes  input on this fd
;;;; totalcount out -- count of all bytes output on this fd
;;;; open-p -- nil if close-buffer was called
;;;; count in -- count of bytes since first write/read
;;;;             since close/initial opening
;;;; count out -- count of bytes since first write/read
;;;;             since close/initial opening
;;;; logfile -- stream to write log to

(defvar *xkb-debug-timestamp* 0)
(defvar *xkb-debug-fd-hashtable* (make-hash-table))

(defstruct xkb-debug-fd-info
  tin
  tout
  open-p
  cin
  cout
  lin
  lout)

(defun xkb-debug-write-to-file (stream result vector start end offset timestamp)
  (format stream "TIME: ~a~%" timestamp)
  ;first line
  (let ((line-offset (- offset (mod offset 8)))
        (pos start))
    (format stream "~6,'0d: " line-offset)
    (dotimes (i (mod offset 8))
      (format stream " XXX"))
    (dotimes (i (min (- 8 (mod offset 8)) (- end start)))
      (format stream " ~3,'0d" (aref vector pos))
      (incf pos))
    (format stream "~%")
    (incf line-offset 8)
    ;body-lines
    (loop while (>= (- end pos) 8)
          do (progn
               (format stream "~6,'0d: " line-offset)
               (dotimes (i 8)
                 (format stream " ~3,'0d" (aref vector pos))
                 (incf pos))
               (format stream "~%")
               (incf line-offset 8)))
    ;last line
    (when (> (- end pos) 0)
      (format stream "~6,'0d: " line-offset)
      (dotimes (i (- end pos))
        (format stream " ~3,'0d" (aref vector pos))
        (incf pos))
      (format stream "~%"))
    (format stream "RESULT: ~a~%" result)))

(defun xkb-debug-open-fd-log (fd)
  (let ((entry (make-xkb-debug-fd-info
                :tin 0
                :tout 0
                :open-p t
                :cin 0
                :cout 0
                :lin (open (make-pathname :name (format nil "~a-input.txt" fd)) :direction :output)
                :lout (open (make-pathname :name (format nil "~a-output.txt" fd)) :direction :output))))
    (setf (gethash fd *xkb-debug-fd-hashtable*) entry)
    entry))

(defun xkb-debug-log-to-input (fd vector start end result)
  (let ((entry (gethash fd *xkb-debug-fd-hashtable*)))
    (when (not entry)
      (setf entry (xkb-debug-open-fd-log fd)))
    (when (not (xkb-debug-fd-info-open-p entry))
      (setf (xkb-debug-fd-info-open-p entry) t)
      (format (xkb-debug-fd-info-lin entry) "REOPENED~%"))
    (xkb-debug-write-to-file (xkb-debug-fd-info-lin entry) result vector start end
                             (xkb-debug-fd-info-cin entry) (incf *xkb-debug-timestamp*))
    (incf (xkb-debug-fd-info-tin entry) (- end start))
    (incf (xkb-debug-fd-info-cin entry) (- end start))))

(defun xkb-debug-log-to-output (fd vector start end result)
 (let ((entry (gethash fd *xkb-debug-fd-hashtable*)))
    (when (not entry)
      (setf entry (xkb-debug-open-fd-log fd)))
    (when (not (xkb-debug-fd-info-open-p entry))
      (setf (xkb-debug-fd-info-open-p entry) t)
      (format (xkb-debug-fd-info-lout entry) "REOPENED~%"))
    (xkb-debug-write-to-file (xkb-debug-fd-info-lout entry) result vector start end
                             (xkb-debug-fd-info-cout entry) (incf *xkb-debug-timestamp*))
    (incf (xkb-debug-fd-info-tout entry) (- end start))
    (incf (xkb-debug-fd-info-cout entry) (- end start))))

(defvar *old-buffer-input* #'buffer-input)
(defun buffer-input (buffer vector start end &optional timeout)
  (let ((result
          (funcall *old-buffer-input* 
                   buffer vector start end timeout)))
    (xkb-debug-log-to-input (sb-sys:fd-stream-fd (display-input-stream buffer))
                            vector start end result)
    result))

(defvar *old-buffer-write* #'buffer-write)
(defun buffer-write (vector buffer start end)
  (xkb-debug-log-to-output (sb-sys:fd-stream-fd (display-output-stream buffer))
                          vector start end nil)
  (funcall *old-buffer-write* vector buffer start end))

(defvar *old-close-buffer* #'close-buffer)
(defun close-buffer (buffer &key abort)
  (labels ((doit (stream)
             (when stream
               (let ((entry (gethash (sb-sys:fd-stream-fd stream)
                                     *xkb-debug-fd-hashtable*)))
                 (when entry
                   (setf
                    (xkb-debug-fd-info-open-p entry) nil
                    (xkb-debug-fd-info-cin entry) 0
                    (xkb-debug-fd-info-cout entry) 0)
                   (format (xkb-debug-fd-info-lin entry)
                           "CLOSED~%")
                   (format (xkb-debug-fd-info-lout entry)
                           "CLOSED~%"))))))
    (doit (display-input-stream buffer))
    (doit (display-output-stream buffer)))
  (funcall *old-close-buffer* buffer :abort abort))

(defun xkb-debug-reset ()
  (setf
   (symbol-function 'close-buffer) *old-close-buffer*
   (symbol-function 'buffer-write) *old-buffer-write*
   (symbol-function 'buffer-input) *old-buffer-input*)
  (maphash (lambda (key value)
             (declare (ignore key))
             (close (xkb-debug-fd-info-lin value))
             (close (xkb-debug-fd-info-lout value)))
           *xkb-debug-fd-hashtable*)
  (setf *xkb-debug-fd-hashtable* (make-hash-table)))
