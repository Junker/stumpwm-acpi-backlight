;;;; acpi-backlight.lisp

(in-package :acpi-backlight)

;; formatters.
(add-screen-mode-line-formatter #\Q 'modeline)


(defvar *path* "/sys/class/backlight/")
(defvar *max-brightness* nil)
(defvar *brightness-file* nil)
(defvar *step* 5)

(defun init (device)
  (let ((max-brightness-file (concat *path* device "/max_brightness")))
    (when (not (probe-file max-brightness-file))
      (error (format nil "Backlight device '~A' not found." device)))
    (setf *brightness-file* (concat *path* device "/brightness"))
    (setf *max-brightness* (parse-integer (uiop:read-file-string max-brightness-file)))))


(defun pct-to-val (pct)
  (round (* (/ *max-brightness* 100) pct)))

(defun val-to-pct (val)
  (round (* (/ val *max-brightness*) 100)))

(defun get-brightness ()
  (parse-integer (uiop:read-file-string *brightness-file*)))

(defun get-brightness-pct ()
  (val-to-pct (get-brightness)))

(defun set-brightness (val)
  (with-open-file (strm *brightness-file*
                        :direction :output
                        :if-exists :overwrite)
    (format strm "~A" val)))

(defun set-brightness-pct (pct)
  (set-brightness (pct-to-val (max 0 (min 100 pct)))))

(defun modeline (ml)
  (declare (ignore ml))
  (format nil "~A%" (get-brightness-pct)))


(defcommand backlight-up () ()
  "Increase the brightness by N percents"
  (set-brightness-pct (+ (get-brightness-pct) *step*)))

(defcommand backlight-down () ()
  "Decrease the brightness by N percents"
  (set-brightness-pct (- (get-brightness-pct) *step*)))

(defcommand backlight-set (pct) ((:string "Brightness percentage:"))
  "Set backlight brightness"
  (set-brightness-pct (parse-integer pct)))
