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

(defun inc-brightness (step)
  (set-brightness-pct (+ (get-brightness-pct) step)))

(defun dec-brightness (step)
  (set-brightness-pct (- (get-brightness-pct) step)))

(defun modeline (ml)
  (declare (ignore ml))
  (format nil "~A%" (get-brightness-pct))
  :ml-acpi-backlight-on-click nil)

(defun ml-on-click (code id &rest rest)
  (declare (ignore rest))
  (declare (ignore id))
  (let ((button (stumpwm::decode-button-code code)))
    (case button
      ((:wheel-up)
       (inc-brightness *step*))
      ((:wheel-down)
       (dec-brightness *step*))))
  (stumpwm::update-all-mode-lines))


(when (fboundp 'stumpwm::register-ml-on-click-id) ;check in case of old stumpwm version
  (register-ml-on-click-id :ml-acpi-backlight-on-click #'ml-on-click))

(defcommand backlight-up () ()
  "Increase the brightness by N percents"
  (inc-brightness *step*))

(defcommand backlight-down () ()
  "Decrease the brightness by N percents"
  (dec-brightness *step*))

(defcommand backlight-set (pct) ((:string "Brightness percentage:"))
  "Set backlight brightness"
  (set-brightness-pct (parse-integer pct)))
