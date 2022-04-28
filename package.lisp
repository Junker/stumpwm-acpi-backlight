;;;; package.lisp

(defpackage :acpi-backlight
  (:use #:cl :stumpwm)
  (:export #:init
           #:get-brightness
           #:get-brightness-pct
           #:set-brightness
           #:set-brightness-pct
           #:*step*))
