;;;; acpi-backlight.asd

(asdf:defsystem #:acpi-backlight
  :description "ACPI backlight control module for StumpWM"
  :author "Dmitrii Kosenkov"
  :license  "GPLv3"
  :version "0.1.0"
  :serial t
  :depends-on (#:stumpwm)
  :components ((:file "package")
               (:file "acpi-backlight")))
