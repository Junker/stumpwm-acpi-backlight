# StumpWM ACPI-Backlight

ACPI backlight control module for StumpWM.

## Installation

```bash
ls ~/.stumpwm.d/modules/
git clone https://github.com/Junker/stumpwm-acpi-backlight acpi-backlight
```

get name of your backlight device:

```bash
$ ls /sys/class/backlight
intel_backlight
```

add lisp code:

```lisp
(stumpwm:add-to-load-path "~/.stumpwm.d/modules/acpi-backlight")
(load-module "acpi-backlight")
(acpi-backlight:init "intel_backlight") ; use name of your ACPI backlight device
```

## Usage

```lisp
  (define-key *top-map* (kbd "XF86MonBrightnessUp") "backlight-up")
  (define-key *top-map* (kbd "XF86MonBrightnessDown") "backlight-down")
```

### Additional commands

- backlight-set value

### Additional functions

- get-brightness
- get-brightness-pct
- set-brightness
- set-brightness-pct

### Parameters

- acpi-backlight:\*step\* - brightness increase/decrease percent step

### Modeline

%Q - backlight formatter

#### Modeline mouse interaction

- **wheel up**: brightness up
- **wheel down**: brightness down

## Troubleshooting

Sometimes only `root` can change the brightness by this method. To allow users
in the `video` group to change the brightness, a udev rule such as the following
can be used:

```bash
$ cat /etc/udev/rules.d/backlight.rules
ACTION=="add", SUBSYSTEM=="backlight", RUN+="/bin/chgrp video $sys$devpath/brightness", RUN+="/bin/chmod g+w $sys$devpath/brightness"
```

Then you need to add your user to the video group.

```bash
usermod -aG video <user>
```
