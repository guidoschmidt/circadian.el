<p align="center">
<a href="https://melpa.org/#/circadian" target="_blank"><img src="https://melpa.org/packages/circadian-badge.svg" alt="MELPA"/></a><br><a href="https://travis-ci.org/guidoschmidt/circadian.el" target="_blank"><img src="https://travis-ci.org/guidoschmidt/circadian.el.svg?branch=master" alt="https://travis-ci.org/guidoschmidt/circadian.el"/></a>
<br>
<br>
<br>
<img src="logo.png" alt="Logo"/>

<h1 align="center"><a href="https://guidoschmidt.github.io/circadian.el" target="_blank">circadian</a></h1>
<h3 align="center">Theme-switching for Emacs based on daytime</h3>
</p>

### Conception
Circadian tries to help reducing eye strain that may arise
from difference of your display brightness and the
surrounding light.

Inspired by color temperature shifting tools and brightness
adaption software like:
- [redshift](https://wiki.archlinux.org/index.php/Redshift)
- [f.lux](https://justgetflux.com/news/pages/mac/)
- [Lumen](https://github.com/anishathalye/lumen)


---


### Example usage
##### Switching themes on time of day

Example usage featuring [hemera-themes](https://github.com/GuidoSchmidt/emacs-hemera-theme)
and [nyx-theme](https://github.com/GuidoSchmidt/emacs-nyx-theme) (with use-package). Make sure
to use `:defer` keyword. Omitting it may lead to broken colors 
(see [issue 9](https://github.com/guidoschmidt/circadian.el/issues/9)):

```elisp
;; Install additinal themes from melpa
;; make sure to use :defer keyword
(use-package hemera-theme :ensure :defer)
(use-package nyx-theme :ensure :defer)

(use-package circadian
  :ensure t
  :config
  (setq circadian-themes '(("8:00" . hemera)
                           ("19:30" . nyx)))
  (circadian-setup))
```

##### Switching themes on sunrise & sunset
Be sure to set your latitude and longitude (Get them e.g. at [latlong.net](https://www.latlong.net/)):

```elisp
;; Install additinal themes from melpa
;; make sure to use :defer keyword
(use-package apropospriate-theme :ensure :defer)
(use-package nord-theme :ensure :defer)

(use-package circadian
  :ensure t
  :config
  (setq calendar-latitude 49.0)
  (setq calendar-longitude 8.5)
  (setq circadian-themes '((:sunrise . apropospriate-light)
                           (:sunset  . nord)))
  (circadian-setup))
```


---


### Hooks
**circadian** provides two hooks:
- `circadian-before-load-theme-hook`
- `circadian-after-load-theme-hook`

e.g. I like to override any themes cursor color to a very bright color via:

```elisp
(add-hook 'circadian-after-load-theme-hook
          #'(lambda (theme)
              ;; Line numbers appearance
              (setq linum-format 'linum-format-func)
              ;; Cursor
              (set-default 'cursor-type 'box)
              (set-cursor-color "#F52503")))
```


---


### Todos & Ideas
- Is it possible to interpolate between themes/colors?
- Can brightness sensors (e.g. laptops) be queried to control dimming?
- Load themes by mode [reddit.com/r/emacs](https://www.reddit.com/r/emacs/comments/72ukrx/theme_preferences/)
- Load themes by machine name [reddit.com/r/emacs](https://www.reddit.com/r/emacs/comments/72ukrx/theme_preferences/)
- Load themes by wifi/location? [reddit.com/r/emacs](https://www.reddit.com/r/emacs/comments/72ukrx/theme_preferences/)
