<p align="center">
  <a href="https://melpa.org/#/circadian" target="_blank">
    <img src="https://melpa.org/packages/circadian-badge.svg" alt="MELPA"/>
  </a>
  <br>
  <a href="https://github.com/guidoschmidt/circadian.el/actions/workflows/ci.yml" target="_blank">
    <img src="https://github.com/guidoschmidt/circadian.el/actions/workflows/ci.yml/badge.svg"
         alt="https://travis-ci.org/guidoschmidt/circadian.el"/>
  </a>
  <br>
  <br>
  <br>
  <img src="logo.png" alt="Logo"/>

  <h1 align="center"><a href="https://guidoschmidt.github.io/circadian.el" target="_blank">circadian</a></h1>
  <h3 align="center">Theme-switching for Emacs based on daytime</h3>
</p>

## Conception

Circadian tries to help reducing eye strain that may arise
from difference of your display brightness and the
surrounding light.

Inspired by color temperature shifting tools and brightness
adaption software like:

- [redshift](https://wiki.archlinux.org/index.php/Redshift)
- [f.lux](https://justgetflux.com/news/pages/mac/)
- [Lumen](https://github.com/anishathalye/lumen)

---

## Usage & Configuration
Install circadian.el with
[use-package](https://www.gnu.org/software/emacs/manual/html_mono/use-package.html)
or [straight.el](https://github.com/radian-software/straight.el)

### Configuration with times
To auto-switch a theme on a specific time, use time strings:

```elisp
(use-package circadian
  :ensure t
  :config
  (setq circadian-themes '(("8:00" . wombat)
                           ("19:30" . adwaita)))
  (circadian-setup))
```


### Configuration with `:sunrise` and `:sunset`
To auto-switch a theme based on your current locations sunrise and sunset times:

1. Make sure to set your latitude and longitude (Get them e.g. at
   [latlong.net](https://www.latlong.net/)):
    ```elisp
      (setq calendar-latitude 40.712776)
      (setq calendar-longitude -74.005974)
    ```
2. Configure `circadian-themes` using the `:sunset` and `:sunset`
    ```elisp
    (use-package circadian
      :ensure t
      :config
      (setq circadian-themes '((:sunrise . adwaita)
                               (:sunset  . wombat)))
      (circadian-setup))
    ```


### Randomly selection from theme list
Circadian.el can randomly select a theme from a given list, e.g here using [doom-themes](https://github.com/doomemacs/themes) at sunset:

```elisp
(use-package doom-themes)

(use-package circadian
  :config
  (setq circadian-themes '((:sunrise . doom-gruvbox-light)
                           (:sunset . '(doom-dracula doom-gruvbox)) ))
  (add-hook 'emacs-startup-hook #'circadian-setup)
  (circadian-setup))

```


### Use with custom themes
To use custom themes, install them from MELPA using
e.g. [use-package](https://www.gnu.org/software/emacs/manual/html_mono/use-package.html)
or [straight.el](https://github.com/radian-software/straight.el).

Example usage featuring [hemera-themes](https://github.com/GuidoSchmidt/emacs-hemera-theme)
and [nyx-theme](https://github.com/GuidoSchmidt/emacs-nyx-theme) (with use-package). Make sure
to use `:defer` keyword. Omitting it may lead to broken colors
(see [issue 9](https://github.com/guidoschmidt/circadian.el/issues/9)):

```elisp
;; Install additinal themes from melpa
;; make sure to use :defer keyword
(use-package hemera-theme :ensure :defer)
(use-package nyx-theme :ensure :defer)
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

---

### Development & Testing

Install Emacs [cask](https://github.com/cask/cask) environment. On macOS you
canr use [homebrew](https://brew.sh/) with: `brew install cask`.

0. Clone the circadian.el repository: `git clone git@github.com:guidoschmidt/circadian.el.git`
1. `cd` into the git project directory with `cd circadian.el` and install dependencies via [`cask`](https://github.com/cask/cask)
2. Run test using `make test`
