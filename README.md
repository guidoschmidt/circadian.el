# circadian.el
> Theme-switching for Emacs based on daytime

![Logo Circadian](logo.png)

### Why?
Circadian tries to help reducing eye strain that may arise
from difference of your display brightness and the
surrounding light.

Inspired by color temperature shifting tools and brightness
adaption software like:
- [redshift](https://wiki.archlinux.org/index.php/Redshift)
- [f.lux](https://justgetflux.com/news/pages/mac/)
- [Lumen](https://github.com/anishathalye/lumen)

### Example usage
Example usage with default themes (leuven at 7:30, wombat at 19:30):
```elisp
(use-package circadian
  :ensure t
  :config
  (circadian-setup))
```

Example usage featuring [hemera-themes](https://github.com/GuidoSchmidt/emacs-hemera-theme)
and [nyx-theme](https://github.com/GuidoSchmidt/emacs-nyx-theme) (with use-package):

```elisp
(use-package circadian
  :load-path "~/.emacs.d/config/circadian/"
  :ensure t
  :config
  (setq circadian-themes '(("8:00" . hemera)
                           ("19:30" . nyx)))
  (circadian-setup))
```

---

### Themes
Circadian features two themes: Nyx & Hemera

#### Nyx (Nighttime)
![Nyx Theme](./themes/preview/nyx.png)

#### Hemera (Daytime)
![Hemera Theme](./themes/preview/hemera.png)

---

### TODOs & Ideas
- Possible to interpolate colors of themes?
- Query brightness sensors of laptops?
