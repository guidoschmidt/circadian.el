# circadian
Theme-switching for emacs based on daytime

### Usage
- Clone this repository into your `.emacs.d/` directory
- Add this to your `init.el` (with use-package):

```elisp
(use-package circadian
  :load-path "~/.emacs.d/circadian/"
  :config
  (setq circadian-day-start-hour 8)
  (setq circadian-day-theme 'hemera)
  (setq circadian-night-start-hour 21)
  (setq circadian-night-theme 'nyx))
```

---

### Themes
Circadian features two themes: Nyx & Hemera, you can find them in the `themes` directory.
To enable them in emacs use the following:

```elisp
(add-to-list 'custom-theme-load-path "~/.emacs.d/circadian/themes/")
```

**Nyx**
![Nyx Theme](./themes/preview/nyx.png)

**Hemera**
![Hemera Theme](./themes/preview/hemera.png)
