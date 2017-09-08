;;; circadian --- Theme-switching for Emacs based on daytime
;;; Commentary:

;; Author: Guido Schmidt
;; Maintainer: Guido Schmidt <guido.schmidt.2912@gmail.com>
;; URL: https://github.com/GuidoSchmidt/circadian
;; Version: 0.1
;; Keywords: circadian, themes
;; Package-Requires: ((emacs "24"))

;;; Code:
(defcustom circadian-day-theme 'hemera
  "Theme to use during the day."
  :type 'string
  :group 'circadian)

(defcustom circadian-night-theme 'nyx
  "Theme to use during the night."
  :type 'string
  :group 'circadian)

(defcustom circadian-night-start-hour 19
  "Night theme is enabled after this time of day."
  :type 'integer
  :group 'circadian)

(defcustom circadian-day-start-hour 7
  "Day theme is enabled after this time of day."
  :type 'integer
  :group 'circadian)

;; ---
(defun circadian-nighttime? ()
  "Check if night-theme needs to be enabled."
  (let ((current-hour (nth 2 (decode-time (date-to-time (current-time-string))))))
    (or (>= current-hour circadian-night-start-hour)
        (<= current-hour circadian-day-start-hour))))

(defun load-theme-if-needed (theme)
  "Load the THEME when it is not already loaded."
  (when (not (find theme custom-enabled-themes))
    (load-theme theme t)))

;; before loading new theme
(defun load-theme--disable-old-theme(theme &rest args)
  "Disable current theme before loading new one."
  (mapcar #'disable-theme custom-enabled-themes))
(advice-add 'load-theme :before #'load-theme--disable-old-theme)

;; After loading new theme
(defun load-theme--restore-line-numbering(theme &rest args)
  "Set linum-format again after loading any theme."
  (setq linum-format 'linum-format-func))
(advice-add 'load-theme :after #'load-theme--restore-line-numbering)

;; Custom hook for determining the day time theme
(defun circadian-theme-hook ()
  "Hook to automatically load defined circadian themes."
  (if (circadian-nighttime?)
      (load-theme-if-needed circadian-night-theme)
    (load-theme-if-needed circadian-day-theme)))
(add-hook 'after-change-major-mode-hook 'circadian-theme-hook)
(add-hook 'emacs-startup-hook 'circadian-theme-hook)

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory
                (file-name-directory load-file-name))))

(provide 'circadian)
;;; circadian.el ends here
