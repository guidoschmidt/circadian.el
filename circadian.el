;;; circadian.el --- Theme-switching based on daytime

;; Copyright (C) 2010-2017 Guido Schmidt

;; Author: Guido Schmidt
;; Maintainer: Guido Schmidt <guido.schmidt.2912@gmail.com>
;; URL: https://github.com/GuidoSchmidt/circadian
;; Version: 0.1
;; Keywords: circadian, themes
;; Package-Requires: ((emacs "24.4"))

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Circadian provides automated theme switching based on daytime.
;; Example usage (with `use-package'):
;;
;; (use-package circadian
;;   :load-path "~/.emacs.d/config/circadian/"
;;   :config
;;   (setq circadian-day-start-hour 8)
;;   (setq circadian-day-theme 'hemera)
;;   (setq circadian-night-start-hour 19)
;;   (setq circadian-night-theme 'nyx))

;;; Change Log:

;; 0.1
;; - Initial release
;; - Variables for day/night hour
;; - Themes included: hemera-theme, nyx-theme

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

(defun circadian-nighttime? ()
  "Check if night-theme needs to be enabled."
  (let ((current-hour (nth 2 (decode-time (date-to-time (current-time-string))))))
    (or (>= current-hour circadian-night-start-hour)
        (<= current-hour circadian-day-start-hour))))

(defun circadian-load-theme-if-needed (theme)
  "Load the THEME when it is not already loaded."
  (when (not (member theme custom-enabled-themes))
    (load-theme theme t)))

;; before loading new theme
(defun circadian-load-theme-disable-old-theme(theme &rest args)
  "Disable current theme before loading new one."
  (mapcar #'disable-theme custom-enabled-themes))
(advice-add 'load-theme :before #'circadian-load-theme-disable-old-theme)

;; After loading new theme
(defun circadian-load-theme-restore-line-numbering(theme &rest args)
  "Set linum-format again after loading any theme."
  (setq linum-format 'linum-format-func))
(advice-add 'load-theme :after #'circadian-load-theme-restore-line-numbering)

;; Custom hook for determining the day time theme
(defun circadian-theme-hook ()
  "Hook to automatically load defined circadian themes."
  (if (circadian-nighttime?)
      (circadian-load-theme-if-needed circadian-night-theme)
    (circadian-load-theme-if-needed circadian-day-theme)))
(add-hook 'after-change-major-mode-hook 'circadian-theme-hook)
(add-hook 'emacs-startup-hook 'circadian-theme-hook)

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory
                (file-name-directory load-file-name))))

(provide 'circadian)
;;; circadian.el ends here
