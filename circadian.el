;;; circadian.el --- Theme-switching based on daytime -*- lexical-binding: t -*-

;; Copyright (C) 2017 Guido Schmidt

;; Author: Guido Schmidt
;; Maintainer: Guido Schmidt <git@guidoschmidt.cc>
;; URL: https://github.com/GuidoSchmidt/circadian
;; Version: 0.3.2
;; Keywords: themes
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
;;
;; Example usage (with `use-package') - make sure to properly set your
;; latitude & longitude:
;;
;; (use-package circadian
;;   :config
;;   (setq calendar-latitude 49.0)
;;   (setq calendar-longitude 8.5)
;;   (setq circadian-themes '((:sunrise . wombat)
;;                            ("8:00"   . tango)
;;                            (:sunset  . adwaita)
;;                            ("23:30"  . tango)))
;;   (circadian-setup))

;;; Code:
(require 'cl-lib)
(require 'solar)

(defcustom circadian-before-load-theme-hook nil
  "Functions to run before the theme is changed."
  :type 'hook
  :group 'circadian)

(defcustom circadian-after-load-theme-hook nil
  "Functions to run after the themes has changed."
  :type 'hook
  :group 'circadian)

(defcustom circadian-themes '(("7:30" . tango)
                              ("19:30" . wombat))
  "List of themes mapped to the time they should be loaded."
  :type 'alist
  :group 'circadian)

(defun circadian-enable-theme (theme)
  "Clear previous `custom-enabled-themes' and load THEME."
  (mapc #'disable-theme custom-enabled-themes)
  (condition-case nil
      (progn
        (run-hook-with-args 'circadian-before-load-theme-hook theme)
        (load-theme theme t)
        (message "circadian.el — enabled %s" theme)
        (run-hook-with-args 'circadian-after-load-theme-hook theme))
    (error "Problem loading theme %s" theme)))

(defun circadian--encode-time (hour min)
  "Encode HOUR hours and MIN minutes into a valid format for `run-at-time'."
  (let ((now (decode-time)))
    (let ((day (nth 3 now))
          (month (nth 4 now))
          (year (nth 5 now))
          (zone (current-time-zone)))
      (encode-time 0 min hour day month year zone))))

(defun circadian-mapc (entry)
  "Map over `circadian-themes' to run a timer for each ENTRY."
  (let ((time       (circadian-match-sun (cl-first entry)))
        (24-hours   86400))
    (let ((theme        (cdr entry))
          (repeat-after 24-hours)
          (run-at       (circadian--encode-time (cl-first time)
                                                (cl-second time))))
      (run-at-time run-at
                   repeat-after
                   'circadian-enable-theme
                   theme))))

;;; --- TIME COMPARISONS
(defun circadian-now-time ()
  "Get the current time as string in the format (HH MM)."
  (reverse (cl-subseq (decode-time) 1 3)))

(defun circadian-a-earlier-b-p (time-a time-b)
  "Compare to time strings TIME-A and TIME-B by hour and minutes."
  (or (and (= (cl-first time-a) (cl-first time-b))
           (<= (cl-second time-a) (cl-second time-b)))
      (< (cl-first time-a) (cl-first time-b))))

(defun circadian-filter-inactivate-themes (theme-list now-time)
  "Filter THEME-LIST to consist of themes that are due NOW-TIME."
  (cl-remove-if (lambda (entry)
                  (let ((theme-time (circadian-match-sun (cl-first entry))))
                    (not (circadian-a-earlier-b-p theme-time now-time))))
                theme-list))

(defun circadian-activate-latest-theme ()
  "Check which themes are overdue to be activated and load the last.)
`circadian-themes' is expected to be sorted by time for now."
  (let ((past-themes (circadian-filter-inactivate-themes
                        circadian-themes
                        (circadian-now-time))))
    ;; (print past-themes)
   (let ((entry (cl-first (reverse past-themes))))
     (let ((theme (cdr entry)))
       (if (equal theme nil)
           (circadian-enable-theme (cdr (cl-first (last circadian-themes))))
         (circadian-enable-theme theme))))))

;; --- Sunset-sunrise
(defun circadian--frac-to-time (f)
  "Convert fractional time F to (HH MM)."
  (let ((l (cl-floor f)))
       (list (cl-first l)
             (floor (* 60 (cl-second l))))))

(defun circadian-sunrise ()
  "Get clean sunrise time string from Emacs' `sunset-sunrise'`."
  (circadian--frac-to-time (cl-first (cl-first (solar-sunrise-sunset (calendar-current-date))))))

(defun circadian-sunset ()
  "Get clean sunset time string from Emacs' `sunset-sunrise'`."
  (circadian--frac-to-time (cl-first (cl-second (solar-sunrise-sunset (calendar-current-date))))))

(defun circadian--string-to-time (input)
  "Parse INPUT string to '(HH MM)."
  (cl-map 'list #'string-to-number (split-string input ":")))

(defun circadian-match-sun (input)
  "Match INPUT to a case for setting up timers."
  (cond ((cl-equalp input :sunrise) (circadian-sunrise))
        ((cl-equalp input :sunset) (circadian-sunset))
        ((stringp input) (circadian--string-to-time input))))

;;;###autoload
(defun circadian-setup ()
  "Setup circadian based on `circadian-themes'."
  (mapc 'circadian-mapc circadian-themes)
  (circadian-activate-latest-theme))

(provide 'circadian)
;;; circadian.el ends here
