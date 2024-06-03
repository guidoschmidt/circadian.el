;;; circadian.el --- Theme-switching based on daytime -*- lexical-binding: t -*-

;; Copyright (C) 2017 Guido Schmidt

;; Author: Guido Schmidt
;; Maintainer: Guido Schmidt <git@guidoschmidt.cc>
;; URL: https://github.com/GuidoSchmidt/circadian
;; Version: 1.0.0
;; Keywords: themes
;; Package-Requires: ((emacs "27.2"))

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

(defcustom circadian-verbose nil
  "Timer to execute on next theme switch."
  :type 'boolean
  :group 'circadian)

(defcustom circadian-next-timer nil
  "Timer to execute on next theme switch."
  :type 'timer
  :group 'circadian)

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
  ;; Only load the argument theme, when `custom-enabled-themes'
  ;; does not contain it.
  
  (condition-case nil
      (progn
        (run-hook-with-args 'circadian-before-load-theme-hook theme)

        (if (equal nil (member theme custom-enabled-themes))
            (progn
              (mapc #'disable-theme custom-enabled-themes)
              (load-theme theme t)
              (if (not (equal nil circadian-next-timer))
                  (cancel-timer circadian-next-timer))
              (setq circadian-next-timer nil)
              (if circadian-verbose
                  (message "[circadian.el] → Enabled %s theme @ %s"
                           theme
                           (format-time-string "%H:%M:%S %Z"))))
          (progn
            (if circadian-verbose
                (message "[circadian.el] → %s already enabled"
                         theme))))

        (circadian-schedule)

        (run-hook-with-args 'circadian-after-load-theme-hook theme))
    (error "[circadian.el/ERROR] → Problem loading theme %s" theme)))

(defun circadian-encode-time (hour min)
  "Encode HOUR hours and MIN minutes into a valid format for `run-at-time'."
  (let* ((now (decode-time))
         (is-earlier (circadian-a-earlier-b-p (list hour min) (list (nth 2 now) (nth 1 now))))
         (tomorrow (decode-time (+ (* 24 60 60)
                                   (time-to-seconds (current-time)))))
         (day (if is-earlier
                  (nth 3 tomorrow)
                (nth 3 now)))
         (month (if is-earlier
                    (nth 4 now)
                  (nth 4 tomorrow)))
         (year (if is-earlier
                   (nth 5 now)
                 (nth 5 tomorrow))))
    (encode-time 0 min hour day month year nil -1 nil)))

(defun circadian-themes-parse ()
  "Parse `circadian-themes', filter the list and sort it by time.
Uses `circadian-check-calendar' to filter out entries which use `:sunrise'
or `:sunset' if either `calendar-latitude' or `calendar-longitude' is not
set and  and sort the final list by time."
  (sort
    (mapcar
     (lambda (entry)
       (cons (circadian-match-sun (cl-first entry)) (cdr entry)))
     (seq-filter
      (lambda (entry)
        (if (equal nil (circadian-check-calendar))
            (and (not (equal :sunrise (cl-first entry)))
                 (not (equal :sunset (cl-first entry))))
          t))
      circadian-themes))
   (lambda (a b) (circadian-a-earlier-b-p (car a) (car b)))))

;;; --- TIME COMPARISONS
(defun circadian-now-time ()
  "Get the current time as string in the format (HH MM SS)."
  (reverse (cl-subseq (decode-time) 0 3)))

(defun circadian-a-earlier-b-p (time-a time-b)
  "Compare to time strings TIME-A and TIME-B by hour and minutes."
  (or (and (= (cl-first time-a) (cl-first time-b))
           (<= (cl-second time-a) (cl-second time-b)))
      (< (cl-first time-a) (cl-first time-b))))

(defun circadian-filter-inactivate-themes (theme-list now-time)
  "Filter THEME-LIST to consist of themes that are due NOW-TIME."
  (cl-remove-if (lambda (entry)
                  (let ((theme-time (cl-first entry)))
                    (not (circadian-a-earlier-b-p theme-time now-time))))
                theme-list))

(defun circadian-activate-current ()
  "Check which theme should be active currently and return a time for the next run."
  (let* ((themes (circadian-themes-parse))
         (now (circadian-now-time))
         (past-themes (circadian-filter-inactivate-themes themes now))
         (entry (car (last (or past-themes themes))))
         (theme-or-theme-list (cdr entry))
         (theme (if (listp theme-or-theme-list)
                    (progn
                      (nth (random (length theme-or-theme-list)) theme-or-theme-list))
                  theme-or-theme-list)))
    (circadian-enable-theme theme)))

(defun circadian-schedule()
  "Schedule the next timer for circadian."
  (random (format-time-string "%H:%M" (decode-time)))
  (let* ((themes (circadian-themes-parse))
         (now (circadian-now-time))
         (past-themes (circadian-filter-inactivate-themes themes now))
         (entry (car (last (or past-themes themes))))
         (next-entry (or (cadr (member entry themes))
                         (if (circadian-a-earlier-b-p (circadian-now-time) (cl-first entry))
                             (car themes)
                           (cl-first past-themes))))
         (next-theme-or-theme-list (cdr next-entry))
         (next-theme (if (listp next-theme-or-theme-list)
                         (progn
                           (nth (random (length next-theme-or-theme-list)) next-theme-or-theme-list))
                       next-theme-or-theme-list))
         (next-time (circadian-encode-time
                     (cl-first (cl-first next-entry))
                     (cl-second (cl-first next-entry)))))
    (if (equal nil circadian-next-timer)
        (progn (setq circadian-next-timer
                     (run-at-time
                      next-time
                      nil
                      #'circadian-enable-theme next-theme))
               (if circadian-verbose
                   (message "[circadian.el] → Next theme %s @ %s"
                            (if (listp next-theme)
                                (concat "one of " (format "%s" next-theme))
                              next-theme)
                            (format-time-string "%H:%M:%S %Z" next-time)))))))

;; --- Sunset-sunrise
(defun circadian-frac-to-time (f)
  "Convert fractional time F to (HH MM)."
  (let ((l (cl-floor f)))
       (list (cl-first l)
             (floor (* 60 (cl-second l))))))

(defun circadian-check-calendar ()
  "Check if either calendar-latitude or calendar-longitude is not set."
  ;; 1. Check `calendar-latitude' ond message user if it's not set.
  (if (and circadian-verbose (equal nil calendar-latitude))
      (message "calendar-latitude not set. Consider using fixed time strings, e.g.

(setq circadian-themes '((\"9:00\" . wombat)
                         (\"20:00\" . tango))

or set calendar-latitude:
    (setq calendar-latitude 49.0)"))

  ;; 2. Check `calendar-longitude' ond message user if it's not set.
  (if (and circadian-verbose (equal nil calendar-longitude))
      (message "calendar-longitude not set. Consider using fixed time strings, e.g.

(setq circadian-themes '((\"9:00\" . wombat)
                         (\"20:00\" . tango))

or set calendar-longitude:
    (setq calendar-longitude 8.5)"))

  (cond ((equal nil calendar-latitude)
         (progn
           nil))
        
        ((equal nil calendar-longitude)
         (progn
           nil))
        
        (t)))

(defun circadian-sunrise ()
  "Get clean sunrise time string from Emacs' `sunset-sunrise'`."
  (let ((solar-result (solar-sunrise-sunset (calendar-current-date))))
    (let ((sunrise-numeric (cl-first (cl-first solar-result))))
      (if (equal nil sunrise-numeric)
          (if circadian-verbose
              (message "[circadian.el/ERROR] No valid sunrise from solar-sunrise-sunset, consider using fixed time strings, e.g. (setq circadian-themes '((\"9:00\" . wombat) (\"20:00\" . tango)))"))
        (circadian-frac-to-time sunrise-numeric)))))

(defun circadian-sunset ()
  "Get clean sunset time string from Emacs' `sunset-sunrise'`."
  (let ((solar-result (solar-sunrise-sunset (calendar-current-date))))
    (let ((sunset-numeric (cl-first (cl-second solar-result))))
      (if (equal nil sunset-numeric)
          (if circadian-verbose
              (message "[circadian.el/ERROR] No valid sunset from solar-sunrise-sunset, consider using fixed time strings, e.g. (setq circadian-themes '((\"9:00\" . wombat) (\"20:00\" . tango)))"))
        (circadian-frac-to-time sunset-numeric)))))

(defun circadian-string-to-time (input)
  "Parse INPUT string to `(HH MM)'."
  (cl-map 'list #'string-to-number (split-string input ":")))

(defun circadian-match-sun (input)
  "Match INPUT to a case for setting up timers."
  (cond ((cl-equalp input :sunrise)
         (let  ((sunrise (circadian-sunrise)))
           (if (equal sunrise nil)
               (if circadian-verbose
                (message "[circadian.el/ERROR] Could not get valid sunset time — check your time zone settings")))
           sunrise))

        ((cl-equalp input :sunset)
         (let ((sunset (circadian-sunset)))
           (if (equal sunset nil)
               (if circadian-verbose
                   (message "[circadian.el/ERROR] Could not get valid sunset time — check your time zone settings")))
           sunset))

        ((stringp input) (circadian-string-to-time input))))

;;;###autoload
(defun circadian-setup ()
  "Setup circadian based on `circadian-themes'."
  (interactive)
  (circadian-activate-current)
  (circadian-schedule))

(defun circadian-stop ()
  "Stop `circadian-next-timer' - To re-schedule, call `circadian-setup' again."
  (interactive)
  (if (not (equal nil circadian-next-timer))
      (progn
        (cancel-timer circadian-next-timer)
        (setq circadian-next-timer nil))))

(provide 'circadian)
;;; circadian.el ends here
