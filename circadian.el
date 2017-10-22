;;; circadian.el --- Theme-switching based on daytime -*- lexical-binding: t -*-

;; Copyright (C) 2017 Guido Schmidt

;; Author: Guido Schmidt
;; Maintainer: Guido Schmidt <guido.schmidt.2912@gmail.com>
;; URL: https://github.com/GuidoSchmidt/circadian
;; Version: 0.3.0
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
;;
;; Example usage (with `use-package') featuring `nyx-theme' and `hemera-theme':
;;
;; (use-package circadian
;;   :config
;;   (setq circadian-themes '(("8:00" . hemera)
;;                            ("19:30" . nyx)))
;;   (circadian-setup))

;;; Code:
(require 'cl-lib)

(defcustom circadian-themes '(("7:30" . leuven)
                              ("19:30" . wombat))
  "List of themes mapped to the time they should be loaded."
  :type 'alist
  :group 'circadian)

(defun circadian-enable-theme (theme)
  "Clear previous `custom-enabled-themes' and load THEME."
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme theme t))

(defun circadian-mapc (entry)
  "Map over `circadian-themes' to run a timer for each ENTRY."
  (let ((time (circadian-match-sun (cl-first entry))))
    (let ((theme (cdr entry))
          (repeat-after 86400))
      (run-at-time time repeat-after 'circadian-enable-theme theme))))

;;; --- TIME COMPARISONS
(defun circadian-time-string-from-list (time-list)
  "Concat list items from TIME-LIST to a time string separated by ':'."
  (mapconcat 'identity time-list ":"))

(defun circadian-now-time-string ()
  "Get the current time as string in the format 'HH:MM'."
  (let ((only-time (split-string (cl-fourth (split-string (current-time-string))) ":")))
    (circadian-time-string-from-list (butlast only-time 1))))

(defun circadian-compare-hours (hour-a hour-b)
  "Compare two hours HOUR-A and HOUR-B."
  (> hour-a hour-b))

(defun circadian-compare-minutes (min-a min-b)
  "Compare two minutes MIN-A and MIN-B."
  (>= min-a min-b))

(defun circadian-a-earlier-b-p (time-a time-b)
  "Compare to time strings TIME-A and TIME-B by hour and minutes."
  (let ((parsed-time-a (parse-time-string time-a))
        (parsed-time-b (parse-time-string time-b)))
    (let ((hour-a (cl-third parsed-time-a))
          (hour-b (cl-third parsed-time-b))
          (minute-a (cl-second parsed-time-a))
          (minute-b (cl-second parsed-time-b)))
      (cond ((cl-equalp hour-b hour-a)
             (circadian-compare-minutes minute-b minute-a))
            (t (circadian-compare-hours hour-b hour-a))))))


(defun circadian-filter-inactivate-themes (theme-list now-time)
  "Filter THEME-LIST to consist of themes that are due NOW-TIME."
  (cl-remove-if (lambda (entry)
                  (let ((theme-time (circadian-match-sun (cl-first entry))))
                    (not (circadian-a-earlier-b-p theme-time now-time))))
                theme-list))

(defun circadian-activate-latest-theme ()
  "Check which themes are overdue to be activated and load the last.
`circadian-themes' is expected to be sorted by time for now."
  (let ((entry (cl-first (last (circadian-filter-inactivate-themes
                                circadian-themes
                                (circadian-now-time-string))))))
    (let ((theme (cdr entry)))
      (if (equal theme nil)
          (circadian-enable-theme (cdr (cl-first (last circadian-themes))))
        (circadian-enable-theme theme)))))

;; --- Sunset-sunrise
(defun clean-string (string)
  "Clean Emacs' STRING derived from `sunset-sunrise' result."
  (replace-regexp-in-string "[[:space]]" "" (replace-regexp-in-string
                                             "sun.[A-za-z]+" ""
                                             (replace-regexp-in-string
                                              "(CEST)" ""
                                              string))))

(defun circadian-sunrise ()
  "Get clean sunrise time string from Emacs' `sunset-sunrise'`."
  (clean-string
   (cl-first (split-string (sunrise-sunset) ","))))

(defun circadian-sunset ()
  "Get clean sunset time string from Emacs' `sunset-sunrise'`."
  (let ((sunset-string (clean-string (cl-second (split-string (sunrise-sunset) ",")))))
    (replace-regexp-in-string "[[:space]]" "" (replace-regexp-in-string
                                               "at.+" ""
                                               sunset-string))))

(defun circadian-12-to-24h-offset (string)
  "Match STRING for am/am and return the offset to 24h system."
  (cond ((string-match-p "am" string)
         0)
        ((string-match-p "pm" string)
         12)
        ;; default
        (t 0)))

(defun circadian-clear-12h-postfix (input)
  "Remove am/pm post-fix from INPUT."
  (replace-regexp-in-string ".m" "" input))

(defun circadian-parse-time-string (input)
  "Parse INPUT and return corrected 24h time string."
  (let ((splitted (split-string input ":"))
        (add-hour (circadian-12-to-24h-offset input)))
    (let ((hours (string-to-number (cl-first splitted)))
          (minutes (circadian-clear-12h-postfix (cl-second splitted))))
      (concat (number-to-string (+ hours add-hour)) ":" minutes))))

(defun circadian-match-sun (input)
  "Match INPUT to a case for setting up timers."
  (cond ((cl-equalp input :sunrise)
         (circadian-parse-time-string (circadian-sunrise)))
        ((cl-equalp input :sunset)
         (circadian-parse-time-string (circadian-sunset)))
        ((stringp input)
         input)))

;;;###autoload
(defun circadian-setup ()
  "Setup circadian based on `circadian-themes'."
  (mapc 'circadian-mapc circadian-themes)
  (circadian-activate-latest-theme))

(provide 'circadian)
;;; circadian.el ends here
