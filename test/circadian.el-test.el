;;; circadian.el-test.el --- Tests for circadian.el
;;; Commentary:

;;; Code:
(require 'cl)
(require 'el-mock)
(load (expand-file-name "circadian.el" default-directory))

(ert-deftest test-circadian-time-comparison ()
  (let ((time-a "7:00")
        (time-b "14:00"))
    (should (equal t (circadian-compare-time-strings time-a time-b)))
    (should (equal nil (not (circadian-compare-time-strings time-a time-b))))
    (should (equal nil (circadian-compare-time-strings time-b time-a)))
    (should (equal t (not (circadian-compare-time-strings time-b time-a))))))

(ert-deftest test-circadian-filter-themes ()
  (setq circadian-themes '(("5:00"  . wombat)
                           ("7:00"  . leuven)
                           ("9:00"  . wombat)
                           ("11:00" . leuven)
                           ("12:00" . wombat)
                           ("13:00" . leuven)
                           ("14:00" . tango)
                           ("15:00" . adwaita)
                           ("16:00" . wombat)
                           ("17:00" . leuven)
                           ("18:00" . tango)
                           ("19:00" . adwaita)
                           ("21:00" . wombat)
                           ("23:00" . leuven)))
  (let ((time-now "4:10"))
    (should (equal 0 (length (circadian-filter-inactivate-themes
                              circadian-themes
                              time-now)))))
  (let ((time-now "5:03"))
    (should (equal 1 (length (circadian-filter-inactivate-themes
                              circadian-themes
                              time-now)))))
  (let ((time-now "7:20"))
    (should (equal 2 (length (circadian-filter-inactivate-themes
                              circadian-themes
                              time-now)))))
  (let ((time-now "11:52"))
    (should (equal 4 (length (circadian-filter-inactivate-themes
                              circadian-themes
                              time-now)))))
  (let ((time-now "00:14"))
    (should (equal 0 (length (circadian-filter-inactivate-themes
                              circadian-themes
                              time-now))))))

(ert-deftest test-circadian-activate-latest-theme ()
  (setq circadian-themes '(("7:00" . wombat)
                           ("16:00" . leuven)))
  (with-mock
   (stub circadian-now-time-string => "7:21")
   (circadian-activate-latest-theme)
   (should (equal 'wombat (cl-first custom-enabled-themes))))
  (with-mock
   (stub circadian-now-time-string => "17:00")
   (circadian-activate-latest-theme)
   (should (equal 'leuven (cl-first custom-enabled-themes)))))
;;; circadian.el-test.el ends here
