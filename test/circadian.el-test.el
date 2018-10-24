;;; circadian.el-test.el --- Tests for circadian.el
;;; Commentary:

;;; Code:
(require 'cl-lib)
(require 'el-mock)

(load (expand-file-name "circadian.el" default-directory))

(ert-deftest test-circadian-filter-themes ()
  "Test filtering of `circadian-themes' list.`"
  (print "-> TEST: circadian-filter-themes")
  (setq circadian-themes '(("5:01"  . wombat)
                           ("7:15"  . adwaita)
                           ("9:30"  . wombat)
                           ("11:31" . adwaita)
                           ("12:35" . wombat)
                           ("13:45" . adwaita)
                           ("14:47" . tango)
                           ("15:58" . adwaita)
                           ("16:59" . wombat)
                           ("17:02" . adwaita)
                           ("18:09" . tango)
                           ("19:54" . adwaita)
                           ("21:12" . wombat)
                           ("23:59" . adwaita)))
  (setq circadian-themes-parsed (circadian-themes-parse))
  (let ((time-now '(4 10)))
    (should (equal 0 (length (circadian-filter-inactivate-themes
                              circadian-themes-parsed
                              time-now)))))
  (let ((time-now '(5 03)))
    (should (equal 1 (length (circadian-filter-inactivate-themes
                              circadian-themes-parsed
                              time-now)))))
  (let ((time-now '(7 20)))
    (should (equal 2 (length (circadian-filter-inactivate-themes
                              circadian-themes-parsed
                              time-now)))))
  (let ((time-now '(11 52)))
    (should (equal 4 (length (circadian-filter-inactivate-themes
                              circadian-themes-parsed
                              time-now)))))
  (let ((time-now '(00 14)))
    (should (equal 0 (length (circadian-filter-inactivate-themes
                              circadian-themes-parsed
                              time-now))))))



(ert-deftest test-circadian-activate-latest-theme ()
  "Test `circadian-activate-latest-theme' used in `circadian-setup'."
  (print "-> TEST: circadian-activate-latest-theme")
  (setq circadian-themes '(("7:00" . wombat)
                           ("16:00" . tango)))
  (with-mock
   (stub circadian-now-time => '(7 21))
   (circadian-activate-latest-theme)
   (should (equal 'wombat (cl-first custom-enabled-themes))))
  (with-mock
   (stub circadian-now-time => '(17 00))
   (circadian-activate-latest-theme)
   (should (equal 'tango (cl-first custom-enabled-themes)))))



(ert-deftest test-circadian-sunrise-sunset ()
  "Test :sunrise and :sunset keywords for theme switching."
  (print "-> TEST: sunrise/sunset")
  (setq calendar-latitude 49.329896)
  (setq calendar-longitude 8.570925)
  (setq circadian-themes '((:sunrise . wombat)
                           (:sunset . adwaita)))
  (setq circadian-themes-parsed (circadian-themes-parse))
  (with-mock
   (stub sunrise-sunset => "Sunrise 4:32am (CEST), sunset 4:24pm (CEST) at 8N, 49E (11:52 hrs daylight)")
   (should (equal 1 (length (circadian-filter-inactivate-themes
                             circadian-themes-parsed
                             '(14 51)))))
   (with-mock
    (stub circadian-now-time-string => '(14 21))
    (circadian-activate-latest-theme)
    (should (equal 'wombat (cl-first custom-enabled-themes))))
   (should (equal 2 (length (circadian-filter-inactivate-themes
                             circadian-themes-parsed
                             '(23 59)))))))



(ert-deftest test-circadian-sunrise-sunset-north-america ()
  "Test :sunrise and :sunset for north american calendar."
  (print "-> TEST: sunrise/sunset (New York Time Zone)")
  (setq calendar-latitude   40.712775)
  (setq calendar-longitude -74.005973)
  (setq calendar-time-zone -360)
  (setq circadian-themes '((:sunrise . wombat)
                           (:sunset . adwaita)))
  (with-mock
   (stub circadian-now-time-string => "00:43")
   (circadian-activate-latest-theme)
   (should (equal 'adwaita (cl-first custom-enabled-themes))))
  (with-mock
   (stub circadian-now-time-string => "12:43")
   (circadian-activate-latest-theme)
   (should (equal 'wombat (cl-first custom-enabled-themes)))))




(ert-deftest test-circadian-time-comparisons ()
  "Test comparison of time strings.
   Time A: 17:59
   Time B: 17:58
   B should be earlier than A
   => `circadian-a-earlier-b-p' should return `t'."
  (print "-> TEST: time comparisons")
  (should (equal t (circadian-a-earlier-b-p '(7 50) '(7 51))))
  (should (equal nil (circadian-a-earlier-b-p '(19 20) '(19 19))))
  (should (equal t (circadian-a-earlier-b-p '(20 20) '(20 20)))))

(provide 'circadian.el-test)
;;; circadian.el-test.el ends here
