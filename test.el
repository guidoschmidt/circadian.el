;;; circadian.el-test.el --- Tests for circadian.el
;;; Commentary:

;;; Code:
(require 'cl-lib)
(require 'el-mock)
(require 'benchmark)

(load (expand-file-name "circadian.el" default-directory))

(ert-deftest test-circadian-filter-and-activate-themes ()
  "Test filtering of `circadian-themes' list`."
  (setq circadian-themes '(("5:01"  . wombat)
                           ("14:47" . tango)
                           ("23:59" . adwaita)))
  (setq circadian-themes-parsed (circadian-themes-parse))

  ;; Before 5:01
  (let ((time-now '(4 10)))
    (should (equal 0 (length (circadian-filter-inactivate-themes
                              circadian-themes-parsed
                              time-now)))))
  (with-mock
   (stub circadian-now-time => '(5 0 0))
   (circadian-activate-latest-theme)
   (should (equal 'adwaita (cl-first custom-enabled-themes))))

  ;; After 5:01, before 14:47
  (let ((time-now '(5 2)))
    (should (equal 1 (length (circadian-filter-inactivate-themes
                              circadian-themes-parsed
                              time-now)))))
  (with-mock
   (stub circadian-now-time => '(5 2 0))
   (circadian-activate-latest-theme)
   (should (equal 'wombat (cl-first custom-enabled-themes))))

  ;; After 14:47, before 23:59
  (let ((time-now '(14 47)))
    (should (equal 2 (length (circadian-filter-inactivate-themes
                              circadian-themes-parsed
                              time-now)))))
  (with-mock
   (stub circadian-now-time => '(14 47 1))
   (circadian-activate-latest-theme)
   (should (equal 'tango (cl-first custom-enabled-themes))))

  ;; After 23:59
  (let ((time-now '(23 59)))
    (should (equal 3 (length (circadian-filter-inactivate-themes
                              circadian-themes-parsed
                              time-now)))))
  (with-mock
   (stub circadian-now-time => '(23 59 15))
   (circadian-activate-latest-theme)
   (should (equal 'adwaita (cl-first custom-enabled-themes))))

  ;; Surpassing midnight
  (let ((time-now '(0 2)))
    (should (equal 0 (length (circadian-filter-inactivate-themes
                              circadian-themes-parsed
                              time-now)))))
  (with-mock
   (stub circadian-now-time => '(0 2 10))
   (circadian-activate-latest-theme)
   (should (equal 'adwaita (cl-first custom-enabled-themes)))))



(ert-deftest test-circadian-activate-latest-theme ()
  "Test `circadian-activate-latest-theme' used in `circadian-setup'."
  ;; (print "→ TEST: circadian-activate-latest-theme")
  (setq circadian-themes '(("7:00" . wombat)
                           ("16:00" . tango)))
  (with-mock
   (stub circadian-now-time => '(7 21 0))
   (circadian-activate-latest-theme)
   (should (equal 'wombat (cl-first custom-enabled-themes))))
  (with-mock
   (stub circadian-now-time => '(17 0 0))
   (circadian-activate-latest-theme)
   (should (equal 'tango (cl-first custom-enabled-themes)))))



(ert-deftest test-circadian-sunrise-sunset ()
  "Test :sunrise and :sunset keywords for theme switching.
@TODO currently failing, needs a fix"
  (setq calendar-latitude 49.329896)
  (setq calendar-longitude 8.570925)
  (setq circadian-themes '((:sunrise . wombat)
                           (:sunset  . adwaita)))
  (circadian-setup)

  (with-mock
    (stub circadian-now-time => '(14 21 0))
    (circadian-activate-latest-theme)
    (should (equal 'wombat (cl-first custom-enabled-themes))))

  (with-mock
    (stub circadian-now-time-string => '(16 50 0))
    (circadian-activate-latest-theme)
    (should (equal 'adwaita (cl-first custom-enabled-themes)))))



(ert-deftest test-circadian-sunrise-sunset-timezones ()
  "Test :sunrise and :sunset for different time zones (TODO)."
  ;; (print "→ TEST: sunrise/sunset (New York Time Zone)")
  (setq calendar-latitude   40.712775)
  (setq calendar-longitude -74.005973)
  (setq calendar-time-zone -360)
  (setq circadian-themes '((:sunrise . wombat)
                           (:sunset . adwaita))))



(ert-deftest test-circadian-time-comparisons ()
  "Test comparison of time strings.
Time A: 17:59
Time B: 17:58
B should be earlier than A
=> `circadian-a-earlier-b-p' should return t."
  ;; (print "→ TEST: time comparisons")
  (should (equal t (circadian-a-earlier-b-p '(7 50) '(7 51))))
  (should (equal nil (circadian-a-earlier-b-p '(19 20) '(19 19))))
  (should (equal t (circadian-a-earlier-b-p '(20 20) '(20 20)))))



(ert-deftest test-circadian-setup-benchmark ()
  "Benchmark (circadian-setup)."
  (setq calendar-latitude 49)
  (setq calendar-longitude 5)
  (setq circadian-themes '((:sunrise . wombat)
                           (:sunset  . tango)))
  (let ((elapsed (benchmark-elapse (circadian-setup))))
    (should (equal t (< elapsed 0.05)))
    (print (concat "(circadian-setup) took " (format "%.10f seconds" elapsed)))))



(ert-deftest test-circadian-invalid-solar-sunrise-sunset ()
  :expected-result :failed
  "Test to address issue #27.
https://github.com/guidoschmidt/circadian.el/issues/27"
  (setq calendar-latitude 79.482623)
  (setq calendar-longitude 5.318703)
  (setq circadian-themes '((:sunrise . wombat)
                           (:sunset  . tango)))
  (circadian-setup))



(defvar test-order '(member
                     test-circadian-filter-and-activate-themes
                     test-circadian-activate-latest-theme
                     test-circadian-sunrise-sunset
                     test-circadian-sunrise-sunset-timezones
                     test-circadian-time-comparisons
                     test-circadian-setup-benchmark
                     test-circadian-invalid-solar-sunrise-sunset))

(provide 'circadian.el-test)
;;; circadian.el-test.el ends here
