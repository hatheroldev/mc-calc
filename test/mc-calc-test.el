;;; mc-calc-test.el --- Tests for mc-calc
(require 'cl)

(ert-deftest mc-calc-test/calc-with-reset-config ()
  (setq mc-calc-major-mode-eval-options-alist nil)
  (setq mc-calc-eval-options nil)
  (should (equal (mc-calc--eval-first-param "x") '("x"))))

(ert-deftest mc-calc-test/calc-with-set-config ()
  (setq mc-calc-major-mode-eval-options-alist nil)
  (setq mc-calc-eval-options '(calc-language c))
  (should (equal (mc-calc--eval-first-param "x") '("x" calc-language c))))

(ert-deftest mc-calc-test/calc-with-current-config ()
  (setq mc-calc-major-mode-eval-options-alist nil)
  (setq mc-calc-eval-options t)
  (should (equal (mc-calc--eval-first-param "x") "x")))

(ert-deftest mc-calc-test/eval ()
  (setq mc-calc-major-mode-eval-options-alist nil)
  (setq mc-calc-eval-options t)
  (should (equal (mc-calc--eval "2 ms * 10") "20 ms")))

(ert-deftest mc-calc-test/eval-args ()
  (setq mc-calc-major-mode-eval-options-alist nil)
  (setq mc-calc-eval-options t)
  (should (equal (mc-calc--eval "$ ms * $$" nil 2 10) "20 ms")))

(ert-deftest mc-calc-test/set-values ()
  (mc-calc--set-values)
  (should (equal (format "%S" mc-calc-from-buffer) "#<buffer  *temp*>"))
  (should (equal (format "%S" mc-calc-was-started-p) "nil"))
  (should (equal var-mccursors 1)))

(ert-deftest mc-calc-test/vec-grab-1 ()
  (with-calc
    (mc-calc-vec--grab '("10"))
    (should (equal (calc-eval 1 'top) "[10]"))))

(ert-deftest mc-calc-test/vec-grab-2 ()
  (with-calc
    (mc-calc-vec--grab '("usimplify(1 ms + 9000 us)" "evalv(mccursors)"))
    (should (equal (calc-eval 1 'top) "[10. ms, 1]"))))

(ert-deftest mc-calc-test/vec-top-get-2 ()
  (with-calc
    (calc-eval "[10 ms, 20 ms]" 'push)
    (should (equal (calc-eval 0 'pop) 1))
    (should (equal (mc-calc-vec--top-get) '("10 ms" "20 ms")))
    (should (equal (calc-eval 0 'pop) 0))))
;;; mc-calc-test.el ends here
