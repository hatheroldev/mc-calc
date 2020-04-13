;;; test-helper.el --- Helpers for mc-calc-test.el
(require 'mc-calc)

(defmacro with-calc (&rest body)
  "Evaluate BODY in an clean calc state."
  `(cl-letf (((symbol-function 'message) #'format)) ; Silence calc
     (calc-eval (calc-eval 0 'pop) 'pop) ; Remove all stack elements
     ,@body))
;;; test-helper.el ends here
