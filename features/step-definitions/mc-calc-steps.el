(require 'cl)

(Then "^I should have \\([0-9]+\\) cursors$"
      (lambda (num)
        (let ((actual (mc/num-cursors)))
          (cl-assert (eq (string-to-number num) actual) nil
                     "Expected to have %s cursors, but have %d." num actual))))

(Then "^\"\\([^\"]+\\)\" should return \"\\([^\"]+\\)\"$"
      (lambda (command expected)
        (setq expected (replace-regexp-in-string "''" "\"" expected))
        (let ((actual (format "%S" (funcall (intern command))))
              (message "Expected\n%s\nto be part of:\n%s"))
          (cl-assert (s-contains? expected actual) nil message expected actual))))

(When "^I pop to buffer \"\\([^\"]+\\)\"$"
      "Pop to BUFFER."
      (lambda (buffer)
        (pop-to-buffer buffer)))

(Then "^the buffer \"\\([^\"]+\\)\" should not be visible$"
      (lambda (buffer)
        (cl-assert (not (get-buffer-window buffer 'visible))
                   nil
                   "Expected %s to be visible")))

(Then "^the buffer \"\\([^\"]+\\)\" should be visible$"
      (lambda (buffer)
        (cl-assert (get-buffer-window buffer 'visible)
                   nil
                   "Expected %s to be visible")))
