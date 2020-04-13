(require 'f)
(require 'calc-ext)

(defvar mc-calc-support-path
  (f-dirname load-file-name))

(defvar mc-calc-features-path
  (f-parent mc-calc-support-path))

(defvar mc-calc-root-path
  (f-parent mc-calc-features-path))

(add-to-list 'load-path mc-calc-root-path)

;; Ensure that we don't load old byte-compiled versions
(let ((load-prefer-newer t))
  (require 'mc-calc)
  (require 'espuds)
  (require 'ert))

(Setup
 ;; Before anything has run
 )

(Before
 (switch-to-buffer
  (get-buffer-create "mc-calc-test"))
 (multiple-cursors-mode 0)
 (erase-buffer)
 (deactivate-mark)
 (setq mc-calc-major-mode-eval-options-alist nil)
 (setq mc-calc-eval-options t)
 (calc)
 (calc-reset 0)
 (calc-quit))

(After
 ;; After each scenario is run
 )

(Teardown
 ;; After when everything has been run
 )
