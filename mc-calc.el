;;; mc-calc.el --- Combine multiple-cursors and calc  -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Frank Roland

;; Author: Frank Roland hatheroldev@fgmail.com>
;; Keywords: convenience
;; Package-Requires: ((emacs "24.4") (multiple-cursors "1.2.1"))
;; Version: 0.0.1
;; URL: https://github.com/hatheroldev/mc-calc

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package allows the use of calc in regions with multiple cursors.

;;; Code:

(require 'multiple-cursors)
(require 'subr-x)                       ; string-join
(require 'calc)                         ; calc-quit, math-format-value
(require 'calc-vec)                     ; mat-map-vec

(defgroup mc-calc nil
  "Combine multiple-cursors and calc."
  :prefix "mc-calc-"
  :tag    "mc-calc"
  :group 'convenience)

(defcustom mc-calc-major-mode-eval-options-alist nil
  "Alist of major modes with `calc-eval' options.

Note: calc automatically sets `calc-language' from `major-mode'."
  :group 'mc-calc
  :type '(alist :key-type (symbol :tag "Major mode")
                :value-type (list :tag "Key/value pairs")))

(defcustom mc-calc-eval-options nil
  "Control calc settings.

This variable defines how to build first argument to `calc-eval' in
`mc-calc-eval'.

You can
- set it to t to use the current calc settings, this also disables
  automatic options,
- set it to nil to reset all calc settings to default,
- set key value pairs to be used as calc settings,
  e.g. '(calc-number-radix 16)."
  :group 'mc-calc
  :type '(list :tag "Key/value pairs"))

(defvar mc-calc-from-buffer nil
  "The buffer to return to.")

(defvar mc-calc-was-started-p nil
  "Non-nil if calc buffer was already visible.")

(defun mc-calc--eval-first-param (val)
  "Build fist `calc-eval' parameter and prepend VAL."
  (if (listp mc-calc-eval-options)
      ;; Explicit calc setting.
      (let ((lang (cl-assoc-if
                   #'derived-mode-p
                   mc-calc-major-mode-eval-options-alist)))
        (append (cons val mc-calc-eval-options)
                (when lang
                  (cdr lang))))
    ;; Use calc as currently set.
    val))

(defun mc-calc--eval (val &rest rest)
  "Call `calc-eval' with VAL plus options and REST."
  (apply #'calc-eval (mc-calc--eval-first-param val) rest))

;;;###autoload
(defun mc-calc-eval ()
  "Eval each cursor region in calc.

You can use $ and $$ in the region:
- $ will be substituted by the cursor number (starting with 0) and
- $$ will be substituted by the number of cursors.

Set `mc-calc-eval-options' to configure calc options."
  (interactive)
  (let* ((vals (mc--ordered-region-strings))
         (num (length vals))
         (i -1))                       ; Cursor number, usable as $ in formula.
    (setq mc--strings-to-replace
          (mapcar
           (lambda (val)
             (setq i (1+ i))
             (mc-calc--eval
              val
              nil
              i                         ; Available as $ in calc-eval.
              num))                     ; Available as $$ in calc-eval.
           vals)))
  (mc--replace-region-strings))

(defun mc-calc--set-values ()
  "Set values in order to use `mc-calc-copy-to-buffer'."
  (setq mc-calc-from-buffer (current-buffer))
  (setq mc-calc-was-started-p (get-buffer-window "*Calculator*" 'visible))
  (with-no-warnings
    (setq var-mccursors (mc/num-cursors))))

(defun mc-calc--quit ()
  "Quit calc, if it was not already visible, and return to previous buffer."
  (when mc-calc-from-buffer
    (if mc-calc-was-started-p
        (pop-to-buffer mc-calc-from-buffer)
      (calc-quit t)
      (switch-to-buffer mc-calc-from-buffer)))
  (setq mc-calc-from-buffer nil))

;;;###autoload
(defun mc-calc ()
  "Open calc and set values in order to use `mc-calc-copy-to-buffer'."
  (interactive)
  (mc-calc--set-values)
  (calc))

;;;###autoload
(defun mc-calc-grab ()
  "Collect each cursor region into a vector and push it to calc.

After some operations are performed on the vector
the result can be copied back with `mc-calc-copy-to-buffer'."
  (interactive)
  (mc-calc-vec--grab (mc--ordered-region-strings)))

(defun mc-calc-vec--grab (data)
  "Interpret DATA as list of strings and grab them into a calc vector.

The variables `mc-calc-from-buffer' and `mc-calc-was-started-p' are set so that
after some operations are performed on the vector the result can be copied
back with `mc-calc-copy-to-buffer'."
  (mc-calc--set-values)
  (calc)
  (mc-calc--eval
   (concat "[" (string-join (mapcar #'calc-eval data) ", ") "]")
   'push))

(defun mc-calc-vec--top-get ()
  "Get calc top vector and return it as list of strings."
  (let ((vals (mc-calc--eval 1 'rawtop))
        data)
    (math-map-vec
     (lambda (val)
       (setq data (cons (math-format-value val) data)))
     vals)
    (calc-eval 1 'pop)                  ; Remove top element from calc stack.
    (nreverse data)))

;;; Do not autoload
(defun mc-calc-copy-to-buffer ()
  "Copy the top of stack (vector or single element) into an editing buffer.

You must have used `mc-calc-grab' or `mc-calc' before you can use this function
from within the calc buffer."
  (interactive)
  (when (memq major-mode '(calc-mode calc-trail-mode))
    (pop-to-buffer mc-calc-from-buffer))
  (let ((data (mc-calc-vec--top-get))
        (num-cursors (mc/num-cursors)))
    (when (eq (length data) 1)
      ;; Extend single element to number of cursors.
      (setq data (make-list num-cursors (car data))))
    (when (not (eq (length data) num-cursors))
      (user-error "Vector length does not match number of multiple cursors"))
    (setq mc--strings-to-replace data))
  (mc--replace-region-strings)
  (mc-calc--quit))

(provide 'mc-calc)
;;; mc-calc.el ends here
