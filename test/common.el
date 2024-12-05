;; -*- lexical-binding: t -*-

(require 'js-comint)
(require 'ert)

(defmacro with-new-js-comint-buffer (&rest body)
  "Run BODY with a fresh js-comint as current buffer and exit after."
  (declare (indent 0) (debug t))
  `(progn
     (when (js-comint-get-process)
       (kill-process (js-comint-get-process)))
     (sleep-for 0.2)
     (kill-matching-buffers (js-comint-get-buffer-name) nil t)
     (run-js)
     (unwind-protect
         (with-current-buffer (js-comint-get-buffer)
           (font-lock-mode -1)
           (sit-for 1) ;; prevent race condition on start
           ,@body)
       (when (js-comint-get-process)
         (kill-process (js-comint-get-process)))
       (sleep-for 0.2)
       (kill-matching-buffers (js-comint-get-buffer-name) nil t))))

(defun js-comint-test-output-matches (input regex)
  "Verify that sending INPUT yields output that matches REGEX."
  (with-new-js-comint-buffer
    (js-comint-send-string input)
    (sit-for 1)
    (let ((output (buffer-substring-no-properties
                   comint-last-input-end
                   (car comint-last-prompt))))
      (should (string-match-p regex output)))))
