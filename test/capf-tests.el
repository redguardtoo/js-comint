;; -*- lexical-binding: t -*-

(require 'js-comint)
(require 'ert)
(require 'ert-async)

(load-file "./test/common.el")

;;; Integration tests for native completion

;; quick fix for eask bug that improperly fails tests which use minibuffer
;; see https://github.com/emacs-eask/cli/pull/286
;; can be removed once fix is available in CI images
(when (advice-member-p 'eask-test-ert--message 'message)
  (message "override advice")
  (define-advice message (:before-until (&rest args))
    (not (car args))))

(ert-deftest js-comint/test-capf-init ()
  "When company is not loaded should use CAPF."
  (when (featurep 'company)
    (unload-feature 'company 't))
  (with-new-js-comint-buffer
    (should-not (equal completion-at-point-functions
                       (default-value 'completion-at-point-functions)))))

(ert-deftest js-comint/test-capf-unique-result ()
  "Completing with a unique result.
E.g. Arr => Array, or conso => console."
  (when (featurep 'company)
    (unload-feature 'company 't))
  (with-new-js-comint-buffer
    (sit-for 1)
    (insert "Arra")
    (completion-at-point)
    (should (looking-back "Array"))))

;; note that completion-at-point does not seem to work for an empty prompt

(ert-deftest js-comint/test-capf-complete-props ()
  "Completing props of an object.
E.g. should complete \"Array.\" to all properties."
  (when (featurep 'company)
    (unload-feature 'company 't))
  (with-new-js-comint-buffer
    (sit-for 1)
    (insert "Array.")
    (completion-at-point)
    (with-selected-window (get-buffer-window "*Completions*")
      (next-completion 1)
      (choose-completion))
    (should (looking-back "Array.__proto__"))))

(ert-deftest js-comint/test-capf-complete-long-line ()
  "Completing part of a line.
E.g. 'if (true) { console.'"
  (when (featurep 'company)
    (unload-feature 'company 't))
  (with-new-js-comint-buffer
    (sit-for 1)
    (insert "if (true) { console.")
    (completion-at-point)
    (with-selected-window (get-buffer-window "*Completions*")
      (next-completion 1)
      (choose-completion))
    (should (looking-back "console.__proto__"))))

(ert-deftest-async js-comint/test-capf-no-props (done)
  "Completing a string with trailing dot should not hang."
  (when (featurep 'company)
    (unload-feature 'company 't))
  (with-new-js-comint-buffer
    (sit-for 1)
    (insert "foo.")
    (should-not (completion-at-point))
    ;; detects if completion-at-point hangs
    (funcall done)))
