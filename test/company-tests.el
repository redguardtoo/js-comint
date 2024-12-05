;; -*- lexical-binding: t -*-

(require 'js-comint)
(require 'ert)
(require 'company)

(load-file "./test/common.el")

;;; Company Integration Tests

;; sanity check: node should interpret ^U
(ert-deftest js-comint--clear-input-async/test-integration ()
  "Tests whether clear works in a live comint."
  (with-new-js-comint-buffer
    (process-send-string (get-buffer-process (current-buffer)) "5")
    (js-comint--clear-input-async)
    (comint-send-input)
    (let ((output (buffer-substring-no-properties
                   comint-last-input-end
                   (car comint-last-prompt))))
      ;; if it fails, node will see 5^U and fail, or see just 5 and echo it
      (should (string-empty-p output)))))

(ert-deftest js-comint/test-dumb-term ()
  "TERM env var should not be dumb."
  (js-comint-test-output-matches "process.env['TERM']" "emacs"))

(ert-deftest js-comint/test-company-global ()
  "Tests completion with an empty prompt."
  (with-new-js-comint-buffer
    (company-mode)
    (sit-for 1)
    (company-manual-begin)
    ;; register callback to see globals
    (company-complete-selection)
    (should (looking-back "AbortController"))))

(ert-deftest js-comint/test-company-unique-result ()
  "Completing with a unique result.
E.g. Arr => Array, or conso => console."
  (with-new-js-comint-buffer
    (company-mode)
    (setq company-async-timeout 5)
    (sit-for 1)
    (insert "Arra")
    (company-complete)
    (should (looking-back "Array"))))

(ert-deftest js-comint/test-company-complete-props ()
  "Completing props of an object.
E.g. should complete \"Array.\" to all properties."
  (with-new-js-comint-buffer
    (company-mode)
    (sit-for 1)
    (insert "Array.")
    (company-manual-begin)
    (company-complete-selection)
    (should (looking-back "Array.__proto__"))))

(ert-deftest js-comint/test-company-complete-long-line ()
  "Completing part of a line.
E.g. 'if (true) { console.'"
  (with-new-js-comint-buffer
    (company-mode)
    (sit-for 1)
    (insert "if (true) { console.")
    (company-manual-begin)
    (company-complete-selection)
    (should (looking-back "console.__proto__"))))

(ert-deftest js-comint/test-company-quick-typing ()
  "When completion is triggered while one is already running."
  (with-new-js-comint-buffer
    (company-mode)
    (sit-for 1)
    (insert "scrog.")
    (company-manual-begin)
    (insert "foo")
    (company-manual-begin)
    (should (looking-back "scrog.foo"))))
