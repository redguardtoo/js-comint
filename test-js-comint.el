;; -*- lexical-binding: t -*-

(require 'js-comint)
(require 'ert)

(defun js-comint-test-buffer-matches (regex)
  "Search the js-comint buffer for the given regular expression.
Return 't if a match is found, nil otherwise."
  (with-current-buffer (js-comint-get-buffer)
    (save-excursion
      (goto-char (point-min))
      (if (re-search-forward regex nil t) t nil))))

(defun js-comint-test-output-matches (input regex)
  "Verify that sending INPUT yields output that matches REGEX."

  ;; Start an instance to run tests on.
  (js-comint-reset-repl)

  (sit-for 1)

  (js-comint-send-string input)

  (sit-for 1)

  (js-comint-test-buffer-matches regex))

(ert-deftest js-comint-test-multiline-dotchain-line-start ()
  "Test multiline statement with dots at beginning of lines."
  (should (js-comint-test-output-matches "[1, 2, 3]
  .map((it) => it + 1)
  .filter((it) => it > 0)
  .reduce((prev, curr) => prev + curr, 0);" "^9$")))

(ert-deftest js-comint-test-multiline-dotchain-line-start-dos ()
  "Test multiline statement with dots at beginning of lines, with
DOS line separators."
  (should (js-comint-test-output-matches "[1, 2, 3]\r
  .map((it) => it + 1)\r
  .filter((it) => it > 0)\r
  .reduce((prev, curr) => prev + curr, 0);\r
" "^9$")))

(ert-deftest js-comint-test-multiline-dotchain-line-end ()
  "Test multiline statement with dots at end of lines."
  (should (js-comint-test-output-matches "[1, 2, 3].
map((it) => it + 1).
filter((it) => it > 0).
reduce((prev, curr) => prev + curr, 0);" "^9$")))
