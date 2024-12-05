;;; js-comint.el --- JavaScript interpreter in window.  -*- lexical-binding: t -*-

;;; Copyright (C) 2008 Paul Huff
;;; Copyright (C) 2015 Stefano Mazzucco
;;; Copyright (C) 2016-2020 Chen Bin

;;; Author: Paul Huff <paul.huff@gmail.com>, Stefano Mazzucco <MY FIRST NAME - AT - CURSO - DOT - RE>
;;; Maintainer: Chen Bin <chenbin.sh AT gmail DOT com>
;;; Created: 15 Feb 2014
;;; Version: 1.2.0
;;; URL: https://github.com/redguardtoo/js-comint
;;; Package-Requires: ((emacs "28.1"))
;;; Keywords: javascript, node, inferior-mode, convenience

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; at your option any later version.

;; js-comint.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING, or type `C-h C-c'. If
;; not, write to the Free Software Foundation at this address:

;;   Free Software Foundation
;;   51 Franklin Street, Fifth Floor
;;   Boston, MA 02110-1301
;;   USA

;;; Commentary:

;; This program is a comint mode for Emacs which allows you to run a
;; compatible javascript repl like Node.js/Spidermonkey/Rhino inside Emacs.
;; It also defines a few functions for sending javascript input to it
;; quickly.

;; Usage:
;;  Put js-comint.el in your load path
;;  Add (require 'js-comint) to your .emacs or ~/.emacs.d/init.el
;;
;;  Optionally, set the `js-comint-program-command' string
;;  and the `js-comint-program-arguments' list to the executable that runs
;;  the JS interpreter and the arguments to pass to it respectively.
;;  For example, on windows you might need below setup:
;;    (setq js-comint-program-command "C:/Program Files/nodejs/node.exe")
;;
;;  After setup, do: `M-x js-comint-repl'
;;  Away you go.
;;  `node_modules' is *automatically* searched and appended into environment
;;  variable `NODE_PATH'.  So 3rd party javascript is usable out of box.

;;  If you have nvm, you can select the versions of node.js installed and run
;;  them.  This is done thanks to nvm.el.
;;  Please note nvm.el is optional.  So you need *manually* install it.
;;  To enable nvm support, run `js-do-use-nvm'.
;;  The first time you start the JS interpreter with `js-comint-repl', you will
;;be asked to select a version of Node.js
;;  If you want to change version of node js, run `js-comint-select-node-version'
;;
;;  `js-comint-clear' clears the content of REPL.
;;
;; You may get cleaner output by following setup (highly recommended):
;;
;;  Output matching `js-comint-drop-regexp' will be dropped silently
;;
;;  You can add the following lines to your .emacs to take advantage of
;;  cool keybindings for sending things to the javascript interpreter inside
;;  of Steve Yegge's most excellent js2-mode.
;;
;;   (add-hook 'js2-mode-hook
;;             (lambda ()
;;               (local-set-key (kbd "C-x C-e") 'js-send-last-sexp)
;;               (local-set-key (kbd "C-c b") 'js-send-buffer)))

;;; Code:

(require 'js)
(require 'comint)
(require 'ansi-color)
(require 'cl-lib)

(defgroup js-comint nil
  "Run a javascript process in a buffer."
  :group 'js-comint)

(defcustom js-comint-program-command "node"
  "JavaScript interpreter."
  :type 'string
  :group 'js-comint)

(defcustom js-comint-set-env-when-startup t
  "Set environment variable NODE_PATH automatically during startup."
  :type 'boolean
  :group 'js-comint)

(defvar js-comint-module-paths '()
  "List of modules paths which could be used by NodeJS to search modules.")

(defvar js-comint-drop-regexp
  "\\(\x1b\\[[0-9]+[GJK]\\|^[ \t]*undefined[\r\n]+\\)"
  "Regex to silence matching output.")

(defcustom js-comint-program-arguments '()
  "List of command line arguments passed to the JavaScript interpreter."
  :type '(list string)
  :group 'js-comint)

(defcustom js-comint-prompt "> "
  "Prompt used in `js-comint-mode'."
  :group 'js-comint
  :type 'string)

(defcustom js-comint-mode-hook nil
  "*Hook for customizing js-comint mode."
  :type 'hook
  :group 'js-comint)

(defcustom js-use-nvm nil
  "When t, use NVM.  Requires nvm.el."
  :type 'boolean
  :group 'js-comint)

(defvar js-comint-buffer "Javascript REPL"
  "Name of the inferior JavaScript buffer.")

;; process.stdout.columns should be set.
;; but process.stdout.columns in Emacs is infinity because Emacs returns 0 as winsize.ws_col.
;; The completion candidates won't be displayed if process.stdout.columns is infinity.
;; see also `handleGroup` function in readline.js
(defvar js-comint-code-format
  (concat
   "process.stdout.columns = %d;"
   "require('repl').start({
\"prompt\": '%s',
\"ignoreUndefined\": true,
\"preview\": true,
\"replMode\": require('repl')['REPL_MODE_' + '%s'.toUpperCase()]
})"))

(defvar js-nvm-current-version nil
  "Current version of node for js-comint.
Either nil or a list (VERSION-STRING PATH).")

(declare-function nvm--installed-versions "nvm.el" ())
(declare-function nvm--find-exact-version-for "nvm.el" (short))

;; company.el declarations
(defvar company-backends)
(declare-function company-begin-backend "company.el" (backend &optional callback))
(declare-function company-in-string-or-comment "company.el" nil)

(defun js-comint-list-nvm-versions (prompt)
  "List all available node versions from nvm prompting the user with PROMPT.
Return a string representing the node version."
  (let ((candidates (sort (mapcar 'car (nvm--installed-versions)) 'string<)))
    (completing-read prompt
                     candidates nil t nil
                     nil
                     (car candidates))))

;;;###autoload
(defun js-do-use-nvm ()
  "Enable nvm."
  (setq js-use-nvm t))

;;;###autoload
(defun js-comint-select-node-version (&optional version)
  "Use a given VERSION of node from nvm."
  (interactive)
  (require 'nvm)
  (setq js-use-nvm t) ;; NOTE: js-use-nvm could probably be deprecated
  (unless version
    (let* ((old-js-nvm (car js-nvm-current-version))
           (prompt (if old-js-nvm
                       (format "Node version (current %s): " old-js-nvm)
                     "Node version: ")))
      (setq version (js-comint-list-nvm-versions prompt))))

  (setq js-nvm-current-version (nvm--find-exact-version-for version)
        js-comint-program-command (format "%s/bin/node" (cadr js-nvm-current-version))))

(defun js-comint-guess-load-file-cmd (filename)
  "Create Node file loading command for FILENAME."
  (concat "require(\"" filename "\")\n"))

(defun js-comint-quit-or-cancel ()
  "Send ^C to Javascript REPL."
  (interactive)
  (process-send-string (js-comint-get-process) "\x03"))

(defun js-comint--path-sep ()
  "Separator of file path."
  (if (eq system-type 'windows-nt) ";" ":"))

(defun js-comint--suggest-module-path ()
  "Path to node_modules in parent dirs, or nil if none exists."
  (when-let ((dir (locate-dominating-file default-directory "node_modules")))
    (expand-file-name "node_modules" dir)))

(defun js-comint-get-process ()
  "Get repl process."
  (and js-comint-buffer
       (get-process js-comint-buffer)))

;;;###autoload
(defun js-comint-add-module-path ()
  "Add a directory to `js-comint-module-paths'."
  (interactive)
  (let ((dir (read-directory-name "Module path:"
                                  (js-comint--suggest-module-path))))
    (when dir
      (add-to-list 'js-comint-module-paths (file-truename dir))
      (message "\"%s\" added to `js-comint-module-paths'" dir))))

;;;###autoload
(defun js-comint-delete-module-path ()
  "Delete a directory from `js-comint-module-paths'."
  (interactive)
  (cond
   ((not js-comint-module-paths)
    (message "`js-comint-module-paths' is empty."))
   (t
    (let* ((dir (ido-completing-read "Directory to delete: "
                                     js-comint-module-paths)))
      (when dir
        (setq js-comint-module-paths
              (delete dir js-comint-module-paths))
        (message "\"%s\" delete from `js-comint-module-paths'" dir))))))

;;;; Completions:
(defun js-comint--process-completion-output (completion prefix)
  "Format COMPLETION string as a list of candidates.
PREFIX is the original completion prefix string."
  (let* ((completion (replace-regexp-in-string "\\[[[:digit:]]+[[:alpha:]]+" "" completion))
         (completion-lines (split-string completion "\n" 't))
         ;; in a single completion the node REPL optionally prints type information in a comment
         (completion-lines (seq-remove (apply-partially #'string-prefix-p "//")
                                       completion-lines))
         (completion-tokens (seq-mapcat (lambda (x) (split-string x nil 't)) completion-lines))
         (trimmed-prompt (string-trim js-comint-prompt))
         (completion-res (seq-remove (lambda (x) (or (equal x prefix)
                                                     (equal x trimmed-prompt)
                                                     (equal x "...")))
                                     completion-tokens)))
    completion-res))

(defvar-local js-comint--completion-callbacks nil
  "List of pending callbacks.
Each should be a plist with last-prompt-start, input-string, type,
function.  See `js-comint--set-completion-callback'.")

(defvar-local js-comint--completion-buffer nil
  "Buffer for completion output.")

(defun js-comint--clear-completion-state ()
  "Clear stored partial completions."
  (when js-comint--completion-buffer
    (with-current-buffer js-comint--completion-buffer
      (erase-buffer))))

(defun js-comint--callback-active-p (callback)
  "Non-nil if CALLBACK should be used given current prompt location and input."
  (and
   (equal
    ;; marker for the prompt active when callback was created
    (plist-get callback :last-prompt-start)
    ;; start prompt marker, must be in comint buffer
    (car comint-last-prompt))
   ;; check input-string
   (or
    ;; "clear" callbacks always fire
    (equal 'clear (plist-get callback :type))
    ;; otherwise, if inputs are different, then not active
    (equal (js-comint--current-input) (plist-get callback :input-string)))))

(defun js-comint--async-output-filter (output)
  "Dispatches callbacks listening for comint OUTPUT."
  (cond
   ((null js-comint--completion-callbacks)
    output)
   ;; Assuming most recent callbacks are at head of the list
   ;; If the head is not active, then discard all in the list
   ((not (js-comint--callback-active-p (car js-comint--completion-callbacks)))
    (prog1 output
      (js-comint--clear-completion-state)
      (setq js-comint--completion-callbacks nil)))
   (t
    (prog1 ""
      ;; first write output to completion-buffer
      (unless (bufferp js-comint--completion-buffer)
        (setq js-comint--completion-buffer (generate-new-buffer " *js-comint completion*" t)))
      (with-current-buffer js-comint--completion-buffer
        (goto-char (point-max))
        (insert output))
      ;; call only the active ones, discard others
      (let ((active-callbacks (seq-filter
                                 #'js-comint--callback-active-p
                                 js-comint--completion-callbacks)))
        ;; Some callbacks may add further callbacks during their execution.
        ;; Re-add any pending callbacks to avoid overwriting new callbacks.
        (setq js-comint--completion-callbacks nil)
        (dolist (cb active-callbacks)
          ;; if the callback exits with non-nil or signals, remove it
          (unless (condition-case err
                      (funcall (plist-get cb :function))
                    (t (prog1 t
                         (message "Error in callback %s" (cdr err)))))
            (push cb js-comint--completion-callbacks))))
      (if js-comint--completion-callbacks
          (accept-process-output (js-comint-get-process) 0.4)
        ;; otherwise reset
        (js-comint--clear-completion-state))))))

(defun js-comint--completion-looking-back-p (regexp)
  "Call `looking-back' with REGEXP on `js-comint--completion-buffer'."
  (with-current-buffer js-comint--completion-buffer
    (goto-char (point-max))
    (looking-back regexp (line-beginning-position))))

(defun js-comint--set-completion-callback (callback type)
  "Add CALLBACK to listen for completion output.
TYPE is a symbol describing the callback, either \"clear\" or \"completion\"."
  (push `(:last-prompt-start
          ,(car comint-last-prompt)
          :input-string
          ,(js-comint--current-input)
          :type
          ,type
          :function
          ,callback)
        js-comint--completion-callbacks))

(defun js-comint--complete-substring (input-string)
  "Given a full line in INPUT-STRING return the substring to complete."
  (if-let ((match (string-match-p "[[:space:](\\[{;]\\([[:word:].]*\\)$" input-string)))
    (string-trim (substring-no-properties input-string (1+ match)))
    input-string))

(defun js-comint--get-completion-async (input-string callback)
  "Complete INPUT-STRING and register CALLBACK to recieve completion output."
  (js-comint--clear-completion-state)
  (js-comint--set-completion-callback
   ;; callback closure
   (let (tab-sent   ;; flag tracking repeat tabs: Array\t => \t
         check-sent ;; flag tracking whether check has been sent
         finished)  ;; flag tracking if 'callback' has been called, referred to by inner closures
     (lambda ()
       (cond
        ;; case: exact match to input-string in output
        ((and (not (string-empty-p input-string))
              (js-comint--completion-looking-back-p (regexp-quote input-string)))
         ;; Completions like "Array." need a second tab after the response
         (cond
          ((and (string-suffix-p "." input-string)
                (not tab-sent))
           (prog1 nil ;; do not remove callback
             (setq tab-sent 't)
             ;; When completing "Array." this will get a list of props
             (process-send-string (js-comint-get-process) "\t")
             ;; When the completion does not exist, e.g. "foo." there is no response to this tab
             ;; Instead, schedule a callback that produces a prompt, e.g. "> foo."
             ;; This might happen after completion is finished, either by other input or clearing,
             ;; so check to avoid adding garbage output.
             (run-at-time 1 nil (lambda () (unless (or finished (not js-comint--completion-callbacks))
                                             (process-send-string (js-comint-get-process) " \b"))))))
          ;; Output may sometimes be staggered "Arr|ay" so the completion appears stepwise
          ((not check-sent)
           (prog1 nil ;; do not remove callback
             (setq check-sent 't)
             ;; This "wiggles" the cursor making node repeat the prompt with current input
             (process-send-string (js-comint-get-process) " \b")))
          ;; Otherwise there was no match (after retry).
          ;; The wiggle should cause a prompt to be echoed, so this case likely does not occur.
          ;; Probably useful to keep for edge cases though.
          ('t
           (prog1 't ;; remove callback
             (unwind-protect
                 (funcall callback nil)
               (js-comint--clear-input-async))))))
        ;; case: found a control character (usually part of a prompt)
        ((or check-sent
             (js-comint--completion-looking-back-p "\\[[[:digit:]]+[AJG]$"))
         (setq finished 't)
         (let* ((completion-output (with-current-buffer js-comint--completion-buffer
                                     (buffer-string)))
                (completion-res (js-comint--process-completion-output
                                 completion-output
                                 input-string)))
           (unwind-protect
               (funcall callback completion-res)
             (js-comint--clear-input-async))
           't))
        ;; all other cases
        ('t
         ;; expect that the callback will be removed
         nil))))
   'completion)

  ;; Need to send 2x tabs to trigger completion when there is no input
  ;; 1st tab usually does common prefix
  (when (string-empty-p input-string)
    (process-send-string
     (js-comint-get-process)
     "\t"))

  (process-send-string
   (js-comint-get-process)
   (format "%s\t" input-string)))

(defun js-comint--clear-input-async ()
  "Clear input already sent to the REPL.
This is used specifically to remove input used to trigger completion."
  (js-comint--set-completion-callback
   (lambda ()
     ;; mask output until the REPL echoes a prompt
     (js-comint--completion-looking-back-p (concat js-comint-prompt "\\[[[:digit:]]+[AG]$")))
   'clear)

  (process-send-string
   (js-comint-get-process)
   ;; this is the symbol for "cut"
   ""))

(defun js-comint--current-input ()
  "Return current comint input relative to point.
Nil if point is before the current prompt."
  (let ((pmark (process-mark (js-comint-get-process))))
    (when (>= (point) (marker-position pmark))
	    (buffer-substring pmark (point)))))

(defun js-comint--should-complete ()
  "Non-nil if completion should be attempted on text before point."
  (let* ((parse (syntax-ppss))
         (string-or-comment (or (nth 3 parse) (nth 4 parse) (nth 7 parse))))
   (cond
    (string-or-comment
     nil)
    ((looking-back "\\." (line-beginning-position))
     't)
    ((looking-back "[[:punct:][:space:]]" (line-beginning-position))
     nil)
    (t
     't))))

;;;###autoload
(defun company-js-comint (command &optional arg &rest _ignored)
  "Wraps node REPL completion for company."
  (interactive (list 'interactive))
  (cl-case command
    ((interactive)
     (company-begin-backend 'company-js-comint))
    ((prefix)
     (when (equal major-mode 'js-comint-mode)
       (if (js-comint--should-complete)
           (js-comint--complete-substring (js-comint--current-input))
         'stop)))
    ((candidates)
     (cons :async (apply-partially #'js-comint--get-completion-async arg)))))

(with-eval-after-load 'company
  (cl-pushnew #'company-js-comint company-backends))

;; loosely follows cape-company-to-capf
(defun js-comint--capf ()
  "Convert `company-js-comint' function to a completion-at-point-function."
  (when-let ((_ (js-comint--should-complete))
             (initial-input (js-comint--complete-substring (js-comint--current-input))))
    (let* ((end (point))
           (beg (- end (length initial-input)))
           restore-props)
      (list beg end
            (completion-table-dynamic
             (lambda (input)
               (let ((cands 'js-comint--waiting))
                 (js-comint--get-completion-async input (lambda (arg) (setq cands arg)))
                 ;; Force synchronization, not interruptible! We use polling
                 ;; here and ignore pending input since we don't use
                 ;; `sit-for'. This is the same method used by Company itself.
                 (while (eq cands 'js-comint--waiting)
                   (sleep-for 0.01))
                 ;; The candidate string including text properties should be
                 ;; restored in the :exit-function
                 (setq restore-props cands)
                 (cons (apply-partially #'string-prefix-p input) cands)))
             t)
            :exclusive 'no
            :exit-function (lambda (x _status)
                             ;; Restore the candidate string including
                             ;; properties if restore-props is non-nil.  See
                             ;; the comment above.
                             (setq x (or (car (member x restore-props)) x))
                             nil)))))

;;;###autoload
(defun js-comint-save-setup ()
  "Save current setup to \".dir-locals.el\"."
  (interactive)
  (let* (sexp
         (root (read-directory-name "Where to create .dir-locals.el: "
                                    default-directory))
         (file (concat (file-name-as-directory root)
                       ".dir-locals.el")))
    (cond
     (js-comint-module-paths
      (setq sexp (list (list nil (cons 'js-comint-module-paths js-comint-module-paths))))
      (with-temp-buffer
        (insert (format "%S" sexp))
        (write-file file)
        (message "%s created." file)))
     (t
      (message "Nothing to save. `js-comint-module-paths' is empty.")))))

;;;###autoload
(defun js-comint-reset-repl ()
  "Kill existing REPL process if possible.
Create a new Javascript REPL process."
  (interactive)
  (when (js-comint-get-process)
    (process-send-string (js-comint-get-process) ".exit\n")
    ;; wait the process to be killed
    (sit-for 1))
  (js-comint-start-or-switch-to-repl))

(defun js-comint-filter-output (_string)
  "Filter extra escape sequences from last output."
  (let ((beg (or comint-last-output-start
                 (point-min-marker)))
        (end (process-mark (get-buffer-process (current-buffer)))))
    (save-excursion
      (goto-char beg)
      ;; Remove ansi escape sequences used in readline.js
      (while (re-search-forward js-comint-drop-regexp end t)
        (replace-match "")))))

(defun js-comint-get-buffer-name ()
  "Get repl buffer name."
  (format "*%s*" js-comint-buffer))

(defun js-comint-get-buffer ()
  "Get rpl buffer."
  (and js-comint-buffer
       (get-buffer (js-comint-get-buffer-name))))

;;;###autoload
(defun js-comint-clear ()
  "Clear the Javascript REPL."
  (interactive)
  (let* ((buf (js-comint-get-buffer) )
         (old-buf (current-buffer)))
    (save-excursion
      (cond
       ((buffer-live-p buf)
        (switch-to-buffer buf)
        (erase-buffer)
        (switch-to-buffer old-buf)
        (message "Javascript REPL cleared."))
       (t
        (error "Javascript REPL buffer doesn't exist!"))))))
(defalias 'js-clear 'js-comint-clear)


;;;###autoload
(defun js-comint-start-or-switch-to-repl ()
  "Start a new repl or switch to existing repl."
  (interactive)
  (if (js-comint-get-process)
      (pop-to-buffer (js-comint-get-buffer))
    (let* ((node-path (getenv "NODE_PATH"))
           ;; The path to a local node_modules
           (node-modules-path (and js-comint-set-env-when-startup
                                   (js-comint--suggest-module-path)))
           (all-paths-list (flatten-list (list node-path
                                               node-modules-path
                                               js-comint-module-paths)))
           (all-paths-list (seq-remove 'string-empty-p all-paths-list))
           (local-node-path (string-join all-paths-list (js-comint--path-sep)))
           (js-comint-code (format js-comint-code-format
                          (window-width)
                          js-comint-prompt
                          (or (getenv "NODE_REPL_MODE") "sloppy")))
           ;; NOTE: it's recommended not to use NODE_PATH
           (environment `("TERM=emacs"
                          "NODE_NO_READLINE=1"
                          ,(format "NODE_PATH=%s" local-node-path))))
      (pop-to-buffer
       (apply 'make-comint js-comint-buffer "env" nil
              `(,@environment ,js-comint-program-command ,@js-comint-program-arguments "-e" ,js-comint-code)))
      (js-comint-mode))))

;;;###autoload
(defun js-comint-repl (&optional cmd)
  "Start a NodeJS REPL process.
Optional CMD will override `js-comint-program-command' and
`js-comint-program-arguments', as well as any nvm setting.

When called interactively use a universal prefix to
set CMD."
  (interactive
   (when current-prefix-arg
     (list
      (read-string "Run js: "
                   (string-join
                           (cons js-comint-program-command
                                 js-comint-program-arguments)
                           " ")))))
  (if cmd
      (let ((cmd-parts (split-string cmd)))
        (setq js-comint-program-arguments (cdr cmd-parts)
              js-comint-program-command (car cmd-parts))
        (when js-nvm-current-version
          (message "nvm node version overridden, reset with M-x js-comint-select-node-version")
          (setq js-use-nvm nil)))
    (when (and js-use-nvm
               (not js-nvm-current-version))
      (js-comint-select-node-version)))

  (js-comint-start-or-switch-to-repl))

(defalias 'run-js 'js-comint-repl)

(defun js-comint-send-string (str)
  "Send STR to repl."
  (let ((lines (split-string str "\n")))
    (if (= (length lines) 1)
	(comint-send-string (js-comint-get-process)
			    (concat str "\n"))
      (comint-send-string (js-comint-get-process)
			  (concat ".editor" "\n" str "\n"))
      (process-send-eof (js-comint-get-process)))))

;;;###autoload
(defun js-comint-send-region ()
  "Send the current region to the inferior Javascript process.
If no region selected, you could manually input javascript expression."
  (interactive)
  (let* ((str (if (region-active-p)
                  (buffer-substring-no-properties (region-beginning) (region-end))
                (read-string "input js expression: "))))
    (js-comint-send-string str)))

;;;###autoload
(defalias 'js-send-region 'js-comint-send-region)

;;;###autoload
(defun js-comint-send-last-sexp ()
  "Send the previous sexp to the inferior Javascript process."
  (interactive)
  (let* ((b (save-excursion
              (backward-sexp)
              (move-beginning-of-line nil)
              (point)))
         (e (if (and (boundp 'evil-mode)
                     evil-mode
                     (eq evil-state 'normal))
                (+ 1 (point))
              (point)))
         (str (buffer-substring-no-properties b e)))
    (js-comint-send-string str)))

;;;###autoload
(defalias 'js-send-last-sexp 'js-comint-send-last-sexp)

;;;###autoload
(defun js-comint-send-buffer ()
  "Send the buffer to the inferior Javascript process."
  (interactive)
  (js-comint-send-string
   (buffer-substring-no-properties (point-min)
                                   (point-max))))

;;;###autoload
(defalias 'js-send-buffer 'js-comint-send-buffer)

;;;###autoload
(defun js-comint-load-file (file)
  "Load FILE into the javascript interpreter."
  (interactive "f")
  (let ((file (expand-file-name file)))
    (js-comint-repl)
    (comint-send-string (js-comint-get-process) (js-comint-guess-load-file-cmd file))))

;;;###autoload
(defalias 'js-load-file 'js-comint-load-file)

(defalias 'switch-to-js 'js-comint-start-or-switch-to-repl)

(defvar js-comint-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'js-comint-quit-or-cancel)
    map))

(defun js-comint-unload-function ()
  "Cleanup mode settings."
  (when company-backends
    (setq company-backends
          (delete #'company-js-comint company-backends))))

(defun js-comint--cleanup ()
  "Runs after comint buffer is killed."
  (when js-comint--completion-buffer
      (kill-buffer js-comint--completion-buffer)))

;;;###autoload
(define-derived-mode js-comint-mode comint-mode "Javascript REPL"
  :group 'js-comint
  :syntax-table js-mode-syntax-table
  (setq-local font-lock-defaults (list js--font-lock-keywords))
  ;; No echo
  (setq comint-process-echoes t)
  ;; Ignore duplicates
  (setq comint-input-ignoredups t)
  (add-hook 'comint-output-filter-functions 'js-comint-filter-output nil t)
  (add-hook 'comint-preoutput-filter-functions #'js-comint--async-output-filter nil t)
  (add-hook 'kill-buffer-hook #'js-comint--cleanup nil t)
  (unless (featurep 'company)
    (add-hook 'completion-at-point-functions #'js-comint--capf nil t))
  (process-put (js-comint-get-process)
               'adjust-window-size-function (lambda (_process _windows) ()))
  (use-local-map js-comint-mode-map)
  (ansi-color-for-comint-mode-on))

(provide 'js-comint)
;;; js-comint.el ends here
