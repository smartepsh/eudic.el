;;; eudic-gt.el --- Open Eudic from gt -*- lexical-binding: t; -*-

;;; Commentary:

;; Optional integration with gt.  Use `eudic-gt-engine' with a `gt-taker'
;; configured with :pick nil and `eudic-gt-render' for silent output.
;; Lookup results stay in the Eudic application; no definitions are returned.

;;; Code:

(require 'cl-lib)
(require 'gt-core)
(require 'eudic)

(defclass eudic-gt-engine (gt-engine)
  ((tag :initform "Eudic")
   (delimit :initform nil)
   (cache :initarg :cache :initform nil)
   (popup :initarg :popup :initform t :type boolean
          :documentation "Non-nil opens the small window; nil opens the main window."))
  "Dispatch one lookup to Eudic without retrieving a translation.
Use :pick nil on the taker and a single target language.  The target
language is not passed to Eudic.  Each execution dispatches a new lookup.
When `eudic-activate' is nil, use :popup nil for AppleScript lookup.")

(cl-defmethod gt-execute ((engine eudic-gt-engine) task)
  "Open TASK's text using ENGINE and return an empty result placeholder."
  (let ((texts (oref task text))
        (translator (oref task translator)))
    (unless (and (listp texts) (= (length texts) 1))
      (user-error "Eudic requires one text item; use :pick nil and :delimit nil"))
    (when (and translator (> (length (oref translator target)) 2))
      (user-error "Eudic requires one target language; disable gt-polyglot-p"))
    (when (oref engine stream)
      (user-error "Eudic does not support streaming"))
    (funcall (if (oref engine popup)
                 #'eudic-lookup-in-popup
               #'eudic-lookup)
             (car texts))
    ;; gt rejects nil and requires one result for each input item.
    ;; This acknowledges dispatch, not a successful lookup in the app.
    (list "")))

(defun eudic-gt-output (tasks _translator)
  "Report errors from TASKS without displaying their placeholder results.
_TRANSLATOR is supplied by `gt-render'.  Only clear gt's progress message,
preserving other notices such as failures to save a word to a studylist.
This output function suppresses all results, including other engines'."
  (let ((err (cl-loop for task in tasks thereis (oref task err))))
    (cond (err (message "Eudic lookup failed: %s" err))
          ((equal (current-message) "Processing...") (message nil)))))

(defun eudic-gt-render ()
  "Return a standard `gt-render' configured for Eudic-only output.
Errors are reported in the echo area; no result buffer is opened."
  (gt-render :output #'eudic-gt-output))

(provide 'eudic-gt)
;;; eudic-gt.el ends here
