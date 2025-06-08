;;; eudic-utils.el --- Eudic Utils -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defun eudic/is--validate-language (language)
  "Check if LANGUAGE is a valid Eudic language code."
  (member language '("en" "fr" "de" "es")))

(defun eudic/is--validate-string (string)
  "Check if STRING is a valid non-empty string."
  (and (stringp string) (not (string-empty-p string)))
  )

(defun eudic/alist--take (alist keys)
  "Return a new alist containing only the entries from ALIST with keys in KEYS."
  (let ((result '()))
    (dolist (key keys)
      (when-let ((value (assoc key alist)))
        (push value result)))
    (nreverse result))
  )

(provide 'eudic-utils)
