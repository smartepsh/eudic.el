;;; eudic-utils.el --- Eudic Utils -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defvar eudic--languages '(
                           ("English" . en)
                           ("Deutsch" . de)
                           ("Español" . es)
                           ("Français" . fr))
  "Available languages supproted by Eudic.")

(defun eudic--language-display (lang)
  "Return the LANG display string."
  (car (rassoc (intern lang) eudic--languages)))

(defun eudic--languages ()
  "."
  (mapcar 'cdr eudic--languages))

(defun eudic--is-valid-name (string)
  "Check if STRING is a valid non-empty string."
  (and (stringp string) (not (string-empty-p string))))

(defun eudic/alist--take (alist keys)
  "Return a new alist containing only the entries from ALIST with keys in KEYS."
  (let ((result '()))
    (dolist (key keys)
      (when-let ((value (assoc key alist)))
        (push value result)))
    (nreverse result))
  )

(defun eudic--read-language ()
  "Select language from mini buffer or fallback to default."
  (if current-prefix-arg
      (let ((choice (completing-read "Select language: " eudic--languages)))
        (alist-get choice eudic--languages nil nil #'string=))
    eudic-default-language))

(provide 'eudic-utils)
