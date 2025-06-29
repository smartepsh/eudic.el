;;; eudic-studylist.el --- Eudic StudyList -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'eudic-client)
(require 'eudic-utils)

(defvar eudic-studylists nil
  "Cached all eudic studylists.")

(cl-defstruct eudic-studylist id language name add_time)

(defun eudic--make-studylist (alist)
  "Create a new Eudic study list from ARGS."
  (make-eudic-studylist
   :id (alist-get 'id alist)
   :language (alist-get 'language alist)
   :name (alist-get 'name alist)
   :add_time (alist-get 'add_time alist)))

(defun eudic-list-studylists (&optional language)
  "List all studylists under the specific LANGUAGE.
The default language is retrieve from 'eudic-default-language'.
You can use \\C-\\u to select LANGUAGE manually."
  (interactive (list (eudic--read-language)))
  (eudic--list-studylists language))

(defun eudic--list-studylists (language)
  (let* (
         (response (eudic--do-request :url "/v1/studylist/category" :params `(("language" ,language)) :then 'eudic--json-response))
         (studylists (mapcar 'eudic--make-studylist (alist-get 'data response)))
         )
    (setf (alist-get language eudic-studylists nil nil 'equal) studylists)
    (message "%s" studylists)))

(provide 'eudic-studylist)
