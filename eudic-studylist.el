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

(defun eudic--studylists ()
  (interactive)
  (let ((studylists (if eudic-studylists eudic-studylists (refresh-studylists))))
   (mapcan 'cdr studylists)))

(defun eudic-refresh-studylists ()
  "Refresh all LANGUAGE studylists cache."
  (interactive)
  (mapcar 'eudic--refresh-studylists (eudic--languages)))

(defun eudic--refresh-studylists (language)
  (let* (
         (response (eudic--do-request :url "/v1/studylist/category" :params `(("language" ,language)) :then 'eudic--json-response))
         (studylists (mapcar 'eudic--make-studylist (alist-get 'data response)))
         )
    (setf (alist-get language eudic-studylists nil nil 'equal) studylists)
    (message "%s" studylists)))

(provide 'eudic-studylist)
