;;; eudic-studylist.el --- Eudic StudyList -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'eudic-client)

(cl-defstruct eudic-studylist id language name add_time)

(defun eudic/create--studylist (alist)
  "Create a new Eudic study list from ARGS."
  (make-eudic-studylist
   :id (alist-get 'id alist)
   :language (alist-get 'language alist)
   :name (alist-get 'name alist)
   :add_time (alist-get 'add_time alist))
  )

(defun eudic/list--studylists ()
  (let* (
         (response (eudic/do--request :url "/v1/studylist/category" :params '(("language" "en"))))
         (studylists (alist-get 'data response))
         )
    (mapcar 'eudic/create--studylist studylists)))

(provide 'eudic-studylist)
