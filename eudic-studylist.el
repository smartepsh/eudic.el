;;; eudic-studylist.el --- Eudic StudyList -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'eudic-client)
(require 'eudic-utils)


(cl-defstruct eudic-studylist id language name add_time)

(cl-defun eudic/create-studylist (&key language name)
  (if (and (eudic/is--validate-language language)
           (eudic/is--validate-string name))
      (let* ((body `(("language" . ,language)
                       ("name" . ,name)))
             (response (eudic/do--request :method 'post :url "/v1/studylist/category" :body body))
             (studylist (alist-get 'data response)))
        (eudic/create--studylist (eudic/alist--take studylist '(id language name))))
    (error "Language must in en/fr/de/es, and the name must be a non-empty string.")))

(defun eudic/create--studylist (alist)
  "Create a new Eudic study list from ARGS."
  (make-eudic-studylist
   :id (alist-get 'id alist)
   :language (alist-get 'language alist)
   :name (alist-get 'name alist)
   :add_time (alist-get 'add_time alist)))

(defun eudic/list--studylists ()
  (let* ((response (eudic/do--request :url "/v1/studylist/category" :params '(("language" "en"))))
         (studylists (alist-get 'data response)))
    (mapcar 'eudic/create--studylist studylists)))

(provide 'eudic-studylist)
