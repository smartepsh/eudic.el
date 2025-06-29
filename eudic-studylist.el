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
  (let ((studylists (if eudic-studylists eudic-studylists (eudic-refresh-studylists) eudic-studylists)))
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

(defun eudic-delete-studylist ()
  (interactive)
  (eudic--delete-studylist (eudic--select-studylist-from-mini-buffer)))

(defun eudic--delete-studylist (studylist)
  (let* ((status-code (eudic--delete-studylist-with-body `(("id" . ,(eudic-studylist-id studylist))
                                                           ("language" . ,(eudic-studylist-language studylist))
                                                           ("name" . ,(eudic-studylist-name studylist))))))
    (message "%s" status-code)
    (if (= status-code 204)
        (message "success")
      (message "failed")
      )
    ))

;; (defun eudic--delete-studylist (studylist)
;;   (let* ((response (eudic--do-request
;;                     :method 'delete
;;                     :url "/v1/studylist/category"
;;                     :body `(("id" . ,(eudic-studylist-id studylist))
;;                             ("language" . ,(eudic-studylist-language studylist))
;;                             ("name" . ,(eudic-studylist-name studylist))))))
;;     )
;;   )

(defun eudic--studylist-identity-string (studylist)
  (concat (eudic--language-display (eudic-studylist-language studylist)) "/" (eudic-studylist-id studylist) "/" (eudic-studylist-name studylist)))

(defun eudic--select-studylist-from-mini-buffer ()
  (let* ((studylists-alist (mapcar (lambda (studylist) (cons (eudic--studylist-identity-string studylist) studylist)) (eudic--studylists)))
         (choice (completing-read "Select studylist: " studylists-alist)))
    (alist-get choice studylists-alist nil nil #'string=)))

(provide 'eudic-studylist)
