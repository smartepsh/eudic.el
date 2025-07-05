;;; eudic-studylist.el --- Eudic StudyList -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'eudic-client)
(require 'eudic-utils)

(defvar eudic-studylists nil
  "Cached all eudic studylists.")

(defcustom eudic-default-studylist nil
  "Default studylist to use."
  :group 'eudic)

(cl-defstruct eudic-studylist id language name add_time)

(defun eudic--make-studylist (alist)
  "Create a new Eudic study list from ARGS."
  (make-eudic-studylist
   :id (alist-get 'id alist)
   :language (alist-get 'language alist)
   :name (alist-get 'name alist)
   :add_time (alist-get 'add_time alist)))

(defun eudic--studylists ()
  (let ((studylists (if eudic-studylists
                        eudic-studylists
                      (eudic-refresh-studylists)
                      eudic-studylists)))
    (apply #'append (mapcar #'cdr studylists))))

(defun eudic-refresh-studylists ()
  "Refresh all LANGUAGE studylists cache."
  (interactive)
  (setq eudic-studylists nil)
  (mapcar 'eudic--refresh-studylists (eudic--languages))
  (message "Eudic studylists refreshed."))

(defun eudic--refresh-studylists (language)
  (let* (
         (response (eudic--do-request :url "/v1/studylist/category" :params `(("language" ,language)) :then 'eudic--json-response))
         (studylists (mapcar 'eudic--make-studylist (alist-get 'data response)))
         )
    (setf (alist-get language eudic-studylists nil nil 'equal) studylists)))

(defun eudic-delete-studylist ()
  (interactive)
  (eudic--delete-studylist (eudic--select-studylist-from-mini-buffer)))

(defun eudic--delete-studylist (studylist)
  (let* ((status-code (eudic--delete-studylist-with-body `(("id" . ,(eudic-studylist-id studylist))
                                                           ("language" . ,(eudic-studylist-language studylist))
                                                           ("name" . ,(eudic-studylist-name studylist))))))
    (if (= status-code 204)
        (progn
          (eudic--remove-studylist-from-cache studylist)
          (message "Remove succuess: %s" (eudic--studylist-identity-string studylist)))
      (message "Remove failed."))))

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

(defun eudic-create-studylist (name &optional language)
  "Create new studylist with NAME and LANGUAGE."
  (interactive (list (read-string "New Studylist Name: ") (eudic--read-language)))
  (eudic--create-studylist :language language :name name))

(cl-defun eudic--create-studylist (&key language name)
  (if (eudic--is-valid-name name)
      (let* ((body `(("language" . ,language)
                     ("name" . ,name)))
             (response (eudic--do-request :method 'post :url "/v1/studylist/category" :body body :then 'eudic--json-response))
             (studylist (eudic--make-studylist (alist-get 'data response))))
        (eudic--add-studylist-to-cache studylist)
        (message "Success: %s" (eudic--studylist-identity-string studylist))
        )
    (error "Name must be a non-empty string.")))

(defun eudic--add-studylist-to-cache (studylist)
  "Add STUDYLIST to cache."
  (let* ((language (intern (eudic-studylist-language studylist)))
         (studylists (alist-get language eudic-studylists nil nil 'equal))
         (new-studylists (cons studylist studylists)))
    (setf (alist-get language eudic-studylists nil nil 'equal) new-studylists)))

(defun eudic--remove-studylist-from-cache (studylist)
  "Remove STUDYLIST from cache."
  (let* ((language (intern (eudic-studylist-language studylist)))
         (studylists (alist-get language eudic-studylists nil nil 'equal))
         (new-studylists (remove studylist studylists)))
    (setf (alist-get language eudic-studylists nil nil 'equal) new-studylists)))

(defun eudic-add-word-to-studylist (&optional word studylist)
  "Add WORD to STUDYLIST. If STUDYLIST is nil, prompt for one. Use `eudic-default-studylist' if available."
  (interactive)
  (let* ((studylist (or studylist (or eudic-default-studylist (eudic--select-studylist-from-mini-buffer))))
         (word (or word (read-string "New Word: "))))
    (eudic--add-word-to-studylist word studylist)))

(defun eudic--add-word-to-studylist (word studylist)
  "Add WORD to STUDYLIST."
  (let* ((body `((id . ,(eudic-studylist-id studylist))
                 (language . ,(eudic-studylist-language studylist))
                 (category . ,(eudic-studylist-id studylist))
                 (words . ,(list word))))
         (response (eudic--do-request :method 'post :url "/v1/studylist/words" :body body :then 'eudic--json-response)))
    (message "Add word %s to %s successfully." word (eudic--studylist-identity-string studylist))))

(provide 'eudic-studylist)
;; eudic-studylist.el ends here
