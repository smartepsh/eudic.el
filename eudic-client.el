;;; eudic-client.el --- Eudic API Request Client -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'plz)

(defcustom eudic-api-host "https://api.frdic.com/api/open"
  "Base URL for Eudic API."
  :group 'eudic)

(defcustom eudic-api-key nil
  "API key for Eudic.
Retrieve from https://my.eudic.net/OpenAPI/Authorization"
  :group 'eudic)

(cl-defun eudic--do-request (&key (method 'get) url params body (then '(lambda (x) x)) (async nil))
  (let* (
         (query-string (if params (url-build-query-string params)))
         (complete-url (concat eudic-api-host url (if query-string (concat "?" query-string) "")))
         )
    (if async
        (plz method complete-url
          :headers `(("authorization" . ,eudic-api-key) ("content-type" . "application/json; charset=utf-8"))
          :body (if body (json-encode body))
          :as 'response
          :then then
          )
      (funcall then
       (plz method complete-url
         :headers `(("authorization" . ,eudic-api-key) ("content-type" . "application/json; charset=utf-8"))
         :body (if body (json-encode body))
         :as 'response)))))

(defun eudic--json-response (plz-response)
  (json-parse-string (plz-response-body response) :object-type 'alist))

(provide 'eudic-client)

;;; eudic-client.el ends here
