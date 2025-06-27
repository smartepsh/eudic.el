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

(cl-defun eudic--do-request (&key (method 'get) url params body)
  (let* (
         (query-string (if params (url-build-query-string params)))
         (complete-url (concat eudic-api-host url (if query-string (concat "?" query-string) "")))
         )
    (plz method complete-url
      :body (if body (json-encode body))
      :headers `(("authorization" . ,eudic-api-key) ("content-type" . "application/json; charset=utf-8"))
      :body (if body (json-encode body))
      :as 'response)))

(provide 'eudic-client)

;;; eudic-client.el ends here
