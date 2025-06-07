;;; eudic-client.el --- Eudic API Request Client -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'plz)

(defvar eudic-api-host "https://api.frdic.com/api/open"
  "Base URL for Eudic API.")

(defvar eudic-api-key nil
  "API key for Eudic.
from htthttps://api.frdic.com/api/open/v1/studylist/categoryps://my.eudic.net/OpenAPI/Authorization")
(setq eudic-api-key (getenv "EUDIC_API_KEY"))

(cl-defun do-request (&key (method 'get) url params body)
  (let* (
         (query-string (if params (url-build-query-string params)))
         (complete-url (concat eudic-api-host url (if query-string (concat "?" query-string) "")))
         )
    (plz method complete-url
      :headers `(("authorization" . ,eudic-api-key) ("content-type" . "application/json; charset=utf-8"))
      :as 'json-read)))

(provide 'eudic-client)

;;; eudic-client.el ends here
