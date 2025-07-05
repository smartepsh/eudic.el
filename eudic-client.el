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
                 :headers `(("Authorization" . ,eudic-api-key) ("content-type" . "application/json; charset=utf-8"))
                 :body (if body (json-encode body))
                 :as 'response
                 )))))

(defun eudic--delete-studylist-with-body (body)
  "Use url.el to send delete studylist request.  Because plz.el has some errors.
BODY is the request body built by studylist."
  (let* ((url (concat eudic-api-host "/v1/studylist/category"))
         (url-request-method "DELETE")
         (url-request-extra-headers
          `(("Authorization" . ,eudic-api-key)
            ("Content-Type" . "application/json")))
         (url-request-data
          (json-encode body))
         (buffer (url-retrieve-synchronously url))
         )(unwind-protect
         (with-current-buffer buffer
           (url-http-parse-response))  ; 返回状态码
         (when (buffer-live-p buffer)
           (kill-buffer buffer)))
          ))

(defun eudic--json-response (plz-response)
  (json-parse-string (plz-response-body plz-response) :object-type 'alist))

(provide 'eudic-client)
;;; eudic-client.el ends here
