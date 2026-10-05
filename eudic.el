;;; eudic.el --- Eudic API integration -*- lexical-binding: t; -*-

;;; Commentary:

;; Official Website: https://my.eudic.net/OpenAPI/Doc_Index

;;; Code:

(require 'browse-url)
(require 'subr-x)
(require 'thingatpt)
(require 'url-util)

(defgroup eudic nil
  "Look up words and manage Eudic studylists."
  :group 'applications)

(defcustom eudic-default-language 'en
  "Default language for Eudic studylist operations."
  :type '(radio
          (const :tag "English (en)" en)
          (const :tag "Deutsch (de)" de)
          (const :tag "Español (es)" es)
          (const :tag "Français (fr)" fr)
          )
  :group 'eudic)

(defcustom eudic-auto-add-to-studylist nil
  "When non-nil, add looked-up words to the default studylist.
Requires `plz' and a configured `eudic-api-key'.  Words are added after
dispatching the lookup URL; the app cannot report lookup results back
to Emacs.  The request is asynchronous."
  :type 'boolean
  :group 'eudic)

(defcustom eudic-default-studylist-id "0"
  "Studylist ID used when automatically saving looked-up words.
The ID \"0\" refers to Eudic's built-in default studylist.
The language is selected by `eudic-default-language'."
  :type 'string
  :group 'eudic)

(defcustom eudic-open-url-function #'browse-url-default-browser
  "Function used to open an Eudic URL through the operating system.
It must accept one URL argument.  The default bypasses any custom
`browse-url-browser-function', such as an Emacs-internal browser."
  :type 'function
  :group 'eudic)

(autoload 'eudic-refresh-studylists "eudic-studylist" nil t)
(autoload 'eudic-create-studylist "eudic-studylist" nil t)
(autoload 'eudic-delete-studylist "eudic-studylist" nil t)
(autoload 'eudic-add-word-to-studylist "eudic-studylist" nil t)

(autoload 'eudic--auto-add-word-to-studylist "eudic-studylist")

(defun eudic--read-word ()
  "Use the active region or word at point, or prompt for a word."
  (or (and (use-region-p)
           (buffer-substring-no-properties (region-beginning) (region-end)))
      (thing-at-point 'word t)
      (read-string "Look up word: ")))

(defun eudic--lookup (word prefix)
  "Look up WORD using URL PREFIX and optionally save it."
  (unless (and (stringp word) (not (string-empty-p (string-trim word))))
    (user-error "Please provide a non-empty word"))
  (setq word (string-trim word))
  (funcall eudic-open-url-function
           (concat prefix (url-hexify-string word)))
  (when eudic-auto-add-to-studylist
    (condition-case err
        (eudic--auto-add-word-to-studylist word)
      (error
       (message "Eudic lookup opened; could not save word: %s"
                (error-message-string err))))))

;;;###autoload
(defun eudic-lookup (word)
  "Look up WORD in Eudic using eudic://dict/.
Interactively use the active region, then the word at point, or prompt.
When `eudic-auto-add-to-studylist' is non-nil, also save WORD."
  (interactive (list (eudic--read-word)))
  (eudic--lookup word "eudic://dict/"))

;;;###autoload
(defun eudic-lookup-in-popup (word)
  "Look up WORD in Eudic's small window using eudic://lp-dict/.
Interactively use the active region, then the word at point, or prompt.
When `eudic-auto-add-to-studylist' is non-nil, also save WORD."
  (interactive (list (eudic--read-word)))
  (eudic--lookup word "eudic://lp-dict/"))

(provide 'eudic)
;;; eudic.el ends here
