;;; eudic.el --- Eudic API integration -*- lexical-binding: t; -*-

;;; Commentary:

;; Official Website: https://my.eudic.net/OpenAPI/Doc_Index

(defcustom eudic-default-language 'en
  "Default Language for Eudic. Defualt is en"
  :type '(radio
          (const :tag "English (en)" 'en)
          (const :tag "Deutsch (de)" 'de)
          (const :tag "Español (es)" 'es)
          (const :tag "Français (fr)" 'fr)
          )
  :group 'eudic)

(provide 'eudic)
;;; eudic.el ends here
