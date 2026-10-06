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
dispatching the lookup URL, or after the background AppleScript succeeds.
The app does not return definitions to Emacs.  Saving is asynchronous."
  :type 'boolean
  :group 'eudic)

(defcustom eudic-default-studylist-id "0"
  "Studylist ID used when automatically saving looked-up words.
The ID \"0\" refers to Eudic's built-in default studylist.
The language is selected by `eudic-default-language'."
  :type 'string
  :group 'eudic)

(defcustom eudic-activate t
  "Whether to use the normal URL opener, which may activate Eudic.
When non-nil, dispatch through `eudic-open-url-function'.
When nil, query the main window asynchronously via AppleScript without
an activate command.  This requires macOS and the com.eusoft.eudic app.
LightPeek lookup is not supported in this mode.  Focus retention has been
verified with an already running Eudic; first-launch behavior may differ."
  :type 'boolean
  :group 'eudic)

(defcustom eudic-open-url-function #'browse-url-default-browser
  "Function used to open an Eudic URL through the operating system.
It must accept one URL argument.  The default bypasses any custom
`browse-url-browser-function', such as an Emacs-internal browser.
Used only when `eudic-activate' is non-nil."
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

(defconst eudic--lookup-script
  "on run argv
  with timeout of 10 seconds
    tell application id \"com.eusoft.eudic\"
      show dic with word (item 1 of argv)
    end tell
  end timeout
end run"
  "AppleScript for main-window lookup without requesting activation.
Pass the query as an argument, never interpolate it into the script.")

(defun eudic--save-after-lookup (word)
  "Save WORD, reporting any setup error without failing the lookup."
  (condition-case err
      (eudic--auto-add-word-to-studylist word)
    (error
     (message "Eudic lookup opened; could not save word: %s"
              (error-message-string err)))))

(defun eudic--lookup-in-background (word on-success)
  "Query WORD asynchronously without activation, then call ON-SUCCESS.
Report script errors in the echo area and do not call ON-SUCCESS on failure."
  (unless (eq system-type 'darwin)
    (user-error "Non-activating Eudic lookup requires macOS"))
  (let ((program (or (executable-find "osascript")
                     (user-error "Cannot find osascript for Eudic lookup")))
        (output (generate-new-buffer " *eudic-osascript*")))
    (condition-case err
        (make-process
         :name "eudic-lookup"
         :buffer output
         :command (list program "-e" eudic--lookup-script "--" word)
         :connection-type 'pipe
         :coding 'utf-8-unix
         :noquery t
         :sentinel
         (lambda (process event)
           (when (and (memq (process-status process) '(exit signal))
                      (buffer-live-p output))
             (let ((detail (with-current-buffer output
                             (string-trim (buffer-string)))))
               (kill-buffer output)
               (if (and (eq (process-status process) 'exit)
                        (zerop (process-exit-status process)))
                   (when on-success (funcall on-success))
                 (message "Eudic lookup failed: %s"
                          (if (string-empty-p detail)
                              (string-trim event)
                            detail)))))))
      (error
       (kill-buffer output)
       (signal (car err) (cdr err))))))

(defun eudic--lookup (word prefix)
  "Look up WORD using URL PREFIX or AppleScript, and optionally save it."
  (unless (and (stringp word) (not (string-empty-p (string-trim word))))
    (user-error "Please provide a non-empty word"))
  (setq word (string-trim word))
  (let ((on-success (when eudic-auto-add-to-studylist
                      (lambda () (eudic--save-after-lookup word)))))
    (if eudic-activate
        (progn
          (funcall eudic-open-url-function
                   (concat prefix (url-hexify-string word)))
          (when on-success (funcall on-success)))
      (unless (equal prefix "eudic://dict/")
        (user-error "LightPeek requires eudic-activate to be non-nil; use the main window instead"))
      (eudic--lookup-in-background word on-success))))

;;;###autoload
(defun eudic-lookup (word)
  "Look up WORD in Eudic's main window.
Use eudic://dict/ or AppleScript according to `eudic-activate'.
Interactively use the active region, then the word at point, or prompt.
When `eudic-auto-add-to-studylist' is non-nil, also save WORD."
  (interactive (list (eudic--read-word)))
  (eudic--lookup word "eudic://dict/"))

;;;###autoload
(defun eudic-lookup-in-popup (word)
  "Look up WORD in Eudic's small window using eudic://lp-dict/.
Requires `eudic-activate' to be non-nil.
Interactively use the active region, then the word at point, or prompt.
When `eudic-auto-add-to-studylist' is non-nil, also save WORD."
  (interactive (list (eudic--read-word)))
  (eudic--lookup word "eudic://lp-dict/"))

(provide 'eudic)
;;; eudic.el ends here
