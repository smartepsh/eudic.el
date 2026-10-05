;;; eudic-gt-test.el --- gt integration tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'eudic-gt)

(defun eudic-gt-test--translator (texts &optional main-window langs)
  "Build a translator for TEXTS, MAIN-WINDOW and LANGS."
  (gt-translator
   :taker (gt-taker :text (lambda () texts) :pick nil :prompt nil
                    :langs (or langs '(en zh)))
   :engines (eudic-gt-engine :popup (not main-window))
   :render (eudic-gt-render)))

(defun eudic-gt-test--run (translator)
  "Run TRANSLATOR through gt's real lifecycle, waiting up to two seconds."
  (gt-start translator)
  (let ((deadline (+ (float-time) 2)))
    (while (and (< (oref translator state) 3) (< (float-time) deadline))
      (accept-process-output nil 0.01)))
  (should (= (oref translator state) 3))
  (oref translator tasks))

(ert-deftest eudic-gt-dispatch-completes-and-repeats ()
  (let ((gt-polyglot-p nil)
        (eudic-auto-add-to-studylist nil)
        urls)
    (let ((eudic-open-url-function (lambda (url) (push url urls))))
      (dolist (main '(nil t))
        (let ((translator (eudic-gt-test--translator '(" café 中文 ") main)))
          (dotimes (_ 2)
            (let ((tasks (eudic-gt-test--run translator)))
              (should (= (length tasks) 1))
              (should-not (oref (car tasks) err))
              (should (equal (oref (car tasks) res) '(""))))))))
    (should (equal (nreverse urls)
                   '("eudic://lp-dict/caf%C3%A9%20%E4%B8%AD%E6%96%87"
                     "eudic://lp-dict/caf%C3%A9%20%E4%B8%AD%E6%96%87"
                     "eudic://dict/caf%C3%A9%20%E4%B8%AD%E6%96%87"
                     "eudic://dict/caf%C3%A9%20%E4%B8%AD%E6%96%87")))))

(ert-deftest eudic-gt-rejects-multiple-parts-and-blank-text ()
  (let ((gt-polyglot-p nil)
        (eudic-open-url-function (lambda (_) (ert-fail "Unexpected lookup"))))
    (dolist (texts '(("one" "two") (" \t ")))
      (let ((task (car (eudic-gt-test--run (eudic-gt-test--translator texts)))))
        (should (oref task err))
        (should-not (oref task res))))))

(ert-deftest eudic-gt-rejects-multiple-targets ()
  (let ((gt-polyglot-p t)
        (eudic-open-url-function (lambda (_) (ert-fail "Unexpected lookup"))))
    (let ((tasks (eudic-gt-test--run
                  (eudic-gt-test--translator '("hello") nil '(en zh ja)))))
      (should (= (length tasks) 2))
      (dolist (task tasks)
        (should (string-match-p "one target language" (oref task err)))))))

(ert-deftest eudic-gt-open-failure-finishes-with-visible-error ()
  (let ((gt-polyglot-p nil)
        (eudic-auto-add-to-studylist nil)
        (eudic-open-url-function (lambda (_) (error "Cannot open URL")))
        messages)
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args)
                 (when fmt (push (apply #'format fmt args) messages)))))
      (let ((task (car (eudic-gt-test--run
                       (eudic-gt-test--translator '("hello"))))))
        (should (oref task err))
        (should-not (oref task res))))
    (should (cl-some (lambda (s)
                       (string-match-p "Eudic lookup failed:.*Cannot open URL" s))
                     messages))))

(ert-deftest eudic-gt-output-clears-progress-but-preserves-notices ()
  (let ((task (gt-task :text '("hello")))
        calls)
    (cl-letf (((symbol-function 'message)
               (lambda (&rest args) (push args calls))))
      (cl-letf (((symbol-function 'current-message) (lambda () "Processing...")))
        (gt-output (eudic-gt-render) (let ((tr (gt-translator)))
                                      (oset tr tasks (list task))
                                      tr)))
      (should (equal calls '((nil))))
      (setq calls nil)
      (cl-letf (((symbol-function 'current-message)
                 (lambda () "Eudic lookup opened; could not save word")))
        (eudic-gt-output (list task) nil))
      (should-not calls))))

(ert-deftest eudic-gt-preserves-auto-add-behavior ()
  (let ((gt-polyglot-p nil)
        (eudic-auto-add-to-studylist t)
        events)
    (let ((eudic-open-url-function (lambda (_) (push 'open events))))
      (cl-letf (((symbol-function 'eudic-add-word-to-studylist)
                 (lambda (word) (push word events))))
        (eudic-gt-test--run (eudic-gt-test--translator '(" hello ")))))
    (should (equal (nreverse events) '(open "hello")))))

(provide 'eudic-gt-test)
;;; eudic-gt-test.el ends here
