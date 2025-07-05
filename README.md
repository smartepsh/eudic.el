# eudic.el

## Variables

- `eudic-api-key`
- `eudic-default-language`
- `eudic-default-studylist`

## Functions

- Studylists
  - `eudic-refresh-studylists`
  - `eudic-create-studylist`
  - `eudic-delete-studylist`
  - `eudic-add-word-to-studylist`

## with [go-translate](https://github.com/lorniu/go-translate)

```emacs-lisp
  (defun private/add-word-to-eudic (gt-translator)
    (let* ((word (car (oref gt-translator text))))
    (eudic-add-word-to-studylist word)))

  (gt-taker :langs '(en zh) :text 'word :then 'private/add-word-to-eudic)
```
