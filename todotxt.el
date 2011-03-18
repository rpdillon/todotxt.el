;; Variables that are available for customization
(setq todotxt-file (expand-file-name "~/Dropbox/memex/todo.txt"))
(setq todotxt-buffer-name "*todotxt*")

;; Setup a major mode for todotxt
(define-derived-mode todotxt-mode text-mode "todotxt" "Major mode for working with todo.txt files. \\{todotxt-mode-map}"
  (setq buffer-read-only t))

;; Setup key map
(define-key todotxt-mode-map (kbd "l") 'todotxt-unhide-all)      ; (L)ist
(define-key todotxt-mode-map (kbd "i") 'todotxt-show-incomplete) ; list (I)ncomplete
(define-key todotxt-mode-map (kbd "c") 'todotxt-complete-toggle) ; (C)omplete item
(define-key todotxt-mode-map (kbd "a") 'todotxt-add-item)        ; (A)dd item
(define-key todotxt-mode-map (kbd "d") 'todotxt-delete-item)     ; (D)elete item
(define-key todotxt-mode-map (kbd "q") 'todotxt-bury)            ; (Q)uit
(define-key todotxt-mode-map (kbd "e") 'todotxt-edit-line)       ; (E)dit
(define-key todotxt-mode-map (kbd "/") 'todotxt-filter-for)      ; 
(define-key todotxt-mode-map (kbd "s") 'save-buffer)             ; (S)ave
(define-key todotxt-mode-map (kbd "n") 'next-line)               ; (N)ext
(define-key todotxt-mode-map (kbd "p") 'previous-line)           ; (P)revious

;; Utility functions

; Test whether or not the current line contains text that matches the provided regular expression
(defun current-line-re-match (re)
  (let ((line-number (line-number-at-pos)))
    (save-excursion
      (beginning-of-line)
      (if (re-search-forward re nil 't)
          (equal line-number (line-number-at-pos))
        nil))))

; Test whether or not the current line contains text that matches the provided string
(defun current-line-match (s)
  (let ((line-number (line-number-at-pos)))
    (save-excursion
      (beginning-of-line)
      (if (search-forward s nil 't)
          (equal line-number (line-number-at-pos))
        nil))))

; Returns whether or not the current line is "complete"
(defun complete-p ()
  (current-line-re-match "^x .*?$"))

; Hides the current line, returns 't if executed
(defun hide-line ()
  (beginning-of-line)
  (let ((beg (point)))
    (forward-line)
    (setq inhibit-read-only 't)
    (put-text-property beg (point) 'invisible 't)
    (setq inhibit-read-only nil)
    't))

; Returns whether or not the current line is empty
(defun line-empty-p ()
  (save-excursion
    (beginning-of-line)
    (let ((b (point)))
      (end-of-line)
      (equal (point) b))))

; Hides lines for which the provided predicate returns 't
(defun filter-out (predicate)
  (save-excursion
    (beginning-of-buffer)
    (while (progn
             (if (and (not (line-empty-p)) (funcall predicate))
                 (hide-line)
               (equal (forward-line) 0))))))

;; Externally visible functions
(defun todotxt ()
  (interactive)
  (let ((buf (find-file-noselect todotxt-file)))
    (if (equal (get-buffer-window buf) nil)
        (progn
          (let* ((height (nth 3 (window-edges)))
            (nheight (- height (/ height 3)))
            (win (split-window (selected-window) nheight)))
          (select-window win)
          (switch-to-buffer buf)
          (todotxt-mode))))
    (beginning-of-buffer)))

(defun todotxt-show-incomplete ()
  (interactive)
  (filter-out 'complete-p))

(defun todotxt-add-item (item)
  (interactive "sItem to add: ")
  (save-excursion
    (setq inhibit-read-only 't)
    (end-of-buffer)
    (insert item)
    (save-buffer)
    (setq inhibit-read-only nil)))

(defun todotxt-delete-item ()
  (interactive)
  (beginning-of-line)
  (setq inhibit-read-only 't)
  (kill-line)
  (kill-line)
  (setq inhibit-read-only nil))

(defun todotxt-bury ()
  (interactive)
  (bury-buffer)
  (delete-window))

(defun todotxt-unhide-all ()
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (let ((beg (point)))
      (end-of-buffer)
      (setq inhibit-read-only 't)
      (remove-text-properties beg (point) '(intangible nil invisible nil))
      (setq inhibit-read-only nil))))

(defun todotxt-filter-for (keyword)
  (interactive "sLook for lines with keyword: ")
  (save-excursion
    (beginning-of-buffer)
    (filter-out (lambda () (not (current-line-match keyword))))))

;todo
(defun todotxt-complete-toggle ()
  (interactive)
  (if (complete-p)
      (message "null")
    (message "non-null")))
  
(provide 'todotxt)
