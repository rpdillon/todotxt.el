
;; Variables that are available for customization
(setq todotxt-file (expand-file-name "~/Dropbox/memex/todo.txt"))
(setq todotxt-buffer-name "*todotxt*")

;; Setup a major mode for todotxt
(define-derived-mode todotxt-mode text-mode "todotxt" "Major mode for working with todo.txt files. \\{todotxt-mode-map}"
  (setq buffer-read-only t))

;; Setup key map
(define-key todotxt-mode-map (kbd "l") 'todotxt-show-all)        ; (L)ist
(define-key todotxt-mode-map (kbd "I") 'todotxt-show-incomplete) ; list (I)ncomplete
(define-key todotxt-mode-map (kbd "c") 'todotxt-complete-toggle) ; (C)omplete item
(define-key todotxt-mode-map (kbd "a") 'todotxt-add-item)        ; (A)dd item
(define-key todotxt-mode-map (kbd "q") 'todotxt-bury)            ; (Q)uit
(define-key todotxt-mode-map (kbd "n") 'next-line)               ; (N)ext
(define-key todotxt-mode-map (kbd "p") 'previous-line)           ; (P)revious
(define-key todotxt-mode-map (kbd "/") 'todotxt-filter-for)      ; Same as Org =)

;; Utility functions
(defun current-line-re-match (re)
  (let ((line-number (line-number-at-pos)))
    (save-excursion
      (beginning-of-line)
      (if (re-search-forward re nil 't)
          (equal line-number (line-number-at-pos))
        nil))))

(defun current-line-match (s)
  (let ((line-number (line-number-at-pos)))
    (save-excursion
      (beginning-of-line)
      (if (search-forward s nil 't)
          (equal line-number (line-number-at-pos))
        nil))))

(defun complete-p ()
  (current-line-re-match "^x .*?$"))

(defun filter-out (predicate)
  (todotxt-show-all)
  (beginning-of-buffer)
  (save-excursion
    (while (progn
             (while (funcall predicate)
               (progn
                 (setq inhibit-read-only 't)
                 (kill-line)
                 (kill-line)
                 (setq inhibit-read-only nil)))
             (equal (forward-line) 0)))))

(defun hide-line ()
  (save-excursion
    (beginning-of-line)
    (let ((beg (point)))
      (forward-line)
      (put-text-property beg (point) 'intangible 't)
      (put-text-property beg (point) 'invisible 't))))


;; Externally visible functions
(defun todotxt-show-all ()
  (interactive)
  (let ((buf (get-buffer-create todotxt-buffer-name)))
    (if (equal (get-buffer-window buf) nil)
        (progn
          (let* ((height (nth 3 (window-edges)))
            (nheight (- height (/ height 3)))
            (win (split-window (selected-window) nheight)))
          (select-window win)
          (switch-to-buffer buf)
          (todotxt-mode))))
    (setq inhibit-read-only 't)
    (erase-buffer)
    (insert-file-contents todotxt-file)
    (setq inhibit-read-only nil)
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
    (setq inhibit-read-only nil)))

(defun todotxt-bury ()
  (interactive)
  (if (equal todotxt-buffer-name (buffer-name))
      (progn
        (bury-buffer)
        (delete-window))))

; Some way to stack filters?
(defun todotxt-filter-for (keyword)
  (interactive "sFilter keyword: ")
  (save-excursion
    (beginning-of-buffer)
    (filter-out (lambda () (not (current-line-match keyword))))))

(defun todotxt-complete-toggle ()
  (interactive)
  (if (complete-p)
      (message "null")
    (message "non-null")))
  
(provide 'todotxt)
