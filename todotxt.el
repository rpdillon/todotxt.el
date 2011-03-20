;; todotxt.el -- A major mode for editing todo.txt files

;; Filename: todotxt.el

;; Description: A major mode for editing todo.txt files
;; Author: Rick Dillon <rpdillon@etherplex.org>
;; Copyright (C) 2011, Rick Dillon, all rights reserved.
;; Created: 14 March 2011
;; Version: 0.1
;; URL: https://github.com/rpdillon/todotxt.el
;; Keywords: todo.txt, todotxt, todotxt.el
;; Compatibility: GNU Emacs 22 ~ 23
;;

;; This file is NOT part of GNU Emacs

;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;; Commentary:
;; This file provides a Emacs interface to the todo.txt file format
;; use by Gina Trapani's Todo.txt command-line tool
;; (http://todotxt.com/) and Android application
;; (https://github.com/ginatrapani/todo.txt-touch).
;;
;; Setup:
;;  - Put todotxt.el somewhere on your Emacs path
;;  - Load todotxt using (require 'todotxt) in you .emacs (or other initialization) file
;;  - Customize the variable 'todotxt-file' with the location of your todo.txt file.
;;  - View the file with M-x todotxt
;;  - Bind 'todotxt' to some accelerator like C-x t: (global-set-key (kbd "C-x t") 'todotxt)
;;
;; Usage:
;;  - Navigate up and down with 'n' and 'p'
;;  - Toggle completion of an item with 'c'
;;  - Add a new item with 'a'
;;  - Tag the current item with 't' (use tab completion as necessary)
;;  - Edit the current item with 'e'
;;  - Show only incomplete items with 'i'
;;  - Filter for any keyword or tag with '/'

;; Variables that are available for customization
(defcustom todotxt-file (expand-file-name "~/todo.txt")
       "The location of your todo.txt file."
       :type 'string
       :require 'todotxt
       :group 'todotxt)

(setq tags-regexp "\[+|@][^[:space:]]*") ; Used to find keywords for completion
(setq projects-regexp "\+[^[:space:]]*")
(setq contexts-regexp "\@[^[:space:]]*")
(setq complete-regexp "^x .*?$")

;; Font Lock and Faces
(defface todotxt-complete-face '(
  (t (:strike-through t)))
  "Todotxt face used for completed task."
  :group 'todotxt-highlighting-faces)

(defvar todotxt-complete-face 'todotxt-complete-face
  "Todotxt mode face used for completed task.")

(setq todotxt-highlight-regexps
      `((,projects-regexp 0 font-lock-variable-name-face t)
        (,contexts-regexp 0 font-lock-keyword-face t)
        (,complete-regexp 0 todotxt-complete-face t)))

;; Setup a major mode for todotxt
(define-derived-mode todotxt-mode text-mode "todotxt" "Major mode for working with todo.txt files. \\{todotxt-mode-map}"
  (setq font-lock-defaults '(todotxt-highlight-regexps))
  (setq buffer-read-only t))

;; Setup key map
(define-key todotxt-mode-map (kbd "l") 'todotxt-unhide-all)      ; (L)ist
(define-key todotxt-mode-map (kbd "i") 'todotxt-show-incomplete) ; list (I)ncomplete
(define-key todotxt-mode-map (kbd "c") 'todotxt-complete-toggle) ; (C)omplete item
(define-key todotxt-mode-map (kbd "a") 'todotxt-add-item)        ; (A)dd item
(define-key todotxt-mode-map (kbd "q") 'todotxt-bury)            ; (Q)uit
(define-key todotxt-mode-map (kbd "r") 'todotxt-prioritize)      ; P(r)ioritize
(define-key todotxt-mode-map (kbd "P") 'todotxt-purge)           ; (P)urge completed items
(define-key todotxt-mode-map (kbd "e") 'todotxt-edit-item)       ; (E)dit item
(define-key todotxt-mode-map (kbd "t") 'todotxt-tag-item)        ; (T)ag item
(define-key todotxt-mode-map (kbd "/") 'todotxt-filter-for)      ; 
(define-key todotxt-mode-map (kbd "s") 'save-buffer)             ; (S)ave
(define-key todotxt-mode-map (kbd "n") 'next-line)               ; (N)ext
(define-key todotxt-mode-map (kbd "p") 'previous-line)           ; (P)revious

;; Utility functions


(defun todotxt-current-line-re-match (re)
  "Test whether or not the current line contains text that matches the provided regular expression"
  (let ((line-number (line-number-at-pos)))
    (save-excursion
      (beginning-of-line)
      (if (re-search-forward re nil 't)
          (equal line-number (line-number-at-pos))
        nil))))


(defun todotxt-current-line-match (s)
  "Test whether or not the current line contains text that matches the provided string"
  (let ((line-number (line-number-at-pos)))
    (save-excursion
      (beginning-of-line)
      (if (search-forward s nil 't)
          (equal line-number (line-number-at-pos))
        nil))))

(defun todotxt-complete-p ()
  "Returns whether or not the current line is 'complete'. Used as part of a redefined filter for showing incomplete items only"
  (todotxt-current-line-re-match complete-regexp))

(defun todotxt-has-priority-p ()
  (todotxt-current-line-re-match "^\([A-Z]\) .*?$"))


(defun todotxt-hide-line ()
  "Hides the current line, returns 't"
  (beginning-of-line)
  (let ((beg (point)))
    (forward-line)
    (setq inhibit-read-only 't)
    (put-text-property beg (point) 'invisible 't)
    (setq inhibit-read-only nil)
    't))


(defun todotxt-line-empty-p ()
  "Returns whether or not the current line is empty"
  (save-excursion
    (beginning-of-line)
    (let ((b (point)))
      (end-of-line)
      (equal (point) b))))


(defun todotxt-filter-out (predicate)
  "Hides lines for which the provided predicate returns 't"
  (save-excursion
    (goto-char (point-min))
    (while (progn
             (if (and (not (todotxt-line-empty-p)) (funcall predicate))
                 (todotxt-hide-line)
               (equal (forward-line) 0))))))

(defun todotxt-get-tag-completion-list-from-string (string)
  "Search the buffer for tags (strings beginning with either '@' or '+') and return a list of them."
  (save-excursion
    (let ((completion-list '())
          (start-index 0))
      (while (string-match tags-regexp string start-index)
        (let ((tag (match-string-no-properties 0 string)))
          (if (not (member tag completion-list))
              (progn
                (setq completion-list (cons tag completion-list))))
          (setq start-index (match-end 0))))
      completion-list)))

(defun todotxt-get-current-line-as-string ()
  "Return the text of the line in which the point currently resides."
  (save-excursion
    (beginning-of-line)
    (let ((beg (point)))
      (end-of-line)
      (buffer-substring beg (point)))))

;;; Externally visible functions
(defun todotxt ()
  "Open the todo.txt buffer.  If one already exists, bring it to the front and focus it.  Otherwise, create one and load the data from 'todotxt-file'."
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
    (goto-char (point-min))))

(defun todotxt-show-incomplete ()
  "Filter out complete items from the todo list."
  (interactive)
  (todotxt-filter-out 'todotxt-complete-p))

(defun todotxt-add-item (item)
  "Prompt for an item to add to the todo list and append it to the file, saving afterwards."
  (interactive "sItem to add: ")
  (save-excursion
    (setq inhibit-read-only 't)
    (goto-char (point-max))
    (insert item)
    (save-buffer)
    (setq inhibit-read-only nil)))

(defun todotxt-prioritize ()
  (interactive)
  (let ((priority (read-from-minibuffer "Priority: ")))
    (save-excursion
      (setq inhibit-read-only 't)
      (if (todotxt-has-priority-p)
          (progn
            (beginning-of-line)
            (delete-char 4)))
      (if (not (equal priority ""))
          (progn
            (beginning-of-line)
            (insert (concat "(" priority ") "))
            (save-buffer)
            (setq inhibit-read-only nil))))))

(defun todotxt-edit-item ()
  (interactive)
  (save-excursion
    (let ((new-text (read-from-minibuffer "Edit: " (todotxt-get-current-line-as-string))))
      (beginning-of-line)
      (setq inhibit-read-only 't)
      (kill-line)
      (insert new-text)
      (save-buffer)
      (setq inhibit-read-only nil))))

(defun todotxt-tag-item ()
  (interactive)
  (let* ((new-tag (completing-read "Tags: " (todotxt-get-tag-completion-list-from-string (buffer-string))))e
         (new-text (concat (todotxt-get-current-line-as-string) " " new-tag)))
    (beginning-of-line)
    (setq inhibit-read-only 't)
    (kill-line)
    (insert new-text)
    (save-buffer)
    (setq inhibit-read-only nil)))

(defun todotxt-purge ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (setq inhibit-read-only 't)    
    (while (progn
             (if (and (not (todotxt-line-empty-p)) (todotxt-complete-p))
                 ;; Todo: Consider a push to complete.txt?
                 (progn
                   (kill-line 1)
                   't)
               (equal (forward-line) 0))))
    (setq inhibit-read-only nil)))

(defun todotxt-bury ()
  (interactive)
  (bury-buffer)
  (delete-window))

(defun todotxt-unhide-all ()
  (interactive)
  (goto-char (point-min))
  (let ((beg (point)))
    (goto-char (point-max))
    (setq inhibit-read-only 't)
    (remove-text-properties beg (point) '(intangible nil invisible nil))
    (setq inhibit-read-only nil))
  (goto-char (point-min)))

(defun todotxt-filter-for (arg)
  (interactive "p")
  (let* ((keyword (completing-read "Tag or keyword: " (todotxt-get-tag-completion-list-from-string (buffer-string)))))
    (save-excursion
      (if (equal arg 4)
          (todotxt-unhide-all))
      (goto-char (point-min))
      (todotxt-filter-out (lambda () (not (todotxt-current-line-match keyword)))))))

(defun todotxt-complete-toggle ()
  (interactive)
  (save-excursion
    (setq inhibit-read-only 't)
    (if (todotxt-complete-p)
        (progn
          (beginning-of-line)
          (delete-char 2))
      (progn
        (beginning-of-line)
        (insert "x ")))
    (setq inhibit-read-only nil)
    (save-buffer)))
  
(provide 'todotxt)
