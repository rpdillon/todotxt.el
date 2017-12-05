(ert-deftest todotxt-test-get-variable ()
  "It fetches existing and nil variables"
  (should (equal (todotxt-get-variable "foo:bar" "foo") "bar"))
  (should (equal (todotxt-get-variable "(A) This is a task foo:bar" "foo") "bar"))
  (should (eq (todotxt-get-variable "(A) This is a task" "foo") nil)))

(ert-deftest todotxt-test-set-variable ()
  "It sets existing and nil variables"
  (let* ((str "(A) Some other task")
         (new (todotxt-set-variable str "foo" "bar")))
    (should (equal new "(A) Some other task foo:bar")))
  (let* ((str "(A) Some other task foo:baz")
         (new (todotxt-set-variable str "foo" "bar")))
    (should (equal new "(A) Some other task foo:bar")))
  (let* ((str "(A) foo:baz Some other task")
         (new (todotxt-set-variable str "foo" "bar")))
    (should (equal new "(A) foo:bar Some other task"))))

(ert-deftest todotxt-test-sort-key-for-string ()
  "It returns the expected sort key"
  (let* ((str "(A) Some other task")
         (actual (todotxt-sort-key-for-string str))
         (expected "9999-99-99 A"))
    (should (equal actual expected)))
  (let* ((str "Some task due:1776-07-04")
         (actual (todotxt-sort-key-for-string str))
         (expected "1776-07-04 a"))
    (should (equal actual expected)))
  (let* ((str "(B) Some task due:1776-07-04")
         (actual (todotxt-sort-key-for-string str))
         (expected "1776-07-04 B"))
    (should (equal actual expected))))

(ert-deftest todotxt-test-tag-completion-list ()
  "It returns a list of projects and contexts"
  (let* ((str "Task +project1 +project2 @context1
               Task +project1 @context2 @context3")
         (expected '("@context1" "@context2" "@context3" "+project1" "+project2")))
    (should (equal (sort (todotxt-get-tag-completion-list-from-string str) 'string>)
                   (sort expected 'string>)))))

(ert-deftest todotxt-test-transpose-lines-up-at-top ()
  (with-temp-buffer
    (insert "abc
def
ghi
")
    (goto-char (point-min))
    (todotxt-transpose-lines-up)
    (should (equal "abc
def
ghi
"
                   (buffer-string)))
    (should (equal nil (char-before)))
    (should (equal ?a (char-after)))))

(ert-deftest todotxt-test-transpose-lines-up-without-priority ()
  (with-temp-buffer
    (insert "abc
def
ghi
")
    (goto-char (point-min))
    (forward-line)
    (forward-char)
    (todotxt-transpose-lines-up)
    (should (equal "def
abc
ghi
"
                   (buffer-string)))
    (should (equal nil (char-before)))
    (should (equal ?d (char-after)))))

(ert-deftest todotxt-test-transpose-lines-up-with-higher-priority ()
  (with-temp-buffer
    (insert "(A) abc
def
ghi
")
    (goto-char (point-min))
    (forward-line)
    (forward-char)
    (todotxt-transpose-lines-up)
    (should (equal "(A) abc
def
ghi
"
                   (buffer-string)))
    (should (equal ?d (char-before)))
    (should (equal ?e (char-after)))))

(ert-deftest todotxt-test-transpose-lines-up-with-same-priority ()
  (with-temp-buffer
    (insert "(A) abc
(A) def
ghi
")
    (goto-char (point-min))
    (forward-line)
    (forward-char)
    (todotxt-transpose-lines-up)
    (should (equal "(A) def
(A) abc
ghi
"
                   (buffer-string)))
    (should (equal nil (char-before)))
    (should (equal ?\( (char-after)))))

(ert-deftest todotxt-test-transpose-lines-up-in-filterd-buffer ()
  (with-temp-buffer
    (insert "abc +a
def
ghi +a
")
    (todotxt-filter (eval `(lambda () (not (todotxt-current-line-match "+a")))))
    (todotxt-apply-active-filters)
    (goto-char (point-min))
    (forward-line)
    (todotxt-transpose-lines-up)
    (todotxt-unhide-all)
    (should (equal "ghi +a
abc +a
def
"
                   (buffer-string)))
    (should (equal nil (char-before)))
    (should (equal ?g (char-after)))))

(ert-deftest todotxt-test-transpose-lines-down-at-bottom ()
  (with-temp-buffer
    (insert "abc
def
ghi
")
    (goto-char (point-max))
    (todotxt-transpose-lines-down)
    (should (equal "abc
def
ghi
"
                   (buffer-string)))
    (should (equal ?\C-j (char-before)))
    (should (equal nil (char-after)))))

(ert-deftest todotxt-test-transpose-lines-down-without-priority ()
  (with-temp-buffer
    (insert "abc
def
ghi
")
    (goto-char (point-min))
    (forward-line)
    (forward-char)
    (todotxt-transpose-lines-down)
    (should (equal "abc
ghi
def
"
                   (buffer-string)))
    (should (equal ?\C-j (char-before)))
    (should (equal ?d (char-after)))))

(ert-deftest todotxt-test-transpose-lines-down-with-higher-priority ()
  (with-temp-buffer
    (insert "abc
def
(A) ghi
")
    (goto-char (point-min))
    (forward-line)
    (forward-char)
    (todotxt-transpose-lines-down)
    (should (equal "abc
def
(A) ghi
"
                   (buffer-string)))
    (should (equal ?d (char-before)))
    (should (equal ?e (char-after)))))

(ert-deftest todotxt-test-transpose-lines-down-with-same-priority ()
  (with-temp-buffer
    (insert "abc
(A) def
(A) ghi
")
    (goto-char (point-min))
    (forward-line)
    (forward-char)
    (todotxt-transpose-lines-down)
    (should (equal "abc
(A) ghi
(A) def
"
                   (buffer-string)))
    (should (equal ?\C-j (char-before)))
    (should (equal ?\( (char-after)))))

(ert-deftest todotxt-test-transpose-lines-down-in-filterd-buffer ()
  (with-temp-buffer
    (insert "abc +a
def
ghi +a
")
    (todotxt-filter (eval `(lambda () (not (todotxt-current-line-match "+a")))))
    (todotxt-apply-active-filters)
    (goto-char (point-min))
    (todotxt-transpose-lines-down)
    (todotxt-unhide-all)
    (should (equal "def
ghi +a
abc +a
"
                   (buffer-string)))
    (should (equal ?\C-j (char-before)))
    (should (equal ?a (char-after)))))
