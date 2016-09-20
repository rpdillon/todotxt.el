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
