(ert-deftest todotxt-test-get-variable ()
  "Tests fetching existing and nil variables from the string"
  (should (equal (todotxt-get-variable "foo:bar" "foo") "bar"))
  (should (equal (todotxt-get-variable "(A) This is a task foo:bar" "foo") "bar"))
  (should (eq (todotxt-get-variable "(A) This is a task" "foo") nil)))

(ert-deftest todotxt-test-set-variable ()
  "Tests setting existing and nil variables"
  (let* ((str "(A) Some other task")
         (new (todotxt-set-variable str "foo" "bar")))
    (should (equal new "(A) Some other task foo:bar")))
  (let* ((str "(A) Some other task foo:baz")
         (new (todotxt-set-variable str "foo" "bar")))
    (should (equal new "(A) Some other task foo:bar")))
  (let* ((str "(A) foo:baz Some other task")
         (new (todotxt-set-variable str "foo" "bar")))
    (should (equal new "(A) foo:bar Some other task"))))
