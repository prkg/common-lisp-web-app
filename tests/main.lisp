(defpackage cl-web-app-example/tests
  (:use :cl :rove))

(in-package :cl-web-app-example/tests)

(deftest test-target-1
  (testing "should (= 1 4) to be true"
    (ok (= 1 4))))
