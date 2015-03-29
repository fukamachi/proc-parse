(in-package :cl-user)
(defpackage proc-parse-test
  (:use :cl
        :proc-parse
        :prove)
  (:shadowing-import-from :proc-parse
                          :skip))
(in-package :proc-parse-test)

(plan 15)

(defmacro with-vector-parsing-test ((target) &body body)
  `(progn
     (subtest "with-string-parsing"
       (with-string-parsing (,target)
         ,@body))
     (subtest "with-octets-parsing"
       (with-octets-parsing (,(babel:string-to-octets target))
         ,@body))))

(subtest "current"
  (with-vector-parsing-test ("a")
    (is (current)
        #\a
        "can return the current character.")))

(subtest "advance"
  (with-vector-parsing-test ("ab")
    (advance)
    (is (current)
        #\b
        "can increment the current position.")
    (is-error (advance)
              'eof
              "can raise the eof error without rest characters.")))

(subtest "advance*"
  (with-vector-parsing-test ("ab")
    (advance*)
    (is (current)
        #\b
        "can increment the current position.")
    (ok (not (advance*))
        "doesn't raise the eof error without rest characters.")))

(subtest "skip"
  (with-vector-parsing-test ("ab")
    (skip #\a)
    (is (current)
        #\b
        "can skip the spcified character.")
    (is-error (skip #\c)
              'match-failed
              "can raise the match-failed error with unmatched character.")))

(subtest "skip*"
  (with-vector-parsing-test ("aaabbb")
    (skip* #\a)
    (is (current)
        #\b
        "can skip some spcified character.")
    (ok (not (skip* #\c))
        "doesn't raise the match-failed error with unmatched character.")
    (is (current)
        #\b
        "doesn't skip any characters when unmatched character spcified.")))

(subtest "skip+"
  (with-vector-parsing-test ("aaabbb")
    (skip+ #\a)
    (is (current)
        #\b
        "can skip some spcified character.")
    (is-error (skip+ #\c)
              'match-failed
              "can raise the match-failed error with unmatched character.")))

(subtest "skip?"
  (with-vector-parsing-test ("ab")
    (skip? #\a)
    (is (current)
        #\b
        "can skip the spcified character.")
    (ok (not (skip? #\c))
        "doesn't raise the match-failed error with unmatched character.")
    (is (current)
        #\b
        "doesn't skip any characters when unmatched character spcified.")))

(subtest "skip-until"
  (subtest "with-string-parsing"
    (with-string-parsing ("aaab")
      (skip-until (lambda (c) (char/= c #\a)))
      (is (current)
          #\b
          "can skip until form returns T.")
      (skip-until (lambda (c) (char/= c #\c)))
      (is (current)
          #\b
          "can skip until eof.")))

  (subtest "with-octets-parsing"
    (with-octets-parsing ((babel:string-to-octets "aaab"))
      (skip-until (lambda (c) (/= c (char-code  #\a))))
      (is (current)
          #\b
          "can skip until form returns T.")
      (skip-until (lambda (c) (/= c (char-code #\c))))
      (is (current)
          #\b
          "can skip until eof."))))

(subtest "skip-while"
  (subtest "with-string-parsing"
    (with-string-parsing ("aaab")
      (skip-while (lambda (c) (char= c #\a)))
      (is (current)
          #\b
          "can skip when form returns T.")
      (skip-while (lambda (c) (char= c #\b)))
      (is (current)
          #\b
          "can skip until eof.")))

  (subtest "with-octets-parsing"
    (with-octets-parsing ((babel:string-to-octets "aaab"))
      (skip-while (lambda (c) (= c (char-code #\a))))
      (is (current)
          #\b
          "can skip when form returns T.")
      (skip-while (lambda (c) (= c (char-code #\b))))
      (is (current)
          #\b
          "can skip until eof."))))

(subtest "bind"
  (subtest "with-string-parsing"
    (with-string-parsing ("aaab")
      (bind (str1 (skip-while (lambda (c) (char= c #\a))))
        (is str1
            "aaa"
            "can bind string with form."))
      (bind (str2 (skip-while (lambda (c) (char= c #\b))))
        (is str2
            "b"
            "can bind string until eof."))))

  (subtest "with-octets-parsing"
    (with-octets-parsing ((babel:string-to-octets "aaab"))
      (bind (str1 (skip-while (lambda (c) (= c (char-code #\a)))))
        (is str1
            "aaa"
            "can bind string with form."))
      (bind (str2 (skip-while (lambda (c) (= c (char-code #\b)))))
        (is str2
            "b"
            "can bind string until eof.")))))

(subtest "match"
  (with-vector-parsing-test ("abc")
    (match "cd" "ab")
    (is (current)
        #\c
        "can skip the matched one of specified strings.")
    (is-error (match "e" "fg")
              'match-failed
              "can raise the match-failed error with unmatched strings.")))

(subtest "match-i"
  (with-vector-parsing-test ("ABC")
    (match-i "cd" "ab")
    (is (current)
        #\C
        "can skip the case-insensitively matched one of specified strings.")
    (is-error (match-i "e")
              'match-failed
              "can raise the match-failed error with case-insensitively unmatched strings.")))

(subtest "match?"
  (with-vector-parsing-test ("abc")
    (match? "ab")
    (is (current)
        #\c
        "can skip the matched one of specified strings.")
    (match? "de")
    (is (current)
        #\c
        "doesn't raise the match-failed error with unmatched strings.")))

(subtest "match-case"
  (with-vector-parsing-test ("abc")
    (is (match-case
         ("a" 0)
         ("b" 1))
        0
        "can return the value the body form of the matched case returns.")
    (is (match-case
         ("c" 0)
         (otherwise 1))
        1
        "can return the value the otherwise form returns.")
    (is-error (match-case
               ("c"))
              'match-failed
              "can raise the match-failed error with unmatched cases.")
    (is (match-case
         ("bc" 0))
        0
        "can return the value the body form of the matched case returns even thogh eof.")))

(subtest "match-i-case"
  (with-vector-parsing-test ("ABC")
    (is (match-i-case
         ("a" 0)
         ("b" 1))
        0
        "can return the value the body form of the case-insensitively matched case returns.")
    (is (match-i-case
         ("c" 0)
         (otherwise 1))
        1
        "can return the value the otherwise form returns.")
    (is-error (match-i-case
               ("c"))
              'match-failed
              "can raise the match-failed error with case-insensitively unmatched cases.")
    (is (match-i-case
         ("bc" 0))
        0
        "can return the value the body form of the matched case returns even thogh eof.")))

(finalize)
