# Proc-Parse

<blockquote>
Question: Are these parser macros for speed or just to make your application look cool?<br>
Answer: Both.
</blockquote>

This is a string/octets parser library for Common Lisp with speed and readability in mind. Unlike other libraries, the code is not a pattern-matching-like, but a char-by-char procedural parser.

Although the design is good for speed, the code could look ugly with `tagbody` and `go`. Proc-Parse wraps the code with sexy macros.

I believe we don't have to give up speed for the readability while we use Common Lisp.

## Usage

```common-lisp
(defun parse-url-scheme (data &key (start 0) end)
  "Return a URL scheme of DATA as a string."
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (block nil
    (with-vector-parsing (data :start start :end end)
      (match-i-case
       ("http:" (return "http"))
       ("https:" (return "https"))
       (otherwise (unless (standard-alpha-char-p (current))
                    (return nil))
                  (bind (scheme (skip* (not #\:)))
                    (return scheme)))))))
```

## API

### with-vector-parsing

```Lisp
(with-vector-parsing ("It's Tuesday!" :start 5 :end 12)
  (bind (str (skip-until
              (lambda (c)
                (declare (ignore c))
                (eofp))))
    (print str))) ; "Tuesday"

(with-vector-parsing ((babel:string-to-octets "It's Tuesday!") :start 5 :end 12)
  (bind (str (skip-until
              (lambda (c)
                (declare (ignore c))
                (eofp))))
    (print str))) ; "Tuesday"
```

### with-string-parsing

```Lisp
(with-string-parsing ("It's Tuesday!" :start 5 :end 12)
  (bind (str (skip-until
              (lambda (c)
                (declare (ignore c))
                (eofp))))
    (print str))) ; "Tuesday"
```

### with-octets-parsing

```Lisp
(with-octets-parsing ((babel:string-to-octets "It's Tuesday!") :start 5 :end 12)
  (bind (str (skip-until
              (lambda (c)
                (declare (ignore c))
                (eofp))))
    (print str))) ; "Tuesday"
```

### eofp

```Lisp
(with-vector-parsing ("hello")
  (print (eofp)) ; NIL
  (match "hello")
  (print (eofp))) ; T
```

### current

```Lisp
(with-vector-parsing ("hello")
  (print (current)) ; #\h
  (skip #\h)
  (print (current))) ; #\e
```

### pos

```Lisp
(with-vector-parsing ("hello")
  (print (pos)) ; 0
  (skip #\h)
  (print (pos))) ; 1
```

### advance

```Lisp
(with-vector-parsing ("hello")
  (print (current)) ; #\h
  (advance)
  (print (current)) ; #\e
  (match "ello")
  (print (current)) ; #\o
  (advance)
  (print "Hi")) ; "Hi" won't displayed.
```

### advance*

```Lisp
(with-vector-parsing ("hello")
  (print (current)) ; #\h
  (advance*)
  (print (current)) ; #\e
  (match "ello")
  (print (current)) ; #\o
  (advance*)
  (print (current)) ; #\o
  (print "Hi")) ; "Hi"
```

### skip

```Lisp
(with-vector-parsing ("hello")
  (print (current)) ; #\h
  (skip #\h)
  (print (current)) ; #\e
  (skip (not #\h))
  (print (current)) ; #\l
  (skip #\f))
;; => Condition MATCH-FAILED was signalled.
```

### skip*

```Lisp
(with-vector-parsing ("hello")
  (skip* #\h)
  (print (current)) ; #\e
  (skip* (not #\l))
  (print (current)) ; #\l
  (skip* #\l)
  (print (current)) ; #\o
  (skip* #\f))
```

### skip+

```Lisp
(with-vector-parsing ("hello")
  (skip+ #\h)
  (print (current)) ; #\e
  (skip* (not #\l))
  (print (current)) ; #\l
  (skip+ #\l)
  (print (current)) ; #\o
  (skip+ #\f))
;; => Condition MATCH-FAILED was signalled.
```

### skip?

```Lisp
(with-vector-parsing ("hello")
  (print (current)) ; #\h
  (skip? #\h)
  (print (current)) ; #\e
  (skip? (not #\h))
  (print (current)) ; #\l
  (skip? #\f)) ; MATCH-FAILED won't be raised.
```

### skip-until

```Lisp
(with-vector-parsing ("hello")
  (skip-until (lambda (char) (char= char #\o)))
  (print (current)) ; #\o
  (print (eofp)) ; NIL
  (skip-until (lambda (char) (char= char #\f)))
  (print (eofp))) ; T
```

### skip-while

```Lisp
(with-vector-parsing ("hello")
  (skip-while (lambda (char) (char/= char #\o)))
  (print (current)) ; #\o
  (print (eofp)) ; NIL
  (skip-while (lambda (char) (char/= char #\f)))
  (print (eofp))) ; T
```

### bind

```Lisp
(with-vector-parsing ("hello")
  (bind (str1 (skip* (not #\l)))
    (print str1)) ; "he"
  (bind (str2 (skip* (not #\f)))
    (print str2))) ; "llo"
```

### match

```Lisp
(with-vector-parsing ("hello")
  (match "he")
  (print (current)) ; #\l
  (match "l" "ll")
  (print (current)) ; #\o
  (match "f"))
;; => Condition MATCH-FAILED was signalled.
```

### match-i

```Lisp
(with-vector-parsing ("hello")
  (match-i "He")
  (print (current)) ; #\l
  (match-i "L" "LL")
  (print (current)) ; #\o
  (match-i "F"))
;; => Condition MATCH-FAILED was signalled.
```

### match?

```Lisp
(with-vector-parsing ("hello")
  (match? "he")
  (print (current)) ; #\l
  (match? "l" "ll")
  (print (current)) ; #\o
  (match? "f")) ; MATCH-FAILED won't be raised.
```

### match-case

```Lisp
(with-vector-parsing ("hello")
  (print
   (match-case
    ("he" 0)
    ("ll" 1)
    (otherwise 2))) ; 0
  (print (current)) ; #\l
  (print
   (match-case
    ("he" 0)
    ("ll" 1)
    (otherwise 2))) ; 1
  (print (current)) ; #\o
  (print
   (match-case
    ("he" 0)
    ("ll" 1)
    (otherwise 2))) ; 2
  (print (current)) ; #\o
  (print
   (match-case
    ("he" 0)
    ("ll" 1))))
;; => Condition MATCH-FAILED was signalled.
```

### match-i-case

```Lisp
(with-vector-parsing ("hello")
  (print
   (match-i-case
    ("He" 0)
    ("LL" 1)
    (otherwise 2))) ; 0
  (print (current)) ; #\l
  (print
   (match-i-case
    ("He" 0)
    ("LL" 1)
    (otherwise 2))) ; 1
  (print (current)) ; #\o
  (print
   (match-i-case
    ("He" 0)
    ("LL" 1)
    (otherwise 2))) ; 2
  (print (current)) ; #\o
  (print
   (match-i-case
    ("He" 0)
    ("LL" 1))))
;; => Condition MATCH-FAILED was signalled.
```

### match-failed

```Lisp
(with-vector-parsing ("hello")
  (print (current)) ; #\h
  (skip #\f))
;; => Condition MATCH-FAILED was signalled.
```

## Author

* Eitaro Fukamachi
* Rudolph Miller

## Copyright

Copyright (c) 2015 Eitaro Fukamachi & Rudolph Miller

## License

Licensed under the BSD 2-Clause License.
