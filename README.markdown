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

## Author

* Eitaro Fukamachi
* Rudolph Miller

## Copyright

Copyright (c) 2015 Eitaro Fukamachi & Rudolph Miller

## License

Licensed under the BSD 2-Clause License.
