(in-package :cl-user)
(defpackage proc-parse-test
  (:use :cl
        :proc-parse
        :prove)
  (:shadowing-import-from :proc-parse
                          :skip))
(in-package :proc-parse-test)

(plan nil)

(let ((str "Tuesday"))
  (is (with-vector-parsing (str)
        (is (match-case
             ("Sun" 0)
             ("Mon" 1)
             ("Tue" 2)
             ("Wed" 3)
             ("Thu" 4)
             ("Fri" 5)
             ("Sat" 6))
            2
            "match-case"))
      3
      "Return value")
  (is (with-vector-parsing (str)
        (match-case
         ("Sun" (match? "day"))
         ("Mon" (match? "day"))
         ("Tue" (match? "sday"))
         ("Wed" (match? "nesday"))
         ("Thu" (match? "rsday"))
         ("Fri" (match? "day"))
         ("Sat" (match? "urday"))))
      7))

(finalize)
