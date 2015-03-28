(in-package :cl-user)
(defpackage proc-parse
  (:use :cl)
  #+(or sbcl openmcl cmu allegro)
  (:import-from #+sbcl :sb-cltl2
                #+openmcl :ccl
                #+cmu :ext
                #+allegro :sys
                :variable-information)
  (:import-from :alexandria
                :with-gensyms
                :once-only
                :ensure-cons
                :ignore-some-conditions)
  (:export :with-vector-parsing
           :with-string-parsing
           :with-octets-parsing
           :eofp
           :current
           :pos
           :advance
           :skip
           :skip*
           :skip+
           :skip?
           :skip-until
           :skip-while
           :bind
           :match
           :match-i
           :match?
           :match-case
           :match-i-case
           :eof
           :match-failed)
  (:use :cl))
(in-package :proc-parse)

(define-condition match-failed (error) ())

(define-condition eof (condition) ())

(defmacro typed-case (var &body cases)
  (cond
    ((or (characterp (car (first cases)))
         (and (consp (car (first cases)))
              (every #'characterp (car (first cases)))))
     `(locally (declare (type character ,var))
        (cond
          ,@(loop for (chars . body) in cases
                  if (consp chars)
                    collect `((or ,@(loop for ch in chars
                                          collect `(char= ,var ,ch))) ,@body)
                  else if (eq chars 'otherwise)
                         collect `(t ,@body)
                  else
                    collect `((char= ,var ,chars) ,@body)))))
    ((or (integerp (car (first cases)))
         (and (consp (car (first cases)))
              (every #'integerp (car (first cases)))))
     `(locally (declare (type (unsigned-byte 8) ,var))
        (cond
          ,@(loop for (chars . body) in cases
                  if (consp chars)
                    collect `((or ,@(loop for ch in chars
                                          collect `(= ,var ,ch))) ,@body)
                  else if (eq chars 'otherwise)
                         collect `(t ,@body)
                  else
                    collect `((= ,var ,chars) ,@body)))))
    (t `(case ,var ,@cases))))

(defmacro vector-case (elem-var vec-and-options &body cases)
  (destructuring-bind (vec &key case-insensitive)
      (ensure-cons vec-and-options)
    (with-gensyms (otherwise end-tag)
      (labels ((case-candidates (el)
                 (cond
                   ((not case-insensitive) el)
                   ((characterp el)
                    (cond
                      ((char<= #\a el #\z)
                       `(,el
                         ,(code-char
                           (- (char-code el)
                              #.(- (char-code #\a) (char-code #\A))))))
                      ((char<= #\A el #\Z)
                       `(,el
                         ,(code-char
                           (+ (char-code el)
                              #.(- (char-code #\a) (char-code #\A))))))
                      (t el)))
                   ((typep el '(unsigned-byte 8))
                    (cond
                      ((<= #.(char-code #\a) el #.(char-code #\z))
                       `(,el
                         ,(- el #.(- (char-code #\a) (char-code #\A)))))
                      ((<= #.(char-code #\A) el #.(char-code #\Z))
                       `(,el
                         ,(+ el #.(- (char-code #\a) (char-code #\A)))))
                      (t el)))
                   (t el)))
               (build-case (i cases vec)
                 (when cases
                   (let ((map (make-hash-table)))
                     (map nil
                          (lambda (case)
                            (unless (vectorp (car case))
                              (error "The first element of cases must be a constant vector"))
                            (unless (<= (length (car case)) i)
                              (push case (gethash (aref (car case) i) map))))
                          cases)
                     (let (res-cases)
                       (maphash (lambda (el cases)
                                  (let ((next-case (build-case (1+ i) cases vec)))
                                    (cond
                                      (next-case
                                       (push
                                        `(,(case-candidates el)
                                          (unless (advance*)
                                            ,(if (= (length (caar cases)) (1+ i))
                                                 `(progn ,@(cdr (car cases))
                                                         (go ,end-tag))
                                                 `(go ,otherwise)))
                                          (typed-case ,elem-var
                                            ,@next-case
                                            (otherwise (go ,otherwise))))
                                        res-cases))
                                      (t
                                       (push (with-gensyms (eofp)
                                               `(,(case-candidates el)
                                                 (let ((,eofp (advance*)))
                                                   ,@(cdr (car cases))
                                                   (and ,eofp
                                                        (error 'eof)))))
                                             res-cases)))))
                                map)
                       res-cases)))))
        (let ((otherwise-case nil))
          (when (eq (caar (last cases)) 'otherwise)
            (setq otherwise-case (car (last cases))
                  cases (butlast cases)))
          `(tagbody
              (typed-case ,elem-var
                ,@(build-case 0 cases vec)
                (otherwise (go ,otherwise)))
              (go ,end-tag)
              ,otherwise
              ,@(when otherwise-case
                  (cdr otherwise-case))
              ,end-tag))))))

(defun variable-type (var &optional env)
  (declare (ignorable env))
  (cond
    ((constantp var) (type-of var))
    #+(or sbcl openmcl cmu allegro)
    ((and (symbolp var)
          (cdr (assoc 'type (nth-value 2 (variable-information var env))))))
    ((and (listp var)
          (eq (car var) 'the)
          (cadr var)))))

(deftype octets (&optional (len '*))
  `(simple-array (unsigned-byte 8) (,len)))

(defun variable-type* (var &optional env)
  (let ((type (variable-type var env)))
    (cond
      ((null type) nil)
      ((subtypep type 'string) 'string)
      ((subtypep type 'octets) 'octets))))

(defun check-skip-elems (elems)
  (or (every (lambda (elem)
               (or (characterp elem)
                   (and (consp elem)
                        (null (cddr elem))
                        (eq (first elem) 'not)
                        (characterp (second elem)))))
             elems)
      (error "'skip' takes only constant characters, or a cons starts with 'not'.")))

(defun check-match-cases (cases)
  (or (every (lambda (case)
               (and (consp case)
                    (or (eq (car case) 'otherwise)
                        (stringp (car case)))))
             cases)
      (error "'match-case' takes only constant strings at the car position.~%  ~S" cases)))


(defmacro bind ((symb &body bind-forms) &body body)
  (declare (ignore symb bind-forms body)))

(defun string-skip (elem-var elems)
  (check-skip-elems elems)
  `(if (or ,@(loop for el in elems
                   if (and (consp el)
                           (eq (car el) 'not))
                     collect `(not (char= ,(cadr el) ,elem-var))
                   else
                     collect `(char= ,el ,elem-var)))
       (advance)
       (error 'match-failed)))

(defun octets-skip (elem-var elems)
  (check-skip-elems elems)
  `(if (or ,@(loop for el in elems
                   if (and (consp el)
                           (eq (car el) 'not))
                     collect `(not (= ,(char-code (cadr el)) ,elem-var))
                   else
                     collect `(= ,(char-code el) ,elem-var)))
       (advance)
       (error 'match-failed)))

(declaim (ftype (function () fixnum) pos))
(declaim (ftype (function () character) current))

(defmacro with-string-parsing ((data &key start end) &body body)
  (with-gensyms (g-end elem p)
    (once-only (data)
      `(let ((,elem #\Nul)
             (,p ,(if start
                      `(or ,start 0)
                      0))
             (,g-end ,(if end
                          `(or ,end (length ,data))
                          `(length ,data))))
         (declare (type string ,data)
                  (type fixnum ,p ,g-end)
                  (type character ,elem))
         (macrolet ((advance (&optional (step 1))
                      `(or (advance* ,step)
                           (error 'eof)))
                    (advance* (&optional (step 1))
                      `(locally (declare (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 0)))
                         (incf ,',p ,step)
                         ,@(if (= step 0)
                               ()
                               `((if (<= ,',g-end ,',p)
                                     nil
                                     (progn
                                       (setq ,',elem
                                             (aref ,',data ,',p))
                                       t))))))
                    (skip (&rest elems)
                      `(locally (declare (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 0)))
                         ,(string-skip ',elem elems)))
                    (skip* (&rest elems)
                      `(ignore-some-conditions (match-failed)
                         (loop (skip ,@elems))))
                    (skip+ (&rest elems)
                      `(progn
                         (skip ,@elems)
                         (skip* ,@elems)))
                    (skip? (&rest elems)
                      (check-skip-elems elems)
                      `(ignore-some-conditions (match-failed)
                         (skip ,@elems)))
                    (skip-until (fn)
                      `(loop until ,(if (symbolp fn)
                                        `(,fn ,',elem)
                                        `(funcall ,fn ,',elem))
                             do (advance)))
                    (skip-while (fn)
                      `(loop while ,(if (symbolp fn)
                                        `(,fn ,',elem)
                                        `(funcall ,fn ,',elem))
                             do (advance)))
                    (bind ((symb &body bind-forms) &body body)
                      (with-gensyms (start main)
                        `(let ((,start ,',p))
                           (flet ((,main ()
                                    (let ((,symb (subseq ,',data ,start ,',p)))
                                      ,@body)))
                             (handler-bind ((eof
                                              (lambda (e)
                                                (declare (ignore e))
                                                (,main))))
                               ,@bind-forms)
                             (,main)))))
                    (match (&rest vectors)
                      `(match-case
                        ,@(loop for vec in vectors
                                collect `(,vec))))
                    (match? (&rest vectors)
                      (with-gensyms (start start-elem)
                        `(let ((,start ,',p)
                               (,start-elem ,',elem))
                           (handler-case
                               (match ,@vectors)
                             (match-failed ()
                               (setq ,',p ,start
                                     ,',elem ,start-elem))))))
                    (match-i (&rest vectors)
                      `(match-i-case
                        ,@(loop for vec in vectors
                                collect `(,vec))))
                    (match-case (&rest cases)
                      (check-match-cases cases)
                      `(vector-case ,',elem (,',data)
                         ,@(if (find 'otherwise cases :key #'car :test #'eq)
                               cases
                               (append cases
                                       '((otherwise (error 'match-failed)))))))
                    (match-i-case (&rest cases)
                      (check-match-cases cases)
                      `(vector-case ,',elem (,',data :case-insensitive t)
                         ,@(if (find 'otherwise cases :key #'car :test #'eq)
                               cases
                               (append cases
                                       '((otherwise (error 'match-failed))))))))
           #+sbcl (declare (sb-ext:muffle-conditions sb-ext:code-deletion-note))
           (flet ((eofp ()
                    (<= ,g-end ,p))
                  (current () (the character ,elem))
                  (pos () (the fixnum ,p)))
             (handler-case
                 (progn
                   (tagbody
                      (when (eofp)
                        (error 'eof))
                      (setq ,elem (aref ,data ,p))
                      ,@body)
                   ,p)
               (eof () ,p))))))))

(defmacro with-octets-parsing ((data &key start end) &body body)
  (with-gensyms (g-end elem p)
    (once-only (data)
      `(let ((,elem 0)
             (,p ,(if start
                      `(or ,start 0)
                      0))
             (,g-end ,(if end
                          `(or ,end (length ,data))
                          `(length ,data))))
         (declare (type octets ,data)
                  (type fixnum ,p ,g-end)
                  (type (unsigned-byte 8) ,elem))
         (macrolet ((advance (&optional (step 1))
                      `(or (advance* ,step)
                           (error 'eof)))
                    (advance* (&optional (step 1))
                      `(locally (declare (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 0)))
                         (incf ,',p ,step)
                         ,@(if (= step 0)
                               ()
                               `((if (<= ,',g-end ,',p)
                                     nil
                                     (progn
                                       (setq ,',elem
                                             (aref ,',data ,',p))
                                       t))))))
                    (skip (&rest elems)
                      `(locally (declare (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 0)))
                         ,(octets-skip ',elem elems)))
                    (skip* (&rest elems)
                      `(ignore-some-conditions (match-failed)
                         (loop (skip ,@elems))))
                    (skip+ (&rest elems)
                      `(progn
                         (skip ,@elems)
                         (skip* ,@elems)))
                    (skip? (&rest elems)
                      (check-skip-elems elems)
                      `(ignore-some-conditions (match-failed)
                         (skip ,@elems)))
                    (skip-until (fn)
                      `(loop until ,(if (symbolp fn)
                                        `(,fn (code-char ,',elem))
                                        `(funcall ,fn (code-char ,',elem)))
                             do (advance)))
                    (skip-while (fn)
                      `(loop while ,(if (symbolp fn)
                                        `(,fn (code-char ,',elem))
                                        `(funcall ,fn (code-char ,',elem)))
                             do (advance)))
                    (bind ((symb &body bind-forms) &body body)
                      (with-gensyms (start main)
                        `(let ((,start ,',p))
                           (flet ((,main ()
                                    (let ((,symb (babel:octets-to-string ,',data :start ,start :end ,',p)))
                                      ,@body)))
                             (handler-bind ((eof
                                              (lambda (e)
                                                (declare (ignore e))
                                                (,main))))
                               ,@bind-forms)
                             (,main)))))
                    (match (&rest vectors)
                      `(match-case
                        ,@(loop for vec in vectors
                                collect `(,vec))))
                    (match? (&rest vectors)
                      (with-gensyms (start start-elem)
                        `(let ((,start ,',p)
                               (,start-elem ,',elem))
                           (handler-case
                               (match ,@vectors)
                             (match-failed ()
                               (setq ,',p ,start
                                     ,',elem ,start-elem))))))
                    (match-i (&rest vectors)
                      `(match-i-case
                        ,@(loop for vec in vectors
                                collect `(,vec))))
                    (match-case (&rest cases)
                      (check-match-cases cases)
                      (setf cases
                            (loop for case in cases
                                  if (stringp (car case))
                                    collect (cons (babel:string-to-octets (car case))
                                                  (cdr case))
                                  else
                                    collect case))
                      `(vector-case ,',elem (,',data)
                         ,@(if (find 'otherwise cases :key #'car :test #'eq)
                               cases
                               (append cases
                                       '((otherwise (error 'match-failed)))))))
                    (match-i-case (&rest cases)
                      (check-match-cases cases)
                      (setf cases
                            (loop for case in cases
                                  if (stringp (car case))
                                    collect (cons (babel:string-to-octets (car case))
                                                  (cdr case))
                                  else
                                    collect case))
                      `(vector-case ,',elem (,',data :case-insensitive t)
                         ,@(if (find 'otherwise cases :key #'car :test #'eq)
                               cases
                               (append cases
                                       '((otherwise (error 'match-failed))))))))
           #+sbcl (declare (sb-ext:muffle-conditions sb-ext:code-deletion-note))
           (flet ((eofp ()
                    (<= ,g-end ,p))
                  (current () (the character (code-char ,elem)))
                  (pos () (the fixnum ,p)))
             (handler-case
                 (progn
                   (tagbody
                      (when (eofp)
                        (error 'eof))
                      (setq ,elem (aref ,data ,p))
                      ,@body)
                   ,p)
               (eof () ,p))))))))

(defmacro with-vector-parsing ((data &key (start 0) end) &body body &environment env)
  (let ((data-type (variable-type* data env)))
    (case data-type
      (string `(with-string-parsing (,data :start ,start :end ,end) ,@body))
      (octets `(with-octets-parsing (,data :start ,start :end ,end) ,@body))
      (otherwise (once-only (data)
                   `(etypecase ,data
                      (string (with-string-parsing (,data :start ,start :end ,end) ,@body))
                      (octets (with-octets-parsing (,data :start ,start :end ,end) ,@body))))))))
