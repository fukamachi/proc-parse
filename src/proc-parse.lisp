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
           :advance*
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
           :match-failed)
  (:use :cl))
(in-package :proc-parse)

(define-condition match-failed (error) ())

(defun convert-case-conditions (var chars)
  (cond
    ((consp chars)
     `(or ,@(loop for ch in chars
                  if (characterp ch)
                    collect `(char= ,var ,ch)
                  else
                    collect `(= ,var ,ch))))
    ((eq chars 'otherwise)
     t)
    (t (if (characterp chars)
           `(char= ,var ,chars)
           `(= ,var ,chars)))))

(defun typed-case-tagbodies (var &rest cases)
  (cond
    ((null cases) nil)
    ((= 1 (length cases))
     `((when ,(convert-case-conditions var (car (first cases)))
         ,@(cdr (first cases)))))
    ((and (= 2 (length cases))
          (eq (car (second cases)) 'otherwise))
     `((unless ,(convert-case-conditions var (car (first cases)))
         ,@(cdr (second cases)))
       ,@(cdr (first cases))))
    (t
     (let ((tags (make-array (length cases) :initial-contents (loop repeat (length cases)
                                                                    collect (gensym))))
           (end (gensym "END")))
       `(,@(loop for (chars . body) in cases
                 for i from 0
                 collect `(when ,(convert-case-conditions var chars)
                            (go ,(aref tags i))))
         ,@(loop for case in cases
                 for i from 0
                 append `(,(aref tags i)
                          ,@(cdr case)
                          (go ,end)))
         ,end)))))

(defmacro vector-case (elem-var vec-and-options &body cases)
  (destructuring-bind (vec &key case-insensitive)
      (ensure-cons vec-and-options)
    (with-gensyms (otherwise end-tag vector-case-block)
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
                                          ,@(apply #'typed-case-tagbodies elem-var
                                                   (append
                                                    next-case
                                                    `((otherwise (go ,otherwise))))))
                                        res-cases))
                                      (t
                                       (push `(,(case-candidates el)
                                               (advance*)
                                               (return-from ,vector-case-block
                                                 (progn ,@(cdr (car cases)))))
                                             res-cases)))))
                                map)
                       res-cases)))))
        (let ((otherwise-case nil))
          (when (eq (caar (last cases)) 'otherwise)
            (setq otherwise-case (car (last cases))
                  cases (butlast cases)))
          `(block ,vector-case-block
             (tagbody
                ,@(apply #'typed-case-tagbodies elem-var
                         (append
                          (build-case 0 cases vec)
                          `((otherwise (go ,otherwise)))))
                (go ,end-tag)
                ,otherwise
                ,@(when otherwise-case
                    `(unless (eofp)
                       (return-from ,vector-case-block
                         (progn ,@(cdr otherwise-case)))))
                ,end-tag)))))))

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
  `(when (or ,@(loop for el in elems
                     if (and (consp el)
                             (eq (car el) 'not))
                       collect `(not (char= ,(cadr el) ,elem-var))
                     else
                       collect `(char= ,el ,elem-var)))
     (advance*)
     t))

(defun octets-skip (elem-var elems)
  (check-skip-elems elems)
  `(when (or ,@(loop for el in elems
                     if (and (consp el)
                             (eq (car el) 'not))
                       collect `(not (= ,(char-code (cadr el)) ,elem-var))
                     else
                       collect `(= ,(char-code el) ,elem-var)))
     (advance*)
     t))

(defmacro with-string-parsing ((data &key start end) &body body)
  (with-gensyms (g-end elem p body-block)
    (once-only (data)
      `(let ((,elem #\Nul)
             (,p ,(if start
                      `(or ,start 0)
                      0))
             (,g-end ,(if end
                          `(or ,end (length ,data))
                          `(length ,data))))
         (declare (type simple-string ,data)
                  (type fixnum ,p ,g-end)
                  (type character ,elem))
         (macrolet ((advance (&optional (step 1))
                      `(or (advance* ,step)
                           (go :eof)))
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
                      (check-skip-elems elems)
                      `(locally (declare (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 0)))
                         (if (or ,@(loop for el in elems
                                         if (and (consp el)
                                                 (eq (car el) 'not))
                                           collect `(not (char= ,(cadr el) ,',elem))
                                         else
                                           collect `(char= ,el ,',elem)))
                             (advance)
                             (error 'match-failed))))
                    (skip* (&rest elems)
                      (check-skip-elems elems)
                      `(locally (declare (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 0)))
                         (unless (eofp)
                           (loop
                             (unless (or ,@(loop for el in elems
                                                 if (and (consp el)
                                                         (eq (car el) 'not))
                                                   collect `(not (char= ,(cadr el) ,',elem))
                                                 else
                                                   collect `(char= ,el ,',elem)))
                               (return))
                             (or (advance*) (return))))))
                    (skip+ (&rest elems)
                      `(progn
                         (skip ,@elems)
                         (skip* ,@elems)))
                    (skip? (&rest elems)
                      (check-skip-elems elems)
                      (string-skip ',elem elems))
                    (skip-until (fn)
                      `(loop until (or ,(if (symbolp fn)
                                            `(,fn ,',elem)
                                            `(funcall ,fn ,',elem))
                                       (not (advance*)))))
                    (skip-while (fn)
                      `(loop while (and ,(if (symbolp fn)
                                             `(,fn ,',elem)
                                             `(funcall ,fn ,',elem))
                                        (advance*))))
                    (bind ((symb &body bind-forms) &body body)
                      (with-gensyms (start)
                        `(let ((,start ,',p))
                           ,@bind-forms
                           (prog1
                               (let ((,symb (subseq ,',data ,start ,',p)))
                                 ,@body)
                             (when (eofp)
                               (go :eof))))))
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
           (labels ((eofp ()
                      (declare (optimize (speed 3) (safety 0) (debug 0)))
                      (<= ,g-end ,p))
                    (current () (the character ,elem))
                    (pos () (the fixnum ,p)))
             (declare (ftype (function () fixnum) pos)
                      (ftype (function () character) current)
                      (inline pos current eofp))
             (block ,body-block
               (tagbody
                  (when (eofp)
                    (go :eof))
                  (setq ,elem (aref ,data ,p))
                  ,@body
                  (return-from ,body-block ,p)
                :eof))))))))

(defmacro with-octets-parsing ((data &key start end) &body body)
  (with-gensyms (g-end elem p body-block)
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
                           (go :eof)))
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
                      (check-skip-elems elems)
                      `(locally (declare (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 0)))
                         (if (or ,@(loop for el in elems
                                         if (and (consp el)
                                                 (eq (car el) 'not))
                                           collect `(not (= ,(char-code (cadr el)) ,',elem))
                                         else
                                           collect `(= ,(char-code el) ,',elem)))
                             (advance)
                             (error 'match-failed))))
                    (skip* (&rest elems)
                      (check-skip-elems elems)
                      `(locally (declare (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 0)))
                         (unless (eofp)
                           (loop
                             (unless (or ,@(loop for el in elems
                                                 if (and (consp el)
                                                         (eq (car el) 'not))
                                                   collect `(not (= ,(char-code (cadr el)) ,',elem))
                                                 else
                                                   collect `(= ,(char-code el) ,',elem)))
                               (return))
                             (or (advance*) (return))))))
                    (skip+ (&rest elems)
                      `(progn
                         (skip ,@elems)
                         (skip* ,@elems)))
                    (skip? (&rest elems)
                      (check-skip-elems elems)
                      (octets-skip ',elem elems))
                    (skip-until (fn)
                      `(loop until (or ,(if (symbolp fn)
                                            `(,fn (code-char ,',elem))
                                            `(funcall ,fn (code-char ,',elem)))
                                       (not (advance*)))))
                    (skip-while (fn)
                      `(loop while (and ,(if (symbolp fn)
                                             `(,fn (code-char ,',elem))
                                             `(funcall ,fn (code-char ,',elem)))
                                        (advance*))))
                    (bind ((symb &body bind-forms) &body body)
                      (with-gensyms (start)
                        `(let ((,start ,',p))
                           ,@bind-forms
                           (prog1
                               (let ((,symb (babel:octets-to-string ,',data :start ,start :end ,',p)))
                                 ,@body)
                             (when (eofp)
                               (go :eof))))))
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
           (labels ((eofp ()
                      (<= ,g-end ,p))
                    (current () (the character (code-char ,elem)))
                    (pos () (the fixnum ,p)))
             (declare (ftype (function () fixnum) pos)
                      (ftype (function () character) current)
                      (inline pos current eofp))
             (block ,body-block
               (tagbody
                  (when (eofp)
                    (go :eof))
                  (setq ,elem (aref ,data ,p))
                  ,@body
                  (return-from ,body-block ,p)
                :eof))))))))

(defmacro with-vector-parsing ((data &key (start 0) end) &body body &environment env)
  (let ((data-type (variable-type* data env)))
    (case data-type
      (string `(with-string-parsing (,data :start ,start :end ,end) ,@body))
      (octets `(with-octets-parsing (,data :start ,start :end ,end) ,@body))
      (otherwise (once-only (data)
                   `(etypecase ,data
                      (string (with-string-parsing (,data :start ,start :end ,end) ,@body))
                      (octets (with-octets-parsing (,data :start ,start :end ,end) ,@body))))))))
