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

(defmacro bind ((symb &body bind-forms) &body body)
  (declare (ignore symb bind-forms body)))

(defmacro vector-case (vec-and-options &body cases)
  (destructuring-bind (vec &key (start 0) end case-insensitive)
      (ensure-cons vec-and-options)
    (once-only (vec start)
      (let ((otherwise (gensym "otherwise")))
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
                 (build-case (i cases vec end)
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
                                    (let ((next-case (build-case (1+ i) cases vec end)))
                                      (cond
                                        (next-case
                                         (push
                                          `(,(case-candidates el)
                                            (if (<= ,end (+ ,(1+ i) ,start))
                                                ,@(if (= (length (caar cases)) (1+ i))
                                                      (cdr (car cases))
                                                      '(nil))
                                                (progn
                                                  (advance)
                                                  (case (aref ,vec (+ ,(1+ i) ,start))
                                                    ,@next-case
                                                    (otherwise (go ,otherwise))))))
                                          res-cases))
                                        (t
                                         (with-gensyms (main)
                                           (push `(,(case-candidates el)
                                                   (flet ((,main () ,@(cdr (car cases))))
                                                     (handler-bind ((eof
                                                                      (lambda (e)
                                                                        (declare (ignore e))
                                                                        (,main))))
                                                       (advance))
                                                     (,main)))
                                                 res-cases))))))
                                  map)
                         res-cases)))))
          (let ((end-symb (gensym "END"))
                (otherwise-case nil))
            (when (eq (caar (last cases)) 'otherwise)
              (setq otherwise-case (car (last cases))
                    cases (butlast cases)))
            `(let ((,end-symb ,(or end `(length ,vec))))
               (unless (<= ,end-symb ,start)
                 (block nil
                   (tagbody
                      (return
                        (case (aref ,vec ,start)
                          ,@(build-case 0 cases vec end-symb)
                          (otherwise (go ,otherwise))))
                      ,otherwise
                      ,(when otherwise-case
                         `(return (progn ,@(cdr otherwise-case))))))))))))))

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
                        (and (constantp (car case))
                             (stringp (car case))))))
             cases)
      (error "'match-case' takes only constant strings at the car position.")))

(defun build-skip-condition (data-type elems elem-var)
  (case data-type
    (string
     `(or ,@(loop for el in elems
                  if (and (consp el)
                          (eq (car el) 'not))
                    collect `(not (char= ,(cadr el) ,elem-var))
                  else
                    collect `(char= ,el ,elem-var))))
    (octets
     `(or ,@(loop for el in elems
                  if (and (consp el)
                          (eq (car el) 'not))
                    collect `(not (= ,(char-code (cadr el)) ,elem-var))
                  else
                    collect `(= ,(char-code el) ,elem-var))))
    (otherwise
     (let ((myeql (gensym "MYEQL")))
       `(flet ((,myeql (a b)
                 (if (characterp b)
                     (char= a b)
                     (= (char-code a) b))))
          (or ,@(loop for el in elems
                      if (and (consp el)
                              (eq (car el) 'not))
                        collect `(not (,myeql ,(cadr el) ,elem-var))
                      else
                        collect `(,myeql ,el ,elem-var))))))))

(defun build-match-cases (data-type cases)
  (case data-type
    (string cases)
    (octets
     (dolist (case cases)
       (when (stringp (car case))
         (rplaca case (babel:string-to-octets (car case)))))
     cases)
    (otherwise
     (error "TODO: match-case doesn't support the case when the data has no type declaration for now."))))

(defmacro with-vector-parsing ((data &key (start 0) end) &body body &environment env)
  (with-gensyms (g-end elem p)
    (let ((data-type (variable-type* data env)))
      (once-only (data)
        `(locally (declare (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 0)))
           (let ((,elem ,(case data-type
                           (string #\Nul)
                           (octets 0)))
                 (,p ,start)
                 (,g-end ,(or end
                              `(length ,data))))
             (declare (type fixnum ,p ,g-end))
             ,@(case data-type
                 (string `((declare (type character ,elem))))
                 (octets `((declare (type (unsigned-byte 8) ,elem)))))
             (macrolet ((advance (&optional (step 1))
                          `(locally (declare (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 0)))
                             (incf ,',p ,step)
                             ,@(if (= step 0)
                                   ()
                                   `((when (<= ,',g-end ,',p)
                                       (error 'eof))
                                     (setq ,',elem
                                           (aref ,',data ,',p))))))
                        (skip (&rest elems)
                          (check-skip-elems elems)
                          `(if ,(build-skip-condition ',data-type elems ',elem)
                               (advance)
                               (error 'match-failed)))
                        (skip* (&rest elems)
                          (check-skip-elems elems)
                          `(loop
                             (if ,(build-skip-condition ',data-type elems ',elem)
                                 (advance)
                                 (return))))
                        (skip+ (&rest elems)
                          (check-skip-elems elems)
                          `(if ,(build-skip-condition ',data-type elems ',elem)
                               (progn
                                 (advance)
                                 (skip* ,@elems))
                               (error 'match-failed)))
                        (skip? (&rest elems)
                          (check-skip-elems elems)
                          `(when ,(build-skip-condition ',data-type elems ',elem)
                             (ignore-some-conditions (eof)
                               (advance))))
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
                          (when (and ',data-type
                                     (subtypep ',data-type 'octets))
                            (dolist (case cases)
                              (when (stringp (car case))
                                (rplaca case (babel:string-to-octets (car case))))))
                          `(vector-case (,',data :start ,',p)
                             ,@(if (find 'otherwise cases :key #'car :test #'eq)
                                   cases
                                   (append cases
                                           '((otherwise (error 'match-failed)))))))
                        (match-i-case (&rest cases)
                          (check-match-cases cases)
                          (when (and ',data-type
                                     (subtypep ',data-type 'octets))
                            (dolist (case cases)
                              (when (stringp (car case))
                                (rplaca case (babel:string-to-octets (car case))))))
                          `(vector-case (,',data :start ,',p :case-insensitive t)
                             ,@(if (find 'otherwise cases :key #'car :test #'eq)
                                   cases
                                   (append cases
                                           '((otherwise (error 'match-failed))))))))
               (flet ((eofp ()
                        (<= ,g-end ,p))
                      (current () ,elem)
                      (pos () ,p))
                 (handler-case
                     (progn
                       (tagbody
                          (when (eofp)
                            (error 'eof))
                          (setq ,elem (aref ,data ,p))
                          ,@body)
                       ,p)
                   (eof () ,p))))))))))
