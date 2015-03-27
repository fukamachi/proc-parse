(in-package :cl-user)
(defpackage proc-parse
  (:use :cl)
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
                   (if (and case-insensitive
                            (characterp el))
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
                         (t el))
                       el))
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

(defmacro with-vector-parsing ((data &key (start 0) end) &body body)
  (with-gensyms (g-end elem p)
    (once-only (data)
      `(locally (declare (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 0)))
         (let (,elem
               (,p ,start)
               (,g-end ,(or end
                            `(length ,data))))
           (declare (type fixnum ,p ,g-end))
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
                        `(if (or ,@(loop for el in elems
                                         if (and (consp el)
                                                 (eq (car el) 'not))
                                           collect `(not (eql ,(cadr el) ,',elem))
                                         else
                                           collect `(eql ,el ,',elem)))
                             (advance)
                             (error 'match-failed)))
                      (skip* (&rest elems)
                        `(loop
                           (if (or ,@(loop for el in elems
                                           if (and (consp el)
                                                   (eq (car el) 'not))
                                             collect `(not (eql ,(cadr el) ,',elem))
                                           else
                                             collect `(eql ,el ,',elem)))
                               (advance)
                               (return))))
                      (skip+ (&rest elems)
                        `(if (or ,@(loop for el in elems
                                         if (and (consp el)
                                                 (eq (car el) 'not))
                                           collect `(not (eql ,(cadr el) ,',elem))
                                         else
                                           collect `(eql ,el ,',elem)))
                             (progn
                               (advance)
                               (skip* ,@elems))
                             (error 'match-failed)))
                      (skip? (&rest elems)
                        `(when (or ,@(loop for el in elems
                                           if (and (consp el)
                                                   (eq (car el) 'not))
                                             collect `(not (eql ,(cadr el) ,',elem))
                                           else
                                             collect `(eql ,el ,',elem)))
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
                        `(vector-case (,',data :start ,',p)
                           ,@(if (find 'otherwise cases :key #'car :test #'eq)
                                 cases
                                 (append cases
                                         '((otherwise (error 'match-failed)))))))
                      (match-i-case (&rest cases)
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
                 (eof () ,p)))))))))
