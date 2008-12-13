
;; iozone test result
(defstruct ioz-test
  when
  cmd
  size
  read
  write
  rread
  rwrite
  blocks)


(defmacro parse-ioz-line-cond (l &rest conds)
  (let ((def nil)
        (dat nil)
        (ll  (gensym))
        (s   (gensym))
        (pos (gensym))
        (ss  (gensym))
        (st  (gensym)))
    `(let ((,ll (string-trim " " ,l)))
       (cond ,@(loop for c in conds
                  if (null (car c)) do (setf def (cadr c))
                  if (eq (car c) 'data) do (setf dat (cadr c))
                  if (stringp (car c))
                  collecting `((search ,(car c) ,ll)
                               (let* ((,s   ,(car c))
                                      (,pos (search ,s ,ll))
                                      (,ss  (subseq ,ll (+ ,pos ,(length (car c)))))
                                      (,st  (string-trim " " ,ss)))
                                 (,(cadr c) ,st))))
             ,@(unless (null dat)
                       `(((and (> (length ,ll) 0) (digit-char-p (char ,ll 0)))
                         (,dat ,ll))))

             ;; default section
             ,@(unless (null def)
                       `((t (,def ,ll))))
))))


(defun parse-ioz-line (l obj)
  (parse-ioz-line-cond l 
                       ("Run began:" (lambda (v) (setf (ioz-test-when obj) v)))
                       ("File size set to" (lambda (v) (setf (ioz-test-size obj)
                                                             (* 1024 (parse-integer v :junk-allowed t)))))
                       ("Command line used:" (lambda (v) (setf (ioz-test-cmd obj) v)))
                       (data (lambda (v) (format t "Data: ~a~%" v)))
                       (nil (lambda (v) (format t "Unknown: ~a~%" v))))
  obj)


(defun parse-ioz (fname)
  (let ((obj (make-ioz-test)))
    (with-open-file (in fname)
      (with-standard-io-syntax
        (loop for l = (read-line in nil nil)
           until (null l)
           do (setf obj (parse-ioz-line l obj)))))
    obj))

