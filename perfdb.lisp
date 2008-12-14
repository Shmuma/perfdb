(defvar *ioz-max-block* 30)


;; iozone test result
(defstruct ioz-test
  label
  when
  cmd
  size
  (read (make-array (list *ioz-max-block*)))
  (write (make-array (list *ioz-max-block*)))
  (rread (make-array (list *ioz-max-block*)))
  (rwrite (make-array (list *ioz-max-block*)))
  (blocks (make-array (list *ioz-max-block*)))
)

;; iozone test db
(defvar *ioz-db* nil)


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


(defun string2numlist (l)
  "Convert string with numbers list separated by spaces to list"
  (let* ((r 
          (reduce (lambda (st c) 
                    (let ((lst (car st))
                          (prv (cadr st))
                          (d   (digit-char-p c)))
                      (if d
                          (if (null prv) 
                              (list lst d)
                              (list lst (+ (* 10 prv) d)))
                         (if (null prv)
                             (list lst nil)
                             (list (append lst (list prv)) nil)))))
                  l :initial-value '(nil nil)))
         (lst (car r))
         (prv (cadr r)))
    (if (null prv)
        lst
        (append lst (list prv)))))


(defun parse-ioz-data-line (l obj)
  (let* ((lst (string2numlist l))
         (blk nil)
         (ind 0)
         (wr nil)
         (rd nil)
         (rr nil)
         (rw nil))
    (pop lst)                           ; drop size
    (setf blk (* 1024 (pop lst)))       ; block size
    (setf wr (pop lst))                 ; write
    (pop lst)                           ; drop rewrite
    (setf rd (pop lst))                 ; read
    (pop lst)                           ; drop reread
    (unless (null lst)
      (setf rr (pop lst)))              ; random read
    (unless (null lst)
      (setf rw (pop lst)))              ; random write
    (setf ind (floor (log blk 2)))
    (setf (aref (ioz-test-read obj) ind) rd)
    (setf (aref (ioz-test-write obj) ind) wr)
    (setf (aref (ioz-test-rread obj) ind) rr)
    (setf (aref (ioz-test-rwrite obj) ind) rw)
    (setf (aref (ioz-test-blocks obj) ind) (list rd wr rr rw))))



(defun parse-ioz-line (l obj)
  (parse-ioz-line-cond l 
                       ("Run began:" (lambda (v) (setf (ioz-test-when obj) v)))
                       ("File size set to" (lambda (v) (setf (ioz-test-size obj)
                                                             (* 1024 (parse-integer v :junk-allowed t)))))
                       ("Command line used:" (lambda (v) (setf (ioz-test-cmd obj) v)))
                       (data (lambda  (v) (parse-ioz-data-line v obj)))
;;                       (nil (lambda (v) (format t "Unknown: ~a~%" v))

                       )
  obj)


(defun parse-ioz (fname &optional label)
  (let ((obj (make-ioz-test :label label)))
    (with-open-file (in fname)
      (with-standard-io-syntax
        (loop for l = (read-line in nil nil)
           until (null l)
           do (setf obj (parse-ioz-line l obj)))))
    obj))


(defun ioz-add (fname &optional label)
  (let ((obj (parse-ioz fname label)))
    (if (ioz-test-p obj)
        (push obj *ioz-db*))
    t))


(defun perfdb-save (&optional (fname "perfdb.db"))
  (with-open-file (f fname :direction :output :if-exists :rename)
    (with-standard-io-syntax
      ;; iozone test results
      (write *ioz-db* :stream f)
      t)))


(defun perfdb-read (&optional (fname "perfdb.db"))
  (with-open-file (f fname :if-does-not-exist nil)
    (if (null f)
        nil
        (with-standard-io-syntax 
          (setf *ioz-db* (read f))
          t))))


(defun perfdb-clear ()
    (setf *ioz-db* nil))


(defun perfdb-show ()
  (let ((i 0))
    (dolist (test *ioz-db*)
      (format t "~a: ~a~20T(~a)~%" i (ioz-test-label test) (ioz-test-when test))
      (incf i))))