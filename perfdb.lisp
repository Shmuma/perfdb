(defvar *ioz-max-block* 30)


;; iozone test result
(defstruct ioz-test
  label
  when
  cmd
  size
  disks
  (read (make-array (list *ioz-max-block*)))
  (write (make-array (list *ioz-max-block*)))
  (rread (make-array (list *ioz-max-block*)))
  (rwrite (make-array (list *ioz-max-block*)))
  (blocks (make-array (list *ioz-max-block*)))
)

;; iozone test db
(defvar *ioz-db* nil)


;; --------------------------------------------------
;; iozone parsing
;; --------------------------------------------------
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


(defun parse-ioz (fname &optional label disks)
  "Parse iozone test result file"
  (let ((obj (make-ioz-test :label label :disks disks)))
    (with-open-file (in fname)
      (with-standard-io-syntax
        (loop for l = (read-line in nil nil)
           until (null l)
           do (setf obj (parse-ioz-line l obj)))))
    obj))


;; --------------------------------------------------
;; Database management
(defun ioz-add (fname &optional label)
  "Append data from external file"
  (let ((obj (parse-ioz fname label)))
    (if (ioz-test-p obj)
        (push obj *ioz-db*))
    t))


(defun perfdb-save (&optional (fname "perfdb.db"))
  "Save database to external file"
  (with-open-file (f fname :direction :output :if-exists :rename)
    (with-standard-io-syntax
      ;; iozone test results
      (write *ioz-db* :stream f)
      t)))


(defun perfdb-load (&optional (fname "perfdb.db"))
  "Load database from extrnal file"
  (with-open-file (f fname :if-does-not-exist nil)
    (if (null f)
        nil
        (with-standard-io-syntax 
          (setf *ioz-db* (read f))
          t))))


(defun perfdb-clear ()
  "Resets database"
  (setf *ioz-db* nil))


(defun perfdb-show ()
  "Print summary of tests in DB"
  (let ((i 0))
    (dolist (test *ioz-db*)
      (format t "Test: ~10T~a~%" i)
      (format t "Label: ~10T~a~%"  (ioz-test-label test))
      (format t "When: ~10T~a~%"  (ioz-test-when test))
      (format t "Size: ~10T~a Gb~%" (/ (ioz-test-size test) (* 1024 1024 1024)))
      (format t "Disks: ~10T~a~%" (ioz-test-disks test))
      (format t "~%")
      (incf i))))


(defun ioz-get (index)
  "Get N'th entry in ioz db"
  (nth index *ioz-db*))


(defun ioz-search (label)
  "Lookup test result by label"
  (loop for test in *ioz-db*
       if (string-equal (ioz-test-label test) label)
       return test))


;; --------------------------------------------------
;; Reporting
;; --------------------------------------------------
(defun ioz-extract (obj &key kind)
  "Get data portion from test object. Kind is one of READ, WRITE, RREAD or RWRITE."
  (cond ((eq kind 'read)
         (ioz-test-read obj))
        ((eq kind 'write)
         (ioz-test-write obj))
        ((eq kind 'rread)
         (ioz-test-rread obj))
        ((eq kind 'rwrite)
         (ioz-test-rwrite obj))))


(defun ioz-array2hash (arr)
  "Converts test result array to hash"
  (let ((res (make-hash-table)))
    (loop for i from 0 to (1- *ioz-max-block*)
         unless (and (numberp (aref arr i)) (zerop (aref arr i)))
         do (setf (gethash (expt 2 i) res) (aref arr i)))
    res))


(defun ioz-compare (o1 o2 &key kind (iops nil) (norm nil))
  (if (and o1 o2)
      (let ((d1 (ioz-array2hash (ioz-extract o1 :kind kind)))
            (d2 (ioz-array2hash (ioz-extract o2 :kind kind)))
            (disks1 (if norm (ioz-test-disks o1) nil))
            (disks2 (if norm (ioz-test-disks o2) nil)))
        (format t "'~a' <=> '~a', ~a:~%" (ioz-test-label o1) (ioz-test-label o2) (if iops "IOPs" "KBps"))
        (maphash (lambda (k d)
                   (format t "~10d => ~10,2f ~10,2f~%" k
                           (get-ioz-val k d iops disks1)
                           (get-ioz-val k (gethash k d2) iops disks2))) d1))))


(defun get-ioz-text-formatter (obj kind iops norm)
  (cond ((eq kind 'result)
         (lambda (part &optional block data)
           (let ((o obj)
                 (i iops)
                 (n norm))
             (cond ((eq part 'pre) nil)
                   ((eq part 'post) nil)
                   ((eq part 'title)
                    (progn
                      (format t "Test: ~a~%" (ioz-test-label o))
                      (format t "Data: ~a, ~a~%~%" (if i "IO/s" "MB/s") (if n "per disk" "all disks"))))
                   ((eq part 'header)
                    (format t "~{~@12a~}~%" '("Block" "Read" "Write" "RRead" "RWrite")))
                   ((eq part 'data)
                    (format t "~12d~{~12,4f~}~%" block data))))))))


(defun get-ioz-formatter (&key obj format kind iops norm)
  (cond ((eq format 'text)
         (get-ioz-text-formatter obj kind iops norm))
        (t (lambda (&optional a) nil))))


(defun ioz-show (obj &key (header t) iops norm (format 'text))
  (let ((form (get-ioz-formatter :obj obj :iops iops :norm norm :format format :kind 'result)))
    (funcall form 'pre)
    (if header
        (progn
          (funcall form 'title)
          (funcall form 'header)))
    (maphash (lambda (k d) (funcall form 'data k (ioz-filter-data k d :disks (ioz-test-disks obj) :iops iops :norm norm)))
             (ioz-array2hash (ioz-test-blocks obj)))
    (funcall form 'post)))


(defun ioz-filter-data (block data &key disks iops norm)
  (let ((f (lambda (val)
              (let* ((mb (* 1024 1024))
                     (dat (* val 1024))
                     (res (if iops (/ dat block) (/ dat mb))))
                (if norm (/ res disks) res)))))
    (if (listp data)
        (map 'list f data)
        (funcall f data))))

;; ploticus -font FreeSans -png -o iozone2.png iozone.pls
