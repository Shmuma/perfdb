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
                       (data (lambda  (v) (parse-ioz-data-line v obj))))
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
(defun ioz-add (fname &optional label disks)
  "Append data from external file"
  (let ((obj (parse-ioz fname label disks)))
    (if (ioz-test-p obj)
        (setf *ioz-db* (append *ioz-db* (list obj))))
    t))


(defun ioz-pop ()
  (setf *ioz-db* (reverse (cdr (reverse *ioz-db*))))
  (length *ioz-db*))


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


(defun ioz-get-objs (&rest idxs)
  "Convert list of tests indices to list of objects"
  (mapcar (lambda (i) (ioz-get i))
          idxs))


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


(defun ioz-compare-label (kind)
  (cond ((eq kind 'read) "Read")
        ((eq kind 'write) "Write")
        ((eq kind 'rread) "RRead")
        ((eq kind 'rwrite) "RWrite")))


(defun ioz-array2hash (arr)
  "Converts test result array to hash"
  (let ((res (make-hash-table)))
    (loop for i from 0 to (1- *ioz-max-block*)
         unless (and (numberp (aref arr i)) (zerop (aref arr i)))
         do (setf (gethash (expt 2 i) res) (aref arr i)))
    res))


(defun get-iops-label (iops)
  (if iops "IO/s" "MB/s"))


(defun get-norm-label (norm)
  (if norm "per disk" "all disks"))


(defun get-ioz-text-formatter (objs kind iops norm)
  (let ((label nil))
    (cond ((eq kind 'result)
           (lambda (part &optional block data)
             (cond ((eq part 'pre) nil)
                   ((eq part 'post) nil)
                   ((eq part 'title)
                    (progn
                      (format t "Test: ~a~%" (ioz-test-label (car objs)))
                      (format t "Data: ~a, ~a~%~%" (get-iops-label iops) (get-norm-label norm))))
                   ((eq part 'header)
                    (format t "~{~12a~}~%" '("Block" "Read" "Write" "RRead" "RWrite")))
                   ((eq part 'data)
                    (format t "~12d~{~12,4f~}~%" block data)))))
          ((eq kind 'compare)
           (lambda (part &optional block data)
             (cond ((eq part 'pre) nil)
                   ((eq part 'post) nil)
                   ((eq part 'set-label) (setf label data))
                   ((eq part 'title)
                    (progn
                      (format t "Compare:~%~{~a~%~}"
                              (loop for obj in objs
                                   collect (format nil "~C~a" #\Tab (ioz-test-label obj))))
                      (format t "Data: ~a, ~a~%~%" (get-iops-label iops) (get-norm-label norm))))
                   ((eq part 'header)
                    (format t "~12@a~{~12@a~}~%" "Block"
                            (loop for i from 1 to (length objs)
                                 collect (format nil "~a ~d" label i))))
                   ((eq part 'data)
                    (format t "~12d~{~12,4f~}~%" block data))))))))



(defun get-ioz-wiki-formatter (objs kind iops norm)
  (let ((label nil))
    (cond ((eq kind 'result)
           (lambda (part &optional block data)
             (cond ((eq part 'pre) nil)
                   ((eq part 'post) (format t "|#~%"))
                   ((eq part 'title)
                    (progn
                      (format t "Test: ~a~%" (ioz-test-label (car objs)))
                      (format t "Data: ~a, ~a~%~%" (get-iops-label iops) (get-norm-label norm))
                      (format t "#|~%")))
                   ((eq part 'header)
                    (format t "||~{**~a**|~}|~%" '("Block" "Read" "Write" "RRead" "RWrite")))
                   ((eq part 'data)
                    (format t "||~d|~{~,4f|~}|~%" block data)))))
          ((eq kind 'compare)
           (lambda (part &optional block data)
             (cond ((eq part 'pre) nil)
                   ((eq part 'post) (format t "|#~%"))
                   ((eq part 'set-label) (setf label data))
                   ((eq part 'title)
                    (progn
                      (format t "Compare:~%~{~a~%~}"
                              (loop for obj in objs
                                   collect (format nil "~C~a" #\Tab (ioz-test-label obj))))
                      (format t "Data: ~a, ~a~%~%" (get-iops-label iops) (get-norm-label norm))
                      (format t "#|~%")))
                   ((eq part 'header)
                    (format t "||~{**~a**|~}|~%"
                            (append '("Block")
                            (loop for i from 1 to (length objs)
                                 collect (format nil "~a ~d" label i)))))
                   ((eq part 'data)
                    (format t "||~d|~{~,4f|~}|~%" block data))))))))


(defun make-plot (&key out tmpl data title yaxis opts (fields 3))
  (let ((cwd (truename "."))
        (file (format nil "/tmp/perfdb-~a" (gentemp)))
        (opts (format nil "~{~a='~a' ~}" opts)))
    (with-open-file (f file :direction :output :if-exists :supersede)
      (with-standard-io-syntax
        (write-string data f)))
    (asdf:run-shell-command "GDFONTPATH=~a ploticus -font FreeSans -png -o ~a file='~a' fields='~a' title='~a' yaxis='~a' ~a ~a.pls" cwd out file fields title yaxis opts tmpl)
;    (format t               "GDFONTPATH=~a ploticus -font FreeSans -png -o ~a file='~a' fields='~a' title='~a' yaxis='~a' ~a ~a.pls" cwd out file fields title yaxis opts tmpl)
;    (sb-posix:unlink file)
))




(defun get-ioz-plot-formatter (objs kind iops norm out)
  (let ((csv "")
        (label nil))
    (cond ((eq kind 'result)
           (lambda (part &optional block data)
             (cond ((eq part 'pre) (setf csv ""))
                   ((eq part 'post)
                    (make-plot :out out :tmpl "iozone" :data csv
                               :title (format nil "~a, ~a, ~a" (ioz-test-label (car objs)) (get-iops-label iops) (get-norm-label norm))
                               :yaxis (get-iops-label iops)
                               :opts (list "iops" (if iops "1" "0"))
                               :fields (1+ (length objs))))
                   ((eq part 'title) nil)
                   ((eq part 'header) nil)
                   ((eq part 'data) (setf csv (concatenate 'string csv (format nil "~d~{,~,4f~}~%" block data)))))))
          ((eq kind 'compare)
           (lambda (part &optional block data)
             (cond ((eq part 'pre) (setf csv ""))
                   ((eq part 'set-label) (setf label data))
                   ((eq part 'post)
                    (make-plot :out out :tmpl "compare" :data csv
                               :title (format nil "~{~a ~^vs ~}, ~a, ~a, ~a"
                                              (loop for obj in objs
                                                   collect (ioz-test-label obj))
                                              (get-iops-label iops) (get-norm-label norm) label)
                               :yaxis (get-iops-label iops)
                               :fields (1+ (length objs))
                               :opts (append (list "iops" (if iops "1" "0"))
                                             (loop for i from 1 to (length objs)
                                                  collect (format nil "label~d" i)
                                                  collect (ioz-test-label (nth (1- i) objs))))))
                   ((eq part 'title) nil)
                   ((eq part 'header) nil)
                   ((eq part 'data) (setf csv (concatenate 'string csv (format nil "~d~{,~,4f~}~%" block data))))))))))




(defun get-ioz-formatter (&key objs format kind iops norm out)
  (cond ((eq format 'text)
         (get-ioz-text-formatter objs kind iops norm))
        ((eq format 'wiki)
         (get-ioz-wiki-formatter objs kind iops norm))
        ((eq format 'plot)
         (get-ioz-plot-formatter objs kind iops norm out))
        (t (lambda (&optional a) (type-of a) nil))))



(defun ioz-show (obj &key (header t) iops norm (format 'text) out)
  (let ((form (get-ioz-formatter :objs (list obj) :iops iops :norm norm :format format :kind 'result :out out)))
    (funcall form 'pre)
    (if header
        (progn
          (funcall form 'title)
          (funcall form 'header)))
    (maphash (lambda (k d) (funcall form 'data k (ioz-filter-data k d :disks (ioz-test-disks obj) :iops iops :norm norm)))
             (ioz-array2hash (ioz-test-blocks obj)))
    (funcall form 'post)))



;; check for data present in any object in list
(defun ioz-have-any-data (objs power)
  (let ((res nil))
    (loop for obj in objs
       do (if (listp (aref (ioz-test-blocks obj) power))
              (setq res t)))
  res))



(defun ioz-compare (objs &key (header t) (kind 'read) iops norm (format 'text) out)
  (let ((form (get-ioz-formatter :objs objs :iops iops :norm norm :format format :kind 'compare :out out)))
    (funcall form 'pre)
    (funcall form 'set-label nil (ioz-compare-label kind))
    (if header
        (progn
          (funcall form 'title)
          (funcall form 'header)))
    (loop for i from 0 to (1- *ioz-max-block*)
       do (let ((key (expt 2 i)))
            (if (ioz-have-any-data objs i)
                (funcall form 'data key
                         (mapcar
                          (lambda (obj)
                            (let ((data (ioz-array2hash (ioz-extract obj :kind kind))))
                              (ioz-filter-data key (gethash key data) :disks (ioz-test-disks obj) :iops iops :norm norm)))
                          objs)))))
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


(defun ioz-make-plots (obj prefix)
  (ioz-show obj :iops nil  :norm nil :format 'plot :out (concatenate 'string prefix "-asis.png"))
  (ioz-show obj :iops nil  :norm t   :format 'plot :out (concatenate 'string prefix "-norm.png"))
  (ioz-show obj :iops t    :norm nil :format 'plot :out (concatenate 'string prefix "-iops.png"))
  (ioz-show obj :iops t    :norm t   :format 'plot :out (concatenate 'string prefix "-iops-norm.png")))


(defun ioz-make-compare-plots (objs kind prefix)
  (ioz-compare objs :iops nil  :norm nil :kind kind :format 'plot :out (concatenate 'string prefix "-asis.png"))
  (ioz-compare objs :iops nil  :norm t   :kind kind :format 'plot :out (concatenate 'string prefix "-norm.png"))
  (ioz-compare objs :iops t    :norm nil :kind kind :format 'plot :out (concatenate 'string prefix "-iops.png"))
  (ioz-compare objs :iops t    :norm t   :kind kind :format 'plot :out (concatenate 'string prefix "-iops-norm.png")))


;; Generate plots on which we compare several (up to ten) iozone tests.
;; Arguments:
;; 1. objs: a list of ioz-get results
;; 2. prefix: prefix string for file name
(defun ioz-make-all-compare-plots (objs prefix)
  (dolist (test '(read write rread rwrite))
    (let ((pref (format nil "~a-~a" prefix test)))
      (ioz-make-compare-plots objs test pref))))
