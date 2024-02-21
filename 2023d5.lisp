;;;day5
(ql:quickload '(:alexandria
                :defclass-std
                :str))

(import '(alexandria:assoc-value
          alexandria:compose
          alexandria:curry
          alexandria:last-elt
          defclass-std:class/std))

(defconstant +day-number+ 5)
(defconstant +working-dir+ (uiop:truenamize "~/aoc_2023/"))
(defconstant +input-name-template+ "2023d~dinput.txt")

(defconstant +test-input+ (str:split #\newline  
                           "seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69"))

(defclass/std range nil
  ((start end :type fixnum :with :std 0)
   (length :type fixnum :with :ri :std 0)))

(defmethod compute-length ((rng range))
  (with-slots (start end length) rng
    (setf length (- end start))))

(defmethod (setf range-start) :after (new-start (rng range))
  ":after method to compute new length of range when setting new start"
  (declare (ignore new-start))
  (compute-length rng))

(defmethod (setf range-end) :after (new-end (rng range))
  ":after method to compute new length of range when setting new end"
  (declare (ignore new-end))
  (compute-length rng))

;; (defmethod (setf range-length) :after (new (rng range))
;;   (declare (ignore new))
;;   ":after method to signal error if setting length by itself"
;;   (error "Setting length by itself is ambiguous in ~S" rng))

(defmethod initialize-instance :after ((rng range) &key)
  ":after method to ensure length is set correctly on initialization"
  (with-slots (start end length) rng
    (alexandria:if-let
        ((end-bound? (/= 0 end))
         (len-bound? (/= 0 length)))
      (error "Only one of 'end' or 'length' may be specified in initializing ~S" rng)
      (progn
        (when end-bound?
          (setf length (- end start)))
        (when len-bound?
          (setf end (+ length start)))))))

(defmethod range< ((r1 range) (r2 range))
  (< (range-start r1) (range-start r2)))

(defmethod range= ((r1 range) (r2 range))
  (and (= (range-start r1) (range-start r2))
       (= (range-end r1) (range-end r2))))

(defmethod range-overlap? ((r1 range) (r2 range))
  (let ((ranges (sort (list r1 r2) #'range<)))
    (and (<= (range-start (first ranges)) (range-start (second ranges)))
         (>= (range-end (first ranges)) (range-start (second ranges))))))

(defmethod range-split ((r1 range) (r2 range))
  (if (range-overlap? r1 r2)
      (list ())
      ()))

(defclass/std range-mapping nil
  ((input-range output-range :type range :r)))

(defmethod initialize-instance ((rm range-mapping) &key (in-start 0) (out-start 0) (length 0))
  (with-slots ((in input-range) (out output-range)) rm
    (setf in (make-instance 'range :start in-start :length length))
    (setf out (make-instance 'range :start out-start :length length))))

(defmethod initialize-instance :after ((mapping range-mapping) &key)
  (with-slots (input-range output-range) mapping
    (with-slots ((in-length length)) input-range
      (with-slots ((out-length length)) output-range
        (when (/= in-length out-length)
          (error "input and output range lengths must be equal in ~s" mapping))))))

(defmethod range-translate ((map range-mapping) (input-number integer))
  (with-accessors ((in input-range) (out output-range)) map
    (with-accessors ((in-start range-start)(in-end range-end)) in
      (with-accessors ((out-start range-start)) out
        (if (<= in-start input-number in-end)
            (+ out-start (- input-number in-start))
            input-number)))))

(defmethod range-translate ((map range-mapping) (input-range range))
  )

(defclass/std garden-map-entry nil
  ((in-name out-name :type string)
   (mappings :type cons :std ())))

(defmethod map-entry-sort ((gm garden-map-entry))
  (setf (mappings gm) (sort (mappings gm) #'range< :key #'input-range)))

(defmethod initialize-instance :after ((gm garden-map-entry) &key)
  (map-entry-sort gm))

(defmethod add-mapping ((gm garden-map-entry) (new-map range-mapping))
  (push new-map (mappings gm))
  (map-entry-sort gm))

(defun parse-input (lines)
  (let ((seeds (mapcar #'parse-integer (rest (str:split-omit-nulls #\space (first lines)))))
        (map-alist ())
        (cur-name "")
        (next-name "")
        (names ()))
    (dolist (l (rest lines))
      (cond ((string= l "") nil) ;do nothing
            ((equal #\: (last-elt l)) ; this line is a name, get ready for a new map
             (setf names (mapcar (curry #'str:substring 0 4)
                                 (str:split
                                  "-to-"
                                  l
                                  :end (position #\space l))))
             (setf cur-name (first names)
                   next-name (second names))) 
            ((null (assoc cur-name map-alist :test #'string=)) ;map is empty, 
             (let ((nums (mapcar #'parse-integer (str:split #\space l))))
               (push (cons cur-name (make-instance 'garden-map-entry :in-name cur-name :out-name next-name)) map-alist)
               (add-mapping (cdr (first map-alist))
                            (make-instance 'range-mapping :in-start (second nums) :out-start (first nums) :length (third nums)))))
            (t (let ((nums (mapcar #'parse-integer (str:split #\space l))))
                 (add-mapping (cdr (first map-alist))
                              (make-instance 'range-mapping :in-start (second nums) :out-start (first nums) :length (third nums)))))))
    
    (list seeds map-alist)))

(defun p1 ()
  ) 

(defun p2 ()
  )

(defun main ()
  (let* ((infile-name (format nil +input-name-template+ +day-number+))
         (input-lines (uiop:read-file-lines infile-name))
         (data (parse-input input-lines)))
    (fresh-line)
    (princ "part 1: ")
    (princ (reduce #'+ (p1 data)))
    (fresh-line)
    (princ "part 2: ")
    (fresh-line)
    (princ (reduce #'+ (p2 data)))))
