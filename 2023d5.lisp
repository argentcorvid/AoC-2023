;;;day5
(ql:quickload '(:alexandria
                :defclass-std
                :str))

(import '(alexandria:assoc-value
          alexandria:compose
          alexandria:curry
          alexandria:xor
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
  (and (< (range-start r1) (range-start r2))
       (<= (range-length r1) (range-length r2))))

(defmethod range= ((r1 range) (r2 range))
  (and (= (range-start r1) (range-start r2))
       (= (range-end r1) (range-end r2))))

(defmethod range-overlap? ((r1 range) (r2 range))
  (let ((ranges (sort (list r1 r2) #'range<)))
    (and (<= (range-start (first ranges)) (range-start (second ranges)))
         (>= (range-end (first ranges)) (range-start (second ranges))))))

(defmethod range-split ((rng range) (index fixnum))
  ;; (list (fste start index-1) (fste index end))
  )

(defmethod range-split ((r1 range) (r2 range))
  (cond
    ((range-overlap? r1 r2)
     (list (make-range-with-end)))))

(defclass/std range-mapping nil
  ((input-range output-range :type range)))



(defclass/std garden-map nil
  ((in-name out-name :type string)
   (mappings :type cons)))

(defun parse-input (lines)
  )

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
