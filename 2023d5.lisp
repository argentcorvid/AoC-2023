;;;day5
(ql:quickload '(:alexandria
                :defclass-std
                :str))

(import '(alexandria:assoc-value
          alexandria:compose
          alexandria:curry
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
  ((start end length :type fixnum :with)))

(defmethod make-range-with-end ((start fixnum) (end fixnum))
  (make-instance 'range :range-start start :range-end end :range-length (- end start)))

;; methods must have same number of args :(
;; (defmethod make-range-from-start-to-end ((s-e cons))
;;   (if (= 2 (length s-e))
;;       (make-range-from-start-to-end (first s-e) (second s-e))
;;       (error "argument must be list of numbers of length 2")))

(defmethod make-range-with-length ((start fixnum) (length fixnum))
  (make-instance 'range :range-start start :range-end (+ start length) :range-length length))

;; (defmethod make-range-from-start-to-length ((s-l cons))
;;   (if (= 2 (length s-l))
;;       (make-range-from-start-to-length (first s-l) (second s-l))
;;       (error "argument must be list of numbers of length 2")))

(defmethod range< ((r1 range) (r2 range))
  (and (< (range-start r1) (range-start r2))
       (< (range-length r1) (range-length r2))))

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
