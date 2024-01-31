;;;day5
(ql:quickload '(uiop str alexandria defclass-std))
;; (ql:quickload :lparallel)
(import '(alexandria:iota
          alexandria:assoc-value
          alexandria:lastcar
          alexandria:last-elt
          alexandria:curry
          alexandria:compose
          defclass-std:defclass/std))

(defconstant +day-number+ 5)
(defconstant +working-dir+ (uiop:truenamize "~/aoc_2023/"))
(defconstant +input-name-template+ "2023d~dinput.txt")

(defconstant +test-input+
  (mapcar #'uiop:stripln (str:split #\newline
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
1 0 69

humidity-to-location map:
60 56 37
56 93 4")))

(defclass/std garden-map nil
  ((src-list dest-list :std ())
   (next-map :std "")))

(defmethod add-range ((obj garden-map) dest-start src-start rng-length)
  (push (list src-start rng-length) (src-list obj))
  (push (list dest-start rng-length) (dest-list obj))
  )

(defmethod sort-ranges ((obj garden-map))
  (let ((idx-list
          (sort (iota (length (src-list obj)))
                #'<
                :key (lambda (idx)
                       (first (nth idx (src-list obj)))))))
    (setf (src-list obj) (loop for i in idx-list collecting (nth i (src-list obj))))
    (setf (dest-list obj) (loop for i in idx-list collecting (nth i (dest-list obj))))))

(defun targetp (source itm)
  (and (<= (first itm) source)
       (<  source (+ (lastcar itm) (first itm)))))

(defmethod get-target ((obj garden-map) source)
  (declare (fixnum source))
  (let ((target source)
        found-range)
    
    (when (setf found-range (position-if (curry #'targetp source) (src-list obj)))
      (setf target (+ (- source
                         (first (nth found-range (src-list obj))))
                      (first (nth found-range (dest-list obj))))))
    target))

(defmethod split-input-range ((gmap garden-map) input-range)
  (let* ((input-start (first input-range))
         (input-end (apply '+ input-range))
         (src-ranges (src-list gmap))
         split)
    (dolist (srange src-ranges split)
      (let* ((send (apply '+ srange))
             (left-part (list input-start (min input-end (first srange))))
             (mid-part (list (max input-start (first srange)) (min send input-end)))
             (right-part (list (max send input-start) input-end)))
        (when (apply '< left-part)
          (pushnew left-part split :test 'equal))
        (when (apply '< mid-part)
          (pushnew mid-part split :test 'equal))
        (when (apply '< right-part)
          (pushnew right-part split :test 'equal))))))

(defun parse-input (lines)
  (let ((seeds (mapcar #'parse-integer (rest (str:split-omit-nulls #\space (first lines)))))
        (map-alist ())
        (cur-name "")
        (next-name "")
        (names ()))
    (flet ((add-line (l)
             (apply #'add-range
                    (assoc-value map-alist cur-name :test #'string=)
                    (mapcar #'parse-integer (str:split-omit-nulls #\space l)))))
      (dolist (l (rest lines))
        (declare (string l))
        (cond ((string= l "") nil)
              ((equal #\: (last-elt l))
               (setf names (mapcar (curry #'str:substring 0 4)
                                   (str:split
                                    "-to-"
                                    l
                                    :end (position #\space l))))
               (setf cur-name (first names)
                     next-name (second names))) 
              ((null (assoc cur-name map-alist :test #'string=))
               (push (cons cur-name (make-instance 'garden-map :next-map next-name)) map-alist)
               (add-line l))
              (t (add-line l)))))
    (mapc #'(lambda (gmap)
              (sort-ranges (cdr gmap)))
          map-alist)
    (list seeds map-alist)))

(defun p1 (data)
  (let ((seeds (first data))
        (maps (second data))
        (res ()))
    (labels ((recur-lookup (mapname number)
               (declare (simple-string mapname)
                        (fixnum number))
               (let ((current-map (assoc-value maps mapname :test #'string=)))
                 (if (string= mapname "loca")
                     number
                     (recur-lookup (next-map current-map)
                                   (get-target current-map number))))))
      (setf res (mapcar (curry #'recur-lookup "seed") seeds))
      (values (apply #'min res) res)))) 

(defun p2 (data)
  (let ((seeds (loop for (start length) on (first data)
                     by #'cddr
                     collect (list start length)))
        (maps (second data))
        (res ()))
    (labels ((recur-lookup2 (mapname input-ranges)
               (let ((current-map (assoc-value maps mapname :test #'string=)))
                 (if (string= mapname "loca"))
                 input-ranges
                 (recur-lookup2 (nextmap current-map)
                                (get-target-ranges current-map input-ranges)))))
      (setf res (mapcar (curry #'recur-lookup2 "seed") seeds))
      (values (apply #'min res) res))))

(defun brute-force-p2 (data)
  (let ((seeds (first data))
        (maps (second data)))
    (setf seeds (loop for (seed length) on seeds by #'cddr nconcing (iota length :start seed))) ; crashes lisp - too many numbers!
    (p1 (list seeds maps))))

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
