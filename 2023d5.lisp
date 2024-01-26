;;;day5
(ql:quickload '(uiop str alexandria defclass-std))
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

(defmethod get-target ((obj garden-map) source)
  (let ((target source)
        found-range)
    (flet ((targetp (itm)
             (and (<= (first itm) source)
                  (<  source (+ (lastcar itm) (first itm))))))
      (when (setf found-range (position-if #'targetp (src-list obj)))
        (setf target (+ (- source
                           (first (nth found-range (src-list obj))))
                        (first (nth found-range (dest-list obj))))))
      target)))

(defun parse-input (lines)
  (let ((seeds (mapcar #'parse-integer (rest (str:split-omit-nulls #\space (first lines)))))
        ;(maps (make-hash-table :test #'equal))
        (map-alist ())
        (cur-name "")
        (next-name "")
        (names ()))
    (flet ((add-line (l)
             (apply #'add-range
                    ;(gethash cur-name maps)
                    (assoc-value cur-name map-alist :test #'string=)
                    (mapcar #'parse-integer (str:split-omit-nulls #\space l)))))
      (dolist (l (rest lines))
        (cond ((string= l "") nil)
              ((equal #\: (last-elt l))
               (setf names (mapcar
                              (curry #'str:substring 0 4)
                              (str:split
                               "-to-"
                               l
                               :end (position #\space l))))
               (setf cur-name (first names)
                     next-name (second names))) 
              ((null (assoc cur-name map-alist :test #'string=))
                    ;(gethash cur-name maps)
                
               ;(setf (gethash cur-name maps) (make-instance 'garden-map :next-map next-name))
               (push (cons cur-name (make-instance 'garden-map :next-map next-name)) map-alist)
               (add-line l))
              (t (add-line l)))))
    ;; (maphash (lambda (mapname gmap)
    ;;            (declare (ignorable mapname))
    ;;            (sort-ranges gmap)) maps)
    (mapc #'(lambda (gmap)
              (sort-ranges (cdr gmap)))
          map-alist)
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
