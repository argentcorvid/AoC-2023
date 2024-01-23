;;;day5
(ql:quickload '(uiop str alexandria defclass-std))
(import '(alexandria:iota defclass-std:defclass/std))

(defconstant +day-number+ 5)
(defconstant +working-dir+ (uiop:truenamize "~/aoc_2023/"))
(defconstant +input-name-template+ "2023d~dinput.txt")

(defconstant +test-input+
  (str:split #\newline
             '("seeds: 79 14 55 13

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
  ((src-type dest-type :std "" :type string)
   (src-list dest-list :std ())
   ))

(defmethod add-range ((obj garden-map) src-start dest-start rng-length)
  (push (list src-start rng-length) (src-list obj))
  (push (list dest-start rng-length (dest-list obj)))
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
  (loop for current in (src-list obj)
        for idx from 0
        with target = source
        for start = (first current)
        for end = (+ (cadr current) (first current))
        when (and (<= start source)
                  (< source end))
         do (setf target (+ (- source start) (first (nth idx (dest-list obj)))))
            (loop-finish)
        finally (return target)))

(defparameter maps '(seed-soil 
                     soil-fert 
                     fert-watr 
                     watr-lite 
                     lite-temp 
                     temp-humd
                     humd-locs))

(defun parse-input (lines)
  (let (seeds
        seed-soil 
        soil-fert 
        fert-watr 
        watr-lite 
        lite-temp 
        temp-humd
        humd-locs)
    ))

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
