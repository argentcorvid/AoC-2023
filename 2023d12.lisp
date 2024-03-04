;;;2023 day 12
(eval-when (:compile-toplevel :load-toplevel :execute) (ql:quickload '(:str :alexandria :defclass-std)))

(eval-when (:compile-toplevel)

  (import '(defclass-std:defclass/std
            alexandria:curry
            alexandria:rcurry
            alexandria:compose
            str:split
            str:split-omit-nulls ))
  (defconstant +day-number+ 12)
  (defconstant +working-dir+ (uiop:truenamize "~/aoc_2023/"))
  (defconstant +input-name-template+ "2023d~dinput.txt")

  (defconstant +test-input-1+ '("???.### 1,1,3"
                              ".??..??...?##. 1,1,3"
                              "?#?#?#?#?#?#?#? 1,3,1,6"
                              "????.#...#... 4,1,1"
                              "????.######..#####. 1,6,5"
                              "?###???????? 3,2,1")))

(defclass/std spring-record nil
  ((conditions groups :with)))

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
    (princ (p1 data))
    (fresh-line)
    (princ "part 2: ")
    (princ (p2 data))))
