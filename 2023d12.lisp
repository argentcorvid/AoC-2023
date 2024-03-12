;;;2023 day 12
(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:str
                  :alexandria
                  :defclass-std
                  :fare-memoization)))

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

  (defconstant +test-input-1+ '("???.### 1,1,3"           ; ? unknown
                              ".??..??...?##. 1,1,3"      ; # BROKEN
                              "?#?#?#?#?#?#?#? 1,3,1,6"   ; . good
                              "????.#...#... 4,1,1"
                              "????.######..#####. 1,6,5"
                              "?###???????? 3,2,1"))
  (defconstant +test-results+ '(1 4 1 1 4 10)))


(defun parse-input (lines)
  (mapcar #'(lambda (l)
              (destructuring-bind (spring-map groups) (str:split #\space l)
                (list spring-map (mapcar #'(lambda (n)
                                         (parse-integer n))
                                     (str:split #\, groups)))))
          lines))

;; "backtracking"
;; from https://qsantos.fr/2024/01/04/dynamic-programming-is-not-black-magic
(defun p1-backtrack-recur (condition-map groups &optional (total 0))
  (declare (optimize (debug 3) (safety 3)))
  (when (null groups)
    (if (find #\# condition-map)
        (return-from p1-recur 0)
        (return-from p1-recur 1)))
  (when (= 0 (length condition-map))
    (if (null groups)
        (return-from p1-recur 1)
        (return-from p1-recur 0)))
  (let ((res 0)
        (cml (length condition-map)))
    (when (find (schar condition-map 0) ".?")
      (incf res (p1-recur (subseq condition-map 1) groups)))
    (when (find (schar condition-map 0) "#?")
      (when (and (<= (first groups) cml)
                 (not (find #\. (subseq condition-map 0 (first groups))))
                 (or (= (first groups) cml)
                     (not (char= (schar condition-map (first groups))
                                 #\#))))
        (incf res (p1-recur (if (= cml (first groups))
                                ""
                                (subseq condition-map (+ 1 (first groups))))
                            (rest groups)))))
    res)) 

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
