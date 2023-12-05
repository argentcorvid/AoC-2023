;;;day1

(ql:quickload "uiop" :silent t)

(defconstant +day-number+ 1)
;(defconstant +working-dir+ #p"c:/Users/kdhoka/OneDrive - Emerson/Documents/AoC/2023")
(defconstant +working-dir+ (uiop:truenamize "~/aoc_2023/"))
(defconstant +input-name-template+ "2023d~dinput.txt")

(defconstant +test-input+ '("1abc2"
                            "pqr3stu8vwx"
                            "a1b2c3d4e5f"
                            "treb7uchet"))

(defconstant +test-input-2+ '("two1nine"
                              "eightwothree"
                              "abcone2threexyz"
                              "xtwone3four"
                              "4nineeightseven2"
                              "zoneight234"
                              "7pqrstsixteen"))

(defconstant +number-words+ (loop for x
                                  from 1 to 9
                                  collect (format nil "~r" x ))) ;; ~r is format converting number to words
(defconstant +digits+ (loop for x from 1 to 9
                            collect (format nil "~d" x)))

(defconstant +values+ (pairlis +digits+ ))

(uiop:chdir +working-dir+)

(defun get-line-value (line to-match)
  (let ((digit-location-l (loop for x in to-match
                                with m
                                do (setf m (search x line))
                                if m
                                  collect (cons x m)))
        (digit-location-r (loop for x in to-match
                                with m
                                do (setf m (search x line :from-end t))
                                if m
                                  collect (cons x m)))
        first-digit
        last-digit
        number-as-string)
    (setf first-digit (car (elt (sort digit-location-l '< :key 'cdr) 0)))
    (setf last-digit (car (elt (sort digit-location-r '> :key 'cdr) 0)))
    ;(setf last-digit (car (elt )))
    (setf number-as-string (concatenate 'string (string first-digit) (string last-digit)))
    ;(format t "~&~a~&F: ~a L: ~a~&" line first-digit last-digit)
    (parse-integer number-as-string)))

(defun get-cal (file-lines to-match) 
    (loop for line in file-lines
          sum (get-line-value line to-match)))

(let* ((infile-name (format nil +input-name-template+ +day-number+))
       (input-lines (uiop:read-file-lines infile-name))
       (d-and-w (append +number-words+ +digits+)))
  (format t "~&part 1: ")
  (if (eq 142 (get-cal +test-input+ +digits+))
      (princ (get-cal input-lines +digits+))
      (format t "test case incorrect"))
  (format t "~&part 2: ")
  (if (eq 281 (get-cal +test-input-2+ d-and-w))
      (princ (get-cal input-lines d-and-w))
      (format t "test case incorrect")))
