;;;day6
(ql:quickload '("uiop" "str"))

(defconstant +day-number+ 6)

(defconstant +test-input+
  '(:T (7 15 30)
    :D (9 40 200)))

(defconstant +full-input+
  '(:T (56     71     79     99)
    :D (334   1135   1350   2430)))

(defun p1 ()
  ) 

(defun p2 ()
  )

(defun main ()
  (let* ((data +full-input))
    (fresh-line)
    (princ "part 1: ")
    (princ (reduce #'+ (p1 data)))
    (fresh-line)
    (princ "part 2: ")
    (fresh-line)
    (princ (reduce #'+ (p2 data)))))
