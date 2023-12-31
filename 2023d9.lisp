;;;day9
(ql:quickload 'uiop)

(defconstant +day-number+ 9)
(defconstant +working-dir+ (uiop:truenamize "~/aoc_2023/"))
(defconstant +input-name-template+ "2023d~dinput.txt")

(defconstant +test-input+
  '((0 3 6 9 12 15)
    (1 3 6 10 15 21)
    (10 13 16 21 30 45)))

(defun parse-input (lines)
  (labels ((line-as-numbers (l)
             (mapcar #'parse-integer
                     (uiop:split-string l :separator '(#\space)))))
    (mapcar #'line-as-numbers lines)))

(defun p1 (data-list)
  (reduce #'+
          (mapcar (lambda (l)
                    (reduce #'+ l))
                  (mapcar (lambda (m)
                            (mapcar #'lastc m))
                          (mapcar #'differences data-list)))))

(defun p2 ()
  )


(defun succ-pairs (fn lst)
  (loop for idx from 0
        for a = (nth idx lst)
        for b = (nth (+ 1 idx) lst)
        until (null b)
        collect (funcall fn a b)))

(defun differences (numbers)
  (labels ((rec (lst acc)
             
             (if (every #'zerop lst)
                 (nreverse acc)
                 (progn
                   (push lst acc)
                   (rec (reverse (succ-pairs #'- (reverse lst))) acc)))))
    (rec numbers nil)))

(defun lastc (l)
  (car (last l)))

(defun main ()
  (let* ((infile-name (format nil +input-name-template+ +day-number+))
         (input-lines (uiop:read-file-lines infile-name))
         (data (parse-input input-lines)))
    (fresh-line)
    (princ "part 1: ")
    (princ (p1 data))
    ;; (fresh-line)
    ;; (princ "part 2: ")
    ;; (princ (reduce #'+ (p2 data)))
    ))
