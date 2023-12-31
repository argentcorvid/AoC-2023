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

(defun firsts-of (l)
  (mapcar #'first l))

(defun reduce-subt (l)
  (reduce #'- l))

(defun p2 (data-list)
  (let ((first-columns
          (mapcar #'reverse (mapcar #'firsts-of
                    (mapcar #'differences data-list)))))
    (labels ((rec (lst &optional (acc 0))
             (if (null lst)
                 acc
                 (progn (setf acc (+ (- acc) (pop lst)))
                        (rec lst acc)))))
      (reduce #'+ (mapcar #'rec first-columns)))))


(defun succ-pairs (fn lst)
  (loop for idx from 0
        for a = (nth idx lst)
        for b = (nth (+ 1 idx) lst)
        until (null b)
        collect (funcall fn a b)))

(defun differences (numbers)
  (labels ((rec (lst acc)
             (push lst acc)
             (if (every #'zerop lst)
                 (nreverse acc)
                 (rec (reverse (succ-pairs #'- (reverse lst))) acc))))
    (rec numbers nil)))

(defun lastc (l)
  (car (last l)))

(defun main ()
  (let* ((infile-name (format nil +input-name-template+ +day-number+))
         (input-lines (uiop:read-file-lines infile-name))
         (data (parse-input input-lines)))
    (fresh-line)
    (princ "part 1: ")
    (princ (time (p1 data)))
    (fresh-line)
    (princ "part 2: ")
    (princ (time (p2 data)))
    (fresh-line)
    (princ "part 2, but using part1 with reversed data: ")
    (fresh-line)
    (princ (time (p1 (mapcar #'reverse data))))
    ))
