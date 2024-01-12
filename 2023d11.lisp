;;;day11
(ql:quickload '(uiop cl-ppcre alexandria))

(defconstant +day-number+ 11)
(defconstant +working-dir+ (uiop:truenamize "~/aoc_2023/"))
(defconstant +input-name-template+ "2023d~dinput.txt")

(defconstant +test-input+
"...#......
.......#..
#.........
..........
......#...
.#........
.........#
..........
.......#..
#...#.....")

(defstruct universe
  empty-rows
  empty-cols
  locs)

(defun parse-input (lines)
  (let ((regex (ppcre:create-scanner "#"))
        (my-universe (make-universe))
        (line-idx 0)
        (height (length lines))
        (width (length (first lines))))
    (dolist (l lines)
      (ppcre:do-matches (ms me regex l)
        (push (list line-idx ms) (universe-locs my-universe)))
      (incf line-idx))
    (setf (universe-empty-rows my-universe)
          (nset-difference
           (loop for x from 0 below height collecting x)
           (mapcar #'car (universe-locs my-universe))))
    (setf (universe-empty-cols my-universe)
          (nset-difference
           (loop for x from 0 below width collecting x)

           (mapcar #'cadr (universe-locs my-universe))))
    my-universe))

(defun read-input (stream)
  (let ((my-universe (make-universe))
        (width 0)
        (height 0)
        (total 0))
    (do ((ch (read-char stream) (read-char stream))
         (line-idx 0)
         (ch-idx 0 (1+ ch-idx)))
        ((null ch))
      (case ch
        (#\# (push (list line-idx ch-idx)
                   (universe-locs my-universe)))
        (#\newline (when (zerop line-idx)
                     (setf width ch-idx))
                   (incf line-idx)
                   (setf ch-idx 0)))
      (incf total))
    (setf height (floor total width))
    (setf (universe-empty-rows my-universe)
          (nset-difference
           (loop for x from 0 below height collecting x)
           (mapcar #'car (universe-locs my-universe))))
    (setf (universe-empty-cols my-universe)
          (nset-difference
           (loop for x from 0 below width collecting x)

           (mapcar #'cadr (universe-locs my-universe))))
    my-universe))

(defun man-dist (pt1 pt2)
  "find the 'manhattan' distance between 2 points"
  (reduce #'+ (mapcar (lambda (a b)
                        (abs (- a b)))
                      pt1 pt2)))

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
