;;;day11
(ql:quickload '(uiop alexandria))

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

(defun read-input (stream)
  (let ((my-universe (make-universe))
        (width 0)
        (height 0))
    (do ((ch (read-char stream nil) (read-char stream nil))
         (ch-idx 0 (1+ ch-idx)))
        ((null ch))
      (case ch
        (#\# (push (list height ch-idx)
                   (universe-locs my-universe)))
        (#\newline (when (zerop height)
                     (setf width ch-idx))
                   (incf height)
                   (setf ch-idx -1))))
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

(defun elts-between (start end cands)
  (let ((mn (min start end))
        (mx (max start end)))
   (remove-if (lambda (elm)
                (or (< elm mn mx)
                    (> elm mx mn)))
              cands)))

(defun p1 (my-universe)
  (let (distances-list)
    (alexandria:map-combinations
     (lambda (pt-list)
       (let* ((pt1 (car pt-list))
             (pt2 (cadr pt-list))
             (md (man-dist pt1 pt2))
             (x-empties (length (elts-between (cadr pt1) (cadr pt2) (universe-empty-cols my-universe))))
             (y-empties (length (elt-between (car pt1) (car pt2) (universe-empty-rows my-universe)))))
         (push (+ md x-empties y-empties)
               distances-list)))
     (universe-locs my-universe) :length 2 :copy nil)
    (reduce #'+ distances-list))) 

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
