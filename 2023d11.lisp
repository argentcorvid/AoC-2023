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
                     (setf width (1- ch-idx)))
                   (incf height)
                   (setf ch-idx -1)))) ; gets incremented to 0 entering next iteration
    (setf (universe-empty-rows my-universe)
          (nset-difference
           (alexandria:iota height)
           (mapcar #'car (universe-locs my-universe))))
    (setf (universe-empty-cols my-universe)
          (nset-difference
           (alexandria:iota width)
           (mapcar #'cadr (universe-locs my-universe))))
    my-universe))

(defun man-dist (pt1 pt2)
  "find the 'manhattan' distance between 2 points"
    (reduce #'+ (mapcar (lambda (a b)
                          (abs (- a b)))
                 pt1 pt2)))

(defun elts-between (start end cands)
  (when (= start end)
    (return-from elts-between nil))
  (let ((mn (min start end))
        (mx (max start end)))
    (remove-if (lambda (elm)
                 (or (< elm mn mx)
                     (< mn mx elm)))
               cands)))

(defun p1 (my-universe)
  (let (distances-list)
    (alexandria:map-combinations
     (lambda (pt-list)
       (let* ((pt1 (car pt-list))
             (pt2 (cadr pt-list))
             (md (man-dist pt1 pt2))
             (x-empties (length (elts-between (cadr pt1) (cadr pt2) (universe-empty-cols my-universe))))
             (y-empties (length (elts-between (car pt1) (car pt2) (universe-empty-rows my-universe)))))
         (push (+ md x-empties y-empties)
               distances-list)
         ))
     (universe-locs my-universe) :length 2 :copy nil)
    
    (reduce #'+ distances-list))) 

(defun p2 ()
  )

(defun test (&optional (part 1))
  (when (= part 1)
    (let ((res (with-input-from-string
                   (s +test-input+)
                 (p1 (read-input s))))
          (exp 374))
      (if (= exp res)
          (format t "~&p1 test pass!")
          (format t "~&p1 test fail with ~a. should be ~a" res exp)))))

(defun main ()
  (let ((infile-name (format nil +input-name-template+ +day-number+)))
    (with-open-file (f infile-name)
      (let ((universe (read-input f)))
        (fresh-line)
        (princ "part 1: ")
        (princ (p1 universe))))
    ;; (fresh-line)
    ;; (princ "part 2: ")
    ;; (princ (p2 data))
    ))
