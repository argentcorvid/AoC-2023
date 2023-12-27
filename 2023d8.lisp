;;;day8
(ql:quickload 'uiop)

(defconstant +day-number+ 8)
(defconstant +working-dir+ (uiop:truenamize "~/aoc_2023/"))
(defconstant +input-name-template+ "2023d~dinput.txt")

(defconstant +test-input-1+
  '("RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)"))

(defconstant +test-input-2+
  '("LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)"))

(defstruct tree-node (node-name :type string)
           (step-left :type tree-node)
           (step-right :type tree-node))

(defun parse-input (lines)
  (let* ((input-copy (copy-list lines))
         (directions (pop input-copy))
         (mapping-tree (make-list 1)) ;; or do i want a hash here? struct?
         )
    (pop input-copy) ;; empty
    ()))

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
