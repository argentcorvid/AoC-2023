;;;day8
(ql:quickload '(uiop serapeum))

(setf *print-circle* t)

(defconstant +day-number+ 8)
(defconstant +working-dir+ (uiop:truenamize "~/aoc_2023/"))
(defconstant +input-name-template+ "2023d~dinput.txt")

(defconstant +test-input-1+
  '"RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)")

(defconstant +test-input-2+
  '"LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)")

(defstruct tree-node (node-name :type string)
           (step-left :type tree-node)
           (step-right :type tree-node))

(defun parse-input (lines)
  (let* ((input-copy (copy-list lines))
         (directions (map 'list #'identity
                          (pop input-copy)))
         (mappings (make-hash-table :test #'equal)) ;; or do i want a hash here? struct?
         keys
         targets)
    (pop input-copy) ;; empty
    (dolist (l input-copy)
      (push (subseq l 0 3) keys) ;; subseq 0-index, end is not included
      (push (list (subseq l 7 10) (subseq l 12 16))
            targets))
    (serapeum:pairhash keys targets mappings) ;; zip-to-hash
    (setf (cdr (last directions)) directions) ; ;make infinite circular list
    (list directions mappings)))

(defun p1 (actions)
  (loop with loc = "AAA"
        for dir in (first actions)
        with maps = (second actions)
        until (equal loc "ZZZ")
        for lookup = (gethash maps loc)
        when (equal dir #\L)
          collect (first lookup) into path
        else
          collect (second lookup) into path
        count lookup into len
        do (setf loc (first path))
        finally (return (values len path)))) 

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
