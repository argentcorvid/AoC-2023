;;;day8
(ql:quickload '(uiop))

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

;; (defstruct tree-node (node-name :type string)
;;            (step-left :type tree-node)
;;            (step-right :type tree-node))

(defun parse-input (lines)
  (let* ((input-copy (copy-list lines))
         (directions (coerce (pop input-copy)
                          'list))
         (mappings (make-hash-table :test #'equal)) ;; tree? or do i want a hash here? struct?
         )
    (pop input-copy) ;; empty
    (dolist (l input-copy)
      (setf (gethash (subseq l 0 3) mappings)
            (list (subseq l 7 10) (subseq l 12 15))))
    (setf (cdr (last directions)) directions) ; ;make infinite circular list
    (list directions mappings)))

(defun p1 (actions)
  (loop with loc = "AAA"
        with maps = (second actions)
        for dir in (first actions)
        until (equal loc "ZZZ")
        for lookup = (gethash loc maps)
        when (equal dir #\L)
          collect (first lookup) into path
          and do (setf loc (first lookup))
        else
          collect (second lookup) into path
          and do (setf loc (second lookup))
        end
        count lookup into len
        finally (return (values len path)))) 

(defun p2 ()
  ( ))

(defun p1-test ()
  (let ((in-lines (uiop:split-string +test-input-1+ :separator '(#\newline))))
    (fresh-line)
    (princ "1st test path length: ")
    (princ (p1 (parse-input in-lines)))
    (setf in-lines (uiop:split-string +test-input-2+ :separator '(#\newline)))
    (fresh-line)
    (princ "2nd path length: ")
    (princ (p1 (parse-input in-lines)))))

(defun main ()
  (let* ((infile-name (format nil +input-name-template+ +day-number+))
         (input-lines (uiop:read-file-lines infile-name))
         (data (parse-input input-lines)))
    (fresh-line)
    (princ "part 1: ")
    (princ (p1 data))
    ;; (fresh-line)
    ;; (princ "part 2: ")
    ;; (fresh-line)
    ;; (princ (reduce #'+ (p2 data)))
    ))
