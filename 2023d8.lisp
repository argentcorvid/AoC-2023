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

(defconstant +test-input-3+
  '"LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)")

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

(defun brute-force-p2 (actions)
  (loop with locs = (loop for k being the hash-key of (second actions)
                          when (equal #\A (char k 2))
                           collect k)
        with maps = (second actions)
        for dir in (first actions)
        until (every (lambda (loc)
                       (equal #\Z (char loc 2)))
                     locs)
        for lookups = (mapcar (lambda (loc)
                                (gethash loc maps))
                              locs)
        when (equal dir #\L)
         do (setf locs (mapcar #'first lookups))
        else
         do (setf locs (mapcar #'second lookups))
        end
        do (print locs)
        count lookups))

(defun lcm-p2 (actions)
  (let* ((maps (second actions))
        (locs (loop for k being the hash-key of (second actions) when (equal #\A (char k 2)) collect k))
        (loc-idx 0)
        (steps-to-z (make-list (length locs) :initial-element 0)) )
    (dolist (start-loc locs (apply #'lcm steps-to-z))
        (loop for dir in (first actions)
            with step-loc = start-loc
            until (equal #\Z (char step-loc 2))
            when (equal #\L dir)
                do (setf step-loc (first (gethash step-loc maps)))
            else
              do (setf step-loc (second (gethash step-loc maps)))
            count step-loc into steps
              finally (setf (elt steps-to-z loc-idx) steps))
      (incf loc-idx)
      )))


(defun p1-test ()
  (let ((in-lines (uiop:split-string +test-input-1+ :separator '(#\newline))))
    (fresh-line)
    (princ "1st test path length: ")
    (princ (p1 (parse-input in-lines)))
    (setf in-lines (uiop:split-string +test-input-2+ :separator '(#\newline)))
    (fresh-line)
    (princ "2nd path length: ")
    (princ (p1 (parse-input in-lines)))))

(defun p2-test ()
  (let ((in-lines (uiop:split-string +test-input-3+ :separator '(#\newline))))
    (fresh-line)
    (princ "test path length ")
    (princ (lcm-p2 (parse-input in-lines)))))

(defun main ()
  (let* ((infile-name (format nil +input-name-template+ +day-number+))
         (input-lines (uiop:read-file-lines infile-name))
         (data (parse-input input-lines)))
    (fresh-line)
    (princ "part 1: ")
    (princ (p1 data))
    (fresh-line)
    (princ "part 2: ")
    (fresh-line)
    (princ "starting locations: ")
    (princ (loop for k being the hash-key of (second data)
                 when (equal #\A (char k 2))
                 collect k))
    (princ (lcm-p2 data))
    ))
