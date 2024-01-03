;;;day10
(ql:quickload '(:uiop))

(defconstant +day-number+ 10)
(defconstant +working-dir+ (uiop:truenamize "~/aoc_2023/"))
(defconstant +input-name-template+ "2023d~dinput.txt")

(defconstant +test-input-1+
  '("-L|F7"
    "7S-7|"
    "L|7||"
    "-L-J|"
    "L|-JF"))

(defconstant +pipes+
  (pairlis '(#\| #\- #\J #\F #\7 #\L #\. #\S)
           '(#(:n :s)
             #(:e :w)
             #(:w :n)
             #(:s :e)
             #(:s :w)
             #(:n :e)
             #()
             #(:n :s :e :w))))

(defconstant +mates+
  (pairlis '(:n :s :e :w)
           '(:s :n :w :e)))

(defconstant +look-dir+
  (pairlis '(:n     :s    :e    :w)
           '((-1 0) (1 0) (0 1) (0 -1))))

(defvar start-pt nil) ;;(line char)

(defun gp-to-pipe (grid-pos grid)
    (aref (aref grid (first grid-pos)) (second grid-pos)))

(defun pipe-lookup (char)
  (cdr (assoc char +pipes+)))

(defun mate-lookup (dir)
  (cdr (assoc dir +mates+)))

(defun dir-lookup (dir)
  (cdr (assoc dir +look-dir+)))

(defun parse-input (lines)
  (let ((result (make-array 0 :adjustable t
                              :fill-pointer t))
        (lidx 0)
        (chidx nil))
    (dolist (line lines result)
      (vector-push-extend (map 'vector #'pipe-lookup line) result)
      (when (setf chidx (position #\S line))
        (setf start-pt (list lidx chidx)))
      (incf lidx))))

(defun p1 (grid)
  (let ((next-dir nil)
        (loc start-pt))
    
    (do ((steps 0)
         (visited nil))
        ((and (equal loc start-pt)
              (> 0 steps))
         (floor steps 2))
      
      (loop for d in (gp-to-pipe loc grid)
            for mate = (mate-lookup d)
            for ld = (dir-lookup d)
            for lp = (mapcar #'+ loc ld)
            when (and (every (lambda (x)
                               (<= 0 x))
                             lp)
                      (intersection mate (gp-to-pipe grid lp))
                      (null (intersection (list lp) visited)))
              do (progn (setf loc lp)
                        (incf steps)
                        (push loc visited)))))) 

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
