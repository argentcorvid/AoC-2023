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

(defconstant +test-input-2+
  '("7-F7-"
    ".FJ|7"
    "SJLL7"
    "|F--J"
    "LJ.LJ"))

(defconstant +pipes+
  (pairlis '(#\| #\- #\J #\F #\7 #\L #\. #\S)
           '((:n :s)
             (:e :w)
             (:w :n)
             (:s :e)
             (:s :w)
             (:n :e)
             ()
             (:n :s :e :w))))

(defconstant +mates+
  (pairlis '(:n :s :e :w)
           '(:s :n :w :e)))

(defconstant +look-dir+
  (pairlis '(:n     :s    :e    :w)
           '((-1 0) (1 0) (0 1) (0 -1))))

(defvar start-pt nil) ;;(line char)

(defun gp-to-pipe (grid-pos grid)
    (nth (second grid-pos) (nth (first grid-pos) grid)))

(defun pipe-lookup (char)
  (cdr (assoc char +pipes+)))

(defun mate-lookup (dir)
  (cdr (assoc dir +mates+)))

(defun dir-lookup (dir)
  (cdr (assoc dir +look-dir+)))

(defun parse-input (lines)
  (let ((result ())
        (lidx 0)
        (chidx nil))
    (dolist (line lines (nreverse result))
      (push (map 'list  #'pipe-lookup line) result)
      (when (setf chidx (position #\S line))
        (setf start-pt (list lidx chidx)))
      (incf lidx))))

(defun p1 (grid)
  (do ((steps 0 (1+ steps))
       (visited (list start-pt) (adjoin loc visited :test #'equal))
       (loc start-pt))
      ((and (null loc)
            (< 0 steps))
       (values (floor steps 2)
                 (remove nil visited)))
    (setf loc (loop named inner
                    for d in (gp-to-pipe loc grid)
                    for mate = (mate-lookup d)
                    for ld = (dir-lookup d)
                    for lp = (mapcar #'+ loc ld)
                    when (and (every (lambda (x)
                                       (<= 0 x))
                                     lp)
                              (intersection (list mate) (gp-to-pipe lp grid) :test #'equal)
                              (null (intersection (list lp) visited :test #'equal)))
                    do (return-from inner lp))))) 

;;; Pick's theorem + shoelace theorem
;;; Area = 1/2 sum (i=1 to n) (yi(x(i-1)-x(i+1)))
;;; Area = # int pts + (# boundary pts / 2) - 1
;;; (bdry pts/2) = part 1 answer
;;; int pts = 1/2 sum (i=1 to n) (yi(x(i-1)-x(i+1))) - (p1 ans) + 1
(defun p2 (path p1ans)
    (let ((bpoints (append path (list (first path)))))
      (1+
       (- (floor
           (loop for i from 1 to (length path)
                 for y = (first (nth i bpoints))
                 for x+ = (second (nth (1+ i) bpoints))
                 for x- = (second (nth (1- i) bpoints))
                 summing (* y (- x- x+)))
           2)
          p1ans))))


(defun main ()
  (let* ((infile-name (format nil +input-name-template+ +day-number+))
         (input-lines (uiop:read-file-lines infile-name))
         (data (parse-input input-lines)))
    (multiple-value-bind (p1ans path)
        (p1 data)
      (fresh-line)
      (princ "part 1: ")
      (princ p1ans)
      (fresh-line)
      (princ "part 2: ")
      (princ (p2 path p1ans))))
  )
