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

(defconstant +test-input-p2-1+
  '("..........."
    ".S-------7."
    ".|F-----7|."
    ".||.....||."
    ".||.....||."
    ".|L-7.F-J|."
    ".|..|.|..|."
    ".L--J.L--J."
    "...........")) ;; 4 inside

(defconstant +test-input-p2-2+
  '(".F----7F7F7F7F-7...."
    ".|F--7||||||||FJ...."
    ".||.FJ||||||||L7...."
    "FJL7L7LJLJ||LJ.L-7.."
    "L--J.L7...LJS7F-7L7."
    "....F-J..F7FJ|L7L7L7"
    "....L7.F7||L7|.L7L7|"
    ".....|FJLJ|FJ|F7|.LJ"
    "....FJL-7.||.||||..."
    "....L---J.LJ.LJLJ...")) ;; 8 inside

(defconstant +test-input-p2-3+
  '("FF7FSF7F7F7F7F7F---7"
    "L|LJ||||||||||||F--J"
    "FL-7LJLJ||||||LJL-77"
    "F--JF--7||LJLJ7F7FJ-"
    "L---JF-JLJ.||-FJLJJ7"
    "|F|F-JF---7F7-L7L|7|"
    "|FFJF7L7F-JF7|JL---7"
    "7-L-JL7||F7|L7F-7F7|"
    "L.L7LFJ|||||FJL7||LJ"
    "L7JLJL-JLJLJL--JLJ.L")) ;; 10 inside

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

(defun parse-input-to-hash (lines)
  (loop named outer
        for line in lines
        for line-idx from 0
        with result = (make-hash-table :test #'equal)
        do (loop named inner
                 for ch across line
                 for ch-idx from 0
                 do (setf (gethash (list line-idx ch-idx) result)
                          (pipe-lookup ch))
                 when (equal #\S ch)
                 do (setf start-pt (list line-idx ch-idx)))
        finally (return-from outer result)))

(defun p1-with-hash (grid-hash)
  (let ((visited (make-hash-table :test #'equal)))
    (setf (gethash start-pt visited) t)
    (do ((steps 0 (1+ steps))
         (loc start-pt)
         (visited visited (setf (gethash loc visited) t)))
        ((and (null loc)
              (< 0 steps))
         (values (floor steps 2)
                 visited))
      (setf loc (loop named inner
                      for d in (gethash loc grid-hash)
                      for mate = (mate-lookup d)
                      for ld = (dir-lookup d)
                      for lp = (mapcar #'+ loc ld)
                      when (and (every (lambda (x)
                                         (<= 0 x))
                                       lp)
                                ;; and in bounds the other dir!
                                (intersection (list mate) (gethash lp grid-hash))
                                (null (gethash lp visited)))
                      do (return-from inner lp))))))

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
  (let ((bpoints (append path (list (first path)))) ;; need to make it a loop , could be circular i guess, but that probably doesn't really help
        shoelace)
    (setf shoelace (abs (loop for i from 0 below (* 2 p1ans) ;; total path length
                              for (y x) = (nth i bpoints)
                              for (y+ x+) = (nth (1+ i) bpoints)
                              summing (* (+ y y+) (- x x+)) ;; trapezoid formula of shoelace
                              )))
    (1+ (- (floor shoelace 2) p1ans))))


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
      (princ (p2 path p1ans)))))
  
