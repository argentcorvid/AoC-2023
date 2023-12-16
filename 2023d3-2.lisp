;;;day3
(ql:quickload '("uiop" "cl-ppcre"))

(defconstant +day-number+ 3)
(defconstant +working-dir+ (uiop:truenamize "~/aoc_2023/"))
(defconstant +input-name-template+ "2023d~dinput.txt")

(defconstant +test-input+
  '("467..114.."
    "...*......"
    "..35..633."
    "......#..."
    "617*......"
    ".....+.58."
    "..592....."
    "......755."
    "...$.*...."
    ".664.598.."))

(defconstant +num-pat+ (ppcre:create-scanner "\\d+"))

(defconstant +look-dir+ '((-1 -1)(0 -1)(1 -1)
                          (-1 0)       (1 0)
                          (-1 1) (0 1) (1 1)))

(defun p1-sym-p (ch)
  (and (not (char= #\. ch))
       (not (alphanumericp ch))))

(defun find-all-syms (line)
  "given a string return a list containing a list of the indices of any non-'.', non-alphanumeric characters, and a list the characters"
  (loop for ch across line
        for idx from 0
        if  (p1-sym-p ch)
          collect (list ch idx)))

(defun find-all-nums (line)
  (let ((locs ()))
    
    (ppcre:do-matches (mst mend +num-pat+ line
                       (unless (null locs)
                         (reverse locs)))
      
      (push (list (parse-integer line
                                 :start  mst 
                                 :end mend )
                  (list  mst
                         (- mend 1)))
            locs))))

(defun within-1 (range num)
  (<= (- (first range) 1)
      num
      (+ (second range) 1)))

(defun in-range (range num)
  (<= (first range) num (second range)))

(defun p1 (lines)
  ;; (step
  (let (;;(lidx 0)
        (part-sum 0)
        (sym-locs ()) ;; list of (symbol (line pos)
        (num-locs ()) ;; list of (number (line (start end)))
        (max-lines (length lines))
        (max-wide (length (first lines))))
    (fresh-line)
    (princ "collecting")
    (loop for l in lines
          for idx from 0
          with s-line
          with n-line
          do (setf s-line (find-all-syms l))
             (setf n-line (find-all-nums l))
             (if (= 0 (mod idx 10))
                 (princ "."))
          unless (null s-line)
          append (mapcar (lambda (elt) (list (first elt) (list idx (second elt)))) s-line) into syms
          end
          unless (null n-line)
          append (mapcar (lambda (elt) (list (first elt) (list idx (second elt)))) n-line) into nums
          end
          finally (setf sym-locs syms num-locs nums))
    (princ "done")
    (format t "~&number of symbols: ~a, numbers: ~a" (length sym-locs) (length num-locs))
    ;; (format t "~&symbols: ~a" sym-locs)
    ;; (format t "~&numbers: ~a" num-locs)

    (mapcar (lambda (num) ) num-locs)
    part-sum)) ;; )
