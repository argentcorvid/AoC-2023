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

(defconstant +num-pat+ (ppcre:create-scanner "(?<=[\\W^])\\d+(?=[\\W$])"))

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
        (push (list
             (parse-integer line
                            :start (- mst 1)
                            :end mend)
             (list (- mst 1)
                   (- mend 1))) locs)))

  (defun within-1 (range num)
    (<= (- (first range) 1)
        num
        (+ (second range) 1))))

(defun p1 (lines)
  (let (;;(lidx 0)
        (part-sum 0)
        (sym-locs (mapcar #'find-all-syms lines))
        (num-locs (mapcar #'find-all-nums lines))
        (max-lines (length lines))
        (max-wide (length (first lines))))
    (loop for line-syms in sym-locs
          for lidx from 0
          unless (null line-syms)
            do (dolist (sp line-syms (part-sum))
                 (let ((sym (first sp))
                       (pos (second sp)))
                   (dolist (ld +look-dir+)
                     (let ((look-x (+ pos (first ld)))
                           (look-y (+ lidx (second ld))))
                       (if (and (>= look-x 0)
                                (>= look-y 0)
                                (< look-y max-lines)
                                (< look-x max-wide))
                           (dolist (look-num (nth look-y num-locs))
                             (if (within-1 look-num pos )
                                 (incf part-sum (first look-num)))))))))
          end)))
