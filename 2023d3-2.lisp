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
         (sym-locs (mapcar #'find-all-syms lines))
         (num-locs (mapcar #'find-all-nums lines))
         (max-lines (length lines))
         (max-wide (length (first lines))))
     (loop for line-syms in sym-locs
           for lidx from 0
           unless (null line-syms)
             do (dolist (sp line-syms part-sum)
                  (let ((sym (first sp))
                        (pos (second sp))
                        (last-num 0))
                    (format t "~&doing sym ~a at (~a,~a)" sym pos lidx)
                    (dolist (ld +look-dir+)
                      (let ((look-x (+ pos (first ld)))
                            (look-y (+ lidx (second ld))))
                        (if (and (>= look-x 0)
                                 (>= look-y 0)
                                 (< look-y max-lines)
                                 (< look-x max-wide))
                            (dolist (look-num (nth look-y num-locs))
                              (format t "~&~tlooking at ~a,~a" look-x look-y)
                              (when (/= (first look-num) last-num)
                                (cond ((within-1 (second look-num) pos)
                                       (setf last-num (first look-num))
                                       (incf part-sum (first look-num))
                                       (format t " found ~a, adding, sum ~a" (first look-num) part-sum))))))))))
           end)
     part-sum));;)
