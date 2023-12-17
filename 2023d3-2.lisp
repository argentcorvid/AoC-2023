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

(defun parse-input (lines)
  (let ((sym-locs ()) ;; list of (symbol (line pos)
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
          finally (setf sym-locs syms
                        num-locs nums))
    (princ "done")
    (format t "~&number of symbols: ~a, numbers: ~a" (length sym-locs) (length num-locs))
    ;; (format t "~&symbols: ~a" sym-locs)
    ;; (format t "~&numbers: ~a" num-locs)
    (pairlis '(:n :s) (list num-locs sym-locs))))

(defun p1 (nums-and-syms)
  (let ((num-locs (rest (assoc :n nums-and-syms)))
        (sym-locs (rest (assoc :s nums-and-syms))))
    (mapcar (lambda (num) 
              (let ((rng (second (second num)))
                    (ln (first (second num)))
                    (neighbor))
                (setf neighbor (remove-if-not
                             (lambda (sym)
                               (and (within-1 rng (second sym))
                                    (in-range '(-1 1) (- ln (first sym)))))
                             sym-locs :key #'second))
               (if (null neighbor)
                   0
                   (first num))))
            num-locs))) 

(defun p2 (data)
  (let* ((star-locs (mapcar #'cadr
                        (remove-if-not
                         (lambda (item)
                           (equal #\* item))
                         (rest (assoc :s data))
                         :key #'first)))
         (numbers (rest (assoc :n data)))
         gears
         gear-ratios)
    (fresh-line)
    (princ "number of '*': ")
    (princ (length star-locs))
    (fresh-line)
    ;; (princ star-locs)
    
    (setf gears (loop for s in star-locs
                      with neighbors
                      do (setf neighbors (remove-if-not (lambda (num)

                                                          (and (within-1 (second num) (second s))
                                                               (in-range '(-1 1) (- (first s) (first num))))) numbers :key #'cadr))
                      if (= 2 (length neighbors))
                        collect neighbors))
    (fresh-line)
    (princ "number of gears: ")
    (princ (length gears))
    (fresh-line)
    (setf gear-ratios (mapcar (lambda (g)
                                (funcall #'* (first (first g)) (first (second g))))
                              gears))))

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
