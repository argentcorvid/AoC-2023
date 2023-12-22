;;;day7
(ql:quickload '("uiop" "str"))

(defconstant +day-number+ 7)
(defconstant +working-dir+ (uiop:truenamize "~/aoc_2023/"))
(defconstant +input-name-template+ "2023d~dinput.txt")

(defconstant +test-input+
  '(("32T3K" 765)
    ("T55J5" 684)
    ("KK677" 28)
    ("KTJJT" 220)
    ("QQQJA" 483)))

(defparameter *card-value*
  '(2 3 4 5 6 7 8 9 10 11 12 13 14))

(defparameter *card-rank*
  '(#\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\T #\J #\Q #\K #\A))

(defparameter *hand-type-stats*)

(defun classify-hand (hand)
  (let* ((counts (remove 0 (mapcar (lambda (r)
                                     (count r hand))
                                   *card-rank*)))
         (2counts (count 2 counts))
         (3counts (count 3 counts)))
    (cond ((every (lambda (c)
                    (= 1 c)) counts)
           #x10)
          ((and (= 1 2counts)
                (/= 1 3counts))
           #x20)
          ((= 2 2counts)
           #x30)
          ((and (= 1 3counts)
                (/= 1 2counts))
           #x40)
          ((some (lambda (c)
                   (= c 5)) counts)
           #x70)
          ((some (lambda (c)
                   (= c 4)) counts)
           #x60)
          ((and (= 1 2counts)
                (= 1 3counts))
           #x50))))

(defun card-score (card)
  (position card *card-rank*))

(defun hand-comp (hand1 hand2)
  (let ((h1-type (classify-hand hand1))
        (h2-type (classify-hand hand2)))
    (cond ((< h1-type h2-type) t)
          ((= h1-type h2-type)
           (loop for h1c in (map 'list #'card-score hand1)
                 for h2c in (map 'list #'card-score hand2)
                 if (< h1c h2c)
                   return t
                 else if (> h1c h2c)
                   return nil))
          (t nil))))

(defun parse-input (lines)
  )

(defun p1 (input)
  (let ((sorted-hands (sort (copy-list input) #'hand-comp :key #'first)))
    (loop for h in sorted-hands
          for r from 1
          sum (* (second h) r)))) 

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
