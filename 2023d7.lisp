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
           :hc)
          ((and (= 1 2counts)
                (/= 1 3counts))
           :1p)
          ((= 2 2counts)
           :2p)
          ((and (= 1 3counts)
                (/= 1 2counts))
           :3k)
          ((some (lambda (c)
                   (= c 5)) counts)
           :5k)
          ((some (lambda (c)
                   (= c 4)) counts)
           :4k)
          ((and (= 1 2counts)
                (= 1 3counts))
           :fh))))

(defun card-comp (first-card second-card)
  "return 1 if second > first, -1 if less, 0 if equal"
  (let ((first-rank (position first-card *card-rank*))
        (second-rank (position second-card *card-rank*)))
    (cond ((< first-rank second-rank)
           1)
          ((> first-rank second-rank)
           -1)
          ((= first-rank second-rank)
           0))))

(defun parse-input (lines)
  )

(defun p1 ()
  ) 

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
