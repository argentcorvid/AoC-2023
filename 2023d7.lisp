;;;day7
(ql:quickload 'uiop)

(defconstant +day-number+ 7)
(defconstant +working-dir+ (uiop:truenamize "~/aoc_2023"))
(defconstant +input-name-template+ "2023d~dinput.txt")

(defconstant +test-input+
  '(("32T3K" 765)
    ("T55J5" 684)
    ("KK677" 28)
    ("KTJJT" 220)
    ("QQQJA" 483)))

(defparameter *card-rank*
  '(#\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\T #\J #\Q #\K #\A))

(defun reset-rank ()
  (unless (find #\J *card-rank*)
    (let ((tail (member #\Q *card-rank*))
          (head (butlast *card-rank* 3)))
      (setf *card-rank* (append head '(#\J) tail)))))

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

(defun classify-hand-p2 (hand)
  (let* ((counts (mapcar (lambda (r)
                           (count r hand))
                         *card-rank*))
         (jcounts (count #\J hand))
         (2counts (count 2 counts))
         (3counts (count 3 counts))
         (bestcount (apply #'max counts))
         (j-and-b (+ jcounts bestcount)))
   ;; (break)
    (cond ((< 4 j-and-b)
           #x70) ;; 5k!
          ((= 4 j-and-b)
           #x60) ;; 4k
          ((or (and (= 2 2counts)
                    (= 1 jcounts))
               (and (= 1 2counts)
                    (= 1 3counts)))
           #x50) ;; fh       
          ((or (and (= 1 jcounts 2counts)
                    (= 0 3counts))
               (and (= 1 3counts)
                    (= 0 jcounts))
               (and (= 1 bestcount)
                    (= 2 jcounts)))
           #x40) ;; 3k and not full house
           ((and (= 2 2counts)
                (= 0 jcounts))
           #x30) ;;two pair, any j would make make a better hand   
           ((and (> 2 jcounts)
                (= 2 j-and-b)
                (= 0 3counts))
           #x20) ;;one pair, natural or hc with 1 joker
           ((and (= 0 jcounts)
                (= 1 bestcount))
           #x10) ;; high card, any joker would make a pair
          )))

(defun card-score (card)
    (or (position card *card-rank*) -1))

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

(defun hand-comp-2 (hand1 hand2)
  (let* ((h1-type (third hand1))
         (h2-type (third hand2)))
    (cond ((< h1-type h2-type) t)
          ((= h1-type h2-type)
           (loop for h1c in (map 'list #'card-score (first hand1))
                 for h2c in (map 'list #'card-score (first hand2))
                 if (< h1c h2c)
                   return t
                 else if (> h1c h2c)
                        return nil))
          (t nil))))

(defun parse-input (line)
  (let ((split-pos (position #\Space line)))
    (list (subseq line 0 split-pos) (parse-integer line :start split-pos))))

(defun p1 (input)
  (let ((sorted-hands (sort (copy-list input) #'hand-comp :key #'first)))
    (loop for h in sorted-hands
          for r from 1
          sum (* (second h) r)))) 

(defun p2 (input)
  (setf *card-rank* (delete #\J *card-rank*)) ;; consider J seperately
  (let* ((classed-hands (mapcar (lambda (h)
                                  (append h (list (classify-hand-p2 (first h)))))
                                input))
         (sorted-hands (sort classed-hands #'hand-comp-2)))
    (break)
    (loop for h in sorted-hands
          for r from 1
          sum (* (second h) r))))

(defun main ()
  (reset-rank)
  ;;(uiop:chdir +working-dir+)
  (let* ((infile-name (merge-pathnames +working-dir+ (format nil +input-name-template+ +day-number+)))
         (input-lines (uiop:read-file-lines infile-name))
         (data (mapcar #'parse-input input-lines)))
    (fresh-line)
    (princ "part 1: ")
    (princ  (p1 data))
    (fresh-line)
    (princ "part 2: ")
    (princ  (p2 data))))
