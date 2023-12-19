;;;day4
(ql:quickload '("uiop" "cl-ppcre"))

(defconstant +day-number+ 4)
(defconstant +working-dir+ (uiop:truenamize "~/aoc_2023/"))
(defconstant +input-name-template+ "2023d~dinput.txt")

(defconstant +test-input+
  '("Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"
    "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19"
    "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1"
    "Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83"
    "Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36"
    "Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"))

(defconstant +test-result-p1+ 13)
(defconstant +test-result-p2+ 30)

(defun parse-input (lines)
  (loop for line in lines
        and card-idx from 1
        with winning
        with mine
        with num-sep
        with num-start
        do (setf num-start (+ 2 (position #\: line :test #'equal))
                 num-sep (position #\| line :test #'equal))
           (setf winning (ppcre:all-matches-as-strings "\\d+" line :start num-start :end num-sep)
                 mine (ppcre:all-matches-as-strings "\\d+" line :start (+ 2 num-sep)))
        collect (list card-idx
                      (mapcar #'parse-integer winning)
                      (mapcar #'parse-integer mine))))

(defun count-winners (cards)
  (loop for card in cards
        and idx from 0
        collect (list idx (length (intersection (second card) (third card))))))

(defun p1 (winner-counts)
  (reduce #'+ (mapcar (lambda (x)
                        (if (>= 1 x)
                            (expt 2 (- x 1))
                            0))
                      (mapcar #'second winner-counts))))

(defun p2 (winner-counts)
  (let ((card-counts (loop repeat (length winner-counts) collect 1))
    (dolist (current-card winner-counts card-counts)
      (loop (second current-card) times
            for idx from 1
            do (incf (nth (+ (first current-card) idx) card-counts) 
                     (second (nth (first current-card) card-counts))))))))

(defun test (&optional (parts 0))
  (let* ((data (parse-input +test-input+))
         (num-winners (count-winners data)))
    (if (and (or (= parts 0) ;;these aren't right
                 (= parts 1))
             (= +test-result-p1+ (p1 num-winners)))
        ((fresh-line)
         (princ "p1 test successful!"))
        ((fresh-line)
         (princ "p1 test fail!")
         (return nil)))
    (if (and (or (= parts 0)
                 (= parts 2))
             (= +test-result-p2+ (p2 num-winners)))
        ((fresh-line)
         (princ "p2 test successful!"))
        ((fresh-line)
         (princ "p2 test fail!")
         (return nil)))
    t))

(defun main ()
  (let* ((infile-name (format nil +input-name-template+ +day-number+))
         (input-lines (uiop:read-file-lines infile-name))
         (data (parse-input input-lines))
         (num-of-winners (count-winners data)))
    
    (fresh-line)
    (princ "part 1: ")
    (princ (p1 num-of-winners))))
    ;; (fresh-line)
    ;; (princ "part 2: ")
    ;; (fresh-line)
    ;; (princ (reduce #'+ (p2 data)))))
