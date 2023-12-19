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

(defun 2expt (number)
  (if (>= number 1)
      (expt 2 (- number 1))
      0))

(defun p1 (data)
  (let ((matches-by-card (loop for card in data
                                collect (intersection (second card) (third card)))))
    (reduce #'+ (mapcar (lambda (x)
                          (2expt (length x)))
                        matches-by-card)))) 

(defun make-list-of-numbers (start to-do)
  (loop for i from 1 to to-do
       collect (+ start i)))

(defun make-copy-lookup (cards)
  (mapcar (lambda (c)
            (destructuring-bind (card-no wins) c
              (list card-no
                    (make-list-of-numbers card-no wins)))) cards ))

(defun p2 (data)
  (let ((matches-by-card (loop for card in data
                               collect (list (first data)
                                             (length (intersection (second data)
                                                                   (third data))))))
        (card-counts (loop repeat (length data) collect 1)))
    (dolist (card matches-by-card card-counts)
      (loop (second card) times
            for idx from 0
            do (incf (nth (+ (first card) idx) card-counts) ;; "card numbers" start at 1, lists start at 0
                     (second (nth idx card-counts))))))) ;;might not be rigt mabye need (first card) -1 instead of idx

(defun main ()
  (let* ((infile-name (format nil +input-name-template+ +day-number+))
         (input-lines (uiop:read-file-lines infile-name))
         (data (parse-input input-lines)))
    (fresh-line)
    (princ "part 1: ")
    (princ (p1 data))))
    ;; (fresh-line)
    ;; (princ "part 2: ")
    ;; (fresh-line)
    ;; (princ (reduce #'+ (p2 data)))))
