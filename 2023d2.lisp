;;;day2

(ql:quickload '("uiop" "cl-ppcre"))

(defconstant +day-number+ 2)

(defconstant +working-dir+ (uiop:truenamize "~/aoc_2023/"))
(defconstant +input-name-template+ "2023d~dinput.txt")

(defconstant +test-input+
  '("Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
   "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue"
   "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red"
   "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red"
   "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"))

(defconstant +colors+ '(:r :g :b))

(defconstant +p1-bag-contents+ (pairlis +colors+ '(12 13 14)))

(defconstant +game-split+ (ppcre:create-scanner "\\d+(?=\\:)") )

(defconstant +draw-split+ (ppcre:create-scanner "\\d+\\s[r|b|g]"))

(defun matched-to-number (draw)
  (if (stringp draw)
      (parse-integer (ppcre:scan-to-strings "\\d+\\b" draw))
      0))

(defun game-max (line)
  "take in a string line starting with a game number and containg draws of stones from a bag
return a list containing the game number as integer and an alist of the max stones seen of each color that game"
  (let ((game-number (parse-integer (ppcre:scan-to-strings +game-split+ line)))
        (draws (ppcre:all-matches-as-strings +draw-split+ line)))
    (loop for d in draws
          maximize (matched-to-number (ppcre:scan-to-strings "\\d+\\sr" d)) into r
          maximize (matched-to-number (ppcre:scan-to-strings "\\d+\\sg" d)) into g
          maximize (matched-to-number (ppcre:scan-to-strings "\\d+\\sb" d)) into b
          finally (return (list game-number (pairlis +colors+ (list r g b)))))))

(defun p1-possible-p (gamemax)
    (loop for b in +p1-bag-contents+
        always (>= (cdr b) (cdr (assoc (car b) gamemax)))))

(defun p1-test-case ()
    (dolist (l +test-input+)
      (let* ((r (game-max l))
             (game-number (car r)))
        (format t "~&G ~a: ~s =>~s" game-number (cadr r) (p1-possible-p (cadr r))))))

(defun p2-test-case ()
  (let ((pow-tot 0))
  (dolist (l +test-input+)
    (let ((g (game-max l))
           pow )
      (setf pow (p2-power (cadr g)))
      (incf pow-tot pow)
      (format t "~&G: ~s, power:~s" g pow)
      (format t "total power: ~s" pow-tot)))
     pow-tot))

(defun p2-power (game)
  (apply #'* (mapcar #'cdr game)))

(defun p1-and-p2 ()
  (let ((filename (merge-pathnames +working-dir+ (format nil +input-name-template+ +day-number+)))
        (p1-id-sum 0)
        (p2-power-sum 0)
        parsed-game
        id)
    (with-open-file (f filename)
      (do ((line (read-line f nil)
                 (read-line f nil)))
          ((null line))
        (setf parsed-game (game-max line))
        (setf id (car parsed-game))
        (incf p2-power-sum (p2-power (cadr parsed-game)))
        (if (p1-possible-p (cadr parsed-game))
            (incf p1-id-sum id)))
      (list (cons :p1 p1-id-sum)
            (cons :p2 p2-power-sum)))))

