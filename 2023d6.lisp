;;;day6
(ql:quickload '("uiop" "str"))

(defconstant +day-number+ 6)

(defconstant +test-input+
  '(:AT (7 15 30)
    :RD (9 40 200)))

(defconstant +full-input+
  '(:AT (56     71     79     99)
    :RD (334   1135   1350   2430)))

(defun quad-zeroes (a b c)
  (list (/ (+ (- b) (sqrt (- (* b b) (* 4 a c)))) (* 2 a))
        (/ (- (- b) (sqrt (- (* b b) (* 4 a c)))) (* 2 a))))

(defun beat-record (allowed-time record-distance)
  "distance = velocity * traveltime
 traveltime = allowedtime - chargetime
velocity = chargetime
-> distance = chargetime * (allowedtime - chargetime)
-> distance = ct * at - ct^2
Find ct where d > record distance
-> rd < ct * at - ct^2
-> -ct^2 + ct*at - rd > 0 -> a= -1 b=at c=-rd
-> ct = (-at + sqrt (at^2 + (4 * -1 * -rd)))/(2*-1)"
  (let ((record-chargetimes (quad-zeroes -1 allowed-time (- record-distance)))) ;;returns charge times to get the record. need the next highest int for the first and next lowest int for the second in order to beat the record
   (list (+ (floor (first record-chargetimes)) 1)
         (- (ceiling (second record-chargetimes)) 1))))

(defun p1 (input)
  (let ((times (getf input :AT))
        (distances (getf input :RD))
        winning-times)
    (setf winning-times (mapcar #'beat-record times distances))
    (reduce #'* (mapcar (lambda (range)
                           (+ 1 (apply #'- (reverse range))))
                         winning-times))))

(defun p2 ()
  )

(defun main ()
  (let* ((data +full-input))
    (fresh-line)
    (princ "part 1: ")
    (princ (reduce #'+ (p1 data)))
    (fresh-line)
    (princ "part 2: ")
    (fresh-line)
    (princ (reduce #'+ (p2 data)))))
