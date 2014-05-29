(defpackage :two-opt
  (:use :cl))
(in-package :two-opt)

(deftype vec3 () '(simple-array double-float (3)))

(defun make-point (x y active)
  (let ((vec3 (make-array 3 :element-type 'double-float :initial-element 0.0d0)))
    (setf (aref vec3 0) x
          (aref vec3 1) y
          (aref vec3 2) active)
    vec3))

(defmacro point-x (point)
  `(aref ,point 0))

(defmacro point-y (point)
  `(aref ,point 1))

(defmacro point-active (point)
  `(aref ,point 2))

(defmacro active (point)
  `(= (aref ,point 2) 1.0d0))

(defmacro disable (point)
  `(setf (aref ,point 2) 0.0d0))

(defun distance-squared (p1 p2)
  "for comparing 2 edges."
  (let ((dx (- (point-x p1) (point-x p2)))
	(dy (- (point-y p1) (point-y p2))))
    (+ (* dx dx) (* dy dy))))

(defun distance (p1 p2)
  "euclidean distance."
  (sqrt (distance-squared p1 p2)))

(defun tour-distance (points)
  (let ((tour-length (length points)))
    (do ((i 1 (1+ i))
	 (d 0 (+ d (distance (aref points (1- i)) (aref points i)))))
	((= i tour-length)
	 (+ d (distance (aref points (1- tour-length)) (aref points 0)))))))

(defun line-to-point (line)
  "given a string of the form X Y create an instance of city." 
  (let ((coordinates 
	 (with-input-from-string (s line)
	   (loop
	      :for num := (read s nil nil)
	      :while num
	      :collect num))))
    (make-point (first coordinates)
		(second coordinates)
                1.0d0)))

(defun load-points (file-location)
  "load points from a file into a vector. this vector, when
iterated in order, represents a tour."
  (let ((*read-default-float-format* 'double-float) 
       (in (open file-location))
	(result nil))
    (loop for line = (read-line in nil)
       while line do 
	 (push (line-to-point line) result))
    (close in)
    (make-array (length result)
		:element-type 'vec3
		:initial-contents (reverse result))))

(defun reverse-subseq (array from to)
  (do ((i from (1+ i))
       (j to (1- j)))
      ((>= i j))
    (let ((tmp (svref array i)))
      (setf (svref array i) (svref array j))
      (setf (svref array j) tmp))))

(defun wrap (i max)
  (the fixnum (mod (the fixnum (+ i max)) max)))

(defun move-cost (a b c d)
  (let ((ab (distance-squared a b)) (cd (distance-squared c d))
	(ac (distance-squared a c)) (bd (distance-squared b d)))
    (if (and (< ab ac) (< cd bd))
	1.0d0
	(- (+ (sqrt ac) (sqrt bd))
           (+ (sqrt ab) (sqrt cd))))))

(defun activate (a b c d)
  (setf (point-active a) 1.0d0) (setf (point-active b) 1.0d0)
  (setf (point-active c) 1.0d0) (setf (point-active d) 1.0d0))

(defun try-move (points from to a b c d)
  (let ((delta (move-cost a b c d)))
    (if (< delta 0.0d0)
	(progn 
	  (activate a b c d)
	  (reverse-subseq points (1+ (min from to)) (max from to))
	  delta)
	nil)))

(defun find-move (current points num-cities)
  (let ((current-point (svref points current)))
    (if (not (active current-point))
	0.0d0
	(let* ((prev (wrap (1- current) num-cities))
	       (next (wrap (1+ current) num-cities))
	       (prev-point (svref points prev))
	       (next-point (svref points next)))
	  (do ((i (wrap (+ current 2) num-cities) j)
	       (j (wrap (+ current 3) num-cities) (wrap (1+ j) num-cities)))
	      ((= j current) 0)
	    (let* ((c (svref points i))
		   (d (svref points j))
		   (delta (or 
			   (try-move points prev i prev-point current-point c d)
			   (try-move points current i current-point next-point c d))))
	      (when delta (return-from find-move delta))))
	  (disable current-point) 0.0d0))))

(defun optimise (tour)
  "optimise a tour. 

a tour is a vector containing cities.
the tour is taken in order from the first element to the last, finally
wrapping around from the last to the first to create a circle. the final
distance of the tour is the sum of distances between each city in the
ordered tour.

the function will scan forward to the next city if no improvements can be
made in relation to the adjacent edges of the current city or if the 
current city has been marked as 'inactive'. if an improvement has been made,
the function will go back 1 step.

the function will continuously scan the tour until no further improvements
can be made, in which case the tour is said to be '2 optimal'."
  (do* ((best (tour-distance tour))
	(num-cities (length tour))
	(visited 0) 
	(current 0)
	(modified (find-move current tour num-cities)
		  (find-move current tour num-cities)))
       ((= visited num-cities) best)
    (if (< modified 0.0d0)
	(progn 
	  (setf current (wrap (1- current) num-cities))
	  (setf visited 0)
	  (incf best modified))
	(progn
	  (setf current (wrap (1+ current) num-cities))
	  (incf visited)))))

;; (sb-sprof:with-profiling (:max-samples 1 :report :flat :loop nil) 
;;   (test-optimise))
(defun test-optimise ()
  (let ((tour-array (load-points "greece.points")))
    (format t "starting distance = ~4$~%" (tour-distance tour-array))
    (format t "optimised distance = ~4$~%" (optimise tour-array))))


