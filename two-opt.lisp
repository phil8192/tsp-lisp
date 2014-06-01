(defpackage :net.parasec.two-opt
  (:use :cl)
  (:export :vec3
	   :make-point
	   :load-points
	   :dump-points
	   :tour-distance
	   :optimise))

(in-package :net.parasec.two-opt)

(deftype vec3 () '(simple-array double-float (3)))

(defun make-point (x y active)
  "a city is a point (x,y) with an 'active' bit"
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
	(result nil))
    (with-open-file (in file-location)
      (loop for line = (read-line in nil) while line do
	   (push (line-to-point line) result)))
    (make-array (length result)
		:element-type 'vec3
		:initial-contents (reverse result))))

(defun dump-points (tour file-location)
  "irerate through the tour and save each city point to file."
  (with-open-file (out file-location :direction :output :if-exists :supersede)
    (dotimes (i (length tour))
      (let ((p (aref tour i)))
	(format out "~4$ ~4$~%" (point-x p) (point-y p))))))

(declaim (inline distance-squared))
(defun distance-squared (p1 p2)
  "for comparing 2 edges."
  (declare (type vec3 p1 p2))
  (declare (optimize (debug 0) (space 0) (safety 0) (speed 3)))
  (let ((dx (- (point-x p1) (point-x p2)))
	(dy (- (point-y p1) (point-y p2))))
    (+ (* dx dx) (* dy dy))))

(defun distance (p1 p2)
  "euclidean distance."
  (declare (type vec3 p1 p2))
  (sqrt (distance-squared p1 p2)))

(defun tour-distance (points)
  "total distance of a tour. this is the minimisation objective."
  (declare (type simple-array points))
  (let ((tour-length (length points)))
    (do ((i 1 (1+ i))
	 (d 0 (+ d (distance (aref points (1- i)) (aref points i)))))
	((= i tour-length)
	 (+ d (distance (aref points (1- tour-length)) (aref points 0)))))))

(defun reverse-subseq (array from to)
  "2-opt a tour.

removing 2 edges and replacing them in a way that re-connects
the tour without breaking it is equivalent to:
1. take route[0] to route[i-1] and add them in order to new_route
2. take route[i] to route[k] and add them in reverse order to new_route
3. take route[k+1] to end and add them in order to new_route."
  (declare (type fixnum from to))
  (do ((i from (1+ i))
       (j to (1- j)))
      ((>= i j))
    (let ((tmp (svref array i)))
      (setf (svref array i) (svref array j))
      (setf (svref array j) tmp))))

(declaim (inline wrap))
(defun wrap (i max)
  "a tour is circular. wrap around."
  (declare (type fixnum i max))
  (declare (optimize (debug 0) (space 0) (safety 0) (speed 3)))
  (the fixnum (mod (the fixnum (+ i max)) max)))

(declaim (inline move-cost))
(defun move-cost (a b c d)
  "calculate the cost of performing a move.

the original edges are AB and CD. if the edges are recombined to:
AC and BD respectively, return the difference/delta in tour length.
this function exploits triangle of inequality for an optimisation
avoiding expensive sqrt operations: at least one of the edges in 
the move will be reduced if the tour becomes shorter. as such, if
the distances of _both_ candidate edges AC and BD are greater than
the original edges, AB CD, it is impossible for the resulting tour
to be shorter. for this comparison, it is possible to use the
squared distance (see distance squared function) without invoking
square()."
  (declare (type vec3 a b c d))
  (declare (optimize (debug 0) (space 0) (safety 0) (speed 3)))
  (let ((ab (distance-squared a b)) (cd (distance-squared c d))
	(ac (distance-squared a c)) (bd (distance-squared b d)))
    (if (and (< ab ac) (< cd bd))
	1.0d0
	(- (+ (sqrt ac) (sqrt bd))
           (+ (sqrt ab) (sqrt cd))))))

(declaim (inline try-move))
(defun try-move (points from to a b c d)
  "trys a single move.

if the move results in an improvement, the corresponding vertices
/cities associated with the swaped edges are 'activated' - meaning
that if they were previously disabled, they should be included in
later searches since subtours from these points have changed. in
other words, the search neighborhood has changed."  
  (declare (type fixnum from to)
	   (type vec3 a b c d))
  (declare (optimize (debug 0) (space 0) (safety 0) (speed 3)))
  (let ((delta (move-cost a b c d)))
    (when (< delta 0.0d0)
      (setf (point-active a) 1.0d0) (setf (point-active b) 1.0d0)
      (setf (point-active c) 1.0d0) (setf (point-active d) 1.0d0)
      (reverse-subseq points (1+ (min from to)) (max from to)))
    delta))

(defun find-move (current tour num-cities)
  "try to find a 2-opt move given the current city in the tour.

a 2-opt move will involve removing either the previous edge (arc
from previous city to this city, or the next edge (arc from this 
city to the next city) and some other edge (from city c to city d).
this function will scan for another edge (cd) to use in a 2-opt move
with the previous or next edge. the move is taken if the resulting
tour from recombining the edges in another way results in a shorter
distance. for example, in the following tour ABCD, at city A, the 
previous edge DA (wraps around) is removed along with CD (2nd edge
after the next). this results in no improvement (the order tour
order is reversed, but the length stays the same). the next edge
from A, AC is then removed along with CD. the corresponding 
replacement edges AC and BD result in a shorter tour, so the move
is taken. 

orginal: A-B-C-D     

remove:  D-A, C-D
replace: D-C, A-D
after:   A-D-C-B       

before     remove     replace  after
A   C      A   C      A   C    A   C
|\ /|      |  /       |  /     |\ /|
| \ |      | /        | /      | \ |
|/ \|      |/         |/       |/ \| 
D   B      D   B      D   B    D   B


remove:  A-B, C-D
replace: A-C, B-D
after:   A-C-B-D

before     remove     replace  after
A   C      A   C      A---C    A---C
|\ /|       \ /                |   |
| \ |        \                 |   |
|/ \|       / \                |   | 
D   B      D   B      D---B    D---B

find move will compare all other edges after CD for larger tours until
an improvement is found. the first improvement is returned. (a greedy
search would compare all edges and return the 2-opt swap yielding the
best improvement.       
"
  (declare (type fixnum current num-cities))
  (declare (optimize (debug 0) (space 0) (safety 0) (speed 3)))
  (let ((current-point (svref tour current)))
    (declare (type vec3 current-point))
    (when (active current-point)
      (do* ((prev (wrap (1- current) num-cities))
	    (next (wrap (1+ current) num-cities))
	    (i (wrap (+ current 2) num-cities) j)
	    (j (wrap (+ current 3) num-cities) (wrap (1+ j) num-cities))
    	    (prev-point (svref tour prev))
	    (next-point (svref tour next))
	    (c (svref tour i) d)
	    (d (svref tour j) (svref tour j)))
	   ((= j current) (disable current-point))
	(progn
	  (let ((delta (try-move tour prev i prev-point current-point c d))) 
	    (when (< delta 0.0d0) (return-from find-move delta)))
	  (let ((delta (try-move tour current i current-point next-point c d))) 
	    (when (< delta 0.0d0) (return-from find-move delta))))))
    0.0d0))

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
  (declare (type simple-array tour))
  (do* ((best (tour-distance tour))
	(num-cities (length tour))
	(visited 0) 
	(current 0)
	(modified (find-move current tour num-cities)
		  (find-move current tour num-cities)))
       ((= visited num-cities) best)
    (declare (type double-float best modified)
	     (type fixnum num-cities visited current))
    (if (< modified 0.0d0)
	(progn 
	  (setf current (wrap (1- current) num-cities))
	  (setf visited 0)
	  (incf best modified))
	(progn
	  (setf current (wrap (1+ current) num-cities))
	  (incf visited)))))

(defun test-optimise ()
  (let ((tour-array (load-points "greece.points")))
    (format t "starting distance = ~4$~%" (tour-distance tour-array))
    (format t "optimised distance = ~4$~%" (optimise tour-array))))


