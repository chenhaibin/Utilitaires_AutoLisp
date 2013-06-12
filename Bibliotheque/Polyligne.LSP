;; Bulge to Arc  -  Lee Mac
;; p1 - start vertex
;; p2 - end vertex
;; b  - bulge
;; dir  - trigo
;; Returns: (<center> <start angle> <end angle> <radius> <dir>)

(defun BulgeToArc (p1 b p2 / c r)
  (setq	r (/ (* (distance p1 p2) (1+ (* b b))) 4 b)
	c (polar p1 (+ (angle p1 p2) (- (/ pi 2) (* 2 (atan b)))) r)
  )
  (if (minusp b)
    (list c (angle c p2) (angle c p1) (abs r))
    (list c (angle c p1) (angle c p2) (abs r))
  )
)

;; Point to Bulge
;; c     - center
;; a1,a2 - start, end angle
;; r     - radius
;; Returns: <bulge>

(defun PtToBulge (c pt1 pt2 Dir)
  (setq	ang (if	Dir
	      (AngleTrigo (angle c pt1) (angle c pt2))
	      (AngleTrigo (angle c pt2) (angle c pt1))
	    )
  )
  (* (if Dir
       1
       -1
     )
     (tan (/ ang
	     4.0
	  )
     )
  )
)

;; Angle to Bulge
;; c     - center
;; a1,a2 - start, end angle
;; r     - radius
;; Returns: <bulge>

(defun AngleToBulge (c a1 a2 Dir)
  (setq	ang (if	Dir
	      (AngleTrigo a1 a2)
	      (AngleTrigo a2 a1)
	    )
  )
  (* (if Dir
       1
       -1
     )
     (tan (/ ang
	     4.0
	  )
     )
  )
)

;; Bulge Center  -  Lee Mac
;; p1 - start vertex
;; p2 - end vertex
;; b  - bulge
;; Returns the center of the arc described by the given bulge and vertices

(defun BulgeCenter (p1 b p2)
  (polar p1
	 (+ (angle p1 p2) (- (/ pi 2) (* 2 (atan b))))
	 (/ (* (distance p1 p2) (1+ (* b b))) 4 b)
  )
)

(defun AngleTrigo (AngleDep AngleArr)
  (ang<2pi (- AngleArr AngleDep))
)