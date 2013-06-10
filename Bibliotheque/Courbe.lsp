;;; Renvoi le coté sur lequel se situe un point
;;; début : T
;;; fin : nil

(defun Courbe-Position (Courbe Point)
  (if (> (Courbe-DistanceAuPoint
	   Courbe
	   (Courbe-PointLePlusProche Courbe Point)
	   't
	 )
	 (* (Courbe-Longueur Courbe) 0.5)
      )
    'nil
    't
  )
)


;;; Renvoi le point le plus proche
(defun Courbe-PointLePlusProche	(Courbe lPoint)
  (V2D (vlax-curve-getClosestPointTo
	 (vlax-ename->vla-object Courbe)
	 lPoint
       )
  )
)

;;; Renvoi la distance à un point donné à partir du debut ou de la fin
(defun Courbe-DistanceAuPoint (Courbe Point Fin / dist)
  (setq	dist (vlax-curve-getDistAtPoint
	       (vlax-ename->vla-object Courbe)
	       Point
	     )
  )
  (if Fin
    dist
    (- (Courbe-Longueur Courbe) dist)
  )
)

;;; Renvoi la liste des distances aux points donnés à partir du debut ou de la fin
(defun Courbe-ListeDistancesAuxPoints (Courbe lPoint Fin / dist)
  (mapcar (function
	    (lambda (pt) (Courbe-DistanceAuPoint Courbe pt Fin))
	  )
	  lPoint
  )
)

;;; Renvoi le point à la distance donnée à partir du debut ou de la fin
(defun Courbe-PointALaDistance (Courbe Dist Fin)
  (setq	Dist (if Fin
	       Dist
	       (- (Courbe-Longueur Courbe) Dist)
	     )
  )
  (V2D (vlax-curve-getPointAtDist
	 (vlax-ename->vla-object Courbe)
	 Dist
       )
  )
)


;;; Renvoi la liste des points aux distances données à partir du debut ou de la fin
(defun Courbe-ListePointsAuxDistances (Courbe lDist Fin)
  (mapcar (function
	    (lambda (dist) (Courbe-PointALaDistance Courbe dist Fin))
	  )
	  lDist
  )
)

;;; Renvoi le parametre à la distance donnée suivant à partir du debut ou de la fin
(defun Courbe-ParamALaDistance (Courbe Dist Fin)
  (setq	Dist (if Fin
	       Dist
	       (- (Courbe-Longueur Courbe) Dist)
	     )
  )
  (vlax-curve-getParamAtDist
    (vlax-ename->vla-object Courbe)
    Dist
  )
)

;;; Renvoi le parametre au point donnée
(defun Courbe-ParamAuPoint (Courbe Point)
  (vlax-curve-getParamAtPoint
    (vlax-ename->vla-object Courbe)
    Point
  )
)


;;; Renvoi la derivee 1 d'une courbe
(defun Courbe-Derivee1 (Courbe Param /)
  (V2D (vlax-curve-getFirstDeriv Courbe Param))
)

;;; Renvoi la derivee 2 d'une courbe
(defun Courbe-Derivee2 (Courbe Param /)
  (V2D (vlax-curve-getSecondDeriv Courbe Param))
)

;;; Renvoi la longueur d'une courbe
(defun Courbe-Longueur (Courbe / prop)
  (cond
    ((member '(0 . "ARC") (entget Courbe))
     (setq prop 'ArcLength)
    )
    ((member '(0 . "CIRCLE") (entget Courbe))
     (setq prop 'Circumference)
    )
    ((member '(0 . "LWPOLYLINE") (entget Courbe))
     (setq prop 'Length)
    )
    ((member '(0 . "LINE") (entget Courbe))
     (setq prop 'Length)
    )
  )
  (if prop
    (float
      (vlax-get-property (vlax-ename->vla-object Courbe) prop)
    )
  )
)


;;; 0 : aucun prolongement
;;; 1 : prolonge la courbe 1
;;; 2 : prolonge la courbe 2
;;; 3 : prolonge les deux courbes

(defun Courbe-Intersection (Courbe1 Courbe2 Prolonge / point)
  (setq	point (vlax-variant-value
		(vla-intersectwith
		  (vlax-ename->vla-object Courbe1)
		  (vlax-ename->vla-object Courbe2)
		  Prolonge
		)
	      )
  )
  (if (> (vlax-safearray-get-u-bound point 1) 0)
    (mapcar 'V2D (split-list (vlax-safearray->list point) 3))
  )
)

;; Bulge to Arc  -  Lee Mac
;; p1 - start vertex
;; p2 - end vertex
;; b  - bulge
;; d  - start at start vertex
;; Returns: (<center> <start angle> <end angle> <radius> <StartAtStartVertex>)

(defun BulgeToArc (p1 p2 b / c r)
  (setq	r (/ (* (distance p1 p2) (1+ (* b b))) 4 b)
	c (polar p1 (+ (angle p1 p2) (- (/ pi 2) (* 2 (atan b)))) r)
  )
  (if (minusp b)
    (list c (angle c p2) (angle c p1) (abs r) nil)
    (list c (angle c p1) (angle c p2) (abs r) t)
  )
)