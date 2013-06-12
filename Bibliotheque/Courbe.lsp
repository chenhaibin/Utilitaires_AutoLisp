(vl-load-com)


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

;;; Renvoi le point de depart et d'arrivé
(defun Courbe-PtDepartPtArrive (Courbe)
  (list	(V2D
	  (vlax-curve-getStartPoint (vlax-ename->vla-object Courbe))
	)
	(V2D
	  (vlax-curve-getEndPoint (vlax-ename->vla-object Courbe))
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
  (V2D (vlax-curve-getFirstDeriv
	 (vlax-ename->vla-object Courbe)
	 Param
       )
  )
)

;;; Renvoi la derivee 2 d'une courbe
(defun Courbe-Derivee2 (Courbe Param /)
  (V2D (vlax-curve-getSecondDeriv
	 (vlax-ename->vla-object Courbe)
	 Param
       )
  )
)

;;; Renvoi la longueur d'une courbe
(defun Courbe-Longueur (Courbe / prop vCourbe)
  (setq vCourbe (vlax-ename->vla-object Courbe))
  (- (vlax-curve-getDistAtParam
       vCourbe
       (vlax-curve-getEndParam vCourbe)
     )
     (vlax-curve-getDistAtParam
       vCourbe
       (vlax-curve-getStartParam vCourbe)
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


;;; Decaler une courbe
(defun Courbe-Decaler (Courbe Dist Cote)
  (setq	Dist (*	Dist
		(if Cote
		  1
		  -1
		)
	     )
  )
  (vl-catch-all-apply
    'vla-Offset
    (list (vlax-ename->vla-object Courbe) Dist)
  )
  (entlast)
)