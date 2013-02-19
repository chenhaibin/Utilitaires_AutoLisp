;;; Renvoi le coté sur lequel se situe un point
;;; début : T
;;; fin : nil

(defun Courbe-Position (Courbe Point)
  (if (> (Courbe-DistanceAuPoint Courbe (Courbe-PointLePlusProche Courbe Point) 't)
	 (* (Courbe-Longueur Courbe) 0.5)
      )
    'nil
    't
  )
)


;;; Renvoi le point le plus proche
(defun Courbe-PointLePlusProche	(Courbe lPoint)
  (V2D (vlax-curve-getClosestPointTo (vlax-ename->vla-object Courbe) lPoint))
)

;;; Renvoi la distance à un point donné à partir du debut ou de la fin
(defun Courbe-DistanceAuPoint (Courbe Point Fin / dist)
  (setq dist (vlax-curve-getDistAtPoint (vlax-ename->vla-object Courbe) Point))
  (if Fin
    dist
    (- (Courbe-Longueur Courbe) dist)
  )
)

;;; Renvoi la liste des distances aux points donnés à partir du debut ou de la fin
(defun Courbe-ListeDistancesAuxPoints (Courbe lPoint Fin / dist)
  (mapcar (function (lambda (pt) (Courbe-DistanceAuPoint Courbe pt Fin))) lPoint)
)

;;; Renvoi le point à la distance donnée à partir du debut ou de la fin
(defun Courbe-PointALaDistance (Courbe Dist Fin)
  (setq	Dist (if Fin
	       Dist
	       (- (Courbe-Longueur Courbe) Dist)
	     )
  )
  (V2D (vlax-curve-getPointAtDist (vlax-ename->vla-object Courbe) Dist))
)


;;; Renvoi la liste des points aux distances données à partir du debut ou de la fin
(defun Courbe-ListePointsAuxDistances (Courbe lDist Fin)
  (mapcar (function (lambda (dist) (Courbe-PointALaDistance Courbe dist Fin))) lDist)
)

;;; Renvoi le parametre à la distance donnée suivant à partir du debut ou de la fin
(defun Courbe-ParamALaDistance (Courbe Dist Fin)
  (setq	Dist (if Fin
	       Dist
	       (- (Courbe-Longueur Courbe) Dist)
	     )
  )
  (vlax-curve-getParamAtDist (vlax-ename->vla-object Courbe) Dist)
)

;;; Renvoi le parametre au point donnée
(defun Courbe-ParamAuPoint (Courbe Point)
  (vlax-curve-getParamAtPoint (vlax-ename->vla-object Courbe) Point)
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
(defun Courbe-Longueur (Courbe) (float (vlax-get-property (vlax-ename->vla-object Courbe) 'length)))


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
 ;|«Visual LISP© Format Options»
(100 2 40 2 nil "Fin de " 100 9 0 0 0 T T nil T)
;*** NE PAS AJOUTER de texte au-dessous du commentaire! ***|;
