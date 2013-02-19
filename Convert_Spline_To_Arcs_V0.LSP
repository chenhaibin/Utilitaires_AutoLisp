(vl-load-com)

(defun c:S2A0 (/ Spline Dev_max)
     (if (and (setq Spline  (car (entsel "\nSélectionnez une Spline : "))
		    Dev_max (getreal "\nDistance maximum : ")
	      )
	      (= (cdr (assoc 0 (entget Spline))) "SPLINE")
	 )
	  (convert_spline_to_arc Spline Dev_max)
	  (prompt "\nCe n'est pas une spline.")
     )
     (princ)
)

(defun convert_spline_to_arc (Spline Dev_max /)
     (setq Precision  1
	   Spline_Vl  (vlax-ename->vla-object Spline)
	   Dist	      (+ Precision 0.1)
	   Lg_Spline  (vlax-curve-getDistAtParam
			   Spline_Vl
			   (vlax-curve-getEndParam Spline_Vl)
		      )
	   ListePoint (list (vlax-curve-getPointAtDist Spline_Vl 0.0))
     )
     (while (< Dist Lg_Spline)
	  (setq	ListePoint (cons (vlax-curve-getPointAtDist Spline_Vl Dist) ListePoint)
		b	   (vlax-curve-getPointAtDist Spline_Vl Dist)
		r	   (Rayon3Points
				(vlax-curve-getPointAtDist Spline_Vl (- Dist Precision))
				b
				(vlax-curve-getPointAtDist Spline_Vl (+ Dist Precision))
			   )
		essai	   (princ r)
		Delta	   (if r
				(LgArc r Dev_max)
				nil
			   )
		Delta	   (if Delta
				Delta
				(* Lg_Spline 0.2)
			   )
		Delta	   (if (> Delta (* Lg_Spline 0.2))
				(* Lg_Spline 0.2)
				Delta
			   )
		Dist	   (+ Dist Delta)
	  )
	  (princ Delta)
	  (princ "\n")
	  (princ Dist)
	  (princ "\n---\n")
     )
     (mapcar '(lambda (x) (make_point x '0)) ListePoint)
)


(defun ListeCumul (Lg Nb / liste div)
     (setq liste (list Lg)
	   div	 (/ Lg Nb)
     )
     (repeat Nb (setq liste (cons (- (car liste) div) liste)))
     (cons 0 (cdr liste))
)

(defun Rayon3Points (p1 p2 p3 / mid1 mid2 pperp1 pperp2 int)
     (setq mid1	  (mid p2 p1)
	   mid2	  (mid p2 p3)
	   pperp1 (mapcar '+ mid1 (vperp (vect p2 p1)))
	   pperp2 (mapcar '+ mid2 (vperp (vect p2 p3)))
	   int	  (inters mid1 pperp1 mid2 pperp2 nil)
     )
     (if int
	  (distance p2 int)
	  nil
     )
)

(defun LgArc (Rayon Fleche /)
     (*	2.0
	(asin (/ (sqrt (* (- (* 2.0 Rayon) Fleche) Fleche)) Rayon))
	Rayon
     )
)
(defun Intersection (p1 p2 p3 p4 / D x y)
     (setq D (-	(* (- (car p1) (car p2)) (- (cadr p3) (cadr p4)))
		(* (- (cadr p1) (cadr p2)) (- (car p3) (car p4)))
	     )
	   x (/	(- (* (- (car p3) (car p4))
		      (- (* (car p1) (cadr p2)) (* (cadr p1) (car p2)))
		   )
		   (* (- (car p1) (car p2))
		      (- (* (car p3) (cadr p4)) (* (cadr p3) (car p4)))
		   )
		)
		D
	     )
	   y (/	(- (* (- (cadr p3) (cadr p4))
		      (- (* (car p1) (cadr p2)) (* (cadr p1) (car p2)))
		   )
		   (* (- (cadr p1) (cadr p2))
		      (- (* (car p3) (cadr p4)) (* (cadr p3) (car p4)))
		   )
		)
		D
	     )
     )
     (list x y 0.0)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun Trigo (p1 p2 p3)
     (if (< (sin (- (angle p1 p3) (angle p1 p2))) -1e-14)
	  -1
	  1
     )
)


(defun GetIntersection (Ent1 Ent2 Prolonge / IntLst)
     ;;Valeurs possibles pour Prolonge
     ;;acExtendNone
     ;;acExtendThisEntity
     ;;acExtendOtherEntity
     ;;acExtendBoth
     (vl-load-com)
     (setq IntLst (vlax-invoke Ent1 'IntersectWith Ent2 Prolonge))
     (split-list IntLst 3)
)
