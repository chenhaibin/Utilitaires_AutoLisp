(defun c:S2A (/ Spline Dev_max)
     (vl-load-com)
     (if (and (setq Spline (car (entsel "\nSélectionnez une Spline : ")))
	      (= (cdr (assoc 0 (entget Spline))) "SPLINE")
	 )
	  (progn (setq SplineDxfData (entget Spline)
		       Degree	     (car (dxf '71 SplineDxfData))
		       NoeudsNb	     (car (dxf '72 SplineDxfData))
		       PtControleNb  (car (dxf '73 SplineDxfData))
		       Noeuds	     (dxf '40 SplineDxfData)
		       Poids	     (if (setq Poids (dxf '41 SplineDxfData))
					  Poids
					  (ListeNb 1.0 NoeudsNb)
				     )
		       PtControle    (dxf '10 SplineDxfData)
		       ParamMin	     (apply 'min Noeuds)
		       ParamMax	     (apply 'max Noeuds)
		       Param	     (getreal (strcat "\nParametre ["
						      (rtos ParamMin 2 3)
						      " - "
						      (rtos ParamMax 2 3)
						      "] : "
					      )
				     )
		       FonctionBase  (Cox-DeBoor Param Degree NoeudsNb Noeuds Poids)
		       Point	     (PointFromFonctionBase FonctionBase PtControle)
		 )
		 (princ Point)
		 (make_point Point 0)
	  )
	  (prompt "\nCe n'est pas une spline.")
     )
     (princ)
)

(defun PointFromFonctionBase (FonctionBase PtControle / Point)
     (setq Point (apply	'mapcar
			(cons '+
			      (mapcar '(lambda (coeff Pt) (mapcar '(lambda (x) (* coeff x)) Pt))
				      FonctionBase
				      PtControle
			      )
			)
		 )
     )
)

(defun Cox-DeBoor (Param Degree	NoeudsNb Noeuds	Poids /	Coefficients CoefficientsPonderes CoefficientsSomme FonctionBase)
     (setq Coefficients
	       (mapcar '(lambda	(Tj Tj+1)
			     (if (and (>= Param Tj) (< Param Tj+1))
				  1.0
				  0.0
			     )
			)
		       Noeuds
		       (cdr Noeuds)
	       )
     )
     (princ (length Coefficients))
     (princ Coefficients)
     (terpri)
     (foreach d	(ListeCumulNb Degree)
	  (setq	Coefficients
		    (mapcar '(lambda (Bjd Bjd+1 Tj Tj+1 Tj+d Tj+d+1 /)
				  (+ (if (/= Bjd 0.0)
					  (* (/ (- Param Tj) (- Tj+d Tj)) Bjd)
					  0.0
				     )
				     (if (/= Bjd+1 0.0)
					  (* (/ (- Tj+d+1 Param) (- Tj+d+1 Tj+1)) Bjd+1)
					  0.0
				     )
				  )
			     )
			    Coefficients
			    (cdr Coefficients)
			    Noeuds
			    (cdr Noeuds)
			    (nthcdr d Noeuds)
			    (nthcdr (1+ d) Noeuds)
		    )
	  )
	  (princ (length Coefficients))
	  (princ Coefficients)
	  (terpri)
     )
     (if (= Param (last Noeuds))
	  (setq Coefficients (append (butlast Coefficients) (list 1.0)))
     )
     (setq CoefficientsPonderes
	       (mapcar '* Coefficients Poids)
	   CoefficientsSomme
	       (apply '+ CoefficientsPonderes)
	   FonctionBase
	       (if (/= CoefficientsSomme 0.0)
		    (mapcar '(lambda (coef) (/ coef CoefficientsSomme))
			    CoefficientsPonderes
		    )
		    (ListeNb 0.0 PtControleNb)
	       )
     )
     FonctionBase
)

(defun ListeCumulNb (nb / liste)
     (while (> nb 0)
	  (setq	liste (cons nb liste)
		nb    (1- nb)
	  )
     )
     liste
)

(defun ListeNb (n nb / liste)
     (repeat nb (setq liste (cons n liste)))
     liste
)

