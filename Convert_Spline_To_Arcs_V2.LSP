(defun c:S2A2 (/ Spline Dev_max)
  (vl-load-com)
  (if (and (setq Spline (car (entsel "\nSélectionnez une Spline : ")))
	   (= (cdr (assoc 0 (entget Spline))) "SPLINE")
	   )
    (progn (setq SplineDxfData	   (entget Spline)
		 Degree		   (car (dxf '71 SplineDxfData))
		 NoeudsNb	   (car (dxf '72 SplineDxfData))
		 PtControleNb	   (car (dxf '73 SplineDxfData))
		 Noeuds		   (dxf '40 SplineDxfData)
		 Poids		   (if (setq Poids (dxf '41 SplineDxfData))
				     Poids
				     (ListeNb 1.0 NoeudsNb)
				     )
		 PtControle	   (dxf '10 SplineDxfData)
		 ParamMax	   (apply 'max Noeuds)
		 Param		   (getreal (strcat "\nParametre [ 0 - " (rtos ParamMax 2 3) "] : "))
		 FonctionRationnel (Cox-DeBoor Param Degree NoeudsNb Noeuds Poids)
		 Point1		   (PointFromFonctionBase (car FonctionRationnel) PtControle)
		 Point2		   (PointFromFonctionBase (cadr FonctionRationnel) PtControle)
		 )
	   (princ Point1)
	   (terpri)
	   (setq Point2 (mapcar '+ Point1 (mapcar '(lambda (x) (* x 100)) Point2)))
	   (princ Point2)
	   (make_point Point1 0)
	   (make_point Point2 0)
	   )
    (prompt "\nCe n'est pas une spline.")
    )
  (princ)
  )


;;-----------------------------== PointFromFonctionBase ==-----------------------------;;
(defun PointFromFonctionBase (FonctionBase PtControle /)
  (apply 'mapcar
	 (cons '+
	       (mapcar (function (lambda (coeff Pt) (mapcar '(lambda (x) (* coeff x)) Pt)))
		       FonctionBase
		       PtControle
		       )
	       )
	 )
  )

;;-----------------------------== Cox-DeBoor ==-----------------------------;;
(defun Cox-DeBoor (Param	      Degree		 NoeudsNb	    Noeuds
		   Poids	      /			 Coefficients	    Coefficients1
		   CoefficientsPonderes			 Coefficients1Ponderes
		   CoefficientsSomme  Coefficients1Somme FonctionBase
		   )
  (setq	Coefficients
	 (list (mapcar (function (lambda (Tj Tj+1)
				   (if (and (>= Param Tj) (< Param Tj+1))
				     1.0
				     0.0
				     )
				   )
				 )
		       Noeuds
		       (cdr Noeuds)
		       )
	       )
	)
  (foreach d (ListeCumulNb Degree)
    (setq Coefficients
	   (cons (mapcar (function (lambda (Bjd Bjd+1 Tj Tj+1 Tj+d Tj+d+1 /)
				     (+	(if (/= Bjd 0.0)
					  (* (/ (- Param Tj) (- Tj+d Tj)) Bjd)
					  0.0
					  )
					(if (/= Bjd+1 0.0)
					  (* (/ (- Tj+d+1 Param) (- Tj+d+1 Tj+1)) Bjd+1)
					  0.0
					  )
					)
				     )
				   )
			 (car Coefficients)
			 (cdr (car Coefficients))
			 Noeuds
			 (cdr Noeuds)
			 (nthcdr d Noeuds)
			 (nthcdr (1+ d) Noeuds)
			 )
		 Coefficients
		 )
	  )
    )
  (setq	d Degree
	Coefficients1
	 (mapcar (function (lambda (Bjd Bjd+1 Tj Tj+1 Tj+d Tj+d+1 /)
			     (*	(- (if (/= Bjd 0.0)
				     (/ Bjd (- Tj+d Tj))
				     0.0
				     )
				   (if (/= Bjd+1 0.0)
				     (/ Bjd+1 (- Tj+d+1 Tj+1))
				     0.0
				     )
				   )
				d
				)
			     )
			   )
		 (cadr Coefficients)
		 (cdr (cadr Coefficients))
		 Noeuds
		 (cdr Noeuds)
		 (nthcdr d Noeuds)
		 (nthcdr (1+ d) Noeuds)
		 )
	)
  (setq Coefficients (car Coefficients))
  (if (= Param (last Noeuds))
    (setq Coefficients (append (butlast Coefficients) (list 1.0)))
    )
  (setq	CoefficientsPonderes
	 (mapcar '* Coefficients Poids)
	Coefficients1Ponderes
	 (mapcar '* Coefficients1 Poids)
	CoefficientsSomme
	 (apply '+ CoefficientsPonderes)
	Coefficients1Somme
	 (apply '+ Coefficients1Ponderes)
	FonctionBase
	 (if (/= CoefficientsSomme 0.0)
	   (mapcar (function (lambda (coef) (/ coef CoefficientsSomme)))
		   CoefficientsPonderes
		   )
	   (ListeNb 0.0 PtControleNb)
	   )
	FonctionFirstDerive
	 (if (/= CoefficientsSomme 0.0)
	   (mapcar (function (lambda (coef coef1)
			       (- (/ coef1 CoefficientsSomme)
				  (/ (* coef Coefficients1Somme)
				     (* CoefficientsSomme CoefficientsSomme)
				     )
				  )
			       )
			     )
		   CoefficientsPonderes
		   Coefficients1Ponderes
		   )
	   (ListeNb 0.0 PtControleNb)
	   )
	)
  (list FonctionBase FonctionFirstDerive)
  )

(defun ListeCumulNb (nb / liste)
  (while (> nb 0)
    (setq liste	(cons nb liste)
	  nb	(1- nb)
	  )
    )
  liste
  )

(defun ListeNb (n nb / liste) (repeat nb (setq liste (cons n liste))) liste)

(defun dxf (code alst)
  (apply 'append
	 (mapcar (function (lambda (x)
			     (if (= (car x) code)
			       (list (cdr x))
			       )
			     )
			   )
		 alst
		 )
	 )
  )

(defun nthcdr (i lst) (repeat i (setq lst (cdr lst))) lst)