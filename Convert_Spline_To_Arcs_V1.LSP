(defun c:S2A1 (/ Spline Dev_max)
  (vl-load-com)
  (if (and (setq Spline (car (entsel "\nSélectionnez une Spline : ")))
	   (= (cdr (assoc 0 (entget Spline))) "SPLINE")
	   )
    (progn (setq SplineDxfData (entget Spline)
		 Degree	       (car (dxf '71 SplineDxfData))
		 NoeudsNb      (car (dxf '72 SplineDxfData))
		 PtControleNb  (car (dxf '73 SplineDxfData))
		 Noeuds	       (dxf '40 SplineDxfData)
		 Poids	       (if (setq Poids (dxf '41 SplineDxfData))
				 Poids
				 (ListeNb 1.0 NoeudsNb)
				 )
		 PtControle    (dxf '10 SplineDxfData)
		 ParamMax      (apply 'max Noeuds)
		 Param	       (getreal (strcat "\nParametre [ 0 - " (rtos ParamMax 2 3) "] : "))
		 FxRationnel   (Cox-DeBoor2 Param Degree Noeuds Poids)
		 Point1	       (PointFromFonctionBase2 (car FxRationnel) PtControle Param)
		 Point2	       (PointFromFonctionBase2 (cadr FxRationnel) PtControle Param)
		 ;;;Point3	       (PointFromFonctionBase2 (caddr FxRationnel) PtControle Param)
		 )
	   (princ Point1)
	   (terpri)
	   (princ Point2)
	   (terpri)
	   (setq Point2 (mapcar '+ Point1 (mapcar '(lambda (x) (* x 1000)) Point2)))
	   (princ Point2)
	   (make_point Point1 0)
	   (make_point Point2 0)
	   )
    (prompt "\nCe n'est pas une spline.")
    )
  (princ)
  )


;;-----------------------------== PointFromFonctionBase2 ==-----------------------------;;
(defun PointFromFonctionBase2 (FonctionBase PtControle Param /)
  (apply 'mapcar
	 (cons '+
	       (mapcar (function (lambda (coeff Pt) (mapcar '(lambda (x) (* (eval coeff) x)) Pt)))
		       FonctionBase
		       PtControle
		       )
	       )
	 )
  )

;;-----------------------------== Cox-DeBoor2 ==-----------------------------;;
(defun Cox-DeBoor2 (Param	     Degree	      Noeuds	       Poids
		    /		     Coefficients     Coefficients1    Coefficients2
		    fx		     ffx	      ffFx	       gx
		    ggx		     gggx	      d		       j
		    Temp	     Temp1	      CoefficientsPonderes
		    Coefficients1Ponderes	      Coefficients2Ponderes
		    CoefficientsSomme		      Coefficients1Somme
		    Coefficients2Somme		      FonctionBase
		    )
  (setq	Coefficients
	 (mapcar (function (lambda (Tj Tj+1)
			     (if (and (>= Param Tj) (< Param Tj+1))
			       1.0
			       0.0
			       )
			     )
			   )
		 Noeuds
		 (cdr Noeuds)
		 )
	Coefficients
	 (reverse Coefficients)
	d 1
	)
  (while (<= d Degree)
    (setq j 0)
    (while (<= j (- (length Coefficients) 2))
      (setq fx	 (cons
		   '*
		   (list (list '/ (list '- 'Param (nth j Noeuds)) (- (nth (+ j d) Noeuds) (nth j Noeuds)))
			 (nth j Coefficients)
			 )
		   )
	    gx	 (cons '*
		       (list (list '/
				   (list '- (nth (+ j d 1) Noeuds) 'Param)
				   (- (nth (+ j d 1) Noeuds) (nth (+ j 1) Noeuds))
				   )
			     (nth (+ j 1) Coefficients)
			     )
		       )
	    Temp (cons (cond ((and (= (nth j Coefficients) 0.0) (= (nth (+ j 1) Coefficients) 0.0)) 0.0)
			     ((and (/= (nth j Coefficients) 0.0) (/= (nth (+ j 1) Coefficients) 0.0))
			      (list '+ fx gx)
			      )
			     ((/= (nth j Coefficients) 0.0) fx)
			     ((/= (nth (+ j 1) Coefficients) 0.0) gx)
			     )
		       Temp
		       )
	    )
      (if (= d (1- Degree))
	(setq ffx   (list '/ (nth j Coefficients) (- (nth (+ j d) Noeuds) (nth j Noeuds)))
	      ggx   (list '/ (nth (+ j 1) Coefficients) (- (nth (+ j d 1) Noeuds) (nth (+ j 1) Noeuds)))
	      Temp1 (cons
		      (cond ((and (= (nth j Coefficients) 0.0) (= (nth (+ j 1) Coefficients) 0.0)) 0.0)
			    ((and (/= (nth j Coefficients) 0.0) (/= (nth (+ j 1) Coefficients) 0.0))
			     (list '* (list '- ffx ggx) d)
			     )
			    ((/= (nth j Coefficients) 0.0) (list '* ffx d))
			    ((/= (nth (+ j 1) Coefficients) 0.0) (list '* (list '- 0.0 ggx) d))
			    )
		      Temp1
		      )
	      Temp1 (reverse Temp1)
	      )
	)
      (if (= d Degree)
	(setq ffx	    (list '/ (nth j Coefficients) (- (nth (+ j d) Noeuds) (nth j Noeuds)))
	      ggx	    (list '/ (nth (+ j 1) Coefficients) (- (nth (+ j d 1) Noeuds) (nth (+ j 1) Noeuds)))
	      Coefficients1 (cons
			      (cond
				((and (= (nth j Coefficients) 0.0) (= (nth (+ j 1) Coefficients) 0.0))
				 0.0
				 )
				((and (/= (nth j Coefficients) 0.0) (/= (nth (+ j 1) Coefficients) 0.0))
				 (list '* (list '- ffx ggx) d)
				 )
				((/= (nth j Coefficients) 0.0) (list '* ffx d))
				((/= (nth (+ j 1) Coefficients) 0.0) (list '* (list '- 0.0 ggx) d))
				)
			      Coefficients1
			      )
;;;	      fffx	    (list '/ (nth j Temp1) (- (nth (+ j 1) Noeuds) (nth j Noeuds)))
;;;	      gggx	    (list '/ (nth (+ j 1) Temp1) (- (nth (+ j d 1) Noeuds) (nth (+ j 1) Noeuds)))
;;;	      Coefficients2 (cons
;;;			      (cond ((and (= (- (nth (+ j 1) Noeuds) (nth j Noeuds)) 0.0)
;;;					  (= (- (nth (+ j d 1) Noeuds) (nth (+ j 1) Noeuds)) 0.0)
;;;					  )
;;;				     0.0
;;;				     )
;;;				    ((and (/= (- (nth (+ j 1) Noeuds) (nth j Noeuds)) 0.0)
;;;					  (/= (- (nth (+ j d 1) Noeuds) (nth (+ j 1) Noeuds)) 0.0)
;;;					  )
;;;				     (list '* (list '- fffx gggx) d)
;;;				     )
;;;				    ((/= (- (nth (+ j 1) Noeuds) (nth j Noeuds)) 0.0) (list '* fffx d))
;;;				    ((/= (- (nth (+ j d 1) Noeuds) (nth (+ j 1) Noeuds)) 0.0)
;;;				     (list '* (list '- 0.0 gggx) d)
;;;				     )
;;;				    )
;;;			      Coefficients2
;;;			      )
	      )
	)
      (setq j (1+ j))
      )
    (setq Coefficients
	   (reverse Temp)
	  Coefficients1
	   (reverse Coefficients1)
;;;	  Coefficients2
;;;	   (reverse Coefficients2)
	  Temp nil
	  d (1+ d)
	  )
    )
  (if (= Param (last Noeuds))
    (setq Coefficients (append (butlast Coefficients) (list 1.0)))
    )
  (setq	CoefficientsPonderes
	 (mapcar (function (lambda (x y) (list '* x y))) Coefficients Poids)
	Coefficients1Ponderes
	 (mapcar (function (lambda (x y) (list '* x y))) Coefficients1 Poids)
;;;	Coefficients2Ponderes
;;;	 (mapcar (function (lambda (x y) (list '* x y))) Coefficients2 Poids)
	CoefficientsSomme
	 (cons '+ CoefficientsPonderes)
	Coefficients1Somme
	 (cons '+ Coefficients1Ponderes)
;;;	Coefficients2Somme
;;;	 (cons '+ Coefficients2Ponderes)
	FonctionBase
	 (mapcar (function (lambda (coef) (list '/ coef CoefficientsSomme)))
		 CoefficientsPonderes
		 )
	FonctionFirstDeriv
	 (mapcar (function (lambda (coef coef1)
			     (list '-
				   (list '/ coef1 CoefficientsSomme)
				   (list '/
					 (list '* coef Coefficients1Somme)
					 (list '* CoefficientsSomme CoefficientsSomme)
					 )
				   )
			     )
			   )
		 CoefficientsPonderes
		 Coefficients1Ponderes
		 )
;;;	FonctionSecondDeriv
;;;	 (mapcar
;;;	   (function
;;;	     (lambda (coef coef1 coef2)
;;;	       (list
;;;		 '+
;;;		 (list '-
;;;		       (list '/ coef2 CoefficientsSomme)
;;;		       (list '/
;;;			     (list '+
;;;				   (list '* 2.0 coef1 Coefficients1Somme)
;;;				   (list '* coef Coefficients2Somme)
;;;				   )
;;;			     (list '* CoefficientsSomme CoefficientsSomme)
;;;			     )
;;;		       )
;;;		 (list '/
;;;		       (list '* 2.0 coef Coefficients1Somme Coefficients1Somme)
;;;		       (list '* CoefficientsSomme CoefficientsSomme CoefficientsSomme)
;;;		       )
;;;		 )
;;;	       )
;;;	     )
;;;	   CoefficientsPonderes
;;;	   Coefficients1Ponderes
;;;	   Coefficients2Ponderes
;;;	   )
	)
  (list FonctionBase FonctionFirstDeriv)
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