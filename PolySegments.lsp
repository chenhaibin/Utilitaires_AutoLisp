;;; OFSEGS (gile) 26/08/08
;;; Décale les segments de polyligne sélectionnés.

(defun c:ofsegs	(/ ofdist   ent	     pline    normal   elevat	params
		   points   side     closest  par      bulge	p1
		   p2	    arc_data
		  )
  (vl-load-com)
  (or *acdoc*
      (setq *acdoc* (vla-get-ActiveDocument (vlax-get-acad-object)))
  )
  (initget 6 "Par")
  (if (setq
	ofdist (getdist
		 (strcat "\nSpécifiez la distance de décalage ou [Par] <"
			 (if (< (getvar "OFFSETDIST") 0)
			   "par"
			   (rtos (getvar "OFFSETDIST"))
			 )
			 ">: "
		 )
	       )
      )
    (if	(= ofdist "Par")
      (setvar "OFFSETDIST" -1)
      (setvar "OFFSETDIST" ofdist)
    )
    (setq ofdist (getvar "OFFSETDIST"))
  )
  (if (and (setq ent (entsel "\nSélectionnez un segment à décaler: "))
	   (setq pline (vlax-ename->vla-object (car ent)))
	   (= (vla-get-ObjectName pline) "AcDbPolyline")
	   (setq normal (vlax-get pline 'Normal))
	   (setq elevat (vla-get-Elevation pline))
      )
    (progn
      (setq params (cons (fix (vlax-curve-getParamAtPoint
				pline
				(trans (osnap (cadr ent) "_nea") 1 0)
			      )
			 )
			 params
		   )
      )
      (HighlightSegment pline (car params))
      (while
	(setq ent (entsel "\nSélectionnez le segment suivant ou <Quitter>: "))
	 (if (equal (vlax-ename->vla-object (car ent)) pline)
	   (progn
	     (setq par (fix (vlax-curve-getParamAtPoint
			      pline
			      (trans (osnap (cadr ent) "_nea") 1 0)
			    )
		       )
		   params (if (member par params)
			    (vl-remove par params)
			    (cons par params)
			    )
	     )
	     (redraw)
	     (foreach p params (HighlightSegment pline p))
	   )
	 )
      )
      (if (setq	side (GetPointAboutPlane
		       normal
		       (trans (list 0 0 elevat) normal 0)
		       (if (minusp (getvar "OFFSETDIST"))
			 "\nSpécifiez une valeur pour \"Par le point\": "
			 "\nSpécifiez un point sur le côté à décaler: "
		       )
		     )
	  )
	(progn
	  (redraw)
	  (vla-StartUndoMark *acdoc*)
	  (setq	closest	(vlax-curve-getClosestPointTo pline side T)
		par	(vlax-curve-getParamAtPoint pline closest)
	  )
	  (if (minusp (getvar "OFFSETDIST"))
	    (setq ofdist (distance side closest))
	  )
	  (cond
	    ((equal closest (vlax-curve-getStartPoint pline) 1e-9)
	     (setq side (trans side 0 normal))
	    )
	    ((equal closest (vlax-curve-getEndPoint pline) 1e-9)
	     (setq par	(- par 1)
		   side	(trans side 0 normal)
	     )
	    )
	    ((= (fix par) par)
	     (setq side
		    (polar
		      (trans closest 0 normal)
		      ((if
			 (clockwise-p
			   (trans
			     (vlax-curve-getPointAtParam pline (- par 0.1))
			     0
			     normal
			   )
			   (trans closest 0 normal)
			   (trans
			     (vlax-curve-getPointAtParam pline (+ par 0.1))
			     0
			     normal
			   )
			 )
			  +
			  -
		       )
			(angle '(0 0 0)
			       (trans (vlax-curve-getFirstDeriv pline par)
				      0
				      normal
				      T
			       )
			)
			(/ pi 2)
		      )
		      ofdist
		    )
	     )
	    )
	    (T
	     (setq par	(fix par)
		   side	(trans side 0 normal)
	     )
	    )
	  )
	  (setq	bulge (vla-getBulge pline (fix par))
		p1    (trans (vlax-curve-getPointAtParam pline (fix par))
			     0
			     normal
		      )
		p2    (trans (vlax-curve-getPointAtParam pline (1+ (fix par)))
			     0
			     normal
		      )
	  )
	  (if (zerop bulge)
	    (if	(clockwise-p side p2 p1)
	      (setq ofdist (- ofdist))
	    )
	    (progn
	      (setq arc_data (PolyArc-data bulge p1 p2))
	      (if (minusp bulge)
		(if (< (cadr arc_data)
		       (distance (car arc_data) side)
		    )
		  (setq ofdist (- ofdist))
		)
		(if (< (distance (car arc_data) side)
		       (cadr arc_data)
		    )
		  (setq ofdist (- ofdist))
		)
	      )
	    )
	  )
	  (mapcar
	    (function
	      (lambda (p)
		(vl-catch-all-apply 'vla-Offset (list p ofdist))
		(vla-delete p)
	      )
	    )
	    (Copysegments pline params)
	  )
	  (vla-EndUndoMark *acdoc*)
	)
      )
    )
    (princ "\nEntité non valide.")
  )
  (princ)
)

;;================================================================;;

;; COPSEGS (gile) 26/03/08
;; Copie les segments de polyligne sélectionnés.

(defun c:copsegs (/ ent pl par lst)
  (vl-load-com)
  (if (and (setq ent (entsel "\nSélectionnez un segment à copier: "))
	   (setq pl (vlax-ename->vla-object (car ent)))
	   (= (vla-get-ObjectName pl) "AcDbPolyline")
      )
    (progn
      (setq par	(fix (vlax-curve-getParamAtPoint
		       pl
		       (trans (osnap (cadr ent) "_nea") 1 0)
		     )
		)
	    lst	(cons par lst)
      )
      (HighlightSegment pl par)
      (while
	(setq ent (entsel "\nSélectionnez le segment suivant ou <Quitter>: "))
	 (if (equal (vlax-ename->vla-object (car ent)) pl)
	   (progn
	     (setq par (fix (vlax-curve-getParamAtPoint
			      pl
			      (trans (osnap (cadr ent) "_nea") 1 0)
			    )
		       )
		   lst (if (member par lst)
			 (vl-remove par lst)
			 (cons par lst)
			 )
	     )
	     (redraw)
	     (foreach p lst (HighlightSegment pl p))
	   )
	 )
      )
      (setq lst (vl-sort lst '<))
      (if (setq from (getpoint "\nSpécifiez le point de base: "))
	(while (and
		 (setq to (vl-catch-all-apply
			    'getpoint
			    (list from "\nSpécifiez le deuxième point: ")
			  )
		 )
		 (listp to)
	       )
	  (mapcar (function (lambda (p)
			      (vla-move	p
					(vlax-3d-point (trans from 1 0))
					(vlax-3d-point (trans to 1 0))
			      )
			    )
		  )
		  (CopySegments pl lst)
	  )
	)
      )
      (redraw)
    )
    (princ "\nEntité non valide.")
  )
  (princ)
)

;;================================================================;;


;; CopySegments
;; Copie des segments de polyligne
;; Les segments sont copiés à la même place et conservent leurs propriétés
;; Les segments jointifs sont unis en une polyligne unique
;;
;; Arguments
;; pline : la polyligne source (vla-object)
;; params ; la liste des indices des segment à copier
;;
;; Retour
;; la liste des polylignes créées

(defun CopySegments (pline params / nor space tmp copy ret)
  (vl-load-com)
  (or *acdoc*
      (setq *acdoc* (vla-get-ActiveDocument (vlax-get-acad-object)))
  )
  (setq	params (vl-sort params '<)
	nor    (vlax-get pline 'Normal)
	space  (vla-ObjectIDToObject *acdoc* (vla-get-OwnerID pline))
  )
  (while params  
    (setq tmp	 (cons (car params) tmp)
	  params (cdr params)
    )
    (if	(and (zerop (car tmp))
	     (= (- (vlax-curve-getEndParam pline) 1) (last params))
	     (equal (vlax-curve-getStartPoint pline)
		    (vlax-curve-getEndPoint pline)
		    1e-9
	     )
	)
      (progn
	(setq params (reverse params)
	      tmp    (cons (car params) tmp)
	      params (cdr params)
	)
	(while (= (car params) (1- (car tmp)))
	  (setq	tmp    (cons (car params) tmp)
		params (cdr params)
	  )
	)
	(setq tmp    (reverse tmp)
	      params (reverse params)
	)
      )
    )
    (while (= (car params) (1+ (car tmp)))
      (setq tmp	   (cons (car params) tmp)
	    params (cdr params)
      )
    )
    (setq tmp (reverse (cons (1+ (car tmp)) tmp)))
    (setq
      pts
       (vl-remove nil
		  (mapcar
		    (function
		      (lambda (pa / pt)
			(if (setq pt (vlax-curve-getPointAtParam pline pa))
			  ((lambda (p)
			     (list (car p) (cadr p))
			   )
			    (trans pt 0 nor)
			  )
			)
		      )
		    )
		    tmp
		  )
       )
    )
    (setq copy
	   (vlax-invoke
	     space
	     'addLightWeightPolyline
	     (apply 'append pts)
	   )
    )
    (foreach p (cdr (reverse tmp))
      (vla-setBulge
	copy
	(vl-position p tmp)
	(vla-getBulge pline p)
      )
      (vla-getWidth pline p 'swid 'ewid)
      (vla-setWidth copy (vl-position p tmp) swid ewid)
    )
    (foreach prop '(Elevation	    Layer	    Linetype
		    LinetypeGeneration		    LinetypeScale
		    Lineweight	    Normal	    Thickness
		    TrueColor
		   )
      (if (vlax-property-available-p pline prop)
	(vlax-put copy prop (vlax-get pline prop))
      )
    )
    (setq tmp nil
	  ret (cons copy ret)
    )
  )
)

;;================================================================;;

;; HighlightSegment
;; Met un segment de polyligne en surbrillance
;;
;; Arguments
;; pl : la polyligne (vla-object)
;; par : l'indice du segment

(defun HighlightSegment	(pl par / p1 p2 n lst)
  (and
    (setq p1 (vlax-curve-getPointAtParam pl par))
    (setq p1 (trans p1 0 1))
    (setq p2 (vlax-curve-getPointAtParam pl (+ par 1)))
    (setq p2 (trans p2 0 1))
    (if	(zerop (vla-getBulge pl par))
      (grvecs (list -255 p1 p2))
      (progn
	(setq n 0)
	(repeat	100
	  (setq	lst (cons (trans (vlax-curve-getPointAtParam pl (+ n par)) 0 1)
			  lst
		    )
		n   (+ n 0.01)
	  )
	)
	(grvecs
	  (cons -255 (apply 'append (mapcar 'list lst (cdr lst))))
	)
      )
    )
  )
)

;;================================================================;;

;;; Clockwise-p
;;; Retourne T si les points p1 p2 et p3 tournent dans le sens horaire

(defun clockwise-p (p1 p2 p3)
  (< (sin (- (angle p1 p3) (angle p1 p2))) -1e-14)
)

;;================================================================;;

;;; Polyarc-data
;;; Retourne la liste des données d'un arc de polyligne (centre rayon angle).

(defun polyarc-data (bu p1 p2 / ang rad cen area cg)
  (setq	ang (* 2 (atan bu))
	rad (/ (distance p1 p2)
	       (* 2 (sin ang))
	    )
	cen (polar p1
		   (+ (angle p1 p2) (- (/ pi 2) ang))
		   rad
	    )
  )
  (list cen (abs rad) ang)
)

;;================================================================;;

;; GETPOINTABOUTPLANE
;; Retourne le point d'intersection de la perpendiculaire à la vue courante passant
;; par le point saisi par l'utilsateur et le plan défini par sa normale et un point.
;;
;; Arguments
;; nor : le vecteur normal du plan d'intersection
;; org : un point sur le plan d'intersection (SCG)
;; msg : le message d'invite ou ""
;;
;; Retour : les coordonnées (SCG) du point d'intersection ou nil

(defun GetPointAboutPlane (nor org msg / p1 p2 sc)
  (if (and (setq p1 (getpoint msg))
	   (setq p1 (trans p1 1 0))
	   (setq p2 (trans p1 0 2))
	   (setq p2 (trans (list (car p2) (cadr p2) (1+ (caddr p2))) 2 0))
	   (/= 0
	       (setq sc (apply '+ (mapcar '* nor (mapcar '- p2 p1))))
	   )
      )
    (mapcar
      (function
	(lambda	(x1 x2)
	  (+ (*	(/ (apply '+ (mapcar '* nor (mapcar '- p1 org))) sc)
		(- x1 x2)
	     )
	     x1
	  )
	)
      )
      p1
      p2
    )
  )
)