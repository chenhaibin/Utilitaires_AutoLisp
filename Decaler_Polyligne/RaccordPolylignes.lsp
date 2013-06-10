(defun c:tst ()
  (Raccord_Polyligne (car (entsel)))
)

(defun Raccord_Polyligne (ent / rayon lst)
  (setq rayon (getvar "FILLETRAD")
	data (entget ent)
	)
  (if (= (cdr (assoc 0 data)) "LWPOLYLINE")
    (progn
      (setq lst (ListeSegments ent))
      (mapcar (function
		(lambda	(seg1 seg2 / ent1 ent2 int pt1 pt2)
		  (cond
		    ;; Arc Arc
		    ((and (> (length seg1) 2) (> (length seg2) 2))
		     (setq ent1	(DecalerArc seg1 rayon t)
			   ent2	(DecalerArc seg2 rayon t)
			   int	(IntersArcArc ent1 ent2)
		     )
		     (if (= (length int) 0)
		       (setq ent1 (DecalerArc seg1 rayon nil)
			     ent2 (DecalerArc seg2 rayon nil)
			     int  (IntersArcArc ent1 ent2)
		       )
		     )
		     (setq int (car int)
			   pt1 (ProjeterPointSurArc seg1 int)
			   pt2 (ProjeterPointSurArc seg2 int)
		     )
		    )

		    ;; Arc Ligne
		    ((and (> (length seg1) 2) (= (length seg2) 2))
		     (setq ent1	(DecalerArc seg1 rayon t)
			   ent2	(DecalerLigne seg2 rayon t)
			   int	(IntersArcLigne ent1 ent2)
		     )
		     (if (= (length int) 0)
		       (setq ent1 (DecalerArc seg1 rayon nil)
			     ent2 (DecalerLigne seg2 rayon nil)
			     int  (IntersArcLigne ent1 ent2)
		       )
		     )
		     (setq int (car int)
			   pt1 (ProjeterPointSurArc seg1 int)
			   pt2 (ProjeterPointSurLigne seg2 int)
		     )
		    )

		    ;; Ligne Arc
		    ((and (= (length seg1) 2) (> (length seg2) 2))
		     (setq ent1	(DecalerLigne seg1 rayon t)
			   ent2	(DecalerArc seg2 rayon t)
			   int	(IntersArcLigne ent2 ent1)
		     )
		     (if (= (length int) 0)
		       (setq ent1 (DecalerLigne seg1 rayon nil)
			     ent2 (DecalerArc seg2 rayon nil)
			     int  (IntersArcLigne ent2 ent1)
		       )
		     )
		     (setq int (car int)
			   pt1 (ProjeterPointSurLigne seg1 int)
			   pt2 (ProjeterPointSurArc seg2 int)
		     )
		    )

		    ;; Ligne Ligne
		    ((and (= (length seg1) 2) (= (length seg2) 2))
		     (setq ent1	(DecalerLigne seg1 rayon t)
			   ent2	(DecalerLigne seg2 rayon t)
			   int	(IntersLigneLigne ent1 ent2)
		     )
		     (if (= (length int) 0)
		       (setq ent1 (DecalerLigne seg1 rayon nil)
			     ent2 (DecalerLigne seg2 rayon nil)
			     int  (IntersLigneLigne ent1 ent2)
		       )
		     )
		     (setq pt1 (ProjeterPointSurLigne seg1 int)
			   pt2 (ProjeterPointSurLigne seg2 int)
		     )
		    )
		  )
		  (make_point int 0)
		  (make_point pt1 0)
		  (make_point pt2 0)
		  (redraw)
		)
	      )
	      (if (= (cdr (assoc 70 (entget ent))) 0)
		(butlast lst)
		lst
	      )
	      (if (= (cdr (assoc 70 (entget ent))) 0)
		(cdr lst)
		(rot1 lst)
	      )

      )
    )
  )
  (princ)
)

;; PROJETERPOINTSURARC
;; Renvoi le point projeté sur l'arc
(defun ProjeterPointSurArc (Arc Pt)
  (setq	ct  (car Arc)
	ang (angle ct Pt)
	ptp (polar ct ang (cadddr Arc))
  )
  (if (TstAngle ang (cadr Arc) (caddr Arc))
    ptp
    nil
  )
)

;; PROJETERPOINTSURLIGNE
;; Renvoi le point projeté sur la ligne
(defun ProjeterPointSurLigne (Ligne Pt)
  (setq	ptp (list (- (+ (car Pt) (cadar Ligne)) (cadadr Ligne))
		  (- (+ (cadr Pt) (caadr Ligne)) (caar Ligne))
	    )
	ptp (inters Pt ptp (car Ligne) (cadr Ligne) nil)
	;; On calcul le point d'intersection
	ptp (inters Pt ptp (car Ligne) (cadr Ligne) t)
	    ;; On verifie si le point est sur la ligne
  )
)


;; DECALERARC
;; Renvoi un arc décalé
(defun DecalerArc (Arc Dist Cote / r)
  (setq	r    (cadddr Arc)
	Dist (*	(if (xor Cote (last Arc))
		  -1
		  1
		)
		Dist
	     )
  )
  (subst (+ r Dist) r Arc)
)

;; DECALERLIGNE
;; Renvoi une ligne décalée
(defun DecalerLigne (Ligne Dist Cote / v vp pt)
  (setq	v  (vect (car Ligne) (cadr Ligne))
	vp (vperp v)
	vp (vunit vp)
	vp (vxs	vp
		(* (if Cote
		     -1
		     1
		   )
		   Dist
		)
	   )
	pt (v+v (car Ligne) vp)
  )
  (list pt (v+v pt v))
)

;; INTERSARCARC
;; Renvoi les points d'intersection entre deux arcs
(defun IntersArcArc (Arc1   Arc2   /	  ct1	 ct2	r1     r2
		     ang1   ang2   c	  alpha	 beta	depA1  arrA1
		     depA2  arrA2
		    )
  (setq	ct1   (car Arc1)
	ct2   (car Arc2)
	r1    (cadddr Arc1)
	r2    (cadddr Arc2)
	ang1  (angle ct1 ct2)
	ang2  (angle ct2 ct1)
	c     (distance ct1 ct2)
	depA1 (cadr Arc1)
	arrA1 (caddr Arc1)
	depA2 (cadr Arc2)
	arrA2 (caddr Arc2)
  )

  (cond
    ;; Si r2 = 0 ou r1 = 0 ou la distance entre les deux centre est supérieure à la somme des rayons
    ;; il n'y a pas d'intersection
    ((or (zerop r2) (zerop r1) (> (- c (+ r1 r2) 1e-9) 0.0))
     nil
    )
    ;; Si la distance entre les deux centre est égale à la somme des rayons
    ;; Une intersection possible
    ((equal c (+ r1 r2) 1e-9)
     (if (and (TstAngle ang1 depA1 arrA1)
	      (TstAngle ang2 depA2 arrA2)
	 )
       (list (polar ct1 ang1 r1))
     )
    )
    ;; Sinon
    ;; Deux intersections possible
    (t
     (setq
       alpha (acos
	       (/ (- (+ (expt r1 2) (expt c 2)) (expt r2 2)) (* 2 r1 c))
	     )
       beta  (acos
	       (/ (- (+ (expt r2 2) (expt c 2)) (expt r1 2)) (* 2 r2 c))
	     )
     )
     (append (if (and (TstAngle (+ ang1 alpha) depA1 arrA1)
		      (TstAngle (- ang2 beta) depA2 arrA2)
		 )
	       (list (polar ct1 (+ ang1 alpha) r1))
	     )
	     (if (and (TstAngle (- ang1 alpha) depA1 arrA1)
		      (TstAngle (+ ang2 beta) depA2 arrA2)
		 )
	       (list (polar ct1 (- ang1 alpha) r1))
	     )
     )
    )
  )
)

;; INTERSARCLIGNE
;; Renvoi le point d'intersection entre un arc et une ligne
(defun IntersArcLigne
       (Arc Ligne / ct r pt ang dst alpha depA arrA depL arrL)
  (setq	ct   (car Arc)
	r    (cadddr Arc)
	pt   (list (- (+ (car ct) (cadar Ligne)) (cadadr Ligne))
		   (- (+ (cadr ct) (caadr Ligne)) (caar Ligne))
	     )
	pt   (inters ct pt (car Ligne) (cadr Ligne) nil)
	ang  (angle ct pt)
	dst  (distance ct pt)
	depA (cadr Arc)
	arrA (caddr Arc)
	depL (min (angle ct (car Ligne)) (angle ct (cadr Ligne)))
	arrL (max (angle ct (car Ligne)) (angle ct (cadr Ligne)))
  )
  (cond
    ;; Si r = 0 ou dst = 0
    ;; il n'y a pas d'intersection
    ((or (zerop r) (zerop dst))
     nil
    )
    ;; Si la distance entre la ligne et le centre est égale au rayon
    ;; Une intersection possible
    ((equal r dst 1e-9)
     (if (and (TstAngle ang depA arrA)
	      (TstAngle ang depL arrL)
	 )
       (list pt)
     )
    )
    ;; Sinon
    ;; Deux intersections possible
    (t
     (setq alpha (acos (/ dst r)))
     (append (if (and (TstAngle (+ ang alpha) depA arrA)
		      (TstAngle (+ ang alpha) depL arrL)
		 )
	       (list (polar ct (+ ang alpha) r))
	     )
	     (if (and (TstAngle (- ang alpha) depA arrA)
		      (TstAngle (- ang alpha) depL arrL)
		 )
	       (list (polar ct (- ang alpha) r))
	     )
     )
    )
  )
)

;; INTERSLIGNELIGNE
;; Renvoi le point d'intersection entre deux lignes
(defun IntersLigneLigne	(Ligne1 Ligne2)
  (inters (car Ligne1)
	  (cadr Ligne1)
	  (car Ligne2)
	  (cadr Ligne2)
	  t
  )
)

;; SEGMENT
;; Renvoi le segment No de la polyligne
(defun Segment (Pl No / Data Bulge Points Pt1 Pt2 Lst)
  (setq Data (entget Pl))
  (and (= (cdr (assoc 0 Data)) "LWPOLYLINE")
       (setq No (fix No))
       (> No 0)
       (< No (+ (length (massoc 10 Data)) (cdr (assoc 70 Data))))
       (setq Data   (if	(cdr (assoc 70 Data))
		      (appendlst Data (assoc 10 Data))
		      Data
		    )
	     Bulge  (dxf '42 Data)
	     Points (dxf '10 Data)
       )
       (setq Pt1 (nth (1- No) Points)
	     Pt2 (nth No Points)
	     Lst (if (zerop (setq B (nth (1- No) Bulge)))
		   (list Pt1 Pt2)
		   (BulgeToArc Pt1 Pt2 B)
		 )
       )
  )
  Lst
)

;; LISTESEGMENTS
;; Renvoi la liste des segments de la polyligne
(defun ListeSegments (Pl / Data Bulge Points Pt1 Pt2 Lst)
  (setq Data (entget Pl))
  (if (= (cdr (assoc 0 Data)) "LWPOLYLINE")
    (progn
      (setq No	   1
	    Rp	   (1- (+ (length (massoc 10 Data)) (cdr (assoc 70 Data))))
	    Data   (if (cdr (assoc 70 Data))
		     (appendlst Data (assoc 10 Data))
		     Data
		   )
	    Bulge  (dxf '42 Data)
	    Points (dxf '10 Data)
      )

      (repeat Rp
	(setq Pt1 (nth (1- No) Points)
	      Pt2 (nth No Points)
	      Lst (append Lst
			  (list	(if (zerop (setq B (nth (1- No) Bulge)))
				  (list Pt1 Pt2)
				  (BulgeToArc Pt1 Pt2 B)
				)
			  )
		  )
	      No  (1+ No)
	)
      )
    )
  )
  Lst
)

;; ==================================================================================================

(defun TstAngle	(AngleTst AngleDep AngleArr)
  (setq AngleTst (ang<2pi (- AngleTst AngleDep)))
  (and (>= AngleTst (- 0 1e-9))
       (<= AngleTst (+ (ang<2pi (- AngleArr AngleDep)) 1e-9))
  )
)