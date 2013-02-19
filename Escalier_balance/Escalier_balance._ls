(vl-load-com)

;;; Variables globales pour la saisie
(setq Var_MarchePaliere	(if Var_MarchePaliere
			  Var_MarchePaliere
			  100.0
			)
      Var_NbGiron	(if Var_NbGiron
			  Var_NbGiron
			  1
			)
      Var_GironMax	(if Var_GironMax
			  Var_GironMax
			  280.0
			)
      Var_HtMarche	(if Var_HtMarche
			  Var_HtMarche
			  0.0
			)
      Var_SupprimerNdM	(if Var_SupprimerNdM
			  Var_SupprimerNdM
			  "Oui"
			)
      Var_SupprimerCrem	(if Var_SupprimerCrem
			  Var_SupprimerCrem
			  "Oui"
			)
)

(defun c:mdc (/)
  (princ "\n----- Modifier la cr�maill�re selon une courbe -----")
  (and
    (setq cremaillere (car (entsel "\nSelectionnez la cremaill�re :")))
    (setq courbe (car (entsel "\nSelectionnez la nouvelle courbe de la cr�maill�re :")))
    (setq p		    (initget "Oui Non")
	  SupprimerCrem	    (getkword (strcat "\nVoulez-vous supprimer l'ancienne [Oui/Non] <"
					      Var_SupprimerCrem
					      "> : "
				      )
			    )
	  SupprimerCrem	    (if	SupprimerCrem
			      SupprimerCrem
			      Var_SupprimerCrem
			    )
	  Var_SupprimerCrem SupprimerCrem
    )
    (setq ;; On r�cup�re les points
	  liste_crem (dxf '10 (entget Cremaillere))
	  ;; si la cr�maill�re comprend une petite partie horizontale au d�but, on commence � l'inde 2
	  i	     (if (equal (caar liste_crem) (caadr liste_crem) 0.001)
		       1
		       2
		     )
    )
    (while (< i (length liste_crem))

      (setq ;; on recup�re le nez de marche
	    pt	      (nth i liste_crem)
	    ;; on cr�er une ligne horizontale temporaire invisible
	    lignetemp (make_line (list pt (mapcar '+ pt '(1.0 0.0))) 1)
	    ;; Point d'intersection de cette ligne avec la courbe
	    nvpt      (car (Courbe-Intersection courbe lignetemp 2))
	    ;; on supprime la ligne
	    p	      (entdel lignetemp)
      )
      ;; Si il y a un point d'intersection, on remplace
      (if nvpt
	(setq ;; on remplace le point
	      liste_crem (subst-i nvpt i liste_crem)
	      ;; on modifie le point du dessous pour que les contremarches soient verticales
	      nvpt	 (list (car nvpt) (cadr (nth (1- i) liste_crem)))
	      ;; et on remplace
	      liste_crem (subst-i nvpt (1- i) liste_crem)
	)
      )
      (setq ;; on passe au nez de marche suivant
	    i
	     (+ i 2)
      )
    )
    ;; On cr�e la nouvelle cr�maill�re
    (make_lwpline liste_crem 0)
    ;; Et on supprime l'ancienne si c'est ok
    (if	(= SupprimerCrem "Oui")
      (entdel cremaillere)
      't
    )
  )

  (princ)
)

(defun c:tb (/)
  (princ "\n----- Tra�er les marches balanc�es � partir de la cr�maill�re -----")

  (and
    (setq sel_ldf (entsel "\nSelectionnez la ligne de foul�e pr�s du d�part :"))
    (setq p		  (princ "\nSelectionnez les nez de marches :")
	  sel_nezdemarche (ssget '((-4 . "<OR")
				   (0 . "LINE")
				   (0 . "LWPOLYLINE")
				   (0 . "CIRCLE")
				   (0 . "ELLIPSE")
				   (0 . "RAY")
				   (0 . "XLINE")
				   (0 . "ARC")
				   (0 . "POLYLINE")
				   (0 . "SPLINE")
				   (-4 . "OR>")
				  )
			  )
    )
    (setq p		   (initget "Oui Non")
	  SupprimerNdM
			   (getkword (strcat "\nVoulez-vous supprimer les anciens nez de marche [Oui/Non] <"
					     Var_SupprimerNdM
					     "> : "
				     )
			   )
	  SupprimerNdM
			   (if SupprimerNdM
			     SupprimerNdM
			     Var_SupprimerNdM
			   )
	  Var_SupprimerNdM SupprimerNdM
    )
    (setq Cremaillere (car (entsel "\nSelectionnez la cr�maill�re :")))
    (setq sel_LimonC
	   (entsel
	     "\nSelectionnez le limon sur lequel se trouve la cr�maill�re pr�s du d�part :"
	   )
    )
    (setq LimonO (car (entsel "\nSelectionnez le limon oppos� :")))

    (setq i	   0
	  liste_pt 'nil
	  ldf	   (car sel_ldf)
	  LimonC   (car sel_LimonC)
	  finldf   (Courbe-Position ldf (cadr sel_ldf))
	  fincrem  (Courbe-Position LimonC (cadr sel_LimonC))
	  p	   't
    )

;;; Liste des points d'intersection entre les nez de marche et la courbe
    (repeat (sslength sel_nezdemarche)
      (if (setq point (car (Courbe-Intersection ldf (ssname sel_nezdemarche i) 2)))
	(setq liste_pt (appendlst liste_pt point))
      )
      (setq i (1+ i))
    )

    (if	(= SupprimerNdM "Oui")
      (mapcar 'entdel (SelEnList sel_nezdemarche))
      't
    )

    (setq liste_dist   (Courbe-ListeDistancesAuxPoints ldf liste_pt finldf)
	  liste_dist   (vl-sort liste_dist '<)
	  liste_crem   (dxf '10 (entget Cremaillere))
	  liste_crem   (if (equal (caar liste_crem) (caadr liste_crem) 0.001)
			 (cons (car liste_crem) liste_crem)
			 liste_crem
		       )
	  liste_crem   (split-list liste_crem 2)
	  liste_crem   (mapcar 'car liste_crem)
	  pt_insertion (car liste_crem)
	  liste_crem   (mapcar (function (lambda (pt) (- (car pt) (car pt_insertion)))) (cdr liste_crem))
    )
    (mapcar (function (lambda (ptldf ptcrem)
			(setq lignetemp	(make_line (list ptldf ptcrem) 1)
			      ptint	(car (Courbe-Intersection LimonO lignetemp 2))
			)
			(entdel lignetemp)
			(make_line (list ptint ptcrem) 0)
		      )
	    )
	    (Courbe-ListePointsAuxDistances ldf liste_dist finldf)
	    (Courbe-ListePointsAuxDistances LimonC liste_crem fincrem)
    )
  )

  (princ)
)

(defun c:tm (/)
  (princ "\n----- Tra�er les marches rayonnantes � la ligne de foul�e -----")

  (and
    (setq sel_ldf (entsel "\nSelectionnez la ligne de foul�e pr�s du d�part :"))
    (setq LimonG (car (entsel "\nSelectionnez le limon gauche :")))
    (setq LimonD (car (entsel "\nSelectionnez le limon droit :")))
    (setq marche_paliere    (getdist (strcat "\nDimension de la marche pali�re <" (rtos Var_MarchePaliere 2) "> : "))
	  marche_paliere    (if	marche_paliere
			      marche_paliere
			      Var_MarchePaliere
			    )
	  Var_MarchePaliere marche_paliere
    )
    (setq p	      (initget 128 "GironMax")
	  nb_giron    (getint (strcat "\nIndiquez le nb de giron ou [GironMax] <" (rtos Var_NbGiron 2 0) "> : "))
	  nb_giron    (cond
			((= nb_giron "GironMax")
			 (setq GironMax	    (getreal (strcat "\nIndiquez le giron maximum <" (rtos Var_GironMax 2) "> : "))
			       GironMax	    (if	GironMax
					      GironMax
					      Var_GironMax
					    )
			       Var_GironMax GironMax
			 )
			 (1+ (fix (/ (Courbe-Longueur (car sel_ldf)) GironMax)))
			)
			(nb_giron nb_giron)
			(t Var_NbGiron)
		      )
	  Var_NbGiron nb_giron
    )
    (setq
      ldf	 (car sel_ldf)
      fin	 (Courbe-Position ldf (cadr sel_ldf))
      giron	 (/ (- (Courbe-Longueur ldf) marche_paliere) nb_giron)
      liste_dist '(0.0)
    )

    (princ (strcat "\nGiron : " (rtos giron 2)))

;;; Liste des positions des marches
    (repeat nb_giron
      (setq liste_dist (appendlst liste_dist (+ (last liste_dist) giron)))
    )

;;; On recup�re la perpendiculaire � la courbe
    (setq liste_point
	   (mapcar
	     (function
	       (lambda (dist / pt)
		 (setq pt (Courbe-PointALaDistance ldf dist fin))
		 (list
		   pt
		   (V+V pt (VPERP (VXS (Courbe-Derivee1 ldf (Courbe-ParamALaDistance ldf dist fin)) 10)))
		 )
	       )
	     )
	     liste_dist
	   )

    )

;;; On trace les marches
    (foreach data liste_point
      (setq ligne (make_line data 1))
      (make_line
	(list (car (Courbe-Intersection LimonG ligne 2)) (car (Courbe-Intersection LimonD ligne 2)))
	0
      )
      (entdel ligne)
    )
  )

  (princ)
)

(defun c:tc (/)
  (princ "\n----- Tra�er la cr�maill�re -----")
  (and
    (setq sel_limon (entsel "\nSelectionnez le limon � d�velopper pr�s du d�part :"))
    (setq p		  (princ "\nSelectionnez les nez de marches :")
	  sel_nezdemarche (ssget '((-4 . "<OR")
				   (0 . "LINE")
				   (0 . "LWPOLYLINE")
				   (0 . "CIRCLE")
				   (0 . "ELLIPSE")
				   (0 . "RAY")
				   (0 . "XLINE")
				   (0 . "ARC")
				   (0 . "POLYLINE")
				   (0 . "SPLINE")
				   (-4 . "OR>")
				  )
			  )
    )
    (setq point_insertion (v2d (getpoint "\nIndiquez le point d'insertion du d�velopp� :")))
    (setq ht_marche    (getdist (strcat "\nIndiquez la hauteur de marche (ou distance) <" (rtos Var_HtMarche 2) "> : "))
	  ht_marche    (if ht_marche
			 ht_marche
			 Var_HtMarche
		       )
	  Var_HtMarche ht_marche
	  ht_marche    (list 0.0 ht_marche)
    )
    (setq i	     0
	  liste_dist 'nil
	  limon	     (car sel_limon)
	  fin	     (Courbe-Position limon (cadr sel_limon))
	  p	     't
    )

;;; Liste des points d'intersection entre les nez de marche et la courbe
    (repeat (sslength sel_nezdemarche)
      (if (setq point (car (Courbe-Intersection limon (ssname sel_nezdemarche i) 2)))
	(setq liste_dist (appendlst liste_dist (Courbe-DistanceAuPoint limon point fin)))
      )
      (setq i (1+ i))
    )

;;; Tri des distances
;;; On initialise liste_point avec le point d'insertion et le premier decalage
;;; On y rajoute une premi�re ht
;;; Distance entre chaque point
;;; On cr�er les vecteurs

    (setq liste_dist  (vl-sort liste_dist '<)
	  liste_point (list point_insertion)
	  liste_point (if (equal (car liste_dist) 0.0 0.001)
			liste_point
			(appendlst liste_point (v+v point_insertion (list (car liste_dist) 0.0)))
		      )
	  liste_point (appendlst liste_point (v+v (last liste_point) ht_marche))
	  liste_diff  (mapcar '- (cdr liste_dist) liste_dist)
	  liste_diff  (mapcar (function (lambda (x) (list x 0.0))) liste_diff)

    )
    (foreach diff liste_diff
      (setq liste_point	(appendlst liste_point (v+v (last liste_point) diff))
	    liste_point	(appendlst liste_point (v+v (last liste_point) ht_marche))
      )
    )
    (setq derniere_dist	(last liste_dist)
	  lg_courbe	(Courbe-Longueur limon)
    )
    (if	(not (equal derniere_dist lg_courbe 0.001))
      (setq liste_point
	     (appendlst
	       liste_point
	       (mapcar '+ (last liste_point) (list (- lg_courbe derniere_dist) 0.0))
	     )
      )
    )
    (make_lwpline liste_point 0)
  )

  (princ)
)

(terpri)
(princ "\n========================================================================")
(princ "\n     Utilitaire d'aide au trac� d'escalier � marches balanc�es          ")
(princ "\n------------------------------------------------------------------------")
(princ "\n	- Tra�er les marches rayonnantes � la ligne de foul�e : TM")
(princ "\n	- Tra�er la cr�maill�re : TC")
(princ "\n	- Modifier la cr�maill�re selon une courbe : MDC")
(princ "\n	- Tra�er les marches balanc�es � partir de la cr�maill�re : TB")
(princ "\n========================================================================")
(terpri)

(princ)
 ;|�Visual LISP� Format Options�
(150 2 40 2 nil "Fin de " 100 9 0 0 0 T T nil T)
;*** NE PAS AJOUTER de texte au-dessous du commentaire! ***|;
