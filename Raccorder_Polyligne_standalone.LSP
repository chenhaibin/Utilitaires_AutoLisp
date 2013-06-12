(defun c:tst ()
  (setvar "FILLETRAD" (getdist "Rayon de raccord"))
  (Raccorder_Polyligne
    (car (entsel "Selectionnez la polyligne à raccorder"))
    (getvar "FILLETRAD")
  )
)

(defun Raccorder_Polyligne (ent	   rayon  /	 data	pts    enttmp ferme
			    lst	   lstPl  lent	 seg1	seg2   ptcomm pt1
			    pt2	   deriv1 deriv2 ss
			   )
  ;; Nettoyage de la polyligne
  ;; On supprime le dernier sommet si celui ci est superposé avec le premier
  ;; et on la déclare fermée
  (and (setq data (entget ent)
	     pts  (dxf 10 data)
       )
       (and (= (cdr (assoc 0 data)) "LWPOLYLINE")
	    (apply 'and
		   (mapcar (function (lambda (x1 x2) (equal x1 x2 1e-9)))
			   (car pts)
			   (last pts)
		   )
	    )
       )
       (setq data (reverse data)
	     data (remove-ele (assoc 10 data) data)
	     data (remove-ele (assoc 40 data) data)
	     data (remove-ele (assoc 41 data) data)
	     data (remove-ele (assoc 42 data) data)
	     data (reverse data)
	     data (subst (cons 70 1) (assoc 70 data) data)
       )
       (entmod data)
  )

  ;; Si elle est fermée, ok
  ;; Si elle n'est pas fermé mais qu'elle à plus de deux sommets, ok
  (if (or (= (cdr (assoc 70 data)) 1) (> (length (dxf 10 (entget ent))) 2))
    (progn
      ;; Pour éviter d'avoir des pb de raccord
      ;; On décale la polyligne dans les deux sens.
      ;; Cela permet de supprimer les segments qui disparaitront lors du raccord
      (setq enttmp (Courbe-Decaler ent rayon nil))

      (entdel ent)
      (setq ent (Courbe-Decaler enttmp (* 2.0 rayon) t))
      (entdel enttmp)
      (setq enttmp (Courbe-Decaler ent rayon nil))
      (entdel ent)

      (setq ent	  enttmp
	    data  (entget ent)
	    ferme (= (cdr (assoc 70 data)) 1)
      )
      (setq lent (entlast))
      (command "_explode" ent)
      (setq lent (entnext lent))
      (while lent
	(setq lst  (cons lent lst)
	      lent (entnext lent)
	)
      )
      (setq lst	  (reverse lst)
	    lstPl lst
      )
      (mapcar
	(function
	  (lambda (seg1 seg2)
	    ;; On recherche le point 
	    (setq ptcomm (car (common-fuzz
				(Courbe-PtDepartPtArrive seg1)
				(Courbe-PtDepartPtArrive seg2)
				1e-4
			      )
			 )
		  pt1	 (Courbe-PointLePlusProche seg1 ptcomm)
		  pt2	 (Courbe-PointLePlusProche seg2 ptcomm)
		  deriv1 (Courbe-Derivee1
			   seg1
			   (Courbe-ParamAuPoint seg1 pt1)
			 )
		  deriv2 (Courbe-Derivee1
			   seg2
			   (Courbe-ParamAuPoint seg2 pt2)
			 )
	    )
	    (if	(not (colinear deriv1 deriv2 1e-5))
	      (progn
		(setq pt1 (Courbe-PointALaDistance
			    seg1
			    0.1
			    (Courbe-Position seg1 pt1)
			  )
		      pt2 (Courbe-PointALaDistance
			    seg2
			    0.1
			    (Courbe-Position seg2 pt2)
			  )
		)
		(if (not (or (zerop (apply '+ pt1)) (zerop (apply '+ pt2))))
		  (progn
		    (setq ent (entlast))
		    (command "_fillet" (list seg1 pt1) (list seg2 pt2))
		    (if	(not (equal ent (entlast)))
		      (setq lstPl (cons (entlast) lstPl))
		    )
		  )
		)
	      )
	    )
	  )
	)
	lst
	(if ferme
	  (rot1 lst)
	  (cdr lst)
	)
      )

      (setq ss (ssadd))

      (mapcar (function (lambda (x) (ssadd x ss))) lstPl)

      (setvar "PEDITACCEPT" 1)
      (command "pedit" "m" ss "")
      (command "j" 0.1 "")

    )
  )
  (princ)
)


;;; =================== Sous-fonction ======================

;; DXF
;; Retourne la liste des données associé au code dxf
(defun dxf (code alst)
  (mapcar 'cdr (massoc code alst))
)


;; MASSOC
;; assoc multiple, retourne toutes les clef key
(defun massoc (key alst)
  (apply 'append
	 (mapcar '(lambda (x)
		    (if	(= (car x) key)
		      (list x)
		    )
		  )
		 alst
	 )
  )
)


;; REMOVE-ELE
;; Retourne la liste sans la première occurence de l'expression

(defun remove-ele (ele lst)
  (if (equal ele (car lst))
    (cdr lst)
    (cons (car lst) (remove-ele ele (cdr lst)))
  )
)

;; COMMON-FUZZ
;; Comme COMMON avec une tolérance dans la comparaison

(defun common-fuzz (l1 l2 fuzz)
  (if l1
    (if	(member-fuzz (car l1) l2 fuzz)
      (cons (car l1) (common-fuzz (cdr l1) l2 fuzz))
      (common-fuzz (cdr l1) l2 fuzz)
    )
  )
)

;; MEMBER-FUZZ
;; Comme MEMBER avec une tolérance dans la comparaison

(defun member-fuzz (expr lst fuzz)
  (while (and lst (not (equal (car lst) expr fuzz)))
    (setq lst (cdr lst))
  )
  lst
)


;;; ROT1 - mettre le premier élément à la fin, version simple,
;;;        (rotation par un)
(defun rot1 (lst) (append (cdr lst) (list (car lst))))

;; COLINEAR

(defun colinear	(v1 v2 fuzz)
  (cond
    ((or (and (equal (car v1) 0 fuzz) (equal (car v2) 0 fuzz))
	 (and (equal (cadr v1) 0 fuzz) (equal (cadr v2) 0 fuzz))
     )
     t
    )
    ((or (equal (car v1) 0 fuzz)
	 (equal (car v2) 0 fuzz)
     )
     nil
    )
    (t
     (equal (/ (cadr v1) (car v1)) (/ (cadr v2) (car v2)) fuzz)
    )
  )
)

;; V2D
;; Retourne un vecteur 2D
;;
;; Arguments : un vecteur
(defun v2d (l)
  (mapcar '+ (append l '(0.0 0.0)) '(0.0 0.0))
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