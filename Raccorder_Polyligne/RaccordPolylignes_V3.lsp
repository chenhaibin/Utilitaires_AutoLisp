(defun c:tsee ()
  (setvar "FILLETRAD" 20)
  (setq r (getvar "FILLETRAD"))
  (Raccorder_Polyligne (car (entsel)) r)
)


(defun c:RCP ()
  (setvar "CMDECHO" 0)

  (command "_zoom" "_e")

  (command "_qsave")



  (setq	Sel (ssget "_X" '((0 . "DIMENSION")))
	i   0
  )

  (if Sel
    (repeat (sslength Sel)
      (entdel (ssname Sel i))
      (setq i (1+ i))
    )
  )

  (setq	Sel   (ssget "_X" '((0 . "LINE") (8 . "LIGNES DE PLIAGE")))
	i     0
	PtMin '(1000000 1000000)
	PtMax '(-1000000 -1000000)
  )

  (if Sel
    (repeat (sslength Sel)
      (setq Ligne (entget (ssname Sel i))
	    PtMin (mapcar 'min
			  PtMin
			  (car (dxf '10 Ligne))
			  (car (dxf '11 Ligne))
		  )
	    PtMax (mapcar 'max
			  PtMax
			  (car (dxf '10 Ligne))
			  (car (dxf '11 Ligne))
		  )
      )
      (setq i (1+ i))
    )
  )

  (setq	PtMin (mapcar '+ PtMin '(3 3))
	PtMax (mapcar '- PtMax '(3 3))
  )

  (setq	Sel (ssget "_W"
		   PtMin
		   PtMax
		   '((0 . "ARC,LINE") (8 . "0"))
	    )
	i   0
  )

  (setvar "PEDITACCEPT" 1)
  (command "pedit" "m" Sel "")
  (command "j" 0.1 "")

;;;  (setq	PtMin (mapcar '+ PtMin '(10 10))
;;;	PtMax (mapcar '- PtMax '(10 10))
;;;  )

  (setq	Sel (ssget "_W"
		   PtMin
		   PtMax
		   '((0 . "LWPOLYLINE") (8 . "0"))
	    )
	i   0
  )

  (setvar "FILLETRAD" 0.26)
  (setq r (getvar "FILLETRAD"))

  (if Sel
    (repeat (sslength Sel)
      (setq pl (ssname Sel i))
      (Raccorder_Polyligne pl r)
      (setq i (1+ i))
    )
  )
  (command "_qsave")
  (princ)
)

(defun Raccorder_Polyligne (ent	   rayon  /	 data	pts    rayon  enttmp
			    ferme  lst	  lstPl	 lent	seg1   seg2   pt1
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