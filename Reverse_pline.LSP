;;; R_PLINE -Gilles Chanteau-
;;; Inverse l'ordre de sommets d'une polyligne (2D, 3D ou optimisée)
;;; Toutes les propriétés de la polyligne sont conservées (arcs, largeurs ...)

;; Inverse l'ordre de sommets d'une lwpolyligne, d'une polyligne 2D ou 3D

(defun reverse_pline (ent / e_lst vtx v_lst p_lst l_vtx)
  (setq e_lst (entget ent))
  (cond
    ((= (cdr (assoc 0 e_lst)) "POLYLINE")
     (setq vtx (entnext ent))
     (while (= (cdr (assoc 0 (entget vtx))) "VERTEX")
       (setq v_lst (cons (entget vtx) v_lst)
	     vtx   (entnext vtx)
       )
     )
    )
    ((= (cdr (assoc 0 e_lst)) "LWPOLYLINE")
     (setq p_lst (vl-remove-if-not
		   '(lambda (x)
		      (member (car x) '(10 40 41 42))
		    )
		   e_lst
		 )
	   e_lst (vl-remove-if
		   '(lambda (x)
		      (member x p_lst)
		    )
		   e_lst
		 )
     )
     (while p_lst
       (setq v_lst (cons
		     (list (car p_lst) (cadr p_lst) (caddr p_lst) (cadddr p_lst))
		     v_lst
		   )
	     p_lst (member (assoc 10 (cdr p_lst)) (cdr p_lst))
       )
     )
    )
  )
  (setq	l_vtx (last v_lst)
	l_vtx (subst (cons 40 (cdr (assoc 41 (car v_lst))))
		     (assoc 40 l_vtx)
		     l_vtx
	      )
	l_vtx (subst (cons 41 (cdr (assoc 40 (car v_lst))))
		     (assoc 41 l_vtx)
		     l_vtx
	      )
	l_vtx (subst (cons 42 (- (cdr (assoc 42 (car v_lst)))))
		     (assoc 42 l_vtx)
		     l_vtx
	      )
  )
  (setq	v_lst
	 (mapcar
	   '(lambda (x y)
	      (setq x (subst (cons 40 (cdr (assoc 41 y))) (assoc 40 x) x)
		    x (subst (cons 41 (cdr (assoc 40 y))) (assoc 41 x) x)
		    x (subst (cons 42 (- (cdr (assoc 42 y)))) (assoc 42 x) x)
	      )
	    )
	   v_lst
	   (cdr v_lst)
	 )
  )
  (if (= (logand 1 (cdr (assoc 70 e_lst))) 1)
    (setq v_lst (append (list l_vtx) v_lst))
    (setq v_lst (append v_lst (list l_vtx)))
  )
  (cond
    ((= (cdr (assoc 0 e_lst)) "POLYLINE")
     (mapcar 'entmake
	     (append (list e_lst) v_lst (list (entget vtx)))
     )
     (entdel ent)
    )
    ((= (cdr (assoc 0 e_lst)) "LWPOLYLINE")
     (setq e_lst (append e_lst (apply 'append v_lst)))
     (entmod e_lst)
    )
  )
)

;;; R_PLINE Fonction d'appel

(defun c:r_pline (/ ent)
  (while (not (setq ent (car (entsel)))))
  (if (or (= (cdr (assoc 0 (entget ent))) "LWPOLYLINE")
	  (and (= (cdr (assoc 0 (entget ent))) "POLYLINE")
	       (zerop (logand 240 (cdr (assoc 70 (entget ent)))))
	  )
      )
    (reverse_pline ent)
    (prompt "\nEntité non valide")
  )
  (princ)
)