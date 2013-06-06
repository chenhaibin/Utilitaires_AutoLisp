(vl-load-com)

(defun c:NP (/		  Deviation    Rayon	    trunc
	     butlast	  sublist      split-list   rot1
	     v2d	  Courbe-Intersection
	    )

;;; SUPPRIMER LES RAYONS DE COURBURE TROP PETIT

  (defun v2d (l)
    (mapcar '+ (append l '(0.0 0.0)) '(0.0 0.0))
  )

  (defun Courbe-Intersection (Courbe1 Courbe2 Prolonge / point)
    (setq point	(vlax-variant-value
		  (vla-intersectwith
		    (vlax-ename->vla-object Courbe1)
		    (vlax-ename->vla-object Courbe2)
		    Prolonge
		  )
		)
    )
    (if	(> (vlax-safearray-get-u-bound point 1) 0)
      (mapcar 'V2D (split-list (vlax-safearray->list point) 3))
    )
  )

  (defun Deviation (pt1 pt2 courbure)
    (* courbure (* (distance pt1 pt2) 0.5))
  )

  (defun Rayon (pt1 pt2 courbure)
    (/ (* (distance pt1 pt2) 0.5)
       (sin (abs (* 2.0 (atan courbure))))
    )
  )

  (defun trunc (expr lst)
    (if	(and lst (not (equal (car lst) expr)))
      (cons (car lst) (trunc expr (cdr lst)))
    )
  )

  (defun butlast (lst) (reverse (cdr (reverse lst))))

  (defun sublist (lst start leng / n r)
    (if	(or (not leng) (< (- (length lst) start) leng))
      (setq leng (- (length lst) start))
    )
    (setq n (+ start leng))
    (repeat leng (setq r (cons (nth (setq n (1- n)) lst) r)))
  )

  (defun split-list (lst n)
    (if	lst
      (cons (sublist lst 0 n) (split-list (sublist lst n nil) n))
    )
  )

  (defun rot1 (lst) (append (cdr lst) (list (car lst))))



  (setq	i    0
	ss   (ssget "_X" '((0 . "LWPOLYLINE")))
	sslg (if ss
	       (sslength ss)
	       0
	     )
  )

  (while (< i sslg)
    (setq ent	   (ssname ss i)
	  entdata  (entget ent)
	  tete	   (trunc (assoc 10 entdata) entdata)
	  fin	   (member (assoc 210 entdata) entdata)
	  points   (butlast (member (assoc 10 entdata) entdata))
	  points   (split-list points 5)
	  nbpoints (length points)
	  mod	   'nil
	  points   (mapcar
		     (function
		       (lambda (l1 l2)
			 (setq pt1 (cdr (assoc 10 l1))
			       pt2 (cdr (assoc 10 l2))
			       c   (cdr (assoc 42 l1))
			 )
			 (if
			   (and
			     (> (setq dev (Deviation pt1 pt2 c)) 0.0)
			     (or
			       (> (/ (distance pt1 pt2) dev)
				  150
			       )
			       ;;(> (Rayon pt1 pt2 c) 50000.0)
			     )
			   )
			    (progn
			      (setq mod 't)
			      (subst (cons 42 0.0) (assoc 42 l1) l1)

			    )
			    l1
			 )
		       )
		     )
		     points
		     (rot1 points)
		   )
	  points   (mapcar
		     (function
		       (lambda (l1 l2)
			 (if (<	(distance (cdr (assoc 10 l1))
					  (cdr (assoc 10 l2))
				)
				1.0
			     )
			   (progn
			     (setq mod 't)
			     (redraw ent 3)
			     'nil
			   )
			   l1
			 )
		       )
		     )
		     points
		     (rot1 points)
		   )
	  points   (vl-remove-if 'null points)
	  points   (apply 'append points)
	  entdata  (append tete points fin)
    )
    (if	mod
      (setq ent (cdr (assoc -1 (entmod entdata))))
    )
    (if	(and
	  ent
	  (> (length (Courbe-Intersection ent ent 0)) (cdr (assoc 90 (setq entdata (entget ent)))))
	)
      (setq entdata (subst (cons 8 "AutoIntersection")
			   (assoc 8 entdata)
			   entdata
		    )
	    ent	    (entmod entdata)
      )
    )
    (setq i (1+ i))
  )

  (princ)
)