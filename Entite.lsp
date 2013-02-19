
(defun TracePolyligne (point / def)
  (setq	def (list (cons 90 (length point))
		  '(70 . 0)
		  '(100 . "AcDbPolyline")
		  '(370 . 0)
		  '(48 . 100.0)
		  '(67 . 0)
		  '(100 . "AcDbEntity")
		  '(0 . "LWPOLYLINE")
	    )
  )
  (mapcar '(lambda (x) (setq def (cons (cons 10 x) def))) point)
  (if (entmake (reverse def))
    (entlast)
  )
)

(defun SplinePointControle (courbe)
  (if (equal (assoc '0 (entget courbe)) '(0 . "SPLINE"))
    (mapcar 'cdr (vl-remove-if-not '(lambda (x) (= (car x) 10)) (entget courbe)))
  )
)

(defun LongeurCourbe (courbe)
  (setq courbe (vlax-ename->vla-object courbe))
  (vlax-curve-getDistAtParam courbe (vlax-curve-getEndParam courbe))
)

(defun CourbeDistAtPoint (pt courbe)
  (setq courbe (vlax-ename->vla-object courbe))
  (vlax-curve-getDistAtPoint courbe (vlax-curve-getClosestPointTo courbe pt))
)


(defun IntersectionCourbes (courbeBase courbeInter prolonger / list3dpoint intersection pt)
  (defun list3dpoint (lst / lst_3dpoint)
    (while lst
      (setq lst_3dpoint	(cons (list (car lst) (cadr lst) (caddr lst)) lst_3dpoint)
	    lst		(cdddr lst)
      )
    )
    lst_3dpoint
  )
;;; Valeur pour 'prolonger :
;;; 0 -> Aucun objet n'est prolongé.
;;; 1 -> L'objet de base (courbeBase) est prolongé.
;;; 2 -> L'objet passé en paramètre (courbeInter) est prolongé.
;;; 3 -> Les deux objets sont prolongés.
  (and (= (type courbeBase) 'ENAME)
       (= (type courbeInter) 'ENAME)
       (setq prolonge (fix prolonger))
       (setq intersection
	      (vlax-variant-value
		(vla-intersectwith
		  (vlax-ename->vla-object courbeBase)
		  (vlax-ename->vla-object courbeInter)
		  prolonger
		)
	      )
       )
  )
  (if (> (vlax-safearray-get-u-bound intersection 1) 1)
    (list3dpoint (vlax-safearray->list intersection))
    nil
  )
)

(defun IsDepartCourbe (pt courbe)
  (setq courbe (vlax-ename->vla-object courbe))
  (if (< (vlax-curve-getDistAtPoint courbe (vlax-curve-getClosestPointTo courbe pt))
	 (/ (vlax-get-property courbe 'length) 2.0)
      )
    t
    nil
  )
)

;;==========================================================;;
;; Tri des points selon x y z
(defun tri_pts (lst)
  (vl-sort (vl-sort (vl-sort lst '(lambda (x y) (< (caddr x) (caddr y))))
		    '(lambda (x y) (< (cadr x) (cadr y)))
	   )
	   '(lambda (x y) (< (car x) (car y)))
  )
)

;;==========================================================;;
;; Valeur du code dxf d'une entité (ename)
(defun val_dxf (code ent) (cdr (assoc code (entget ent))))
;;==========================================================;;
;; Liste des sommets d'une lwpolyligne (dans le SCG), la direction d'extrusion et le calque
(defun lwpoly_pts (ent)
  (cons	(val_dxf 210 ent)
	(cons (val_dxf 8 ent)
	      (mapcar '(lambda (pt) (trans (list (car pt) (cadr pt) (val_dxf 38 ent)) ent 0))
		      (massoc 10 (entget ent))
	      )
	)
  )
)
 ;|«Visual LISP© Format Options»
(100 2 40 2 nil "Fin de " 100 9 0 0 0 T nil nil T)
;*** NE PAS AJOUTER de texte au-dessous du commentaire! ***|;
