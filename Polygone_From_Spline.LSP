(vl-load-com)

(defun c:P4S  (/ ent dev Get_Polygone)
     (defun Get_Polygone  (Spline Dev / PtPolygone Polyligne Inflection)
	  (setq	Spline_Vl  (vlax-ename->vla-object Spline)
		;; Filtre les points du polygone
		PtPolygone (mapcar 'cdr (massoc '10 (entget Spline)))
		;; Trace le polygone (invisible) moins le départ et l'arrivé
		Polyligne  (make_lwpline PtPolygone 0)))
     (if (and (setq ent (car (entsel "\nSélectionnez une Spline : "))))
	  (Get_Polygone ent dev)
	  (prompt "\nCe n'est pas une spline."))
     (princ))

