(princ "\nTOLE - Dessiner une tôle")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun c:tole (/	   *error*     selection   polyline    point_org   util	       point
	       polyline	   rayon_int   axe_tole	   pt	       rayon_int   faceA       faceB
	       point_curve ep_tole     ri	   point_second
	       )
  (defun *error* (msg)
    (setvar "OSMODE" OSMODE)
    (setvar "CMDECHO" CMDECHO)
    (setvar "PEDITACCEPT" PEDITACCEPT)
    (princ "\nErreur")
    (princ)
    )
  (command "_undo" "_m")
  (setq	OSMODE	    (getvar "OSMODE")
	CMDECHO	    (getvar "CMDECHO")
	PEDITACCEPT (getvar "PEDITACCEPT")
	)
  (setvar "OSMODE" 0)
  (setvar "CMDECHO" 0)
  (setvar "PEDITACCEPT" 1)
  (setq	selection (entsel "\nSelectionez une polyligne :")
	polyline  (car selection)
	point_org (trans (vlax-curve-getClosestPointTo
			   (vlax-ename->vla-object polyline)
			   (trans (cadr selection) 1 0)
			   )
			 0
			 1
			 )
	)
  (cond	((or (= "LWPOLYLINE" (cdr (assoc 0 (entget polyline))))
	     (= "POLYLINE" (cdr (assoc 0 (entget polyline))))
	     )
	 (setq point	 (getpoint point_org "\nIndiquez le coté interieur de la tôle :")
	       ep_tole	 (getreal "\nEpaisseur de la tôle :")
	       rayon_int (getreal "\nValeur du rayon interieur:")
	       )
	 )
	((= "LINE" (cdr (assoc 0 (entget polyline))))
	 (setq point   (getpoint point_org "\nIndiquez le coté interieur de la tôle :")
	       ep_tole (getreal "\nEpaisseur de la tôle :")
	       )
	 )
	(t (exit))
	)
  (command "_offset" (/ ep_tole 2.0) polyline point "")
  (setq axe_tole (entlast))
  (if (and (/= "LINE" (cdr (assoc 0 (entget polyline)))) (not (zerop rayon_int)))
    (command "_fillet" "r" (+ rayon_int (/ ep_tole 2.0)) "_fillet" "p" axe_tole)
    )
  (command "_offset" (/ ep_tole 2.0) axe_tole point "")
  (setq	faceA (entlast)
	point_curve
	 (trans
	   (vlax-curve-getClosestPointTo (vlax-ename->vla-object polyline) (trans point 1 0))
	   0
	   1
	   )
	point_second
	 (list (+ (car point_curve) (- (car point_curve) (car point)))
	       (+ (cadr point_curve) (- (cadr point_curve) (cadr point)))
	       )
	)
  (command "_offset" (/ ep_tole 2.0) axe_tole point_second "")
  (setq faceB (entlast))
  (command "_line"
	   (trans (cdr (assoc 10 (entget faceA))) faceA 1)
	   (trans (cdr (assoc 10 (entget faceB))) faceB 1)
	   ""
	   )
  (setq ligne (entlast))
  (command "_PEDIT" "m" faceA ligne faceB "" "j" "" "c" "")
  (setvar "OSMODE" OSMODE)
  (setvar "CMDECHO" CMDECHO)
  (setvar "PEDITACCEPT" PEDITACCEPT)
  (command "_erase" polyline axe_tole "")
  (princ)
  )