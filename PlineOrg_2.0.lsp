;;; PlineOrg (2.0) -Gilles Chanteau- 15/09/07
;;; Change le point de départ de la polyligne fermée
;;; Le nouveau point de départ peut être un situé n'importe où sur la polyligne

(defun c:plineorg (/ erreur os pt pl plst norm nb n blst pa d1 d2 d3)

  (vl-load-com)

  (defun erreur	(msg)
    (if	(= msg "Fonction annulée")
      (princ)
      (princ (strcat "\nErreur: " msg))
    )
    (setvar "OSMODE" os)
    (setq *error* m:err
	  m:err	nil
    )
  )

  (setq	m:err	*error*
	*error*	erreur
	os	(getvar "OSMODE")
  )
  (setvar "OSMODE" 515)
  (if (and
	(setq pt
	       (getpoint
		 "\nSélectionnez le nouveau point de départ sur la polyligne: "
	       )
	)
	(setq pl (car (nentselp pt)))
	(setq pl (vlax-ename->vla-object pl))
	(= (vla-get-ObjectName pl) "AcDbPolyline")
	(= (vla-get-Closed pl) :vlax-true)
      )
    (progn
      (vla-StartUndoMark (vla-get-ActiveDocument (vlax-get-acad-object)))
      (setq plst (vlax-get pl 'Coordinates)
	    norm (vlax-get pl 'Normal)
	    pt   (trans pt 1 0)
	    pa	 (vlax-curve-getParamAtPoint pl pt)
	    nb	 (/ (length plst) 2)
	    n	 nb
      )
      (repeat n
	(setq blst (cons (vla-getBulge pl (setq n (1- n))) blst))
      )
      (if (= pa (fix pa))
	(setq n	   (fix pa)
	      plst (append (sublist plst (* 2 n) nil)
			   (sublist plst 0 (* 2 n))
		   )
	      blst (append (sublist blst n nil) (sublist blst 0 n))
	)
	(setq n	   (1+ (fix pa))
	      d3 (vlax-curve-getDistAtParam pl n)
	      d2 (- d3 (vlax-curve-getDistAtPoint pl pt))
	      d3 (- d3 (vlax-curve-getDistAtParam pl (1- n)))
	      d1 (- d3 d2)
	      pt   (trans pt 0 (vlax-get pl 'Normal))
	      plst (append (list (car pt) (cadr pt))
			   (sublist plst (* 2 n) nil)
			   (sublist plst 0 (* 2 n))
		   )
	      blst (append (list (k*bulge (nth (1- n) blst) (/ d2 d3)))
			   (sublist blst n nil)
			   (sublist blst 0 (1- n))
			   (list (k*bulge (nth (1- n) blst) (/ d1 d3)))
		   )
	)
      )
      (vlax-put pl 'coordinates plst)
      (repeat (setq n (length blst))
	(vla-setBulge pl (setq n (1- n)) (nth n blst))
      )
      (vla-EndUndoMark (vla-get-ActiveDocument (vlax-get-acad-object)))
    )
    (prompt "\nEntité non valide.")
  )
  (princ)
)

;;; SUBLIST Retourne une sous-liste
;;;
;;; Arguments
;;; lst : une liste
;;; start : l'index de départ de la sous liste (premier élément = 0)
;;; leng : la longueur (nombre d'éléments) de la sous-liste (ou nil)
;;;
;;; Exemples :
;;; (sublist '(1 2 3 4 5 6) 2 2) -> (3 4)
;;; (sublist '(1 2 3 4 5 6) 2 nil) -> (3 4 5 6)

(defun sublist (lst start leng / n r)
  (if (or (not leng) (< (- (length lst) start) leng))
    (setq leng (- (length lst) start))
  )
  (setq n (+ start leng))
  (repeat leng
      (setq r (cons (nth (setq n (1- n)) lst) r))
    )
)

;; K*BULGE
;; Retourne le bulge proportionnel au bulge de référence
;; Arguments :
;; b : le bulge
;; k : le rapport de proportion (entre les angles ou les longueurs d'arcs)

(defun k*bulge (b k / a)
  (setq a (atan b))
  (/ (sin (* k a)) (cos (* k a)))
)