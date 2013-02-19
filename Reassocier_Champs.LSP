(defun c:rc  (/ entObject entObjectID)
  (vl-load-com)
  (setq	entTexte   (car (entsel "Selectionnez le texte :"))
	entFenetre (car (entsel "Selectionnez la fenêtre :"))
	strChamps  (entget
		     (cdr
		       (assoc
			 360
			 (entget
			   (cdr	(assoc
				  360
				  (entget
				    (cdr (assoc 360 (entget (cdr (assoc 360 (entget entTexte)))))))))))))
	strChamps  (subst (cons 1000 (cdr (assoc 330 strChamps))) (assoc 330 strChamps) strChamps)
	strChamps  (subst (cons 330 entFenetre) (assoc 330 strChamps) strChamps)
	strChamps  (subst (cons 331 entFenetre) (assoc 331 strChamps) strChamps)
	strChamps  (subst (cons 330 (cdr (assoc 1000 strChamps))) (assoc 1000 strChamps) strChamps)
	)
  (entmod strChamps)
  (princ)
  )
 ;|«Visual LISP© Format Options»
(100 2 40 0 nil "Fin de " 100 9 0 0 0 T nil nil T)
;*** NE PAS AJOUTER de texte au-dessous du commentaire! ***|;
