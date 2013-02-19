(princ "\nRTV - Tourner la vue")

(defun c:rtv  (/ ANGBASE CMDECHO angl ct ht)
  (initget "G _Général")

  (setq ANGBASE (getvar "angbase")
	CMDECHO (getvar "cmdecho")
	)
  (setvar "angbase" 0)
  (setvar "cmdecho" 0)
  (setq	angl (getangle "\nIndiquez la nouvelle direction de l'axe X ou [G] pour Général:")
	ct   (trans (getvar "VIEWCTR") 1 0)
	ht   (getvar "VIEWSIZE"))
  (if (or (= angl "Général") (= angl nil))
    (progn (command "_UCS" "Général") (command "_plan" "g"))
    (progn (command "_UCS" "_z" (* angl (/ 180.000000000 pi))) (command "_plan" "_c")))
  (command "_ZOOM" "_c" (trans ct 0 1) ht)
  (setvar "angbase" ANGBASE)
  (setvar "cmdecho" CMDECHO)
)
 ;|«Visual LISP© Format Options»
(100 2 40 0 nil "Fin de " 100 9 0 0 0 T nil nil T)
;*** NE PAS AJOUTER de texte au-dessous du commentaire! ***|;
