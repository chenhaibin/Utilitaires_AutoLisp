(princ "\nCU - Coupure en 1 point")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun c:cu  (/ cmde)
  (command "_undo" "_m")
  (princ "\n")
  (COMMAND "_BREAK" pause "_first" pause "@")
  (princ)
  )
;|«Visual LISP© Format Options»
(100 2 40 0 nil "Fin de " 100 9 0 0 0 T T nil T)
;*** NE PAS AJOUTER de texte au-dessous du commentaire! ***|;
