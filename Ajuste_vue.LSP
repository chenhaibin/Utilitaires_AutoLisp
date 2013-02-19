(defun c:av ()
  (setq	cmde (getvar "CMDECHO")
	hp   (getvar "HPBOUND")
	snap (getvar "AUTOSNAP")
  ) ;_ Fin de setq
  (setvar "CMDECHO" 0)
  (setvar "HPBOUND" 1)
  (setvar "AUTOSNAP" 2)
  (setq	select (entget (car (entsel "\nSelectionnez la fenêtre :")))
	point  (getpoint
		 "\nIndiquez un point à l'interieur de la nouvelle fenêtre à créer :"
	       )
	point  (list (car point) (cadr point))
  )
  (if
    (not
      (and (= (cdr (assoc '102 select)) "{ACAD_REACTORS")
	   (= (cdr
		(assoc '0 (setq view (entget (cdr (assoc '330 select)))))
	      )
	      "VIEWPORT"
	   )
      )
    )
     (if (= (cdr (assoc '0 select)) "VIEWPORT")
       (setq view select)
       ((princ "Ce n'est pas une fenêtre")
	 (exit)
       )
     )
  )
  (command "_.-boundary" point "")
  (command "_VPCLIP" (cdr (assoc '-1 view)) (entlast))
  (if
    (and (= (cdr (assoc '102 select)) "{ACAD_REACTORS")
	 (= (cdr
	      (assoc '0 (entget (cdr (assoc '330 select))))
	    )
	    "VIEWPORT"
	 )
    )
     (command "_erase" (cdr (assoc '-1 select)) "")
  )
  (setvar "CMDECHO" cmde)
  (setvar "HPBOUND" hp)
  (setvar "AUTOSNAP" snap)
  (princ)
) ;_ Fin de defun
