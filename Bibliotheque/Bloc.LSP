
;;; LST_ATT_BLOC - Liste des attributs d'un bloc
;;;
(defun lst_att_bloc (bname / bdef liste)
  (and (equal (assoc '0 (entget bname)) '(0 . "INSERT"))
       (setq liste (list (cdr (assoc '2 (entget bname))))
	     bname (entnext bname)
       )
       (while (not (equal (assoc '0 (setq bdef (entget bname)))
			  '(0 . "SEQEND")
		   )
	      )
	 (and (equal (assoc '0 bdef) '(0 . "ATTRIB"))
	      (setq liste (cons	(cons (cdr (assoc '2 bdef))
				      (cdr (assoc '1 bdef))
				)
				liste
			  )
	      )
	 )
	 (setq bname (entnext bname))
       )
  )
  (reverse liste)
)


;;;================================================================================================;;;
(princ)