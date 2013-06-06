(defun c:SE0 ()
  
  (princ "\n=============================================\n")
  (setvar "CMDECHO" 0)
  
  (setq	i    0
	ss   (ssget "_X" '((0 . "LINE")))
	sslg (if ss
	       (sslength ss)
	       0
	     )
	sup  0
  )
  (while (< i sslg)
    (setq ent	  (ssname ss i)
	  entdata (entget ent)
	  lg	  (distance (cdr (assoc '10 entdata))
			    (cdr (assoc '11 entdata))
		  )
    )
    (if	(< lg 0.001)
      (progn (setq sup (1+ sup))
	     (entdel ent)
      )
    )
    (setq i (1+ i))
  )

  (princ sup)
  (princ " lignes supprimés")
  (terpri)

  (setq	i    0
	ss   (ssget "_X" '((0 . "ARC")))
	sslg (if ss
	       (sslength ss)
	       0
	     )
	sup  0
  )
  (while (< i sslg)
    (setq ent	  (ssname ss i)
	  entdata (entget ent)
	  lg	  (* (abs (- (cdr (assoc '51 entdata))
			     (cdr (assoc '50 entdata))
			  )
		     )
		     (cdr (assoc '40 entdata))
		  )

    )
    (if	(< lg 0.001)
      (progn (setq sup (1+ sup))
	     (entdel ent)
      )
    )
    (setq i (1+ i))
  )
  (princ sup)
  (princ " arcs supprimés")
  (princ)
)