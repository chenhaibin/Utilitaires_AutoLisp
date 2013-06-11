(vl-load-com)

(defun RaccordPolylignes ()


  (setvar "CMDECHO" 0)

  (command "_zoom" "_e")

  (command "_qsave")

  (setq	Sel   (ssget "_X" '((0 . "LINE") (8 . "LIGNES DE PLIAGE")))
	i     0
	PtMin '(1000000 1000000)
	PtMax '(-1000000 -1000000)
  )

  (if Sel
    (repeat (sslength Sel)
      (setq Ligne (entget (ssname Sel i))
	    PtMin (mapcar 'min
			  PtMin
			  (car (dxf '10 Ligne))
			  (car (dxf '11 Ligne))
		  )
	    PtMax (mapcar 'max
			  PtMax
			  (car (dxf '10 Ligne))
			  (car (dxf '11 Ligne))
		  )
      )
      (setq i (1+ i))
    )
  )

  (setq	Sel (ssget "_W"
		   PtMin
		   PtMax
		   '((0 . "ARC,LINE"))
	    )
	i   0
  )

  (command "pedit" "m" Sel "")
  (command "j" 0.1 "")

  (setq	Sel (ssget "_W"
		   PtMin
		   PtMax
		   '((0 . "LWPOLYLINE")
		     (-4 . "<NOT")
		     (0 . "MTEXT")
		     (-4 . "NOT>")
		     (-4 . "<NOT")
		     (8 . "LIGNES DE PLIAGE")
		     (-4 . "NOT>")
		    )
	    )
	i   0
  )

  (setvar "FILLETRAD" 0.26)

  (if Sel
    (repeat (sslength Sel)
      (setq pl	(ssname Sel i))
      (setq i (1+ i))
    )
  )
  (princ)

)