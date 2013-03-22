(vl-load-com)

;Sw_NettoyerDXF

(defun Sw_NettoyerDXF(/)

  (setq	Sel (ssget "_X" '((2 . "SW_TABLEANNOTATION_0")))
	i   0
  )

  (if sel
    (repeat (sslength Sel)
      (entdel (ssname Sel i))
      (setq i (1+ i))
    )
  )

  (command "_-layer" "n" "NOTE DE PLIAGE" "n" "LIGNES DE PLIAGE" "")

  (setq Sel 'nil
	Sel (ssget "_X" '((1 . "*PLI*")))
	i   0
  )

  (if sel
    (repeat (sslength Sel)
      (setq lst (entget (ssname Sel i))
	    lst (subst (cons 8 "NOTE DE PLIAGE") (assoc 8 lst) lst)
	    lst (subst (cons 62 256) (assoc 62 lst) lst)
	    )
      (entmod lst)
      (setq i (1+ i))
    )
  )

  (setq Sel 'nil
	Sel (ssget "_X" '((-4 . "<AND") (6 . "SLD*")(-4 . "AND>")(-4 . "<NOT") (6 . "SLD-Solid")(-4 . "NOT>")))
	i   0
  )

  (if Sel
    (repeat (sslength Sel)
      (setq lst (entget (ssname Sel i))
	    lst (subst (cons 8 "LIGNES DE PLIAGE") (assoc 8 lst) lst)
	    lst (subst (cons 62 256) (assoc 62 lst) lst)
	    )
      (entmod lst)
      (setq i (1+ i))
    )
  )

  (setq Sel 'nil
	Sel (ssget "_X" '((0 . "INSERT") (8 . "0") (2 . "`*U*") ))
	i   0
  )
  
  (if Sel
    (repeat (sslength Sel)
      (setq blk (ssname Sel i)
	    i (1+ i))
      (command "_EXPLODE" blk "")
    )
  )
  
  (setq	Entite (entlast)
	Sel    (ssget "_X" '((0 . "INSERT") (8 . "QUANTITE")))
	i      0
  )
  (if Sel
    (repeat (sslength Sel)
      (setq blk	(ssname Sel i)
	    i	(1+ i)
      )
      (command "_EXPLODE" blk)
    )
  )

  (setq Entite (entnext Entite))
  (while Entite
    (setq ed (entget Entite)
	  ed (subst '(8 . "QUANTITE") (assoc 8 ed) ed)
	  ed (subst '(62 . 256) (assoc 62 ed) ed)
    )
    (entmod ed)
    (setq Entite (entnext Entite))
  )
  
  (setq	Entite (entlast)
	Sel    'nil
	Sel    (ssget "_X" '((0 . "INSERT") (8 . "QUANTITE")))
	i      0
  )
  
  (if Sel
    (repeat (sslength Sel)
      (setq blk	(ssname Sel i)
	    i	(1+ i)
      )
      (command "_EXPLODE" blk)
    )
  )

  (setq Entite (entnext Entite))
  (while Entite
    (setq ed (entget Entite)
	  ed (subst '(8 . "QUANTITE") (assoc 8 ed) ed)
	  ed (subst '(62 . 256) (assoc 62 ed) ed)
    )
    (entmod ed)
    (setq Entite (entnext Entite))
  )

  (setq Sel 'nil)

  (setq	Entite (entlast)
	Sel    (ssget "_X" '((0 . "INSERT")))
	i      0
  )
  (if Sel
    (repeat (sslength Sel)
      (setq blk	(ssname Sel i)
	    i	(1+ i)
      )
      (command "_EXPLODE" blk)
    )
  )

  (setvar "PLINECONVERTMODE" 1)

  (setq	Sel (ssget "_X" '((0 . "SPLINE")))
	i   0
  )

  (if sel
    (repeat (sslength Sel)
      (setq spl (ssname Sel i))
      (command "_splinedit" spl "_p" 1)
      (setq i (1+ i))
    )
  )

  (command "_-layer" "ch" "NOTE DE PLIAGE" "co" 126 "" "")
  (command "_-layer" "ch" "LIGNES DE PLIAGE" "co" 252 "" "")
  (command "_-layer" "e" "0" "")
  
  
  (command "_zoom" "et")
  (command "_regenall")
  (setvar "MIRRTEXT" 0)
  (command "_-PURGE" "_all" "" "_no")
  (command "_QSAVE")
  (command "_UCSICON" "no")
  (setq Sel 'nil)
  (princ)
)

(defun c:s(/)
	(Sw_NettoyerDXF)
)