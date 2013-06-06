(vl-load-com)

(defun LigneToPolyligne	()

  (princ "\n=============================================\n")
  (princ "Boucle en polyligne")
  (setvar "CMDECHO" 0)

  (setq	i    0
	ss   (ssget "_X")
	sslg (if ss
	       (sslength ss)
	       0
	     )
	pl   (ssname ss i)
	i    (1+ i)
  )
  (command "_.pedit" pl "")
  (command "_.join" (entlast))
  (while (< i sslg)
    (command (ssname ss i))
    (setq i (1+ i))
  )
  (setq ss 'nil)
  (command "")

  (command "_.zoom" "_ex")

  (princ)

)