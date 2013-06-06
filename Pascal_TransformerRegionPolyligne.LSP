(defun c:R2P ()

  ;;; Transformer les regions en polyligne
  
  (princ "\n=============================================\n")
  (setvar "CMDECHO" 0)

  (setq	i    0.0
	ss   (ssget "_X" '((0 . "REGION")))
	sslg (if ss
	       (sslength ss)
	       0
	     )
  )
  (while (< i sslg)
    (setq ent (entlast))
    (command "_.explode" (ssname ss i))
    (setq pl  (entnext ent)
	  ent (entnext pl)
    )
    (command "_.pedit" pl "")
    (command "_.join" (entlast))
    (command ent)
    (while (setq ent (entnext ent))
      (command ent)
    )
    (command "")
    (setq i (1+ i))
  )
  
  (command "_.zoom" "_ex")

  (princ)

)