(defun c:FP (/ remove-ele)

;;; FERMER LES POLYLIGNES OUVERTES

  (princ "\n=============================================\n")
  (setvar "CMDECHO" 0)

  (defun remove-ele (ele lst)
    (if	(equal ele (car lst))
      (cdr lst)
      (cons (car lst) (remove-ele ele (cdr lst)))
    )
  )

  (setq	i    0
	ss   (ssget "_X" '((0 . "LWPOLYLINE") (70 . 0)))
	sslg (if ss
	       (sslength ss)
	       0
	     )
  )

  (while (< i sslg)
    (setq ent	  (ssname ss i)
	  entdata (entget ent)
	  entdata (subst (cons 70 1) (assoc 70 entdata) entdata)
	  entdata (reverse entdata)
	  r91	  (assoc 91 entdata)
	  r42	  (assoc 42 entdata)
	  r41	  (assoc 41 entdata)
	  r40	  (assoc 40 entdata)
	  r10	  (assoc 10 entdata)
	  entdata (remove-ele r91 entdata)
	  entdata (remove-ele r42 entdata)
	  entdata (remove-ele r41 entdata)
	  entdata (remove-ele r40 entdata)
	  entdata (remove-ele r10 entdata)
    )
    (entmod (reverse entdata))
    (setq i (1+ i))
  )

  (princ)
)