;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Retourne les noms des groupes de l'objet selectionné
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun isgroup (obj / list_reac list_type list_grp)
  (setq	list_reac (reverse (cdr (member '(102 . "}") (reverse (cdr (member '(102 . "{ACAD_REACTORS") (entget obj))))))
		  )
	list_type (mapcar '(lambda (x) (assoc '0 (entget (cdr x)))) list_reac)
	list_grp  (mapcar '(lambda (x y)
			     (if (= (cdr x) "GROUP")
			       (cdr y)
			     )
			   )
			  list_type
			  list_reac
		  )
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Ajouter une entité à un groupe
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun addgroup	(group ename / entname i)
  (entmod (reverse (cons (cons 340 ename) (reverse (entget group)))))
)



;;;================================================================================================;;;
(princ)