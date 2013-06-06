(defun c:xp (/ *error* tracer_repere)

  (princ "\n=============================================\n")
  (setvar "CMDECHO" 0)
  
  (defun *error* (msg)
    (setvar "OSMODE" _OSMODE)
    (setvar "FILEDIA" 1)
  )

  (defun tracer_repere (pt1 pt2 / v1 v2 p1 p2)
    (setq v1 (mapcar '- pt2 pt1)
	  v2 (mapcar '- pt1 pt2)
	  v1 (mapcar (function (lambda (x) (* x 0.8))) v1)
	  v2 (mapcar (function (lambda (x) (* x 0.8))) v2)
	  p2 (mapcar '+ pt1 v1)
	  p1 (mapcar '+ pt2 v2)
    )

    (entmake
      (append
	'((0 . "LINE") (67 . 0) (410 . "Model") (8 . "Fait"))
	(list (cons '62 '10)
	      (cons 10 p1)
	      (cons 11 p2)
	)
      )
    )
  )
  
  (setq	_OSMODE	     (getvar "OSMODE")
	Chemin	     (getvar "DWGPREFIX")
	Prefixe	     "PN"
	List_Colonne (list "A"	 "B"   "C"   "D"   "E"	 "F"   "G"
			   "H"	 "I"   "J"   "K"   "L"	 "M"   "N"
			   "O"	 "P"   "Q"   "R"   "S"	 "T"   "U"
			   "V"	 "W"   "X"   "Y"   "Z"
			  )
  )
  (setvar "FILEDIA" 0)
  (setvar "OSMODE" 8)
  (setq	Ind (strcase (getstring "\nIndice de facade + No de rangée : "))
	Ind_Colonne (max 0 (- (length List_Colonne) (length (member (strcase (getstring "\nIndice de colonne : ")) List_Colonne)))))
  (while (setq pt1 (getpoint "\nPremier coin"))
    (setq pt2	     (getcorner pt1 "\nDeuxieme coin")
	  ss	     (ssget "_C" pt1 pt2)
	  ptOrigine  (mapcar 'min pt1 pt2)
	  NomFichier (strcat Chemin
			     Prefixe
			     Ind
			     (strcase (nth (rem Ind_Colonne (length List_Colonne)) List_Colonne))
			     ".dwg"
		     )
    )
    (terpri)
    (princ ptOrigine)
    (terpri)
    (princ NomFichier)
    (terpri)
    (command "EXPORTER" NomFichier "" ptOrigine ss "")
    (command "REPRISE")
    (tracer_repere pt1 pt2)
    (setq Ind_Colonne
	   (1+ Ind_Colonne)
	  ss nil
    )
  )

  (*error* nil)
  (princ)
)