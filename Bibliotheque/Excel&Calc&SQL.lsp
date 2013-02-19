;;;=================================================================
;;;
;;; Liaisons API avec un Tableur
;;;
;;; API_XLS V1.00
;;;
;;; Copyright (C) Patrick_35
;;;
;;;=================================================================


;-------------------------------------------------------------------
;
; Se connecter à Excel
; Connect to Excel
;
; Retourne par exemple #<VLA-OBJECT _Application 18aa5864>
; ou nil si l'application n'est pas installé
;
;-------------------------------------------------------------------
(defun lancer_excel()
  (vlax-get-or-create-object "Excel.Application")
)

;-------------------------------------------------------------------
;
; Se connecter à Open Office
; Connect to Open Office
;
; Retourne par exemple #<VLA-OBJECT 188c40d4>
; ou nil si l'application n'est pas installé
;
;-------------------------------------------------------------------
(defun lancer_oOo()
  (vlax-get-or-create-object "com.sun.star.ServiceManager")
)

;-------------------------------------------------------------------
;
; Se connecter à SQL_SERVER
; Connect To SQL
;
; Retourne par exemple #<VLA-OBJECT _Connection 3b6b0e90>
; ou nil si l'application n'est pas installé
;
;-------------------------------------------------------------------
(defun lancer_ado()
  (vlax-create-object "ADODB.Connection")
)

;-------------------------------------------------------------------
;
; Convertir une chaine de caractères de type UTF-8
; Convert a string to UTF-8
;
; Exemple
; (decodeurl "c:\\Donn%C3%A9es") --> "c:\\Données"
;
;-------------------------------------------------------------------
(defun decodeurl(str / hex new pos txt hexdec)

  (defun hexdec (nb / r i s) 
    (if (= (type nb) 'INT)
      (setq nb (itoa nb))
    )
    (setq r 0 i 0)
    (while (and (= (type nb) 'STR) (< i (strlen nb)))
      (setq i (1+ i)
	    s (strcase (substr nb i 1 ))
	    r (+ (lsh r 4) (- (ascii s) (if (<= s "9") 48 55)))
      )
    )
  )

  (while (setq pos (vl-string-search "%" str))
    (setq hex (substr str (+ pos 2) 2))
    (and (eq (substr str (+ pos 4) 1) "%")
      (setq hex (strcat hex (substr str (+ pos 5) 2)))
    )
    (setq txt (strcat (substr str 1 pos)
			(if (eq (strlen hex) 4)
			  (chr (- 50322 (hexdec hex)))
			  (chr (hexdec hex))
			)
	      )
    )
    (if (eq (strlen hex) 4)
      (setq str (substr str (+ pos 7)))
      (setq str (substr str (+ pos 4)))
    )
    (if new
      (setq new (strcat new txt))
      (setq new txt)
    )
  )
  (if new
    (strcat new str)
    str
  )
)

;-------------------------------------------------------------------
;
; Liste tous les fichiers ouverts de l'application
; Dump open files of application
;
; Entrée --> Liaison Activex
;
; Retourne une liste des fichiers ouverts
; ou nil si aucun.
; 
; Exemple
; (liste_fichiers_ouverts Mon_Appli) --> ("c:\\Données\Test.xls")
;
;-------------------------------------------------------------------
(defun liste_fichiers_ouverts(activex / feu lst tmp)
  (cond
    ( (and (not (vl-catch-all-error-p
		  (setq tmp
		    (vl-catch-all-apply
		      'vlax-get
		      (list
			activex
			'caption
		      )
		    )
		  )
		)
	   )
	   (wcmatch (strcase tmp) "MICROSOFT EXCEL*")
      )
      (vlax-for feu (vlax-get activex 'workbooks)
	(setq lst (cons (vlax-get feu 'fullname) lst))
      )
    )
    ( (and (not (vl-catch-all-error-p
		  (setq feu
		    (vl-catch-all-apply
		      'vlax-invoke
		      (list
			activex
			'createinstance
			"com.sun.star.frame.Desktop"
		      )
		    )
		  )
		)
	   )
      )
      (setq tmp (vlax-invoke feu 'getComponents)
	    cib (vlax-invoke tmp 'createEnumeration)
      )
      (while (eq (vlax-invoke cib 'hasMoreElements) -1)
	(setq tmp (vlax-invoke cib 'nextElement)
	      lst (cons (findfile (decodeurl (substr (vlax-invoke tmp 'getLocation) 9))) lst)
	)
      )
    )
  )
  (reverse lst)
)

;-------------------------------------------------------------------
;
; ouvrir un fichier de l'application
; open file of application
;
; Entrée --> Liaison Activex
;
; Retourne le classeur ou nil si aucun.
;
: Exemple
; (ouvrir_fichier Mon_Appli "c:\\Données\Test.xls")
; Excel --> #<VLA-OBJECT Sheets 1898098c>
; Open Office --> #<VLA-OBJECT 00229ecc>
; SQL  -->#<VLA-OBJECT _Connection 3b6b0e90>
;
;-------------------------------------------------------------------
(defun ouvrir_fichier(activex fichier / feu fil par tmp xls)
  (and (setq fil (findfile fichier))
    (cond
      ( (and (not (vl-catch-all-error-p
		    (setq tmp
		      (vl-catch-all-apply
			'vlax-get
			(list
			  activex
			  'caption
			)
		      )
		    )
		  )
	     )
	     (wcmatch (strcase tmp) "MICROSOFT EXCEL*")
	)
	(vlax-for feu (vlax-get activex 'workbooks)
	  (and (eq (strcase fil) (strcase (vlax-get feu 'fullname)))
	    (setq xls feu)
	  )
	)
	(or xls (vl-catch-all-error-p
		  (setq tmp
		    (vl-catch-all-apply
		      'vlax-invoke
		      (list
			(vlax-get activex 'workbooks)
			'open
			fil
		      )
		    )
		  )
		)
	  (setq xls tmp)
	)
	(and xls (setq xls (vlax-get xls 'sheets)))
      )
      ( (and (not (vl-catch-all-error-p
		    (setq feu
		      (vl-catch-all-apply
			'vlax-invoke
			(list
			  activex
			  'createinstance
			  "com.sun.star.frame.Desktop"
			)
		      )
		    )
		  )
	     )
	)

	(setq tmp (vlax-invoke feu 'getComponents)
	      cib (vlax-invoke tmp 'createEnumeration)
	)
	(while (eq (vlax-invoke cib 'hasMoreElements) -1)
	  (setq tmp (vlax-invoke cib 'nextElement))
	  (and (eq (findfile (decodeurl (substr (vlax-invoke tmp 'getLocation) 9))) fil)
	    (setq xls tmp)
	  )
	)
	(or xls
	  (progn
	    (setq tmp (vlax-invoke activex 'Bridge_GetStruct "com.sun.star.beans.PropertyValue")
		  par (vlax-make-safearray vlax-vbVariant '(0 . 1))
		  fil (strcat "file:///" (vl-string-translate "\\" "/" fil))
	    )
	    (vlax-put tmp 'name "ReadOnly")
	    (vlax-put tmp 'value :vlax-false)
	    (vlax-safearray-put-element par 0 tmp)
	    (vlax-put tmp 'name "Hidden")
	    (vlax-put tmp 'value :vlax-true)
	    (vlax-safearray-put-element par 1 tmp)
	    (setq xls (vlax-invoke-method feu 'loadComponentFromURL fil "_blank" 0 par))
	  )
	)
      )
      ( (and (vlax-property-available-p activex 'provider)
	     (eq (strcase (vlax-get activex 'provider)) "MSDASQL")
	)
	(or (vl-catch-all-error-p
	      (vl-catch-all-apply
		'vlax-invoke-method
		(list
		  activex
		  'Open
		  (strcat "Provider=Microsoft.Jet.OLEDB.4.0;Data Source="
			  fil
			  ";Extended Properties=;Excel 8.0;HDR=No"
		  )
		  ""
		  ""
		  nil
		)
	      )
	    )
	  (setq xls activex)
	)
      )
    )
  )
  xls
)

;-------------------------------------------------------------------
;
; Lister les feuilles d'un classeur
; Dump sheet of sheets
;
; Entrée --> Classeur
;
; Retourne une liste des feuilles ou nil si aucune.
;
; Exemple
; (liste_feuilles Mon_Classeur) --> ("Feuil1" "Feuil2")
;
;-------------------------------------------------------------------
(defun liste_feuilles(classeur / bou feu lst)
  (cond
    ( (not (vl-catch-all-error-p
	     (vl-catch-all-apply
	       'vlax-get
	       (list
		 classeur 
		 'creator
	       )
	     )
	   )
      )
      (vlax-for feu classeur
	(setq lst (cons (vlax-get feu 'name) lst))
      )
      (setq lst (reverse lst))
    )
    ( (and (not (vl-catch-all-error-p
		  (setq feu
		    (vl-catch-all-apply
		      'vlax-get
		      (list
			classeur
			'sheets
		      )
		    )
		  )
		)
	   )
      )
      (repeat (setq bou (vlax-get feu 'count))
	(setq lst (cons (vlax-get (vlax-invoke feu 'getbyindex (setq bou (1- bou))) 'name) lst))
      )
    )
    ( (and (vlax-property-available-p classeur 'provider)
	   (wcmatch (strcase (vlax-get classeur 'provider)) "MICROSOFT*")
      )
      (setq lst (mapcar '(lambda(x) (vl-string-right-trim "$" (vl-string-trim "'" x)))
			(vl-remove-if-not '(lambda(x) (wcmatch x "*$,*$'"))
			  		  (caddr (mapcar '(lambda(x) (mapcar 'vlax-variant-value x))
							 (vlax-safearray->list
							   (vlax-variant-value
							     (vlax-invoke-method
							       (vlax-invoke-method classeur 'OpenSchema 20)
							       "GetRows"
							       65535
							     )
							   )
							 )
						 )
					  )
			)
		)
      )
    )
  )
  lst
)

;-------------------------------------------------------------------
;
; Indique la feuille active
; Activesheet
;
; Entrée --> Classeur
;
; Retourne la feuille active ou nil si aucune.
;
; Exemple
; (feuille_active Mon_Classeur) --> "Feuil1"
;
;-------------------------------------------------------------------
(defun feuille_active(classeur / feu)
  (cond
    ( (not (vl-catch-all-error-p
	     (vl-catch-all-apply
	       'vlax-get
	       (list
		 classeur 
		 'creator
	       )
	     )
	   )
      )
      (setq feu (vlax-get (vlax-get (vlax-get classeur 'parent) 'activesheet) 'name))
    )
    ( (and (not (vl-catch-all-error-p
		  (setq feu
		    (vl-catch-all-apply
		      'vlax-invoke
		      (list
			classeur
			'GetCurrentController
		      )
		    )
		  )
		)
	   )
      )
      (setq feu (vlax-get (vlax-invoke feu 'getactivesheet) 'name))
    )
  )
  (if (vl-catch-all-error-p feu)
    nil
    feu
  )
)

;-------------------------------------------------------------------
;
; Lire une valeur dans une cellule
; Read cell
;
; Entrée --> Classeur
;	 --> Nom de la feuille
;	 --> Cellule
;
; Retourne la valeur de la cellule si tout s'est bien passée ou nil.
;
; Exemple
; (lire_cellule Mon_Classeur "Feuil1" "A1") --> "Test"
;
;-------------------------------------------------------------------
(defun lire_cellule(classeur feuille cellule / adocommand adorecord cel feu val)
  (cond
    ( (not (vl-catch-all-error-p
	     (vl-catch-all-apply
	       'vlax-get
	       (list
		 classeur 
		 'creator
	       )
	     )
	   )
      )
      (and (not (vl-catch-all-error-p
		  (setq feu
		    (vl-catch-all-apply
		      'vlax-get-property
		      (list
			classeur
			'item
			feuille
		      )
		    )
		  )
		)
	   )
	   (not (vl-catch-all-error-p
		  (setq cel
		    (vl-catch-all-apply
		      'vlax-get-property
		      (list
			feu
			'range
			cellule
		      )
		    )
		  )
		)
	   )
        (setq val (vl-catch-all-apply 'vlax-get (list cel 'value2)))
      )
    )
    ( (and (not (vl-catch-all-error-p
		  (setq feu
		    (vl-catch-all-apply
		      'vlax-get
		      (list
			classeur
			'sheets
		      )
		    )
		  )
		)
	   )
      )
      (and (eq (vlax-invoke feu 'hasbyname feuille) -1)
	   (not (vl-catch-all-error-p
		  (setq cel
		    (vl-catch-all-apply
		      'vlax-invoke
		      (list
			(vlax-invoke feu 'getByName feuille)
			'getCellRangeByName
			cellule
		      )
		    )
		  )
		)
	   )
	(setq val (vl-catch-all-apply 'vlax-get (list cel 'string)))
      )
    )
    ( (and (vlax-property-available-p classeur 'provider)
	   (wcmatch (strcase (vlax-get classeur 'provider)) "MICROSOFT*")
      )
      (setq adorecord  (vlax-get-or-create-object "ADODB.Recordset")
	    adocommand (vlax-get-or-create-object "ADODB.Command")
      )
      (vlax-put adocommand 'ActiveConnection classeur)
      (and (not (vl-catch-all-error-p
		  (vl-catch-all-apply
		    'vlax-put
		    (list
		      adocommand
		      'CommandText
		      (strcat "SELECT * FROM [" feuille "$" cellule ":" cellule "]")
		    )
		  )
		)
	   )
	   (not (vl-catch-all-error-p
		  (vl-catch-all-apply
		    'vlax-invoke-method
		    (list
		      adorecord
		      'Open
		      adocommand
		      nil
		      1
		      3
		      nil
		    )
		  )
		)
	   )
	(progn
	  (setq val (vlax-get (vlax-get-property (vlax-get adorecord 'fields) 'item 0) 'value))
	  (vlax-invoke adorecord 'close)
	)
	(setq val nil)
      )
      (vlax-release-object adorecord)
      (vlax-release-object adocommand)
    )
  )
  val
)

;-------------------------------------------------------------------
;
; Ecrire une valeur dans une cellule
; Write cell
;
; Entrée --> Classeur
;	 --> Nom de la feuille
;	 --> Cellule
;	 --> Valeur à écrire
;
; Retourne T si tout s'est bien passée ou nil.
;
; Exemple
; (ecrire_cellule Mon_Classeur "Feuil1" "A1" "Test") --> T
;
;-------------------------------------------------------------------
(defun ecrire_cellule(classeur feuille cellule valeur / adocommand adorecord cel feu val)
  (setq val T)
  (cond
    ( (not (vl-catch-all-error-p
	     (vl-catch-all-apply
	       'vlax-get
	       (list
		 classeur 
		 'creator
	       )
	     )
	   )
      )
      (and (not (vl-catch-all-error-p
		  (setq feu
		    (vl-catch-all-apply
		      'vlax-get-property
		      (list
			classeur
			'item
			feuille
		      )
		    )
		  )
		)
	   )
	(setq val (vl-catch-all-error-p
		    (vl-catch-all-apply
		      'vlax-put
		      (list
			(vlax-get-property
			  feu
			  'range
			  cellule
			)
			'value2
			valeur
		      )
		    )
		  )
	)
      )
    )
    ( (and (not (vl-catch-all-error-p
		  (setq feu
		    (vl-catch-all-apply
		      'vlax-get
		      (list
			classeur
			'sheets
		      )
		    )
		  )
		)
	   )
      )
      (and (eq (vlax-invoke feu 'hasbyname feuille) -1)
	   (not (vl-catch-all-error-p
		  (setq cel
		    (vl-catch-all-apply
		      'vlax-invoke
		      (list
			(vlax-invoke feu 'getByName feuille)
			'getCellRangeByName
			cellule
		      )
		    )
		  )
		)
	   )
	(setq val (vl-catch-all-error-p
		    (vl-catch-all-apply
		      'vlax-put
		      (list
			cel
			'string
			valeur
		      )
		    )
		  )
	)
      )
    )
    ( (and (vlax-property-available-p classeur 'provider)
	   (wcmatch (strcase (vlax-get classeur 'provider)) "MICROSOFT*")
      )
      (setq adorecord  (vlax-get-or-create-object "ADODB.Recordset")
	    adocommand (vlax-get-or-create-object "ADODB.Command")
      )
      (vlax-put adocommand 'ActiveConnection classeur)
      (and (not (vl-catch-all-error-p
		  (vl-catch-all-apply
		    'vlax-put
		    (list
		      adocommand
		      'CommandText
		      (strcat "SELECT * FROM [" feuille "$" cellule ":" cellule "]")
		    )
		  )
		)
	   )
	   (not (vl-catch-all-error-p
		  (vl-catch-all-apply
		    'vlax-invoke-method
		    (list
		      adorecord
		      'Open
		      adocommand
		      nil
		      1
		      3
		      nil
		    )
		  )
		)
	   )
	(progn
	  (or (setq val (vl-catch-all-error-p
			  (vl-catch-all-apply
			    'vlax-put-property
			    (list
			      (vlax-get-property (vlax-get adorecord 'fields) 'item 0)
			      'value
			      valeur
			    )
			  )
			)
	      )
	    (vlax-invoke adorecord 'Update)
	  )
	  (vlax-invoke adorecord 'close)
	)
      )
      (vlax-release-object adorecord)
      (vlax-release-object adocommand)
    )
  )
  (not val)
)

;-------------------------------------------------------------------
;
; Sauvegarder le fichier ouvert
; Save file
;
; Entrée --> Classeur
;
; Retourne T si tout s'est bien passée ou nil.
;
; Exemple
; (sauver_fichier Mon_Classeur) --> T
;
;-------------------------------------------------------------------
(defun sauver_fichier(classeur / val)
  (setq val T)
  (cond
    ( (not (vl-catch-all-error-p
	     (vl-catch-all-apply
	       'vlax-get
	       (list
		 classeur 
		 'creator
	       )
	     )
	   )
      )
      (setq val (vl-catch-all-error-p
		  (vl-catch-all-apply
		    'vlax-invoke
		    (list
		      (vlax-get classeur 'parent)
		      'save
		    )
		  )
		)
      )
    )
    ( (not (vl-catch-all-error-p
	     (vl-catch-all-apply
	       'vlax-invoke
	       (list
		 classeur 
		 'store
	       )
	     )
	   )
      )
      (setq val nil)
    )
  )
  (not val)
)

;-------------------------------------------------------------------
;
; Fermer le fichier ouvert
; Attention, si le fichier a été modifié, il ne sera pas sauvegardé
; Sauf pour une liaison SQL
; Close sheets
;
; Entrée --> Classeur
;
; Retourne T si tout s'est bien passée ou nil.
;
; Exemple
; (fermer_fichier Mon_Classeur) --> T
;
;-------------------------------------------------------------------
(defun fermer_fichier(classeur / val)
  (setq val T)
  (cond
    ( (not (vl-catch-all-error-p
	     (vl-catch-all-apply
	       'vlax-get
	       (list
		 classeur 
		 'creator
	       )
	     )
	   )
      )
      (setq val (vl-catch-all-error-p
		  (vl-catch-all-apply
		    'vlax-invoke
		    (list
		      (vlax-get classeur 'parent)
		      'close
		      :vlax-false
		    )
		  )
		)
      )
    )
    ( (not (vl-catch-all-error-p
	     (vl-catch-all-apply
	       'vlax-invoke
	       (list
		 classeur 
		 'close
		 :vlax-false
	       )
	     )
	   )
      )
      (setq val nil)
    )
    ( (and (vlax-property-available-p classeur 'provider)
	   (wcmatch (strcase (vlax-get classeur 'provider)) "MICROSOFT*")
      )
      (setq val (vl-catch-all-error-p
		  (vl-catch-all-apply
		    'vlax-invoke
		    (list
		      classeur
		      'close
		      :vlax-true
		    )
		  )
		)
      )
    )
  )
  (not val)
)

;-------------------------------------------------------------------
;
; Fermer l'application ouvert
; Attention, si le fichier a été modifié, il ne sera pas sauvegardé
; Close Api
;
; Entrée --> liste de variables
;
; Retourne rien
;
; Exemple
; (fermer_appli (list Mon_Appli Mon_Classeur))
;
;-------------------------------------------------------------------
(defun fermer_appli(variables)
  (and (eq (type variables) 'list)
    (mapcar '(lambda(x) (vl-catch-all-apply 'vlax-release-object)) variables)
  )
  (gc)(gc)
  (princ)
)



;;;================================================================================================;;;
(princ)