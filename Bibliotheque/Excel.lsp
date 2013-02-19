;;-------------------------------------------------------------------------------
;; Program Name: GetExcel.lsp
;; Created By:   Terry Miller (Email: terrycadd@yahoo.com)
;;               (URL: http://web2.airmail.net/terrycad)
;; Date Created: 9-20-03
;; Function:     Several functions to get and put values into Excel cells.
;;-------------------------------------------------------------------------------
; Revision History
; Rev  By     Date    Description
;-------------------------------------------------------------------------------
; 1    TM   9-20-03   Initial version
; 2    TM   8-20-07   Rewrote GetExcel.lsp and added several new sub-functions
;                     including ColumnRow, Alpha2Number and Number2Alpha written
;                     by Gilles Chanteau from Marseille, France.
; 3    TM   12-1-07   Added several sub-functions written by Gilles Chanteau
;                     including Cell-p, Row+n, and Column+n. Also added his
;                     revision of the PutCell function.
; 4    GC   9-20-08   Revised the GetExcel argument MaxRange$ to accept a nil
;                     and get the current region from cell A1.
;;
;;-------------------------------------------------------------------------------
;; Aper�u des principales fonctions*
;;-------------------------------------------------------------------------------
;;
;; GetExcel - Stocke les donn�es d'un classeur Excel dans la liste *ExcelData@
;;   Syntaxe:  (GetExcel ExcelFile$ SheetName$ MaxRange$)
;;   Exemple: (GetExcel "C:\\Dossier\\Fichier.xls" "Feuil1" "L30")
;;
;; GetCell - Retourne la valeur d'une cellule depuis la liste *ExcelData@
;;   Syntaxe:  (GetCell Cell$)
;;   Exemple: (GetCell "H15")
;;
;; Exemple d'utilisation des fonctions:
;; (defun c:Get-Example ()
;;   (GetExcel "C:\\Folder\\Filename.xls" "Feuil1" "L30");<-- Editer Filename.xls
;;   (GetCell "H21");Ou juste utiliser la liste dans la variable globale *ExcelData@
;; );defun
;;
;;-------------------------------------------------------------------------------
;;
;; OpenExcel - Ouvre un classeur Excel
;;   Syntax:  (OpenExcel ExcelFile$ SheetName$ Visible)
;;   Example: (OpenExcel "C:\\Dossier\\Fichier.xls" "Feuil1" nil)
;;
;; PutCell - Met des donn�es dans des cellules Excel
;;   Syntaxe:  (PutCell StartCell$ Data$) or (PutCell StartCell$ DataList@)
;;   Exemple: (PutCell "A1" (list "GP093" 58.5 17 "Base" "3'-6 1/4\""))
;;
;; CloseExcel - Ferme la session Excel
;;   Syntaxe:  (CloseExcel ExcelFile$)
;;   Exemple: (CloseExcel "C:\\Dossier\\Fichier.xls")
;;
;; Exemple d'utilisation des fonctions:
;; (defun c:Put-Example ()
;;   (OpenExcel "C:\\Folder\\Filename.xls" "Feuil1" nil);<-- Editer Filename.xls
;;   (PutCell "A1" (list "GP093" 58.5 17 "Base" "3'-6 1/4\"")); R�p�ter � volont�
;;   (CloseExcel "C:\\Folder\\Filename.xls");<-- Editer Filename.xls
;;   (princ)
;; );defun
;;
;; Note: Point sur les conditions de chaque argument dans les ent�tes des fonctions
;;
;; *Traduction en fran�ais : Gilles Chanteau
;;
;; Modifications :
;; Ajout des routines Column+n Row+n et Cell-p
;; Ajout de la possibilit� de donner un nom de cellule dans PutCell
;;

;;-------------------------------------------------------------------------------
;; GetExcel - Stocke les donn�es d'une feuille Excel dans la liste *ExcelData@
;; Arguments : 3
;;   ExcelFile$ = Chemin et nom ce fichier
;;   SheetName$ = Nom de la feuille ou nil pour non sp�cifi�
;;   MaxRange$ = R�f�rence de la cellule maximum � inclure (ou nil pour toute la region depuis la cellule A1)
;; Exemples de syntaxe :
;; (GetExcel "C:\\Temp\\Temp.xls" "Feuil1" "E19") = Ouvre C:\Temp\Temp.xls sur la Feuille1 et lit jusqu'� la cellule E19
;; (GetExcel "C:\\Temp\\Temp.xls" nil "IV123") = Ouvre C:\Temp\Temp.xls sur la feuille courante et lit jusqu'� la cellule IV123
;;-------------------------------------------------------------------------------
(defun GetExcel	(ExcelFile$    SheetName$    MaxRange$	   /
		 Column#       ColumnRow@    Data@	   ExcelRange^
		 ExcelValue    ExcelValue    ExcelVariant^ MaxColumn#
		 MaxRow#       Range$	     Row#	   Worksheet
		)
  (if (= (type ExcelFile$) 'STR)
    (if	(not (findfile ExcelFile$))
      (progn
	(alert (strcat "Fichier Excel " ExcelFile$ " non trouv�."))
	(exit)
      )
    )
    (progn
      (alert "Fichier Excel non specifi�.")
      (exit)
    )
  )
  (gc)
  (if (setq *ExcelApp% (vlax-get-object "Excel.Application"))
    (progn
      (alert "Fermeture de toutes les feuilles Excel.")
      (vlax-release-object *ExcelApp%)
      (gc)
    )
  )
  (setq *ExcelApp% (vlax-get-or-create-object "Excel.Application"))
  (vlax-invoke-method
    (vlax-get-property *ExcelApp% 'WorkBooks)
    'Open
    ExcelFile$
  )
  (if SheetName$
    (vlax-for Worksheet	(vlax-get-property *ExcelApp% "Sheets")
      (if (= (vlax-get-property Worksheet "Name") SheetName$)
	(vlax-invoke-method Worksheet "Activate")
      )
    )
  )
  (if MaxRange$
    (setq ColumnRow@  (ColumnRow MaxRange$)
	  MaxColumn#  (nth 0 ColumnRow@)
	  MaxRow#     (nth 1 ColumnRow@)
    )
    (setq CurRegion  (vlax-get-property
		       (vlax-get-property
			 (vlax-get-property *ExcelApp% "ActiveSheet")
			 "Range"
			 "A1"
		       )
		       "CurrentRegion"
		     )
	  MaxRow#    (vlax-get-property
		       (vlax-get-property CurRegion "Rows")
		       "Count"
		     )
	  MaxColumn# (vlax-get-property
		       (vlax-get-property CurRegion "Columns")
		       "Count"
		     )
    )
  )
  (setq	*ExcelData@ nil
	Row# 1
  )
  (repeat MaxRow#
    (setq Data@	nil
	  Column# 1
    )
    (repeat MaxColumn#
      (setq Range$	  (strcat (Number2Alpha Column#) (itoa Row#))
	    ExcelRange^	  (vlax-get-property *ExcelApp% "Range" Range$)
	    ExcelVariant^ (vlax-get-property ExcelRange^ 'Value)
	    ExcelValue	  (vlax-variant-value ExcelVariant^)
	    ExcelValue	  (cond
			    ((= (type ExcelValue) 'INT) (itoa ExcelValue))
			    ((= (type ExcelValue) 'REAL) (rtosr ExcelValue))
			    ((= (type ExcelValue) 'STR)
			     (vl-string-trim " " ExcelValue)
			    )
			    ((/= (type ExcelValue) 'STR) "")
			  )
	    Data@	  (append Data@ (list ExcelValue))
	    Column#	  (1+ Column#)
      )
      (or (member (type ExcelValue) '(INT REAL STR))
	  (setq ExcelValue "")
      )
    )
    (setq *ExcelData@
	   (append *ExcelData@ (list Data@))
	  Row# (1+ Row#)
    )
  )
  (vlax-invoke-method
    (vlax-get-property *ExcelApp% "ActiveWorkbook")
    'Close
    :vlax-False
  )
  (vlax-invoke-method *ExcelApp% 'Quit)
  (vlax-release-object *ExcelApp%)
  (gc)
  (setq *ExcelApp% nil)
  *ExcelData@
)

;;-------------------------------------------------------------------------------
;; GetCell - Retourne la valeur d'une cellule depuis la liste *ExcelData@
;; Argument : 1
;;   Cell$ = R�f�rence de la cellule
;; Exemple de syntaxe : (GetCell "E19") = valeur de la cellule E19
;;-------------------------------------------------------------------------------
(defun GetCell (Cell$ / Column# ColumnRow@ Return Row#)
  (setq	ColumnRow@ (ColumnRow Cell$)
	Column#	   (1- (car ColumnRow@))
	Row#	   (1- (cadr ColumnRow@))
	Return	   ""
  )
  (if *ExcelData@
    (if	(and (>= (length *ExcelData@) Row#)
	     (>= (length (nth 0 *ExcelData@)) Column#)
	)
      (setq Return (nth Column# (nth Row# *ExcelData@)))
    )
  )
  Return
)

;;-------------------------------------------------------------------------------
;; OpenExcel - Ouvre une feuille Excel
;; Arguments : 3
;;   ExcelFile$ = Nom de fichier excel ou nil pour une nouvelle feuille
;;   SheetName$ = Nom de la feuille ou nil pour non sp�cifi�
;;   Visible = T pour visible ou nil pour cach�e
;; Exemples de syntaxe :
;; (OpenExcel "C:\\Temp\\Temp.xls" "Feuil2" t) = Ouvre C:\Temp\Temp.xls sur la Feuille2 en session visible
;; (OpenExcel "C:\\Temp\\Temp.xls" nil nil) = Ouvre C:\Temp\Temp.xls sur la feuille courante
;; (OpenExcel nil "Parts List" nil) =  Ouvre un nouveau classeur et cr�e une feuille "Parts List" en session cach�e
;;-------------------------------------------------------------------------------
(defun OpenExcel (ExcelFile$ SheetName$ Visible / Sheet$ Sheets@ Worksheet)
  (if (= (type ExcelFile$) 'STR)
    (if	(findfile ExcelFile$)
      (setq *ExcelFile$ ExcelFile$)
      (progn
	(alert (strcat "FichierExcel " ExcelFile$ " non trouv�."))
	(exit)
      )
    )
    (setq *ExcelFile$ "")
  )
  (gc)
  (if (setq *ExcelApp% (vlax-get-object "Excel.Application"))
    (progn
      (alert "Fermeture de toutes les feuilles.")
      (vlax-release-object *ExcelApp%)
      (gc)
    )
  )
  (setq *ExcelApp% (vlax-get-or-create-object "Excel.Application"))
  (if ExcelFile$
    (if	(findfile ExcelFile$)
      (vlax-invoke-method
	(vlax-get-property *ExcelApp% 'WorkBooks)
	'Open
	ExcelFile$
      )
      (vlax-invoke-method
	(vlax-get-property *ExcelApp% 'WorkBooks)
	'Add
      )
    )
    (vlax-invoke-method
      (vlax-get-property *ExcelApp% 'WorkBooks)
      'Add
    )
  )
  (if Visible
    (vla-put-visible *ExcelApp% :vlax-true)
  )
  (if (= (type SheetName$) 'STR)
    (progn
      (vlax-for	Sheet$ (vlax-get-property *ExcelApp% "Sheets")
	(setq Sheets@ (append Sheets@ (list (vlax-get-property Sheet$ "Name"))))
      )
      (if (member SheetName$ Sheets@)
	(vlax-for Worksheet (vlax-get-property *ExcelApp% "Sheets")
	  (if (= (vlax-get-property Worksheet "Name") SheetName$)
	    (vlax-invoke-method Worksheet "Activate")
	  )
	)
	(vlax-put-property
	  (vlax-invoke-method
	    (vlax-get-property *ExcelApp% "Sheets")
	    "Add"
	  )
	  "Name"
	  SheetName$
	)
      )
    )
  )
  (princ)
)

;;-------------------------------------------------------------------------------
;; PutCell - Met des donn�es dans des cellules Excel
;; Arguments: 2
;;   StartCell$ = R�f�rence de la cellule de d�part ou nom de la cellule
;;   Data@ = Donn�e ou liste de donn�es
;; Exemples de syntaxe :
;; (PutCell "A1" "PART NUMBER") = Met PART NUMBER dans la cellule A1
;; (PutCell "B3" '("Dim" 7.5 "9.75")) = � partir de la cellule B3 met Dim, 7.5 et 9.75
;; (PutCell "Surface" '(35.4)) = Met 35.4 dans la cellule nomm�e Surface
;;-------------------------------------------------------------------------------
(defun PutCell (StartCell$ Data@ / Cell Column# ExcelRange Row#)
  (if (= (type Data@) 'STR)
    (setq Data@ (list Data@))
  )
  (setq ExcelRange (vlax-get-property *ExcelApp% "Cells"))
  (if (Cell-p StartCell$)
    (setq Column#    (car (ColumnRow StartCell$))
	  Row#	     (cadr (ColumnRow StartCell$))
    )
    (if	(vl-catch-all-error-p
	  (setq	Cell
		 (vl-catch-all-apply
		   'vlax-get-property
		   (list (vlax-get-property *ExcelApp% "ActiveSheet")
			 "Range"
			 StartCell$
		   )
		 )
	  )
	)
      (alert (strcat "Cellule \"" StartCell$ "\" introuvable."))
      (setq Column# (vlax-get-property Cell "Column")
	    Row#    (vlax-get-property Cell "Row")
      )
    )
  )
  (if (and Column# Row#)
    (foreach Item Data@
      (vlax-put-property
	ExcelRange
	"Item"
	Row#
	Column#
	(vl-princ-to-string Item)
      )
      (setq Column# (1+ Column#))
    )
  )
  (princ)
)

;;-------------------------------------------------------------------------------
;; CloseExcel - Ferme le classeur Excel
;; Arguments: 1
;;   ExcelFile$ = Nom d'enregistrement du fichier Excel ou nil pour fermer sans enregistrer
;; Exemples de syntaxe :
;; (CloseExcel "C:\\Temp\\Temp.xls") = Enregistre C:\Temp\Temp.xls et ferme
;; (CloseExcel nil) = Ferme sans enregistrer
;;-------------------------------------------------------------------------------
(defun CloseExcel (ExcelFile$ / Saveas)
  (if ExcelFile$
    (if	(= (strcase ExcelFile$) (strcase *ExcelFile$))
      (if (findfile ExcelFile$)
	(vlax-invoke-method
	  (vlax-get-property *ExcelApp% "ActiveWorkbook")
	  "Save"
	)
	(setq Saveas t)
      )
      (if (findfile ExcelFile$)
	(progn
	  (vl-file-delete (findfile ExcelFile$))
	  (setq Saveas t)
	)
	(setq Saveas t)
      )
    )
  )
  (if Saveas
    (vlax-invoke-method
      (vlax-get-property *ExcelApp% "ActiveWorkbook")
      "SaveAs"
      ExcelFile$
      -4143
      ""
      ""
      :vlax-false
      :vlax-false
      nil
    )
  )
  (vlax-invoke-method
    (vlax-get-property *ExcelApp% "ActiveWorkbook")
    'Close
    :vlax-False
  )
  (vlax-invoke-method *ExcelApp% 'Quit)
  (vlax-release-object *ExcelApp%)
  (gc)
  (setq	*ExcelApp% nil
	*ExcelFile$ nil
  )
  (princ)
)

;;-------------------------------------------------------------------------------
;; ColumnRow - Retourne une liste des indices de colonne et rang�e
;; Function By: Gilles Chanteau from Marseille, France
;; Arguments: 1
;;   Cell$ = R�f�rence de la cellule
;; Exemple de syntaxe : (ColumnRow "IV987") = '(256 987)
;;-------------------------------------------------------------------------------
(defun ColumnRow (Cell$ / Column$ Char$ Row#)
  (setq Column$ "")
  (while (< 64 (ascii (setq Char$ (strcase (substr Cell$ 1 1)))) 91)
    (setq Column$ (strcat Column$ Char$)
	  Cell$	  (substr Cell$ 2)
    )
  )
  (if (and (/= Column$ "") (numberp (setq Row# (read Cell$))))
    (list (Alpha2Number Column$) Row#)
    '(1 1) ;_ default to "A1" if there's a problem
  )
)

;;-------------------------------------------------------------------------------
;; Alpha2Number - Convertit une cha�ne alphab�tique en nombre entier
;; Function By: Gilles Chanteau from Marseille, France
;; Arguments: 1
;;   Str$ = Cha�ne � convertir
;; Exemple de syntaxe : (Alpha2Number "BU") = 73
;;-------------------------------------------------------------------------------
(defun Alpha2Number (Str$ / Num#)
  (if (= 0 (setq Num# (strlen Str$)))
    0
    (+ (* (- (ascii (strcase (substr Str$ 1 1))) 64)
	  (expt 26 (1- Num#))
       )
       (Alpha2Number (substr Str$ 2))
    )
  )
)

;;-------------------------------------------------------------------------------
;; Number2Alpha - Convertit un nombre entier en cha�ne alphab�tique
;; Function By: Gilles Chanteau from Marseille, France
;; Arguments: 1
;;   Num# = Nombre � convertir
;; Exemple de syntaxe : (Number2Alpha 73) = "BU"
;;-------------------------------------------------------------------------------
(defun Number2Alpha (Num# / Val#)
  (if (< Num# 27)
    (chr (+ 64 Num#))
    (if	(= 0 (setq Val# (rem Num# 26)))
      (strcat (Number2Alpha (1- (/ Num# 26))) "Z")
      (strcat (Number2Alpha (/ Num# 26)) (chr (+ 64 Val#)))
    )
  )
)

;;-------------------------------------------------------------------------------
;; rtosr - Utilis� pour changer un nombre r�el en cha�ne num�rique avec les
;; z�ros de fin supprim�s.
;; Function By: Gilles Chanteau from Marseille, France
;; Arguments: 1
;;   RealNum~ = Nombre r�el � convertir en cha�ne raccourcie
;; Retourne: ShortReal$ la cha�ne num�rique de la valeur du nombre r�el.
;;-------------------------------------------------------------------------------
(defun rtosr (RealNum~ / old_dimzin ShortReal$)
  (and
    (numberp RealNum~)
    (setq old_dimzin (getvar "DIMZIN"))
    (setvar "DIMZIN" 8)
    (setq ShortReal$ (rtos RealNum~ 2 15))
    (setvar "DIMZIN" old_dimzin)
  )
  ShortReal$
)

;;-------------------------------------------------------------------------------
;; Cell-p Evalue si l'argument est une r�f�rence de cellule valide
;; Function By: Gilles Chanteau from Marseille, France
;; Argument: 1
;;   cell = la cha�ne � �valuer
;; Exemples de syntaxe :
;; (Cell-p "B12") = T
;; (Cell-p "BT") = nil
;;-------------------------------------------------------------------------------
(defun Cell-p (cell)
  (and (= (type cell) 'STR)
       (or (= (strcase cell) "A1")
	   (not (equal (ColumnRow cell) '(1 1)))
       )
  )
)

;;-------------------------------------------------------------------------------
;; Row+n Retourne la cellule situ�e � n rang�es de cell
;; Function By: Gilles Chanteau from Marseille, France
;; Arguments: 2
;;   cell = cellule � incr�menter
;;   n = incr�ment
;; Exemple de syntaxe : (Row+n "B12" 3) = "B15"
;;-------------------------------------------------------------------------------
(defun Row+n (cell n)
  (setq cell (ColumnRow cell))
  (strcat (Number2Alpha (car cell))
	  (itoa (max 1 (+ (cadr cell) n)))
  )
)

;;-------------------------------------------------------------------------------
;; Column+n Retourne la cellule situ�e � n colonnes de cell
;; Function By: Gilles Chanteau from Marseille, France
;; Arguments: 2
;;   cell = cellule � incr�menter
;;   n = incr�ment
;; Exemple de syntaxe : (Column+n "B12" 3) = "E12"
;;-------------------------------------------------------------------------------
(defun Column+n	(cell n)
  (setq cell (ColumnRow cell))
  (strcat (Number2Alpha (max 1 (+ (car cell) n)))
	  (itoa (cadr cell))
  )
)


;;;================================================================================================;;;
(princ)