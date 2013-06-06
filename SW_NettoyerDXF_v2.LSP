(vl-load-com)

					;Sw_NettoyerDXF

(defun Sw_NettoyerDXF (/)

;;; On supprime la liste des pièces soudées
  (setq	Sel (ssget "_X" '((2 . "SW_TABLEANNOTATION_0") (410 . "Model")))
	i   0
  )

  (if sel
    (repeat (sslength Sel)
      (entdel (ssname Sel i))
      (setq i (1+ i))
    )
  )

;;; On crée les calques suivant
  (command "_-layer"	   "n"		   "NOTE DE PLIAGE"
	   "n"		   "LIGNES DE PLIAGE"
	   ""
	  )

;;; Selection des annotations de pliage
  (setq	Sel 'nil
	Sel (ssget "_X" '((1 . "*PLI*") (410 . "Model")))
	i   0
  )

;;; On les change de calque
  (if sel
    (repeat (sslength Sel)
      (setq lst	(entget (ssname Sel i))
	    lst	(subst (cons 8 "NOTE DE PLIAGE") (assoc 8 lst) lst)
	    lst	(subst (cons 62 256) (assoc 62 lst) lst)
      )
      (entmod lst)
      (setq i (1+ i))
    )
  )

;;; Selection des lignes de pliage
  (setq	Sel 'nil
	Sel (ssget "_X"
		   '((410 . "Model")
		     (-4 . "<AND")
		     (6 . "SLD*")
		     (-4 . "AND>")
		     (-4 . "<NOT")
		     (6 . "SLD-Solid")
		     (-4 . "NOT>")
		    )
	    )
	i   0
  )

;;; On recommence, changement de calque
  (if Sel
    (repeat (sslength Sel)
      (setq lst	(entget (ssname Sel i))
	    lst	(subst (cons 8 "LIGNES DE PLIAGE") (assoc 8 lst) lst)
	    lst	(subst (cons 62 256) (assoc 62 lst) lst)
      )
      (entmod lst)
      (setq i (1+ i))
    )
  )

;;; On selectionne les descriptions des corps sous forme de bloc annonyme
  (setq	Sel 'nil
	Sel (ssget "_X"
		   '((0 . "INSERT") (8 . "0") (2 . "`*U*") (410 . "Model"))
	    )
	i   0
  )

;;; On explose tout ca
  (if Sel
    (repeat (sslength Sel)
      (setq blk	(ssname Sel i)
	    i	(1+ i)
      )
      (command "_EXPLODE" blk "")
    )
  )

;;; On renregistre la dernière entite cree
;;; et on selectionne tout les bloc du calque QUANTITE
  (setq	Entite (entlast)
	Sel    (ssget "_X"
		      '((0 . "INSERT") (8 . "QUANTITE") (410 . "Model"))
	       )
	i      0
  )

;;; On les explose
  (if Sel
    (repeat (sslength Sel)
      (setq blk	(ssname Sel i)
	    i	(1+ i)
      )
      (command "_EXPLODE" blk)
    )
  )

;;; On boucle sur les dernieres entites explosees
;;; , on les change de calque et de type de ligne
  (setq Entite (entnext Entite))
  (while Entite
    (setq ed (entget Entite)
	  ed (subst '(8 . "QUANTITE") (assoc 8 ed) ed)
	  ed (subst '(62 . 256) (assoc 62 ed) ed)
    )
    (entmod ed)
    (setq Entite (entnext Entite))
  )

;;; On reselectionne
  (setq	Entite (entlast)
	Sel    'nil
	Sel    (ssget "_X"
		      '((0 . "INSERT") (8 . "QUANTITE") (410 . "Model"))
	       )
	i      0
  )

;;; On reexplose
  (if Sel
    (repeat (sslength Sel)
      (setq blk	(ssname Sel i)
	    i	(1+ i)
      )
      (command "_EXPLODE" blk)
    )
  )

;;; On reselectionne
;;; et on fusionne les textes
  (setq	Sel 'nil
	Sel (ssget "_X"
		   '((0 . "TEXT,MTEXT") (8 . "QUANTITE") (410 . "Model"))
	    )
  )
  (if (and Sel (>= (sslength Sel) 2))
    (progn
      (setq Sel	(vl-remove-if 'listp (mapcar 'cadr (ssnamex Sel)))
	    Sel	(vl-sort Sel
			 (function (lambda (ent1 ent2)
				     (setq pt1 (cdr (assoc '10 (entget ent1)))
					   pt2 (cdr (assoc '10 (entget ent2)))
				     )
				     (if (< (cadr pt1) (cadr pt2))
				       'T
				       (if (< (car pt1) (car pt2))
					 'T
					 'nil
				       )
				     )
				   )
			 )
		)
	    i	-1
      )
      (if (> (length Sel) 1)
	(while (< i (1- (length Sel)))
	  (setq	i	(1+ i)
		T1	(nth i Sel)
		i	(1+ i)
		T2	(nth i Sel)
		IndexT1	(cdr (assoc '1 (entget T1)))
		IndexT2	(cdr (assoc '1 (entget T2)))
		Index	(strcat IndexT1 " " IndexT2)
		T2	(subst (cons '1 Index) (assoc 1 (entget T2)) (entget T2))
	  )
	  (if
	    (entmod T2)
	     (entdel T1)
	  )
	)
      )
    )
  )

;;; On rechange de calque
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

;;; On selectionne tout les blocs et on explose tout
  (setq	Entite (entlast)
	Sel    (ssget "_X" '((0 . "INSERT") (410 . "Model")))
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

;;; On convertis les splines en polyligne compose d'arcs
  (setvar "PLINECONVERTMODE" 1)

  (setq	Sel (ssget "_X" '((0 . "SPLINE") (410 . "Model")))
	i   0
  )

  (if Sel
    (repeat (sslength Sel)
      (setq spl (ssname Sel i))
      (command "_splinedit" spl "_p" 1)
      (setq i (1+ i))
    )
  )

;;; On change la couleur des calques
  (command "_-layer" "ch" "NOTE DE PLIAGE" "co" 126 "" "")
  (command "_-layer" "ch" "LIGNES DE PLIAGE" "co" 252 "" "")
  (command "_-layer" "e" "0" "")

;;; on nettoie
  (command "_zoom" "et")
  (command "_regenall")
  (setvar "MIRRTEXT" 0)
  (command "_-PURGE" "_all" "" "_no")
  (command "_UCSICON" "no")

;;; On convertis les textes à graver en lignes
  (setq	Sel 'nil
	Sel (ssget "_X"
		   '((0 . "MTEXT") (8 . "GRAVURE") (410 . "Model"))
	    )
  )
  (Modtxtexp Sel)

;;; On met les textes d'intitulé à la bonne echelle
  (command "_.zoom" "_e")

;;; les mtext
  (setq	ht  (max (fix (* (/ (getvar "VIEWSIZE") 297.0) 5.0)) 10.0)
	Sel 'nil
	Sel (ssget "_X"
		   '((0 . "MTEXT") (8 . "QUANTITE") (410 . "Model"))
	    )
	i   0
  )

  (if Sel
    (repeat (sslength Sel)
      (setq txt (ssname Sel i))

      (setq txt	(entget txt)
	    txt	(subst (cons 40 ht) (assoc 40 txt) txt)
      )
      (entmod txt)

      (setq i (1+ i))
    )
  )

;;; les text
  (setq	Sel 'nil
	Sel (ssget "_X"
		   '((0 . "TEXT") (8 . "QUANTITE") (410 . "Model"))
	    )
	i   0
  )

  (if Sel
    (repeat (sslength Sel)
      (setq txt (ssname Sel i))

      (setq txt	(entget txt)
	    txt	(subst (cons 40 ht) (assoc 40 txt) txt)
      )
      (entmod txt)

      (setq i (1+ i))
    )
  )


;;; Et on sauve
  (command "_.zoom" "_e")
  ;;(command "_QSAVE")

  (princ)
)

(defun c:s (/)
  (Sw_NettoyerDXF)
)

;;; =================================================================================================
;;; =================================================================================================
;;; =================================================================================================
;;; =================================================================================================
;;; =================================================================================================

;;; Convertir texte en ligne

(defun Modtxtexp (ssTexte /	  grplst  getgname	  blknm
		  FLTR	  GLST	  GDICT	  SS	  VIEW	  UPLFT
		  TMPFIL  TBX	  TMPFIL  CNT	  PT1	  PT2
		  ENT	  TXT	  TXTYP	  PTLST	  ZM	  LOCKED
		  GNAM	  vpna	  vplocked
		 )
  (acet-error-init
    (list
      (list "cmdecho"  0	  "highlight"		1
	    "osmode"   0	  "Mirrtext" 1		"limcheck"
	    0
	   )
      T
    )
  )

					; --------------------- GROUP LIST FUNCTION ----------------------
					;   This function will return a list of all the group names in the
					;   drawing and their entity names in the form:
					;   ((<ename1> . <name1>) ... (<enamex> . <namex>))
					; ----------------------------------------------------------------

  (defun acet-txtexp-grplst (/ GRP ITM NAM ENT GLST)

    (setq GRP (dictsearch (namedobjdict) "ACAD_GROUP"))
    (while (setq ITM (car GRP))		; While edata item is available
      (if (= (car ITM) 3)		; if the item is a group name
	(setq NAM  (cdr ITM)		; get the name
	      GRP  (cdr GRP)		; shorten the edata
	      ITM  (car GRP)		; get the next item
	      ENT  (cdr ITM)		; which is the ename
	      GRP  (cdr GRP)		; shorten the edata
	      GLST			; store the ename and name
		   (if GLST
		     (append GLST (list (cons ENT NAM)))
		     (list (cons ENT NAM))
		   )
	)
	(setq GRP (cdr GRP))		; else shorten the edata
      )
    )
    GLST				; return the list
  )

					; ------------------- GET GROUP NAME FUNCTION --------------------
					;   This function returns a list of all the group names in GLST
					;   where ENT is a member. The list has the same form as GLST
					; ----------------------------------------------------------------

  (defun acet-txtexp-getgname (ENT GLST / GRP GDATA NAM NLST)
    (if	(and GLST (listp GLST))
      (progn
	(foreach GRP GLST
	  (setq GDATA (entget (car GRP)))
	  (foreach ITM GDATA		; step through the edata
	    (if	(and
		  (= (car ITM) 340)	; if the item is a entity name
		  (eq (setq NAM (cdr ITM)) ENT)
					; and the ename being looked for
		)
	      (setq NLST		; store the ename and name
		     (if NLST
		       (append NLST (list (cons (car GRP) (cdr GRP))))
		       (list (cons (car GRP) (cdr GRP)))
		     )
	      )
	    )
	  )
	)
      )
    )
    NLST
  )

					; ----------------------------------------------------------------
					;                          MAIN PROGRAM
					; ----------------------------------------------------------------

  (if (and				; Are we in plan view?
	(equal (car (getvar "viewdir")) 0 0.00001)
	(equal (cadr (getvar "viewdir")) 0 0.00001)
	(> (caddr (getvar "viewdir")) 0)
      )

    (progn

      (Setq GLST  (acet-txtexp-grplst)	; Get all the groups in drawing
	    GDICT (if GLST
		    (dictsearch (namedobjdict) "ACAD_GROUP")
		  )
	    SS	  ssTexte
	    CNT	  0
      )
      ;; filter out the locked layers
      (if SS
	(setq SS (car (bns_ss_mod SS 1 T)))
      )					;if

      ;; if we have anything left
      (if SS
	(progn
	  (setq CNT 0)			; Reset counter
	  (while (setq ENT (ssname SS CNT))
					; step through each object in set

	    (and
	      GLST			; if groups are present in the drawing
	      (setq GNAM (acet-txtexp-getgname ENT GLST))
					; and the text item is in one or more
	      (foreach GRP GNAM		; step through those groups
		(command "_.-group"
			 "_r"		; and remove the text item
			 (cdr GRP)
			 ENT
			 ""
		)
	      )
	    )

	    (setq TBX (acet-geom-textbox (entget ENT) 0))
					; get textbox points

	    (setq TBX (mapcar '(lambda (x)
				 (trans x 1 0)
					; convert the points to WCS
			       )
			      TBX
		      )
	    )

	    (setq PTLST (append PTLST TBX))
					; Build list of bounding box
					; points for text items selected

	    (setq CNT (1+ CNT))		; get the next text item
	  )				; while

	  (setq	PTLST (mapcar '(lambda (x)
				 (trans x 0 1) ; convert all the points
			       )	; to the current ucs
			      PTLST
		      )
	  )

	  (if (setq ZM (acet-geom-zoom-for-select PTLST))
					; If current view does not contain
	    (progn			; all bounding box points
	      (setq ZM
		     (list
		       (list (- (caar ZM) (acet-geom-pixel-unit))
					; increase zoom area by
			     (- (cadar ZM) (acet-geom-pixel-unit))
					; one pixel width to
			     (caddar ZM) ; sure nothing will be lost
		       )
		       (list (+ (caadr ZM) (acet-geom-pixel-unit))
			     (+ (cadadr ZM) (acet-geom-pixel-unit))
			     (caddr (cadr zm))
		       )
		     )
	      )
	      (if (setq vpna (acet-currentviewport-ename))
		(setq vplocked (acet-viewport-lock-set vpna nil))
	      )				;if
	      (command "_.zoom" "_w" (car ZM) (cadr ZM))
					; zoom to include text objects
	    )
	  )

	  (setq	VIEW   (acet-geom-view-points)
		TMPFIL (strcat (getvar "tempprefix") "txtexp.wmf")
		PT1    (acet-geom-midpoint (car view) (cadr view))
		PT2    (list (car PT1) (cadadr VIEW))
	  )

	  (if (acet-layer-locked (getvar "clayer"))
					; if current layer is locked
	    (progn
	      (command "_.layer" "_unl" (getvar "clayer") "")
					; unlock it
	      (setq LOCKED T)
	    )
	  )

	  (command "_.mirror"	     SS	      ""       PT1
		   PT2	    "_y"     "_.WMFOUT"	       TMPFIL
		   SS	    ""
		  )

	  (if (findfile tmpfil)		; Does WMF file exist?
	    (progn
	      (command "_.ERASE" SS "")	; erase the orignal text
	      (setq ss (acet-wmfin TMPFIL)) ; insert the WMF file
	      (command "_.mirror" ss "" PT1 PT2 "_y")
	    )				;progn
	  )				;if


	  (if LOCKED
	    (command "_.layer" "_lock" (getvar "clayer") "")
					; relock if needed
	  )				;if

	  (if ZM
	    (command "_.zoom" "_p")
	  )				; Restore original view if needed
	  (if vplocked
	    (acet-viewport-lock-set vpna T)
					;re-lock the viewport if needed.
	  )				;if
	  (prompt (acet-str-format
		    "\n%1 text object(s) have been exploded to lines."
		    CNT
		  )
	  )
	  (prompt "\nThe line objects have been placed on layer 0.")
	)
      )
    )
    (prompt "\nView needs to be in plan (0 0 1).")
  )					;if equal
  (acet-error-restore)			; Retsore values
  (princ)
)