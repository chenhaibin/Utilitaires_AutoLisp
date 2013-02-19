(vl-load-com)

(defun c:SA
	    (/		  Sel	       LstCercle    i		 Cercle	      ScanPtBG
	     ScanPtHD	  ScanDmin     ScanDmax	    ScanEntraxe	 tmp	      FactEchelle
	     CercleDiam	  CercleCentre
	    )

  (princ "\nSelectionnez les splines à traiter ")
  (setq	Sel	    (ssget '((0 . "SPLINE")))
	ScanEntraxe 10000.0
  )
  (princ "\n\n Scan des entitées, patientez...")
;;; Recherche du premier cercle correpondant au critère de diametre
  (setq i 0)
  (while (not LstCercle)
    (setq Cercle (CentreArcSpline (ssname Sel i))
	  i	 (1+ i)
    )
    (if	(>= (last Cercle) Dmin)
      (setq LstCercle (list Cercle)
	    ScanPtBG  (car Cercle)
	    ScanPtHD  (car Cercle)
	    ScanDmin  (cadr Cercle)
	    ScanDmax  (cadr Cercle)
      )
    )
  )

;;; Boucle sur les cercles suivants correspondant au critère de diametre et de position
  (repeat (- (sslength Sel) (1+ i))
    (setq Cercle (CentreArcSpline (ssname Sel i))
	  i	 (1+ i)
    )
    (if
      (not (vl-every (function (lambda (a b) (equal a b 1.0))) (caar LstCercle) (car Cercle)))
       (setq ScanPtBG	 (mapcar 'min ScanPtBG (caar LstCercle) (car Cercle))
	     ScanPtHD	 (mapcar 'max ScanPtHD (caar LstCercle) (car Cercle))
	     ScanEntraxe (min ScanEntraxe (distance (caar LstCercle) (car Cercle)))
	     ScanDmin	 (min ScanDmin (cadar LstCercle) (cadr Cercle))
	     ScanDmax	 (max ScanDmax (cadar LstCercle) (cadr Cercle))
	     LstCercle	 (cons Cercle LstCercle)
       )
    )
  )

  (princ (strcat "\nDiametre mini : " (rtos ScanDmin 2 1) " , maxi : " (rtos ScanDmax 2 1)))
  (princ (strcat "\nEntraxe " (rtos ScanEntraxe 2 3)))
  (princ "\n\n==== Filtrage des cercles ====")
  (setq	tmp	     (getreal (strcat "\nDiametre mini"
				      (if (or (not Dmin) (not (numberp Dmin)))
					""
					(strcat " <" (rtos Dmin 2 1) ">")
				      )
				      " : "
			      )
		     )
	Dmin	     (if (not tmp)
		       Dmin
		       tmp
		     )
	tmp
		     (NettoyerEspaces
		       (getstring t
				  (strcat "\nListe des poinçons (séparés par un espace)"
					  (if (or (not ListePoincon) (not (listp ListePoincon)))
					    ""
					    (strcat " <" (lst2str ListePoincon " ") ">")
					  )
					  " : "
				  )
		       )
		     )
	ListePoincon (if (= tmp "")
		       ListePoincon
		       (mapcar 'atof (str2lst tmp " "))
		     )
	tmp	     (getreal (strcat "\nEntraxe du poinçonnage"
				      (if (or (not Entraxe) (not (numberp Entraxe)))
					""
					(strcat " <" (rtos Entraxe 2 1) ">")
				      )
				      " : "
			      )
		     )
	Entraxe	     (if (not tmp)
		       Entraxe
		       tmp
		     )
	FactEchelle  (/ (- (apply 'max ListePoincon) Dmin) (- ScanDmax Dmin))
  )

  (foreach Cercle LstCercle
    (setq CercleDiam   (NormaliseDiam (cadr Cercle) FactEchelle Dmin)
	  CercleCentre (mapcar
			 (function (lambda (x y)
				     (+ (* (fix (+ (/ (- x y) ScanEntraxe) 0.5)) Entraxe) y)
				   )
			 )
			 (car Cercle)
			 ScanPtBG
		       )
    )
    (if	(>= CercleDiam Dmin)
      (make_cercle CercleCentre (* (Poincon CercleDiam ListePoincon) 0.5) (lst2str ListePoincon "-"))
    )
  )

  (setq Sel 'nil)
  (princ)
)


;;; ============================ Sous fonctions ============================================

;;; Met le cercle à l'echelle suivant le facteur et le diam mini
(defun NormaliseDiam (Diam Fact Dmini)
  (+ (* (- Diam Dmini) FactEchelle) Dmini)
)


;;; Recherche le poincon le plus proche dans la liste donnée
(defun Poincon (Diam ListePoincon / Intervales j)
  (setq	Intervales
	 (mapcar 'Moyenne
		 ListePoincon
		 (append (cdr ListePoincon) '(10000))
	 )
	j 0
  )
  (while (> Diam (nth j Intervales))
    (setq j (1+ j))
  )
  (nth j ListePoincon)
)


;;; Renvoi le centre d'un cercle représenté par une spline parcourant 1/4 de cercle
(defun CentreArcSpline (ent / lst pDepart pArrive pInter pCentre vec)
  (setq	lst	(dxf '10 (entget ent))
	pDepart	(trans (car lst) ent 0)
	pArrive	(trans (last lst) ent 0)
	pInter	(trans (cadr lst) ent 0)
  )
  (if (= (trigo	pDepart
		pArrive
		(setq pCentre (list (car pDepart) (cadr pArrive)))
	 )
	 (trigo pDepart pArrive pInter)
      )
    (setq pCentre (list (car pArrive) (cadr pDepart)))
  )
  (list pCentre (* 2.0 (abs (- (car pDepart) (car pArrive)))))
)


;;; Creer un cercle et renvoi son identifiant
(defun make_cercle (centre rayon Calque /)
  (entmake (append '((0 . "CIRCLE") (410 . "Model"))
		   (list (cons '8 Calque))
		   (list (cons '10 centre))
		   (list (cons '40 rayon))
	   )
  )
  (entlast)
)


;;; Moyenne entre deux nombres
(defun Moyenne (a b)
  (* (+ a b) 0.5)
)


;;; Renvoi la liste des listes associées au code dxf
(defun dxf (code alst)
  (apply 'append
	 (mapcar '(lambda (x)
		    (if	(= (car x) code)
		      (list (cdr x))
		    )
		  )
		 alst
	 )
  )
)


;;; Chaine en liste
(defun str2lst (str sep / pos)
  (if (setq pos (vl-string-search sep str))
    (cons (substr str 1 pos) (str2lst (substr str (+ (strlen sep) pos 1)) sep))
    (list str)
  )
)


;;; Liste en chaine
(defun lst2str (lst sep)
  (if (cadr lst)
    (strcat (vl-princ-to-string (car lst)) sep (lst2str (cdr lst) sep))
    (vl-princ-to-string (car lst))
  )
)


;;; Supprime les espaces double, de début et de fin
(defun NettoyerEspaces (str)
  (vl-string-trim " " (chr_to_str (remove_doubles_ele '32 (str_to_chr str))))
)


;; Supprime les doublons ELE d'une liste
(defun remove_doubles_ele (ele lst)
  (if (member ele lst)
    (append (trunc ele lst)
	    (cons (car (member ele lst)) (remove_doubles_ele ele (remove-left ele (member ele lst))))
    )
    lst
  )
)

;; STR_TO_CHR
;;
;; Converti une chaine en liste de code ascii
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun str_to_chr (str /)
  (if (> (strlen str) 1)
    (cons (ascii (substr str 1 1)) (str_to_chr (substr str 2)))
    (list (ascii str))
  )
)

;; TRUNC
;; Retourne la liste tronquée à partir de la première occurrence de l'expression
;; (liste complémentaire de celle retournée par MEMBER)

(defun trunc (expr lst)
  (if (and lst (not (equal (car lst) expr)))
    (cons (car lst) (trunc expr (cdr lst)))
  )
)

;;; REMOVE-LEFT - enleve tous les éléments ele en partant de la gauche
;;; tant que (car lst) = ele

(defun remove-left (ele lst)
  (if (and (listp lst) (equal (car lst) ele))
    (REMOVE-LEFT ele (cdr lst))
    lst
  )
)

;; CHR_TO_STR
;;
;; Converti une liste de code ascii en chaine de caractère
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun chr_to_str (lst /) (apply 'strcat (mapcar 'chr lst)))

;; Trigo  -  (gile)
;; Returns the midpoint of two points

(defun Trigo (p1 p2 p3)
  (< (sin (- (angle p1 p2) (angle p1 p3))) -1e-14)
)

(princ "\nTapez SA pour utiliser la macro")
(princ)

