;;; Quelques routines de calcul vectoriel et matriciel.


;;=================== CALCUL VECTORIEL ===================;;

;; V+V
;; Retourne le somme de deux vecteur
;;
;; Arguments : deux vecteurs

(defun v+v (v1 v2) (mapcar '+ v1 v2))

;; V2D
;; Retourne un vecteur 2D
;;
;; Arguments : un vecteur
(defun v2d (l)
  (mapcar '+ (append l '(0.0 0.0)) '(0.0 0.0))
)

;; V3D
;; Retourne un vecteur 3D
;;
;; Arguments : un vecteur
(defun v3d (l)
  (mapcar '+ (append l '(0.0 0.0 0.0)) '(0.0 0.0 0.0))
)

;; VXS (gile)
;; Retourne le produit d'un vecteur par un scalaire
;;
;; Arguments : un vecteur et un réel

(defun vxs (v s) (mapcar (function (lambda (x) (* x s))) v))

;; VXV (gile)
;; Retourne le produit scalaire (réel) de deux vecteurs
;;
;; Arguments : deux vecteurs

(defun vxv (v1 v2) (apply '+ (mapcar '* v1 v2)))


;; V^V (gile)
;; Retourne le produit vectoriel (vecteur) de deux vecteurs
;;
;; Arguments : deux vecteurs

(defun v^v (v1 v2)
  (list	(- (* (cadr v1) (caddr v2)) (* (caddr v1) (cadr v2)))
	(- (* (caddr v1) (car v2)) (* (car v1) (caddr v2)))
	(- (* (car v1) (cadr v2)) (* (cadr v1) (car v2)))
  )
)

;; VUNIT (gile)
;; Retourne le vecteur unitaire d'un vecteur
;;
;; Argument : un vecteur

(defun vunit (v)
  ((lambda (l)
     (if (/= 0 l)
       (mapcar (function (lambda (x) (/ x l))) v)
     )
   )
    (distance '(0 0 0) v)
  )
)

;; VEC1 (gile)
;; Retourne le vecteur normé (1 unité) de sens p1 p2
;;
;; Arguments : deux points

(defun vec1 (p1 p2)
  ((lambda (d)
     (if (not (zerop d))
       (mapcar (function (lambda (x1 x2) (/ (- x2 x1) d))) p1 p2)
     )
   )
    (setq d (distance p1 p2))
  )
)

;; NORM_3PTS (gile)
;; Retourne le vecteur normal du plan défini par 3 points
;;
;; Arguments : trois points

(defun norm_3pts (p0 p1 p2)
  (vunit (v^v (mapcar '- p1 p0) (mapcar '- p2 p0)))
)

;; Retourne la distance entre deux points
;;
;; Arguments : deux points

(defun dist (p1 p2)
  (sqrt
    (apply '+
	   (mapcar '(lambda (x) (expt x 2)) (mapcar '- p2 p1))
    )
  )
)

;; Retourne un vecteur
;;
;; Arguments : deux points

(defun vect (p1 p2) (mapcar '- p2 p1))

;; VROTATION
;;
;; Retourne le vecteur avec une rotation suivant l'angle en radian donné
;; Le vecteur doit être en 2D
;; Arguments : un vecteur et un réel

(defun vrotation (v a)
  (cons	(- (* (car v) (cos a)) (* (cadr v) (sin a)))
	(cons (+ (* (car v) (sin a)) (* (cadr v) (cos a)))
	      (cddr v)
	)
  )
)

;; VNORME
;; Retourne la norme d'un vecteur
;;
;; Arguments : un vecteur

(defun vnorme (v1) (distance '(0 0 0) v1))

;; VANGLE
;; Retourne l'angle entre deux vecteurs
;;
;; Arguments : deux vecteurs

(defun vangle (v1 v2)
  (acos (/ (vxv v1 v2) (* (vnorme v1) (vnorme v2))))
)

;; VPERP
;; Retourne le vecteur perpendiculaire à un vecteur
;;
;; Arguments : un vecteurs

(defun vperp (v)
  (cons (* (cadr v) -1.0) (cons (car v) (cddr v)))
)

;; Midpoint  -  Lee Mac
;; Returns the midpoint of two points

(defun mid (a b)
    (mapcar (function (lambda (a b) (/ (+ a b) 2.0))) a b)
)

;; Trigo  -  (gile)
;; Returns the midpoint of two points

(defun Trigo ( p1 p2 p3 )
  (< (sin (- (angle p1 p2) (angle p1 p3))) -1e-14)
)

;; Exemples d'utilisation -------------------------------

;; Project Point onto Line  -  Lee Mac
;; Projects pt onto the line defined by p1,p2

(defun ProjectPointToLine ( pt p1 p2 / nm )
    (setq nm (mapcar '- p2 p1)
          p1 (trans p1 0 nm)
          pt (trans pt 0 nm)
    )
    (trans (list (car p1) (cadr p1) (caddr pt)) nm 0)
)

;; LINEARP (gile)
;; Retourne T si tous les points de la liste sont alignés
;;
;; Arguments : une liste de points

(defun linearp (lst)
  (or
    (null (cddr lst))
    (and (or (equal (vec1 (car lst) (cadr lst))
		    (vec1 (car lst) (caddr lst))
		    1e-9
	     )
	     (equal (vec1 (car lst) (cadr lst))
		    (vec1 (caddr lst) (car lst))
		    1e-9
	     )
	 )
	 (linearp (cdr lst))
    )
  )
)

;; PARALLELP (gile)
;; Retourne T si les segments p1 p2 et p3 p4 sont parallèles
;;
;; Arguments : quatre points

(defun parallel-p (p1 p2 p3 p4)
  (equal '(0 0 0)
	 (v^v (mapcar '- p1 p2) (mapcar '- p3 p4))
	 1e-9
  )
)

;; PERPENDICULARP (gile)
;; Retourne T si les segments p1 p2 et p3 p4 sont perpendiculaires
;;
;; Arguments : quatre points

(defun perpendicularp (p1 p2 p3 p4)
  (equal (vxv (mapcar '- p2 p1) (mapcar '- p4 p3)) 0 1e-9)
)

;; COPLANP
;; Retourne T si tous les points de la liste sont coplanaires
;;
;; Arguments : une liste de points

(defun coplanp (lst)
  (or
    (null (cdddr lst))
    (and
      (equal (vxv (v^v (vec1 (car lst) (cadr lst))
		       (vec1 (car lst) (caddr lst))
		  )
		  (vec1 (car lst) (cadddr lst))
	     )
	     0.0
	     1e-9
      )
      (coplanar-p (cdr lst))
    )
  )
)

;; ELEV (gile)
;;  Retourne l'élévation du point pt par rapport au plan défini par un point 
;; du plan et sa normale
;;
;; Arguments
;; pt : le point dont on cherche l'élévation
;; org : un point queconque du plan de projection
;; nor : le vecteur normal du plan de projection

(defun elev (pt org nor)
  (/ (vxv nor (mapcar '- pt org)) (distance '(0 0 0) nor))
)

;; PROJ_PT (gile)
;; Retourne les coordonnées de la projection orthogonale
;; du point pt sur le plan défini par son origine et sa normale
;;
;; Arguments
;; pt : le point dont on cherche la projection
;; org : un point queconque du plan de projection
;; nor : le vecteur normal du plan de projection

(defun proj_pt (pt org norm)
  (mapcar '-
	  pt
	  (mapcar
	    (function
	      (lambda (x)
		(* x
		   (cos (angle_3pts org (mapcar '+ org norm) pt))
		   (distance org pt)
		)
	      )
	    )
	    norm
	  )
  )
)

;; ILP (gile)
;;  Retourne le point d'intersection de la droite définie par p1 p2
;; et du plan défini par un point et sa normale.
;;
;; Arguments
;; p1 et p2 : les points définissant la droite dont on cherche l'intersection
;; org : un point queconque du plan d'intersection
;; nor : le vecteur normal du paln d'intersection

(defun ilp (p1 p2 org nor / scl)
  (if (and
	(/= 0 (setq scl (vxv nor (mapcar '- p2 p1))))
	(setq scl (/ (vxv nor (mapcar '- p1 org)) scl))
      )
    (mapcar (function (lambda (x1 x2) (+ (* scl (- x1 x2)) x1)))
	    p1
	    p2
    )
  )
)


;;=================== CALCUL MATRICIEL ===================;;

;; M+M
;; Additionne 2 matrices
;;
;; Arguments : deux matrices

(defun m+m (m1 m2)
  (mapcar
    (function (lambda (v1 v2) (mapcar '+ v1 v2)))
    m1
    m2
  )
)

;; MXS
;; Multiplie une martice par un nombre
;;
;; Arguments : une matrice et un nombre

(defun mxs (m s)
  (mapcar (function (lambda (v) (vxs v s))) m)
)

;; TRP
;; transpose une matrice -Doug Wilson-
;;
;; Argument : une matrice

(defun trp (m) (apply 'mapcar (cons 'list m)))

;; MXV
;; Applique une matrice de transformation à un vecteur -Vladimir Nesterovsky-
;;
;; Arguments : une matrice et un vecteur

(defun mxv (m v)
  (mapcar (function (lambda (r) (vxv r v))) m)
)

;; MXM
;; Multiple (combine) deux matrices -Vladimir Nesterovsky-
;;
;; Arguments : deux matrices

(defun mxm (m q)
  (mapcar (function (lambda (r) (mxv (trp q) r))) m)
)

;; SQUARE-P
;; Evalue si une matrice est carrée
;;
;; Argument : une matrice

(defun square-p	(m)
  (vl-every (function (lambda (v) (= (length v) (length m))))
	    m
  )
)

;; UNIFORM-P
;; Evalue si une matrice de transformation (3X3 ou 4X4) a un échelle uniforme

(defun uniform-p (m)
  (and (or (= 3 (length m)) (setq m (mapcar 'VMbutlast (VMbutlast m))))
       (vl-every
	 (function
	   (lambda (v)
	     (equal (distance '(0 0 0) (car m))
		    (distance '(0 0 0) v)
		    1e-12
	     )
	   )
	 )
	 m
       )
  )
)

;; TMATRIX-VALID
;; Evalue si une matrice est valide pour une utilisation avec vlax-tmatrix

(defun tmatrix-valid (m)
  (and (= 4 (setq l (length m)))
       (square-p m)
       (uniform-p m)
  )
)

;; IMAT
;; Crée une matrice d'identité de dimension n
;;
;; Argument
;; n : la dimension de la matrice
;; i : toujours 0

(defun Imat (n i / sub)
  (defun sub (n i)
    (if	(< 0 n)
      (if (zerop i)
	(cons 1.0 (sub (1- n) n))
	(cons 0.0 (sub (1- n) (1- i)))
      )
    )
  )
  (if (< i n)
    (cons (sub n i) (imat n (1+ i)))
  )
)

;; GaussJordan (gile)
;; Applique la méthode d'élimination de Gauss-Jordan à deux matrices
;;
;; Arguments : 2 matrices (la première doit être carrée)

(defun GaussJordan (m1 m2 / mat len ind row piv new)
  (setq len (length m1))
  (if (= len (length m2))
    (progn
      (setq mat	(mapcar (function (lambda (x1 x2) (append x1 x2))) m1 m2)
	    ind	0
      )
      (while (< ind len)
	(setq row (nth ind mat))
	(if (zerop (car row))
	  (progn
	    (if	(vl-every 'zerop (mapcar 'car (member row mat)))
	      (setq mat nil)
	      (while (zerop (car row))
		(setq mat (append (vl-remove row mat) (list row))
		      row (nth ind mat)
		)
	      )
	    )
	  )
	)
	(if mat
	  (setq	piv (float (car row))
		new (vxs row (/ 1 piv))
		mat (mapcar
		      (function
			(lambda	(x)
			  (if (equal x row)
			    (cdr new)
			    (cdr
			      (mapcar
				'-
				x
				(vxs new (car x))
			      )
			    )
			  )
			)
		      )
		      mat
		    )
		ind (1+ ind)
	  )
	  (setq ind len)
	)
      )
      mat
    )
  )
)

;; INVERSE (gile)
;; Inverse une matrice carrée (méthode Gauss-Jordan)
;;
;; Argument: la matrice
;; Retour : la matrice inverse ou nil (si non inversible)

(defun inverse (mat)
  (GaussJordan mat (Imat (length mat) 0))
)

;; COFACT (gile)
;; Retourne le cofacteur associé à l'élément ij d'une matrice
;;
;; Arguments
;; i = indice de la ligne (première ligne = 1)
;; j = indice de la colonne (première colonne = 1)
;; m = une matrice

(defun cofact (i j m)
  (* (determ
       (VMremove-i
	 (1- i)
	 (mapcar (function (lambda (x) (VMremove-i (1- j) x))) m)
       )
     )
     (expt -1 (+ i j))
  )
)

;; DETERM (gile)
;; Retourne le déterminant d'une matrice carré
;;
;; Argument : une matrice

(defun determ (m)
  (if (= 2 (length m))
    (- (* (caar m) (cadadr m)) (* (caadr m) (cadar m)))
    ((lambda (r n)
       (apply
	 '+
	 (mapcar
	   (function (lambda (x) (* x (cofact 1 (setq n (1+ n)) m))))
	   r
	 )
       )
     )
      (car m)
      0
    )
  )
)

;; ADJ-MAT (gile)
;; Retourne la matrice adjointe d'une matrice
;;
;; Argument : une matrice

(defun adj-mat (m / i)
  (setq i 0)
  (trp
    (mapcar
      (function
	(lambda	(v / j)
	  (setq	i (1+ i)
		j 0
	  )
	  (mapcar
	    (function (lambda (x) (cofact i (setq j (1+ j)) m)))
	    v
	  )
	)
      )
      m
    )
  )
)

;; INV-MAT (gile)
;; Retourne la matrice inverse d'une matrice (méthode des cofacteurs)
;;
;; Argument : une matrice

(defun inv-mat (m / d)
  (if (/= 0 (setq d (determ m)))
    (mxs (adj-mat m) (/ 1 d))
  )
)


;; Exemples d'applications ------------------------------

;; NENT2NENTP (gile)
;; Transforme la matrice 4X3 type nentsel en une matrice 4X4 type nentselp

(defun nent2nentp (mat)
  (append (trp mat) (list '(0.0 0.0 0.0 1.0)))
)

;; NENTP2NENT (gile)
;; Transforme la matrice 4X4 type nentselp en une matrice 4X3 type nentsel

(defun nentp2nentl (mat)
  (trp (reverse (cdr (reverse mat))))
)

;; RCS2WCS (gile)
;; Traduit les coordonnées du Système de Coordonnées Reference (bloc ou xref) vers le SCG
;;
;; Arguments :
;; pt : un point dans le RCS
;; mat : une matrice de transformation, retournée par (caddr (nentsel)) ou (caddr (nentselp))

(defun RCS2WCS (pt mat)
  (setq pt (trans pt 0 0))
  (if (= 3 (length (car mat)))
    (mapcar '+ (mxv (trp (VMbutlast mat)) pt) (last mat))
    (mapcar '+
	    (mxv (mapcar 'VMbutlast (VMbutlast mat)) pt)
	    (VMbutlast (mapcar 'last mat))
    )
  )
)

;; WCS2RCS (gile) - d'après une idée de JoeBurke-
;; Traduit les coordonnées du SCG vers le Système de Coordonnées Reference (bloc ou xref)
;;
;; Arguments :
;; pt : un point dans le SCG
;; mat : une matrice de transformation, retournée par (caddr (nentsel)) ou (caddr (nentselp))

(defun WCS2RCS (pt mat)
  (setq pt (trans pt 0 0))
  (if (= 3 (length (car mat)))
    (setq mat (append (trp mat) (list '(0.0 0.0 0.0 1.0))))
  )
  (setq mat (inv-mat mat))
  (mapcar '+ (mxv mat pt) (VMbutlast (mapcar 'last mat)))
)

;; TransNested (gile)
;; Convertit les coordonnées d'un point entre le SCG ou le SCU  et le SCR -systéme de
;; coordonées d'une référence (xref ou bloc) quelque soit son niveau d'imbrication-
;;
;; Arguments
;; pt : le point à convertir
;; rlst : la liste des entités "parents" de la plus imbriqué à celle insérée dans
;;        l'espace courant -indentique à (last (nentsel)) ou (last (nentselp))
;; from to : comme avec trans : 0 pour le SCG, 1 pour le SCU courant, 2 pour le SCR

(defun TransNested (pt rlst from to / mat dep)
  (setq mat '((1 0 0) (0 1 0) (0 0 1)))
  (and (= 1 from) (setq pt (trans pt 1 0)))
  (and (= 2 to) (setq rlst (reverse rlst)))
  (and (or (= 2 from) (= 2 to))
       (while rlst
	 (setq geom (if	(= 2 to)
		      (RevRefGeom (car rlst))
		      (RefGeom (car rlst))
		    )
	       rlst (cdr rlst)
	       mat  (mxm (car geom) mat)
	       pt   (mapcar '+ (mxv (car geom) pt) (cadr geom))
	 )
       )
  )
  (if (= 1 to)
    (trans pt 0 1)
    pt
  )
)

;; RefGeom (gile)
;; Retourne une liste dont le premier élément est une matrice de transformation
;; (rotation, échelles, mormale) de dimension 3X3 et le second le point
;; d'insertion de l'objet dans son "parent" (xref, bloc ou espace)

(defun RefGeom (ename / elst ang norm)
  (setq	elst (entget ename)
	ang  (cdr (assoc 50 elst))
	norm (cdr (assoc 210 elst))
  )
  (list
    (mxm
      (mapcar (function (lambda (v) (trans v 0 norm T)))
	      '((1.0 0.0 0.0) (0.0 1.0 0.0) (0.0 0.0 1.0))
      )
      (mxm
	(list (list (cos ang) (- (sin ang)) 0.0)
	      (list (sin ang) (cos ang) 0.0)
	      '(0.0 0.0 1.0)
	)
	(list (list (cdr (assoc 41 elst)) 0.0 0.0)
	      (list 0.0 (cdr (assoc 42 elst)) 0.0)
	      (list 0.0 0.0 (cdr (assoc 43 elst)))
	)
      )
    )
    (trans (cdr (assoc 10 elst)) norm 0)
  )
)

;; RevRefGeom (gile)
;; Fonction inverse de RefGeom

(defun RevRefGeom (ename / entData ang norm mat)
  (setq	entData	(entget ename)
	ang	(- (cdr (assoc 50 entData)))
	norm	(cdr (assoc 210 entData))
  )
  (list
    (setq mat
	   (mxm
	     (list (list (/ 1 (cdr (assoc 41 entData))) 0.0 0.0)
		   (list 0.0 (/ 1 (cdr (assoc 42 entData))) 0.0)
		   (list 0.0 0.0 (/ 1 (cdr (assoc 43 entData))))
	     )
	     (mxm
	       (list (list (cos ang) (- (sin ang)) 0.0)
		     (list (sin ang) (cos ang) 0.0)
		     '(0.0 0.0 1.0)
	       )
	       (mapcar (function (lambda (v) (trans v norm 0 T)))
		       '((1.0 0.0 0.0) (0.0 1.0 0.0) (0.0 0.0 1.0))
	       )
	     )
	   )
    )
    (mapcar '-
	    (mxv mat (trans (cdr (assoc 10 entData)) norm 0))
    )
  )
)


;; Fonctions de manipulation des listes utilisées dans les routines précédentes

;; VMremove-i
;; Retourne la liste privée de l'élément à l'indice spécifié (premier élément = 0)
;;
;; Arguments : la liste et l'indice de l'élément à supprimer

(defun VMremove-i	(ind lst)
  (if (or (zerop ind) (null lst))
    (cdr lst)
    (cons (car lst) (VMremove-i (1- ind) (cdr lst)))
  )
)

;; VMbutlast
;; Retourne la liste privée du dernier élément
;;
;; Argument : une liste

(defun VMbutlast (l) (reverse (cdr (reverse l))))


;;;================================================================================================;;;
(princ)