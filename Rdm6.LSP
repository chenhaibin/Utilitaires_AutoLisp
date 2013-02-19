(defun c:rdm6 (/		  info_entete	     info_groupe	info_materiau
	       format_rdm6	  afficher_geometrie afficher_groupes	decomposer_poutres
	       current_date	  i
	      )
  (setq
;;;===========================   Entête   =========================;;;
	info_entete   "RDM - Ossatures
Calcul des Structures par la Méthode des Éléments Finis
$debut du fichier
$version
6.17
$SI unites
$nom du fichier
%NOM_FICHIER%
$date
%JOUR%
$heure
%HEURE%
$ossature
spatiale"
;;;===========================   Groupe   =========================;;;
	info_groupe   "TYPE QUELCONQUE
NOM *
DESIGNATION *
LOGO 0
AIRE  0.00000000000E+00
IYY  0.00000000000E+00
IZZ  0.00000000000E+00
alpha  0.00000000000E+00
WPY  0.00000000000E+00
WPZ  0.00000000000E+00
TORSION  0.00000000000E+00
KYY  1.0000000
KZZ  1.0000000
IWW  0.00000000000E+00
YCISAILLEMENT  0.00000000000E+00
ZCISAILLEMENT  0.00000000000E+00
BTY  0.00000000000E+00
BTZ  0.00000000000E+00
BTW  0.00000000000E+00
///"
;;;=========================   Materiaux   ========================;;;
	info_materiau "$materiaux
11
NOM Acier S235
MOD  2.100E+11
POI 0.2850
MAS 7850.00
DIL  1.2500E-05
LIM  2.350E+08
///
0
$gpesanteur
10.000
$cas de charges
1
////
$modes propres
nombre 1
methode sous_espace
precision 1.00000E-02
decalage_spectral 0.00000E+00
////
$maillage
20
$fin du fichier"
  )
;;; Retourne la liste des ENAMEs des entités d'un jeu de sélection
  (defun ss_list (ss)
    (if	ss
      (vl-remove-if 'listp (mapcar 'cadr (ssnamex ss)))
    )
  )
;;; VEC1 (gile)
;;; Retourne le vecteur normé (1 unité) de sens p1 p2
;;;
;;; Arguments : deux points
  (defun vec1 (p1 p2)
    ((lambda (d)
       (if (not (zerop d))
	 (mapcar (function (lambda (x1 x2) (/ (- x2 x1) d))) p1 p2)
       )
     )
      (setq d (distance p1 p2))
    )
  )
;;; V^V (gile)
;;; Retourne le produit vectoriel (vecteur) de deux vecteurs
;;;
;;; Arguments : deux vecteurs
  (defun v^v (v1 v2)
    (list (- (* (cadr v1) (caddr v2)) (* (caddr v1) (cadr v2)))
	  (- (* (caddr v1) (car v2)) (* (car v1) (caddr v2)))
	  (- (* (car v1) (cadr v2)) (* (cadr v1) (car v2)))
    )
  )
;;;==========================================================;;
;;; Renvoi le nombre (el) à la précision (p) demandée par excès
  (defun precision (el p / fun)
    (defun fun (nb e)
      (if (>= (setq nb (* nb (/ 1.0 e))) (+ (fix nb) 0.5))
	(* (1+ (fix nb)) e)
	(* (fix nb) e)
      )
    )
    (cond ((numberp el) (fun el p))
	  ((listp el) (mapcar '(lambda (n) (fun n p)) el))
	  (t nil)
    )
  )
;;;==========================================================;;
;;; Tri des points selon x y z
  (defun tri_pts (lst)
    (vl-sort (vl-sort (vl-sort lst '(lambda (x y) (< (caddr x) (caddr y))))
		      '(lambda (x y) (< (cadr x) (cadr y)))
	     )
	     '(lambda (x y) (< (car x) (car y)))
    )
  )
;;;==========================================================;;
;;; Valeur du code dxf d'une entité (ename)
  (defun val_dxf (code ent) (cdr (assoc code (entget ent))))
;;;==========================================================;;
;;; Liste des sommets d'une lwpolyligne (dans le SCG), la direction d'extrusion et le calque
  (defun lwpoly_pts (ent)
    (cons (val_dxf 210 ent)
	  (cons	(val_dxf 8 ent)
		(mapcar	'(lambda (pt) (trans (list (car pt) (cadr pt) (val_dxf 38 ent)) ent 0))
			(massoc 10 (entget ent))
		)
	  )
    )
  )
;;;==========================================================;;
;;; Formater une liste au format rdm6
  (defun format_rdm6 (lst)
    (if	(listp lst)
      (lst2str (mapcar '(lambda	(x)
			  (if (listp x)
			    (lst2str (nb_to_e x 11) " ")
			    x
			  )
			)
		       lst
	       )
	       " "
      )
    )
  )
;;;==========================================================;;
;;; Affiche la geometrie selon le formatage RDM6
  (defun afficher_geometrie (lst titre)
    (strcat "\n$"
	    titre
	    "( "
	    (itoa (length lst))
	    " )"
	    (apply 'strcat (mapcar '(lambda (x) (strcat "\n" (format_rdm6 (unshift x)))) lst))
	    "\n0"
    )
  )
;;;==========================================================;;
;;; Affiche les groupes selon le formatage RDM6
  (defun afficher_groupes (lst)
    (strcat "\n$sections"
	    (apply 'strcat (mapcar '(lambda (x) (strcat "\n" (itoa (last x)) info_groupe)) lst))
	    "\n0"
    )
  )
;;;==========================================================;;
;;; Affiche les liaisons selon le formatage RDM6
  (defun afficher_liaisons (lst)
;;;$liaisons ( 2 )
;;;rotule 1
;;;encastrement 2
;;;///
    (strcat "\n$liaisons( " (itoa (length lst)) " )" "\n///")
  )
;;;==========================================================;;
;;; Affiche l'entete du fichier RDM6
  (defun afficher_entete (/ date jour heure ossature)
    (setq date	 (mapcar 'fix (current_date))
	  date	 (split-list date 3)
	  jour	 (lst2str (reverse (car date)) "/")
	  heure	 (lst2str (last date) "/")
	  lst_pt (mapcar 'car lst_pt)
    )
    (str_edit info_entete '(nom_fichier jour heure) (list nom_fichier jour heure))
  )
;;;==========================================================;;
;;; Décomposer une polyligne en segment à partir de la fonction lwpoly_pts
  (defun decomposer_poutres (ptr / info pt1 pt2)
    (if	(> (length ptr) 4)
      (progn (setq info	(list (car ptr) (cadr ptr))
		   pt1	(caddr ptr)
		   ptr	(cdddr ptr)
	     )
	     (mapcar '(lambda (pt2 / lst)
			(setq lst (list pt1 pt2)
			      pt1 pt2
			)
			(append info lst)
		      )
		     ptr
	     )
      )
      (list ptr)
    )
  )
;;;==========================================================;;
;;; Date sous forme de liste
  (defun current_date (/ yr mo day hh mm sec rst)
    (setq cdate	(getvar "CDATE")
	  yr	(fix (/ cdate 10000.0))
	  cdate	(- cdate (* 10000.0 yr))
	  mo	(fix (/ cdate 100.0))
	  cdate	(- cdate (* 100.0 mo))
	  day	(fix cdate)
	  rst	(- cdate day)
	  hh	(fix (* rst 100))
	  rst	(- rst (/ hh 100.0))
	  mm	(fix (* rst 10000))
	  sec	(* 1000000.0 (- rst (/ mm 10000.0)))
    )
    (list yr mo day hh mm sec)
  )
;;;==========================================================;;;
;;;                                                          ;;;
;;;                        Main                              ;;;
;;;                                                          ;;;
;;;==========================================================;;;
  (and (setq fichier	 (getfiled "Emplacement du fichier" "" "por" 1)
	     nom_fichier (strcat (vl-filename-base fichier) ".por")
	     flag	 (open fichier "w")
       )
       (princ "Selectionnez la structure à exporter :")
       ;; Selection des éléments de la structure
       (setq structure (ss_lst (ssget '((0 . "LWPOLYLINE"))))
	     structure (mapcar 'lwpoly_pts structure)
	     ;;=======================;;
	     ;;  Analyse des points   ;;
	     ;;=======================;;
	     points    (apply 'append (mapcar 'cddr structure))
	     points    (mapcar '(lambda (pt) (precision pt 1.0)) points)
	     points    (remove_doubles (tri_pts points))
	     ;;  Numérotation des points
	     i	       0
	     points    (mapcar '(lambda (pt) (setq i (1+ i)) (list pt i)) points)
	     ;;=======================;;
	     ;;  Analyse des groupes  ;;
	     ;;=======================;;
	     groupes   (remove_doubles (vl-sort (mapcar 'cadr structure) '<))
	     i	       10
	     groupes   (mapcar '(lambda (gp) (setq i (1+ i)) (list gp i)) groupes)
	     ;;=======================;;
	     ;;  Analyse des poutres  ;;
	     ;;=======================;;
	     poutres   (apply 'append (mapcar 'decomposer_poutres structure))
	     i	       0
	     poutres   (mapcar '(lambda	(ptr / pt1 pt2 vec)
				  (setq	i	(1+ i)
					pt1	(caddr ptr)
					pt2	(cadddr ptr)
					vec_seg	(vec1 pt1 pt2)
					vec_ext	(car ptr)
					;; Vecteur axe Z
					vec	(v^v vec_seg vec_ext)
				  )
				  (list	"RIRI"
					(last (assoc (precision pt1 1.0) points))
					(last (assoc (precision pt2 1.0) points))
					(precision vec 0.001)
					(cadr (assoc (cadr ptr) groupes))
					11
					i
				  )
				)
			       poutres
		       )
	     ;; Convertis les unités en metre
	     points    (mapcar
			 '(lambda (lst) (list (mapcar '(lambda (val) (* val 0.001)) (car lst)) (cadr lst)))
			 points
		       )
       )
       (write-line (afficher_entete) flag)
       (write-line (afficher_geometrie points "noeuds") flag)
       (write-line (afficher_geometrie poutres "poutres") flag)
       (write-line (afficher_groupes groupes) flag)
       ;;(write-line (afficher_liaisons liaisons) flag)
       (write-line info_materiau flag)
       (close flag)
  )
  (princ)
)
 ;|«Visual LISP© Format Options»
(100 2 40 2 nil "Fin de " 100 9 0 0 0 T nil nil T)
;*** NE PAS AJOUTER de texte au-dessous du commentaire! ***|;
