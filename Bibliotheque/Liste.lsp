(princ "\nRoutines Liste chargé")

;; Manipulation des listes

;;; POSITION - retourne l'index du premier élément dans la liste,
;;; base 0, ou nil si non trouvé
;;;   (position x '(a b c)) -&gt; nil, (position b '(a b c d)) -&gt; 1
(defun position	 (x lst / ret)
  (if (not (zerop (setq ret (length (member x lst)))))
    (- (length lst) ret)))

;;; CONSP  - un lisp non vide
(defun consp (x) (and x (listp x)))

;; BUTLAST
;; Retourne la liste sans le dernier élément

(defun butlast (lst) (reverse (cdr (reverse lst))))

;; DXF
;; Retourne la liste des données associé au code dxf
(defun dxf (code alst)
  (mapcar 'cdr (massoc code alst))
)

;; MASSOC
;; assoc multiple, retourne toutes les clef key
(defun massoc (key alst)
  (apply 'append
	 (mapcar '(lambda (x)
		    (if	(= (car x) key)
		      (list x)
		    )
		  )
		 alst
	 )
  )
)

;;; APPENDLST - Ajoute une liste dans une liste
;;; (appendlst '(1 2 3) '(4 5)) => '(1 2 3 (4 5))
(defun appendlst (l1 l2)
  (append l1 (list l2))
)

;;; list behind (including) the nth element
;;;   (std-nthcdr 1 '(0 1 2 3) => '(1 2 3)
(defun nthcdr (i lst)
  (repeat i
    (setq lst (cdr lst)))
  lst
)

;; CAR2LAST
;; Déplace le premier élément à la fin de la liste
(defun car2last (lst) (reverse (cons (car lst) (reverse (cdr lst)))))

;;; ROT1 - mettre le premier élément à la fin, version simple,
;;;        (rotation par un)
(defun rot1 (lst) (append (cdr lst) (list (car lst))))

;;; DIVISE_ELE
;;; Divise une liste en sous-liste par l'élément ele
(defun divise_ele  (ele lst)
  (if (setq lst (remove-left ele lst))
    (cons (trunc ele lst) (divise_ele ele (cdr (member ele lst))))))

;;; CONCAT_ELE
;;; Concatene les sous-listes avec l'element ele
(defun concat_ele  (sep lst)
  (if (and (listp lst)(cadr lst))
    (append (car lst) (cons sep (concat_ele sep (cdr lst))))
    (car lst)))

;;; REPLACE
;;; Remplace les éléments OLD_ELE par NEW_ELE

(defun replace	(old_ele new_ele lst)
  (if lst
    (mapcar '(lambda (x)
	       (if (= x old_ele)
		 new_ele
		 x))
	    lst)))

;; REMOVE_DOUBLES
;; Supprime tous les doublons d'une liste

(defun remove_doubles  (lst)
  (if lst
    (cons (car lst) (remove_doubles (vl-remove (car lst) lst)))))

;; REMOVE_DOUBLES_ELE
;; Supprime les doublons ELE d'une liste

(defun remove_doubles_ele  (ele lst)
  (if (member ele lst)
    (append (trunc ele lst)
	    (cons (car (member ele lst)) (remove_doubles_ele ele (remove-left ele (member ele lst)))))
    lst))

;; REMOVE-I
;; Retourne la liste privée de l'élément à l'indice spécifié
;; (premier élément = 0)

(defun remove-i	 (ind lst)
  (if (or (zerop ind) (null lst))
    (cdr lst)
    (cons (car lst) (remove-i (1- ind) (cdr lst)))))

;; REMOVE_FT
;; Retourne la liste privée des élements du premier indice
;; au second indice spécifié (premier élément = 0)

(defun remove_ft  (lst from to)
  (cond	((or (null lst) (zerop to)) (cdr lst))
	((< 0 from) (cons (car lst) (remove_ft (cdr lst) (1- from) (1- to))))
	(T (remove_ft (cdr lst) (1- from) (1- to)))))

;; REMOVE-ELE
;; Retourne la liste sans la première occurence de l'expression

(defun remove-ele  (ele lst)
  (if (equal ele (car lst))
    (cdr lst)
    (cons (car lst) (remove-ele ele (cdr lst)))))

;;; REMOVE - Enlève un article d'une liste (les éléments en doubles sont permis)
;;;   (remove 0 '(0 1 2 3 0)) -&gt; (1 2 3)
					; (c) par Serge Volkov

(defun remove (ele lst) (apply 'append (subst nil (list ele) (mapcar 'list lst))))

;;; REMOVE-IF - Enlèvement conditionnel d'un article d'une liste simple
;;; fun demande exactement 1 argument
;;;   (remove-if 'zerop '(0 1 2 3 0)) -&gt; (1 2 3)
;;;   (remove-if 'numberp '(0 (0 1) "")) -&gt; ((0 1) "")

(defun remove-if  (fun from)
  (cond	((atom from) from)		;nil or symbol (return that)
	((apply fun (list (car from))) (remove-if fun (cdr from)))
	(t (cons (car from) (remove-if fun (cdr from))))))

;;; REMOVE-IF-NOT  - conserve tous les éléments auxquels l'attribut s'applique
;;; disons : " conserve si ", doit être défini récursivement, comme ceci
;;; par Vladimir Nesterowsky

(defun remove-if-not  (pred lst)
  (apply 'append
	 (mapcar '(lambda (e)
		    (if	(pred e)
		      (list e)))
		 lst)))

;;; REMOVE-LEFT - enleve tous les éléments ele en partant de la gauche
;;; tant que (car lst) = ele

(defun remove-left  (ele lst)
  (if (and (listp lst) (equal (car lst) ele))
    (REMOVE-LEFT ele (cdr lst))
    lst))

;;; REMOVE-RIGHT - enleve tous les éléments ele en partant de la droite
;;; tant que (car lst) = ele

(defun remove-right (ele lst) (if (listp lst) (reverse (REMOVE-LEFT ele (reverse lst)))))

;; SUBST-I
;; Remplace l'élément à l'indice spécifié par un nouvel élément

(defun subst-i	(new ind lst)
  (if (or (zerop ind) (null lst))
    (cons new (cdr lst))
    (cons (car lst) (subst-i new (1- ind) (cdr lst)))))

;; COMMON
;;; Retourne la liste des éléments communs à lst1 et lst2

(defun common  (l1 l2)
  (if l1
    (if	(member (car l1) l2)
      (cons (car l1) (common (cdr l1) l2))
      (common (cdr l1) l2))))

;; EXCLUSIVE
;; Retourne une liste contenant les éléments appartenant exclusivement à l1

(defun exclusive  (l1 l2)
  (if l1
    (if	(member (car l1) l2)
      (exclusive (cdr l1) l2)
      (cons (car l1) (exclusive (cdr l1) l2)))))

;;; ADJOIN -  ajoute un élément dans une liste s'il n'y est pas déjà 
;;; astuce : accepte des listes quotées aussi, tel que
;;;  (setq l '(1 2 3) (adjoin 0 'l)
;;;  -&gt; !l (0 1 2 3)
(defun adjoin  (ele lst / tmp)
  (if (= (type lst) 'SYM)
    (setq tmp lst
	  lst (eval tmp)))
  (setq	lst (cond ((member ele lst) lst)
		  (t (cons ele lst))))
  (if tmp
    (set tmp lst)
    lst))

;; EQUAL_EVERY
;; Évalue si tous les membres d'une liste sont égaux à "fuzz" près

(defun equal_every (lst fuzz) (vl-every (function (lambda (x) (equal x (car lst) fuzz))) (cdr lst)))

;; TRUNC
;; Retourne la liste tronquée à partir de la première occurrence de l'expression
;; (liste complémentaire de celle retournée par MEMBER)

(defun trunc  (expr lst)
  (if (and lst (not (equal (car lst) expr)))
    (cons (car lst) (trunc expr (cdr lst)))))

;; TRUNC-IF
;; Retourne la liste tronquée à partir de la première occurrence qui
;; retourne T à la fonction (complémentaire de celle retournée par VL-MEMBER-IF)

(defun trunc-if	 (fun lst)
  (if (and lst (not ((eval fun) (car lst))))
    (cons (car lst) (trunc-if fun (cdr lst)))))

;; MEMBER-FUZZ
;; Comme MEMBER avec une tolérance dans la comparaison

(defun member-fuzz  (expr lst fuzz)
  (while (and lst (not (equal (car lst) expr fuzz))) (setq lst (cdr lst)))
  lst)

;; TRUNC-FUZZ 
;; Comme TRUNC avec une tolérance dans la comparaison

(defun trunc-fuzz  (expr lst fuzz)
  (if (and lst (not (equal (car lst) expr)))
    (cons (car lst) (trunc-fuzz expr (cdr lst) fuzz))))

;;; SUBLIST
;;;  Retourne une sous-liste
;;;
;;; Arguments
;;; lst : une liste
;;; start : l'index de départ de la sous liste (premier élément = 0)
;;; leng : la longueur (nombre d'éléments) de la sous-liste (ou nil)
;;;
;;; Exemples :
;;; (sublist '(1 2 3 4 5 6) 2 2) -> (3 4)
;;; (sublist '(1 2 3 4 5 6) 2 nil) -> (3 4 5 6)

(defun sublist	(lst start leng / n r)
  (if (or (not leng) (< (- (length lst) start) leng))
    (setq leng (- (length lst) start)))
  (setq n (+ start leng))
  (repeat leng (setq r (cons (nth (setq n (1- n)) lst) r))))

;; SPLIT-LIST
;;; Retourne une liste de sous-listes
;;;
;; Arguments
;; - lst : la liste à fractionner
;; - num : un entier, le nombre d'éléments des sous listes
;; Exemples :
;; (split-list '(1 2 3 4 5 6 7 8) 2) -> ((1 2) (3 4) (5 6) (7 8))
;; (split-list '(1 2 3 4 5 6 7 8) 3) -> ((1 2 3) (4 5 6) (7 8))

(defun split-list  (lst n)
  (if lst
    (cons (sublist lst 0 n) (split-list (sublist lst n nil) n))))

;; COMBI (gile)
;; Retourne la liste de toutes les combinaisons possibles
;; (combi '(1 2 3)) -> ((1) (1 2) (1 3) (1 2 3) (2) (2 3) (3))

(defun combi  (lst / subr first tmp1 tmp2 tmp3 ret)
  (defun subr  (l1 l2)
    (if	l2
      (cons (append l1 (list (car l2))) (subr l1 (cdr l2)))))
  (while lst
    (setq first	(list (car lst))
	  tmp1	(subr first (cdr lst))
	  tmp2	(cons first tmp1))
    (while tmp1
      (setq tmp3 (subr (car tmp1) (cdr (member (last (car tmp1)) lst)))
	    tmp1 (cdr (append tmp1 tmp3))
	    tmp2 (append tmp2 tmp3)))
    (setq ret (append ret tmp2)
	  lst (cdr lst)))
  ret)

;; COMBI3 (gile)
;; Retourne la liste de toutes les combinaisons par 3
;; (combi3 '(1 2 3 4)) -> ((1 2 3) (1 2 4) (1 3 4) (2 3 4))

(defun combi3  (l / subr1 subr2)
  (defun subr1	(e1 e2 l)
    (if	l
      (cons (list e1 e2 (car l)) (subr1 e1 e2 (cdr l)))))
  (defun subr2	(e l)
    (if	(cdr l)
      (append (subr1 e (car l) (cdr l)) (subr2 e (cdr l)))))
  (if (cddr l)
    (append (subr2 (car l) (cdr l)) (combi3 (cdr l)))))

;; SORT (gile)
;; Trie une liste à l'aide d'une fonction de comparaison
;; fonctionne comme vl-sort sans supprimer les doublons
(defun sort  (lst fun / merge tmp)
  (defun merge	(l1 l2)
    (if	(and l1 l2)
      (if (fun (car l1) (car l2))
	(cons (car l1) (merge (cdr l1) l2))
	(cons (car l2) (merge l1 (cdr l2))))
      (if l1
	l1
	l2)))
  (setq	lst (mapcar 'list lst)
	fun (eval fun))
  (while (cdr lst)
    (setq tmp lst
	  lst nil)
    (while (cdr tmp)
      (setq lst	(cons (merge (car tmp) (cadr tmp)) lst)
	    tmp	(cddr tmp)))
    (and tmp (setq lst (cons (car tmp) lst))))
  (car lst))

;; LST2STR
;; Concatène une liste et un séparateur en une chaine
;;
;; Arguments
;; lst : la liste à transformer en chaine
;; sep : le séparateur
;;
;; Exemples
;; (lst2str '(1 2 3) ",") -> "1,2,3"
;; (lst2str '("a" "b" "c") " ") -> "a b c"

(defun lst2str	(lst sep)
  (if (cadr lst)
    (strcat (vl-princ-to-string (car lst)) sep (lst2str (cdr lst) sep))
    (vl-princ-to-string (car lst))))

 ;|«Visual LISP© Format Options»
(200 2 40 0 nil "Fin de " 100 9 0 0 0 T nil nil T)
;*** NE PAS AJOUTER de texte au-dessous du commentaire! ***|;
