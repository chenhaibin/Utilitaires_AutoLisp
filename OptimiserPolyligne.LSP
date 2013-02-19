(defun c:OPL (/ CMDECHO CN DENT ENT LST N NLST SEL)
  (princ "\nSélectionner les polylignes à optimiser : ")
  (setq cmdecho (getvar "cmdecho"))
  (setvar "cmdecho" 0)
  (if (setq sel (ssget))
    (progn
      (command "_UNDO" "D")
      (repeat (setq cn (sslength sel))
	(setq ent  (ssname sel (setq cn (1- cn)))
	      dent (entget ent)
	      lst  (remove-doubles
		     (remove-align (vl-remove-if-not '(lambda (x) (= (car x) 10)) (entget ent)))
		   )
	)
	(foreach pt (remove-all lst (vl-remove-if-not '(lambda (x) (= (car x) 10)) (entget ent)))
	  (setq n (vl-position pt dent))
	  (setq nlst (append (sublist dent 0 n) (sublist dent (+ n 4) nil)))
	  (setq dent nlst)
	)
	(entmod nlst)
	(entupd ent)
	(princ "\nPolyligne optimisée.")
      )
    )
  )
  (command "_UNDO" "F")
  (setvar "cmdecho" cmdecho)
  (princ)
)


;;; SUBLIST De GILE

(defun sublist (lst start leng / n r)
  (if (or (not leng) (< (- (length lst) start) leng))
    (setq leng (- (length lst) start))
  )
  (setq n (+ start leng))
  (repeat leng (setq r (cons (nth (setq n (1- n)) lst) r)))
)


;;; REMOVE-ALIGN De GILE

(defun remove-align (lst / rslt)
  (while (caddr lst)
    (if	(betweenp (car lst) (caddr lst) (cadr lst))
      (setq lst (cons (car lst) (cddr lst)))
      (setq rslt (cons (car lst) rslt)
	    lst	 (cdr lst)
      )
    )
  )
  (append (reverse rslt) lst)
)


;;; REMOVE-DOUBLES De GILE

(defun remove-doubles (lst)
  (if lst
    (cons (car lst) (remove-doubles (vl-remove (car lst) lst)))
  )
)


;;; REMOVE-ALL
;;; Supprime tous les éléments d'une liste à partir d'une autre
;;; (REMOVE-ALL '(1 3 5) '(1 2 3 4 5 6 7)) -> (2 4 6 7)

(defun REMOVE-ALL (lise lisc) (foreach pt lise (setq lisc (vl-remove pt lisc))))


;;; BETWEENP Evalue si pt est entre p1 et p2 (ou égal à)
;;;Lisp de GILE

(defun betweenp	(p1 p2 pt)
  (or (equal p1 pt 1e-9) (equal p2 pt 1e-9) (equal (vec1 p1 pt) (vec1 pt p2) 1e-9))
)


;;; VEC1 Retourne le vecteur normé (1 unité) de p1 à p2 (nil si p1 = p2)
;;;Lisp de GILE

(defun vec1 (p1 p2)
  (if (not (equal p1 p2 1e-009))
    (mapcar '(lambda (x1 x2) (/ (- x2 x1) (distance p1 p2))) p1 p2)
  )
)
 ;|«Visual LISP© Format Options»
(100 2 40 2 nil "Fin de " 100 9 0 0 0 T nil nil T)
;*** NE PAS AJOUTER de texte au-dessous du commentaire! ***|;
