;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Selection -> liste
;;; Transforme un jeu de selection en liste de nom d'entité
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun SelEnList(sset / i bcl ss nb lst)
  (setq	i   -1
	lst '())
  (defun bcl  (ss nb)
    (if	(setq ent (ssname ss (setq nb (1+ nb))))
      (setq lst (cons ent (bcl ss nb)))))
  (if sset
    (bcl sset i)
    'nil))

(defun LstEnSel(ss)
  (if ss
    (vl-remove-if 'listp (mapcar 'cadr (ssnamex ss)))))

(defun ssmap(fct ss / n lst)
  (if (= 'PICKSET (type ss))
    (repeat (setq n (sslength ss)) (setq lst (cons (apply fct (list (ssname ss (setq n (1- n))))) lst)))))


;;;================================================================================================;;;
(princ)
