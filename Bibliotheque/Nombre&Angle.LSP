;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Ang<2pi Retourne l'angle, à 2*k*pi près, compris entre 0 et 2*pi
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ang<2pi (ang / ang2pi)
  (defun ang2pi	(ang)
    (if	(and (<= 0 ang) (< ang (* 2 pi)))
      ang
      (ang2pi (rem (+ ang (* 2 pi)) (* 2 pi)))
    )
  )
  (cond
    ((equal ang 0 1e-9)
     0.0
    )
    ((minusp ang)
     (- (* 2 pi) (ang2pi (abs ang)))
    )
    (t (ang2pi ang))
  )
)

;; Tangent  -  Lee Mac
;; Args: x - real

(defun tan (x)
  (if (not (equal 0.0 (cos x) 1e-10))
    (/ (sin x) (cos x))
  )
)

;; ArcSine  -  Lee Mac
;; Args: -1 <= x <= 1

(defun asin (x)
  (cond	((equal (abs x) 1.0 1e-10) (* x pi 0.5))
	((< -1.0 x 1.0) (atan x (sqrt (- 1.0 (* x x)))))
  )
)

;; ArcCosine  -  Lee Mac
;; Args: -1 <= x <= 1

(defun acos (x)
  (cond	((equal x 1.0 1e-10) 0.0)
	((equal x -1.0 1e-10) pi)
	((< -1.0 x 1.0) (atan (sqrt (- 1.0 (* x x))) x))
  )
)

;; fonction carré
(defun carre (var /) (* var var))

;; fonction cube
(defun cube (var /) (* var var var))

;; fonction inverse ou multiplie par -1
(defun inv (var /) (* -1.0 var))

;; Logarithme décimal
(defun log10 (var) (/ (log var) (log 10)))

;; fonction racine cubique
;; le calcul de la racine cubique d'un nombre négatif n'est pas possible dans autolisp
;; donc :
;; test si VAR est négatif --> calcul de la racine cubique avec la valeur absolue puis
;; multiplication du résultat par -1
(defun curt (var /)
  (if (< var 0.0)
    (* -1.0 (expt (abs var) (/ 1.0 3.0)))
    (expt (abs var) (/ 1.0 3.0))
  )
)

;; supprime les 0 en trop et renvoi une chaine
(defun sp0tostr	(var / chn-lst point)
  (setq chn-lst (vl-string->list (rtos var 2)))
  (if (member 46 chn-lst)
    (vl-list->string (remove-right 48 chn-lst))
    (vl-list->string chn-lst)
  )
)
