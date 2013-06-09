(defun Segment (Pl No / Data Bulge Points Pt1 Pt2 Lst)
  (setq Data (entget Pl))
  (and (= (cdr (assoc 0 Data)) "LWPOLYLINE")
       (setq No (fix No))
       (> No 0)
       (< No (+ (length (massoc 10 Data)) (cdr (assoc 70 Data))))
       (setq Data   (if	(cdr (assoc 70 Data))
		      (appendlst Data (assoc 10 Data))
		      Data
		    )
	     Bulge  (dxf '42 Data)
	     Points (dxf '10 Data)
       )
       (setq Pt1 (nth (1- No) Points)
	     Pt2 (nth No Points)
	     Lst (if (zerop (setq B (nth (1- No) Bulge)))
		   (list Pt1 Pt2)
		   (BulgeToArc Pt1 Pt2 B)
		 )
       )
  )
  Lst
)




;;; APPENDLST - Ajoute une liste dans une liste
;;; (appendlst '(1 2 3) '(4 5)) => '(1 2 3 (4 5))
(defun appendlst (l1 l2)
  (append l1 (list l2))
)

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

;; Bulge to Arc  -  Lee Mac
;; p1 - start vertex
;; p2 - end vertex
;; b  - bulge
;; Returns: (<center> <start angle> <end angle> <radius>)

(defun BulgeToArc (p1 p2 b / c r)
  (setq	r (/ (* (distance p1 p2) (1+ (* b b))) 4 b)
	c (polar p1 (+ (angle p1 p2) (- (/ pi 2) (* 2 (atan b)))) r)
  )
  (if (minusp b)
    (list c (angle c p2) (angle c p1) (abs r))
    (list c (angle c p1) (angle c p2) (abs r))
  )
)

(defun BulgeToArc ( p1 p2 b / a c r )
    (setq a (* 2 (atan b))
          r (/ (distance p1 p2) 2 (sin a))
          c (polar p1 (+ (- (/ pi 2) a) (angle p1 p2)) r)
    )
    (if (minusp b)
        (list c (angle c p2) (angle c p1) (abs r))
        (list c (angle c p1) (angle c p2) (abs r))
    )
)