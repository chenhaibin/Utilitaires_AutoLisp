;; Polyline Outline
;; Example Calling Functions (these may be customised to suit your requirements):

;; Single Selection

(defun c:PolyOutline ( / e s )
    (if (setq e (ssget "_+.:E:S:L" '((0 . "LWPOLYLINE") (-4 . "<NOT") (-4 . "<>") (42 . 0.0) (-4 . "NOT>"))))
        (progn
            (LM:PolyOutline (setq e (ssname e 0)))
            (initget "Yes No")
            (if (/= "No" (getkword "\nDelete Original? [Yes/No] <Yes>: ")) (entdel e))
        )
    )
    (princ)
)

;; Multiple Selection

(defun c:mPolyOutline ( / d i s )
    (if (setq s (ssget "_:L" '((0 . "LWPOLYLINE") (-4 . "<NOT") (-4 . "<>") (42 . 0.0) (-4 . "NOT>"))))
        (progn
            (initget "Yes No")
            (setq d (/= "No" (getkword "\nDelete Original? [Yes/No] <Yes>: ")))
            (repeat (setq i (sslength s))
                (LM:PolyOutline (setq e (ssname s (setq i (1- i)))))
                (if d (entdel e))
            )
        )
    )
    (princ)
)

;;------------------=={ LWPolyline Outline }==----------------;;
;;                                                            ;;
;;  Creates an LWPolyline surrounding the boundary of an      ;;
;;  LWPolyline with varying widths. Currently restricted to   ;;
;;  LWPolyline vertices with zero bulge.                      ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2011 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Arguments:                                                ;;
;;  ent - Entity name of LWPolyline                           ;;
;;------------------------------------------------------------;;
;;  Returns:  List of Entity name(s) of outline LWPolyline(s) ;;
;;------------------------------------------------------------;;

(defun LM:PolyOutline ( ent / _vertices lst )

    (defun _vertices ( e )
        (if (setq e (member (assoc 10 e) e))
            (cons
                (list
                    (cdr (assoc 10 e))
                    (cdr (assoc 40 e))
                    (cdr (assoc 41 e))
                )
                (_vertices (cdr e))
            )
        )
    )

    (setq
        ent (entget ent)
        lst (_vertices ent)
        lst (apply 'mapcar
                (cons
                    (function
                        (lambda ( a b )
                            (
                                (lambda ( c )
                                    (mapcar
                                        (function
                                            (lambda ( d )
                                                (mapcar
                                                    (function
                                                        (lambda ( e f )
                                                            (mapcar 'd (car e)
                                                                (mapcar
                                                                    (function
                                                                        (lambda ( g ) (* g (/ f 2.0)))
                                                                    )
                                                                    c
                                                                )
                                                            )
                                                        )
                                                    )
                                                    (list a b) (cdr a)
                                                )
                                            )
                                        )
                                        (list + -)
                                    )
                                )
                                (
                                    (lambda ( v / n )
                                        (setq v (list (- (cadr v)) (car v) 0.0)
                                              n (distance '(0. 0.) v)
                                        )
                                        (if (equal 0.0 n 1e-14)
                                            (list  0.0 0.0 0.0)
                                            (mapcar '/ v (list n n n))
                                        )
                                    )
                                    (mapcar '- (car a) (car b))
                                )
                            )
                        )
                    )
                    (if (= 1 (logand 1 (cdr (assoc 70 ent))))
                        (list
                            (cons (last lst) lst)
                            (append lst (list (car lst)))
                        )
                        (list lst (cdr lst))
                    )
                )
            )
        lst (
                (lambda ( a )
                    (if (zerop (logand 1 (cdr (assoc 70 ent))))
                        (append
                            (list (mapcar 'car  (car  lst)))
                            a
                            (list (mapcar 'cadr (last lst)))
                        )
                        a
                    )
                )
                (apply 'append
                    (mapcar
                        (function
                            (lambda ( a b / c )
                                (if
                                    (setq c
                                        (apply 'append
                                            (mapcar
                                                (function
                                                    (lambda ( d e / f )
                                                        (if (setq f (inters (car d) (cadr d) (car e) (cadr e) nil))
                                                            (list f)
                                                        )
                                                    )
                                                )
                                                a b
                                            )
                                        )
                                    )
                                    (list c)
                                )
                            )
                        )
                        lst (cdr lst)
                    )
                )
            )
    )
    (mapcar
        (function
            (lambda ( a )
                (entmakex
                    (append
                        (subst (cons 43 0.0) (assoc 43 ent)
                            (subst (cons 70 (logior 1 (cdr (assoc 70 ent)))) (assoc 70 ent)
                                (subst (cons 90 (length a)) (assoc 90 ent)
                                    (reverse (member (assoc 39 ent) (reverse ent)))
                                )
                            )
                        )
                        (mapcar '(lambda ( p ) (cons 10 p)) a) (list (assoc 210 ent))
                    )
                )
            )
        )
        (
            (lambda ( a b )
                (if (zerop (logand 1 (cdr (assoc 70 ent))))
                    (list
                        (append
                            (if (equal (car a) (last b) 1e-8)
                                (setq a (cdr a))
                                a
                            )
                            (if (equal (car b) (last a) 1e-8)
                                (setq b (cdr b))
                                b
                            )
                        )
                    )
                    (list a b)
                )
            )
            (mapcar 'car lst) (reverse (mapcar 'cadr lst))
        )
    )
)

(princ)