;; Advanced LWPolyline Outline  -  Lee Mac
;; Example calling functions (these may be customised to suit your requirements):

;; Single selection

(defun c:polyout ( / *error* cmd enl ent lst ped )

    (defun *error* ( msg )
        (if (= 'int (type cmd))
            (setvar 'cmdecho cmd)
        )
        (if (= 'int (type ped))
            (setvar 'peditaccept ped)
        )
        (LM:endundo (LM:acdoc))
        (if (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*"))
            (princ (strcat "\nError: " msg))
        )
        (princ)
    )
    
    (if
        (setq ent
            (LM:ssget "\nSelect LWPolyline: "
               '(
                    "_+.:E:S:L"
                    (
                        (0 . "LWPOLYLINE")
                        (-4 . "<OR")
                            (-4 . "<>") (40 . 0.0)
                            (-4 . "<>") (41 . 0.0)
                        (-4 . "OR>")
                    )
                )
            )
        )
        (progn
            (LM:startundo (LM:acdoc))
            (setq cmd (getvar 'cmdecho)
                  ped (getvar 'peditaccept)
                  enl (entlast)
            )
            (setvar 'cmdecho 0)
            (setvar 'peditaccept 1)
            (LM:AdvPolyOutline (setq ent (ssname ent 0)))
            (if (not (equal enl (entlast)))
                (progn
                    (while (setq enl (entnext enl))
                        (if (wcmatch (cdr (assoc 0 (entget enl))) "LINE,ARC")
                            (setq lst (cons enl lst))
                        )
                    )
                    (if lst
                        (progn
                            (command "_.pedit" "_M")
                            (foreach x lst (command x))
                            (command "" "_J" "" "")
                        )
                    )
                    (initget "Yes No")
                    (if (/= "No" (getkword "\nDelete Original? [Yes/No] <Yes>: ")) (entdel ent))
                )
                (princ "\nUnable to outline selected LWPolyline.")
            )
            (setvar 'cmdecho cmd)
            (setvar 'peditaccept ped)
            (LM:endundo (LM:acdoc))
        )
    )
    (princ)
)

;; Multiple selection

(defun c:mpolyout ( / *error* cmd del enl ent err inc lst ped sel )

    (defun *error* ( msg )
        (if (= 'int (type cmd))
            (setvar 'cmdecho cmd)
        )
        (if (= 'int (type ped))
            (setvar 'peditaccept ped)
        )
        (LM:endundo (LM:acdoc))
        (if (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*"))
            (princ (strcat "\nError: " msg))
        )
        (princ)
    )
    
    (if
        (setq sel
            (LM:ssget "\nSelect LWPolylines: "
               '(
                    "_:L"
                    (
                        (0 . "LWPOLYLINE")
                        (-4 . "<OR")
                            (-4 . "<>") (40 . 0.0)
                            (-4 . "<>") (41 . 0.0)
                        (-4 . "OR>")
                    )
                )
            )
        )
        (progn
            (initget "Yes No")
            (setq del (/= "No" (getkword "\nDelete Original? [Yes/No] <Yes>: ")))
            (LM:startundo (LM:acdoc))
            (setq cmd (getvar 'cmdecho)
                  ped (getvar 'peditaccept)
                  err 0
            )
            (setvar 'cmdecho 0)
            (setvar 'peditaccept 1)
            (repeat (setq inc (sslength sel))
                (setq enl (entlast)
                      lst nil
                )
                (LM:AdvPolyOutline (setq ent (ssname sel (setq inc (1- inc)))))
                (if (not (equal enl (entlast)))
                    (progn
                        (while (setq enl (entnext enl))
                            (if (wcmatch (cdr (assoc 0 (entget enl))) "LINE,ARC")
                                (setq lst (cons enl lst))
                            )
                        )
                        (if lst
                            (progn
                                (command "_.pedit" "_M")
                                (foreach x lst (command x))
                                (command "" "_J" "" "")
                            )
                        )
                        (if del (entdel ent))
                    )
                    (setq err (1+ err))
                )
            )
            (if (< 0 err)
                (princ (strcat "\nUnable to outline " (itoa err) " selected LWPolyline(s)."))
            )
            (setvar 'cmdecho cmd)
            (setvar 'peditaccept ped)
            (LM:endundo (LM:acdoc))
        )
    )
    (princ)
)

;;-------------=={ Advanced LWPolyline Outline }==------------;;
;;                                                            ;;
;;  Constructs the outline silhouette of an LWPolyline entity ;;
;;  with straight and arc segments of varying width. The      ;;
;;  resulting silohuette is composed of Line, Circular Arc &  ;;
;;  Elliptical Arc entities.                                  ;;
;;                                                            ;;
;;  The function is currently limited for use with            ;;
;;  non-self-intersecting LWPolylines.                        ;;
;;                                                            ;;
;;  The function will produce best results for arc segments   ;;
;;  whose vertex locations have not been independently        ;;
;;  modified following creation of the LWPolyline; i.e. arc   ;;
;;  segments which remain tangent to surrounding segments.    ;;
;;                                                            ;;
;;  The function will approximate LWPolyline arc segments     ;;
;;  with varying start & end width with an Elliptical Arc     ;;
;;  entity; however, arc segments spanning more than pi       ;;
;;  radians, or with a high width differential will not       ;;
;;  necessarily define an outlining Elliptical Arc.           ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2013 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Arguments:                                                ;;
;;  ent - Entity name of LWPolyline                           ;;
;;------------------------------------------------------------;;
;;  Returns:  -None-                                          ;;
;;------------------------------------------------------------;;

(defun LM:AdvPolyOutline ( ent / _endpoints cls enx ep1 ep2 lst ocs )

    (defun _endpoints ( en bu / ax bx cn mt p1 pl ra rd zv )
        (setq en (entget en)
              zv (cdr (assoc 210 en))
        )
        (setq pl
            (cond
                (   (= "LINE" (cdr (assoc 0 en)))
                    (list
                        (cdr (assoc 10 en))
                        (cdr (assoc 11 en))
                    )
                )
                (   (= "ARC" (cdr (assoc 00 en)))
                    (setq cn (cdr (assoc 10 en))
                          rd (cdr (assoc 40 en)) 
                    )
                    (mapcar '(lambda ( a ) (trans (polar cn (cdr (assoc a en)) rd) zv 0)) '(50 51))
                )
                (   t
                    (setq cn (trans (cdr (assoc 10 en)) 0 zv)
                          p1 (trans (cdr (assoc 11 en)) 0 zv)
                          ax (distance '(0.0 0.0) p1)
                          bx (* ax (cdr (assoc 40 en)))
                          ra (angle '(0.0 0.0) p1)
                          mt (list (list (cos ra) (- (sin ra))) (list (sin ra) (cos ra)))
                    )
                    (mapcar '(lambda ( u ) (trans (mapcar '+ cn (mxv mt (list (* ax (cos u)) (* bx (sin u))))) zv 0))
                        (list
                            (cdr (assoc 41 en))
                            (cdr (assoc 42 en))
                        )
                    )                 
                )
            )
        )
        (if (minusp bu) (reverse pl) pl)
    )

    (setq enx (entget ent)
          ocs (cdr (assoc 210 enx))
          cls (= 1 (logand 1 (cdr (assoc 70 enx))))
          lst (LM:LWVertices enx)
    )
    (foreach grp
        (setq lst
            (apply 'mapcar
                (cons 'list
                    (mapcar
                        (function
                            (lambda ( a b / bu ew p1 p2 sw )
                                (setq p1 (cdr (assoc 10 a))
                                      p2 (cdr (assoc 10 b))
                                      sw (/ (cdr (assoc 40 a)) 2.0)
                                      ew (/ (cdr (assoc 41 a)) 2.0)
                                      bu (cdr (assoc 42 a))
                                )
                                (mapcar
                                    (function
                                        (lambda ( fn / a1 cn el i1 i2 nm pl rd v1 w1 )
                                            (cons
                                                (cond
                                                    (   (equal 0.0 bu 1e-8)
                                                        (setq v1 (mapcar '- p1 p2)
                                                              v1 (list (- (cadr v1)) (car v1))
                                                              nm (distance '(0.0 0.0) v1)
                                                        )
                                                        (if (equal 0.0 nm 1e-14)
                                                            (setq v1 '(0.0 0.0))
                                                            (setq v1  (mapcar '/ v1 (list nm nm)))
                                                        )
                                                        (entmakex
                                                            (list
                                                               '(0 . "LINE")
                                                                (cons 10 (trans (mapcar 'fn p1 (vxs v1 sw)) ocs 0))
                                                                (cons 11 (trans (mapcar 'fn p2 (vxs v1 ew)) ocs 0))
                                                            )
                                                        )
                                                    )
                                                    (   (equal sw ew 1e-8)
                                                        (setq rd (LM:BulgeRadius p1 p2 bu)
                                                              cn (LM:BulgeCentre p1 p2 bu)
                                                        )
                                                        (entmakex
                                                            (list
                                                               '(0 . "ARC")
                                                                (cons 010 cn)
                                                                (cons 040 (fn rd (if (minusp bu) (- sw) sw)))
                                                                (cons 050 (if (minusp bu) (angle cn p2) (angle cn p1)))
                                                                (cons 051 (if (minusp bu) (angle cn p1) (angle cn p2)))
                                                                (cons 210 ocs)
                                                            )
                                                        )
                                                    )
                                                    (   t
                                                        (setq rd (LM:BulgeRadius p1 p2 bu)
                                                              cn (LM:BulgeCentre p1 p2 bu)
                                                              i1 (atan bu)
                                                              i2 (/ (- ew sw) (if (minusp bu) -4.0 4.0))
                                                              a1 (angle cn p1)
                                                              w1 (fn rd (if (minusp bu) (- sw) sw))
                                                        )
                                                        (repeat 5
                                                            (setq pl (cons (polar cn a1 w1) pl)
                                                                  a1 (+  a1 i1)
                                                                  w1 (fn w1 i2)
                                                            )
                                                        )
                                                        (if (apply 'LM:Clockwise-p (cddr pl))
                                                            (setq pl (reverse pl))
                                                        )
                                                        (if (setq el (apply 'LM:5P-Ellipse pl))
                                                            (entmakex
                                                                (append
                                                                   '(
                                                                        (000 . "ELLIPSE")
                                                                        (100 . "AcDbEntity")
                                                                        (100 . "AcDbEllipse")
                                                                    )
                                                                    el
                                                                    (list
                                                                        (cons 041 (LM:point->param el (car  pl)))
                                                                        (cons 042 (LM:point->param el (last pl)))
                                                                        (cons 210 ocs)
                                                                    )
                                                                )
                                                            )
                                                        )
                                                    )
                                                )
                                                (list p1 p2 sw ew bu)
                                            )
                                        )
                                    )
                                    (list + -)
                                )
                            )
                        )
                        lst
                        (if cls
                            (append (cdr lst) (list (car lst)))
                            (cdr lst)
                        )
                    )
                )
            )
        )
        (mapcar
            (function
                (lambda ( a b / e1 e2 ip p1 p2 q1 q2 )
                    (cond
                        (   (or (null (car a)) (null (car b))))
                        (   (and
                                (equal 0.0 (last a) 1e-8)
                                (equal 0.0 (last b) 1e-8)
                            )
                            (setq e1 (entget (car a))
                                  e2 (entget (car b))
                                  p1 (cdr (assoc 10 e1))
                                  p2 (cdr (assoc 11 e1))
                                  q1 (cdr (assoc 10 e2))
                                  q2 (cdr (assoc 11 e2))
                            )
                            (cond
                                (   (equal
                                        (vx1 (mapcar '- (caddr a) (cadr a)))
                                        (vx1 (mapcar '- (caddr b) (cadr b)))
                                        1e-3
                                    )
                                    (if (not (equal p2 q1 1e-8))
                                        (entmake (list '(0 . "LINE") (cons 10 p2) (cons 11 q1)))
                                    )
                                )
                                (   (setq ip
                                        (inters
                                            (trans p1 0 1)
                                            (trans p2 0 1)
                                            (trans q1 0 1)
                                            (trans q2 0 1)
                                            nil
                                        )
                                    )
                                    (setq ip (trans ip 1 0))
                                    (entmod (subst (cons 11 ip) (assoc 11 e1) e1))
                                    (entmod (subst (cons 10 ip) (assoc 10 e2) e2))
                                )
                            )
                        )
                        (   (not (equal (car (cddddr a)) (cadddr b) 1e-8))
                            (entmake
                                (list
                                   '(0 . "LINE")
                                    (cons 10 (cadr (_endpoints (car a) (last a))))
                                    (cons 11 (car  (_endpoints (car b) (last b))))
                                )
                            )
                        )
                    )
                )
            )
            grp
            (if cls
                (append (cdr grp) (list (car grp)))
                (cdr grp)
            )
        )
    )
    (if (not cls)
        (foreach f (list car cadr)
            (if
                (and (caaar lst) (caaadr lst)
                    (not
                        (equal
                            (setq ep1 (f (_endpoints (caaar  lst) (last (caar  lst)))))
                            (setq ep2 (f (_endpoints (caaadr lst) (last (caadr lst)))))
                            1e-8
                        )
                    )
                )
                (entmake (list '(0 . "LINE") (cons 10 ep1) (cons 11 ep2)))
            )
            (setq lst (mapcar 'reverse lst))
        )
    )
    (princ)
)

;; 5-Point Ellipse  -  Lee Mac
;; Args: p1,p2,p3,p4,p5 - UCS points defining Ellipse
;; Returns a list of:
;; ((10 <WCS Center>) (11 <WCS Major Axis Endpoint from Center>) (40 . <Minor/Major Ratio>))

(defun LM:5P-Ellipse ( p1 p2 p3 p4 p5 / a av b c cf cx cy d e f i m1 m2 rl v x )
    (setq m1
        (trp
            (mapcar
                (function
                    (lambda ( p )
                        (list
                            (* (car  p) (car  p))
                            (* (car  p) (cadr p))
                            (* (cadr p) (cadr p))
                            (car  p)
                            (cadr p)
                            1.0
                        )
                    )
                )
                (list p1 p2 p3 p4 p5)
            )
        )
    )
    (setq i -1.0)
    (repeat 6
        (setq cf (cons (* (setq i (- i)) (detm (trp (append (reverse m2) (cdr m1))))) cf)
              m2 (cons (car m1) m2)
              m1 (cdr m1)
        )
    )
    (mapcar 'set '(f e d c b a) cf) ;; Coefficients of Conic equation ax^2 + bxy + cy^2 + dx + ey + f = 0
    (if (< 0 (setq x (- (* 4.0 a c) (* b b))))
        (progn
            (if (equal 0.0 b 1e-8) ;; Ellipse parallel to coordinate axes
                (setq av '((1.0 0.0) (0.0 1.0))) ;; Axis vectors
                (setq av
                    (mapcar
                        (function
                            (lambda ( v / d )
                                (setq v (list (/ b 2.0) (- v a)) ;; Eigenvectors
                                      d (distance '(0.0 0.0) v)
                                )
                                (mapcar '/ v (list d d))
                            )
                        )
                        (quad 1.0 (- (+ a c)) (- (* a c) (* 0.25 b b))) ;; Eigenvalues
                    )
                )
            )
            (setq cx (/ (- (* b e) (* 2.0 c d)) x) ;; Ellipse Center
                  cy (/ (- (* b d) (* 2.0 a e)) x)
            )
            ;; For radii, solve intersection of axis vectors with Conic Equation:
            ;; ax^2 + bxy + cy^2 + dx + ey + f = 0  }
            ;; x = cx + vx(t)                       }- solve for t
            ;; y = cy + vy(t)                       }
            (setq rl
                (mapcar
                    (function
                        (lambda ( v / vv vx vy )
                            (setq vv (mapcar '* v v)
                                  vx (car  v)
                                  vy (cadr v)
                            )
                            (apply 'max
                                (quad
                                    (+ (* a (car vv)) (* b vx vy) (* c (cadr vv)))
                                    (+ (* 2.0 a cx vx) (* b (+ (* cx vy) (* cy vx))) (* c 2.0 cy vy) (* d vx) (* e vy))
                                    (+ (* a cx cx) (* b cx cy) (* c cy cy) (* d cx) (* e cy) f)
                                )
                            )
                        )
                    )
                    av
                )
            )
            (if (apply '> rl)
                (setq rl (reverse rl)
                      av (reverse av)
                )
            )
            (list
                (cons 10 (trans (list cx cy) 1 0)) ;; WCS Ellipse Center
                (cons 11 (trans (mapcar '(lambda ( v ) (* v (cadr rl))) (cadr av)) 1 0)) ;; WCS Major Axis Endpoint from Center
                (cons 40 (apply '/ rl)) ;; minor/major ratio
            )
        )
    )
)

;; Matrix Determinant  -  ElpanovEvgeniy
;; Args: m - nxn matrix

(defun detm ( m )
    (cond
        (   (null m) 1)
        (   (zerop (caar m)) 0)
        (   (*  (caar m)
                (detm
                    (mapcar
                        (function
                            (lambda ( a / d ) (setq d (/ (car a) (float (caar m))))
                                (mapcar
                                    (function
                                        (lambda ( b c ) (- b (* c d)))
                                    )
                                    (cdr a) (cdar m)
                                )
                            )
                        )
                        (cdr m)
                    )
                )
            )
        )
    )
)

;; Matrix Transpose  -  Doug Wilson
;; Args: m - nxn matrix

(defun trp ( m )
    (apply 'mapcar (cons 'list m))
)

;; Quadratic Solution  -  Lee Mac
;; Args: a,b,c - coefficients of ax^2 + bx + c = 0

(defun quad ( a b c / d r )
    (if (<= 0 (setq d (- (* b b) (* 4.0 a c))))
        (progn
            (setq r (sqrt d))
            (list (/ (+ (- b) r) (* 2.0 a)) (/ (- (- b) r) (* 2.0 a)))
        )
    )
)

;; Unit Vector  -  Lee Mac
;; Args: v - vector in R^2 or R^3

(defun vx1 ( v )
    (   (lambda ( n ) (if (equal 0.0 n 1e-10) nil (mapcar '/ v (list n n n))))
        (distance '(0.0 0.0 0.0) v)
    )
)

;; Vector x Scalar  -  Lee Mac
;; Args: v - vector in R^n, s - real scalar

(defun vxs ( v s )
    (mapcar (function (lambda ( n ) (* n s))) v)
)

;; Matrix x Vector  -  Vladimir Nesterovsky
;; Args: m - nxn matrix, v - vector in R^n

(defun mxv ( m v )
    (mapcar '(lambda ( r ) (apply '+ (mapcar '* r v))) m)
)

;; ArcCosine  -  Lee Mac
;; Args: -1 <= x <= 1

(defun acos ( x )
    (if (<= -1.0 x 1.0)
        (atan (sqrt (- 1.0 (* x x))) x)
    )
)

;; Clockwise-p - Lee Mac
;; Returns T if p1,p2,p3 are clockwise oriented

(defun LM:Clockwise-p ( p1 p2 p3 )
    (<  (* (- (car  p2) (car  p1)) (- (cadr p3) (cadr p1)))
        (* (- (cadr p2) (cadr p1)) (- (car  p3) (car  p1)))
    )
)

;; Point -> Ellipse Parameter  -  Lee Mac
;; Returns the elliptical arc parameter value for the given point
;; dxf  -  Ellipse DXF data (DXF groups 10, 11 & 40)
;; pt   -  Point on Ellipse

(defun LM:point->param ( dxf pt / a b m r u x y )
    (setq m (trans (cdr (assoc 11 dxf)) 0 1)
          a (distance '(0.0 0.0) m)
          b (* (cdr (assoc 40 dxf)) a)
          r (angle '(0.0 0.0) m)
    )
    (mapcar 'set '(x y)
        (mxv (list (list (cos r) (sin r)) (list (- (sin r)) (cos r)))
             (mapcar '- pt (trans (cdr (assoc 10 dxf)) 0 1))
        )
    )
    (if (< y 0)
        (setq u (- (+ pi pi) (acos (/ x a))))
        (setq u (acos (/ x a)))
    )
    (rem (+ pi pi u) (+ pi pi))
)

;; LW Vertices  -  Lee Mac
;; Returns a list of lists in which each sublist describes
;; the position, starting width, ending width and bulge of the
;; vertex of a supplied LWPolyline

(defun LM:LWVertices ( e )
    (if (setq e (member (assoc 10 e) e))
        (cons
            (list
                (assoc 10 e)
                (assoc 40 e)
                (assoc 41 e)
                (assoc 42 e)
            )
            (LM:LWVertices (cdr e))
        )
    )
)
    
;; Bulge Radius  -  Lee Mac
;; p1 - start vertex
;; p2 - end vertex
;; b  - bulge
;; Returns the radius of the arc described by the given bulge and vertices
 
(defun LM:BulgeRadius ( p1 p2 b )
    (/ (* (distance p1 p2) (1+ (* b b))) 4 (abs b))
)
 
;; Bulge Centre  -  Lee Mac
;; p1 - start vertex
;; p2 - end vertex
;; b  - bulge
;; Returns the centre of the arc described by the given bulge and vertices
 
(defun LM:BulgeCentre ( p1 p2 b )
    (polar p1
        (+ (angle p1 p2) (- (/ pi 2) (* 2 (atan b))))
        (/ (* (distance p1 p2) (1+ (* b b))) 4 b)
    )
)

;; ssget  -  Lee Mac
;; A wrapper for the ssget function to permit the use of a custom selection prompt
;;
;; Arguments:
;; msg    - selection prompt
;; params - list of ssget arguments

(defun LM:ssget ( msg params / sel )
    (princ msg)
    (setvar 'nomutt 1)
    (setq sel (vl-catch-all-apply 'ssget params))
    (setvar 'nomutt 0)
    (if (not (vl-catch-all-error-p sel)) sel)
)

;; Start Undo  -  Lee Mac
;; Opens an Undo Group.

(defun LM:startundo ( doc )
    (LM:endundo doc)
    (vla-startundomark doc)
)

;; End Undo  -  Lee Mac
;; Closes an Undo Group.

(defun LM:endundo ( doc )
    (while (= 8 (logand 8 (getvar 'undoctl)))
        (vla-endundomark doc)
    )
)

;; Active Document  -  Lee Mac
;; Returns the VLA Active Document Object

(defun LM:acdoc nil
    (eval (list 'defun 'LM:acdoc 'nil (vla-get-activedocument (vlax-get-acad-object))))
    (LM:acdoc)
)

;;------------------------------------------------------------;;

(vl-load-com)
(princ
    (strcat
        "\n:: AdvancedPolyOutline.lsp | Version 1.0 | \\U+00A9 Lee Mac "
        (menucmd "m=$(edtime,$(getvar,DATE),YYYY)")
        " www.lee-mac.com ::"
        "\n:: Available Commands: "
        "\n::     \"polyout\"  -  Single selection."
        "\n::    \"mpolyout\"  -  Multiple selection."
    )
)
(princ)

;;------------------------------------------------------------;;
;;                        End of File                         ;;
;;------------------------------------------------------------;;