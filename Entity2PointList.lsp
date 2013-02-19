;;----------------=={ Entity to Point List }==----------------;;
;;                                                            ;;
;;  Returns a list of points describing or approximating the  ;;
;;  supplied entity, else nil if the entity is not supported. ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2011 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Arguments:                                                ;;
;;  ent - Entity for which to return Point List.              ;;
;;------------------------------------------------------------;;
;;  Returns:  List of Points describing/approximating entity  ;;
;;------------------------------------------------------------;;

(defun LM:Entity->PointList
       (ent / der di1 di2 di3 elst fun inc lst par rad)
  (setq	elst (entget ent)
	di1  (vlax-curve-getdistatparam
	       ent
	       (vlax-curve-getstartparam ent)
	     )
	di2  (vlax-curve-getdistatparam ent (vlax-curve-getendparam ent))
	inc  (/ di2 25.0)
  )
  (while (< di1 di2)
    (setq lst (cons (vlax-curve-getpointatdist ent di1) lst)
	  der (/ (distance '(0.0 0.0)
			   (vlax-curve-getsecondderiv
			     ent
			     (vlax-curve-getparamatdist ent di1)
			   )
		 )
		 inc
	      )
	  tst (princ der)
	  di1 (+ di1 (/ 1.0 der (* 10. inc)))
    )
  )
  (if (vlax-curve-isclosed ent)
    lst
    (cons (vlax-curve-getendpoint ent) lst)
  )
)




;; Test Function

(defun c:test (/ ent)
  (if (setq ent (car (entsel)))
    (foreach x (LM:Entity->PointList ent)
      (entmake (list '(0 . "POINT") (cons 10 x)))
    )
  )
  (princ)
)
(vl-load-com)
(princ)