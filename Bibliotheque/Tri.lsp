;;;* SORT.LSP
;;;+------------------------------------------------------------------------+
;;;| URBAN X Utilities for AutoCAD                                   Ver 2.9|
;;;| (c) 1991-96 Reinhard URBAN. All rights reserved                        |
;;;| X-Ray, Rechbauerstr. 38, 8010 Graz, Austria, Tel: ++43-316-812646      |
;;;| <rurban@sbox.tu-graz.ac.at>  http://xarch.tu-graz.ac.at/autocad/urban/ |
;;;+------------------------------------------------------------------------+
;;;
;;; Optimized and Benched General Sort Routines:
;;;
;;; MERGE SORT O(n logn): fastest but stack-costly
;;;   merge-sort
;;;   str-sort         from TABLES.LSP (adesk sample)
;;;
;;; QUICK SORT O(n logn): almost as fast as merge-sort
;;;   qsort 	       from lisp2c demo
;;;
;;; BUBBLE SORT O(n^2):
;;;   bub-sort         (cbsort, sort1   from CAD-User 4/92)
;;;
;;; INSERTION SORT O(n^2):
;;;   ins-sort          by Peter Szammer <szammer@sime.com>
;;;
;;; default sort predicate is '<
;;;
;;; no shell or heap sorts so far
;;;
;;; YOU MAY USE THIS FUNCTION AS IT IS FOR ANY PURPOSE
;;; AT YOUR OWN RISK IF YOU RETAIN THIS NOTICE COMPLETE
;;; AND UNALTERED. NO WARRANTIES GIVEN WHATSOEVER.
;;;
;;; sorting 100 elements:
;;;   insertion sort: 20.430023 sec/ 50.06%
;;;   bubble sort   : 15.722076 sec/ 38.53%
;;;   merge sort    :  2.252014 sec/  5.52%
;;;   quick sort    :  2.292969 sec/  5.62%
;;;   vlx-sort      :  0.110016 sec/  0.27% (Vital Lisp internal)

;;;************************************************************************
;;; Merge Sort  O(n logn)
;;;   overall the fastest, but needs a lot of stack
(defun merge-sort (x cmp)
  (cond ((null (cdr x)) x)
        (T (merge cmp
	           (merge-sort (first-half x) cmp)
                   (merge-sort (last-half x) cmp)))))

(defun merge (cmp a b)
  (cond	((null a) b)
	((null b) a)
	((apply cmp (list (car a) (car b)))
	 (cons (car a) (merge cmp (cdr a) b))
	)
	(t
	 (cons (car b) (merge cmp a (cdr b)))
	)
  )
)

;;;************************************************************************
;;; Quick Sort  O(n logn)
;;;   slightly slower than quick sort
;;;   derived from the lisp2c demo version

(defun qsort (lst cmp / x y l e g)
  (if lst
    (progn
      (setq x (nth (/ (length lst) 2) lst))
      (foreach y lst
	(cond
	  ((equal y x)
	    (setq e (cons y e)))
	  ((apply cmp (list y x))
	    (setq l (cons y l)))
	  (T (setq g (cons y g)))
	)
      )
      (append (qsort l cmp) e (qsort g cmp))
    )
  )
)


;;; from  ftp://ftp.netcom.com/pub/hb/hbaker/Share-Unify.html
;|
(defun qs(x)
  (if (null x) nil
    (append (qs1 (low (cdr x) (car x)))
            (cons (car x)
                  (qs2 (high (cdr x) (car x)))))))

(defun high(x i)
  (cond ((null x) nil)
        ((< (car x) i) (high (cdr x) i))
        (t (cons (car x) (high (cdr x) i)))))

(defun low(x i)
  (cond ((null x) nil)
        ((>= (car x) i) (low (cdr x) i))
        (t (cons (car x) (low (cdr x) i)))))
|;

(defun qsort-1 (x cmp)
  (if (null x) nil
    (append (qsort-1 (low (cdr x) (car x) cmp) cmp)
            (cons (car x)
                  (qsort-1 (high (cdr x) (car x) cmp) cmp)))))

(defun high (x i cmp)
  (cond ((null x) nil)
        ((apply cmp (list (car x) i)) (high (cdr x) i cmp))
        (t (cons (car x) (high (cdr x) i cmp)))))

(defun low (x i cmp)
  (cond ((null x) nil)
        ((not (apply cmp (list (car x) i))) (low (cdr x) i cmp))
        (t (cons (car x) (low (cdr x) i cmp)))))



;;;************************************************************************
;;; Insertion Sort  O(n^2)
;;;   fast for already sorted (reversed) lists
(defun ins-sort (L cmp / M N O)
  (setq O L L (list (car O)))
  (while (setq M nil N L O (cdr O))
    (while (and O N (apply cmp (list (car N) (car O))))
      (setq M (append M (list (car N))) N (cdr N))
    )
    (setq N (cons (car O) N) L (append M N))
  )
  L
)

;;;************************************************************************
;;; Bubble Sort O(n^2), iterative
;;;  slowest sort method
(defun bub-sort (lst cmp / new n x)
  (repeat (length lst)
    (setq n (car lst))
    (foreach x lst
      (if (apply cmp (list n x)) (setq n x))	;swap it
    )
    (setq lst (urx-remove n lst)	; this removes an item from a list
          new (cons n new)
    )
  )
  new
)

;;; Bubble Sort O(n^2), recursive
;;;  !! no double elements in lst allowed !!
;;;  slowest sort method
(defun bub-sort-r (lst cmp / x y)
  (setq x (car lst))
  (foreach y (cdr lst)
    (if (apply cmp (list x y)) (setq x y)))
  (if lst
    (append
      (bub-sort-r
        (append
          (cdr (member x lst))
          (cdr (member x (reverse lst)))
        )
        cmp
      )
      (list x)
    )
  )
)

;;;************************************************************************
;;; Helper functions

(defun first-half (l)
  (ur_head l (/ (1- (length l)) 2)))

(defun ur_head (l n)
  (cond ((minusp n) nil)
    (t (cons (car l) (ur_head (cdr l) (1- n))))))

(defun last-half (l)
  (ur_tail l (/ (1- (length l)) 2)))

(defun ur_tail (l n)
  (cond ((minusp n) l)
	(t (ur_tail (cdr l) (1- n)))))

;;;************************************************************************
;;; some examples of compare functions

;;;  STRCMP compares two strings like the clib func
(defun strcmp (a b)
  (cond ((= a b) 0)
        (T (cond ((< (ascii a) (ascii b)) -1)
                 ((> (ascii a) (ascii b))  1)
                 (t (strcmp (substr a 2) (substr b 2)))))))

(defun cmpstr (x1 x2) (> (strcmp x1 x2) 0))
(defun cmpnum (x1 x2) (> x1 x2))
(defun cmppts (x1 x2) (> (car x1) (car x2)))


;;;================================================================================================;;;
(princ)
