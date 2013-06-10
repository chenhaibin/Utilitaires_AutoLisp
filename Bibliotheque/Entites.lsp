(defun make_lwpline (dxf v /)
  (entmake
    (append
      (list
	'(0 . "LWPOLYLINE")
	'(100 . "AcDbEntity")
	'(67 . 0)
	'(410 . "Model")
	'(8 . "0")
	(cons '60 v)
	'(100 . "AcDbPolyline")
      )
      (list (cons '90 (length dxf)))
      (list (cons '43 0.0))
      (apply
	'append
	(mapcar
	  (function
	    (lambda (x) (list (cons '10 (v2d x)) (cons '42 0.0)))
	  )
	  dxf
	)
      )
    )
  )
  (entlast)
)

(defun make_line (dxf v /)
  (entmake
    (append
      '((0 . "LINE") (67 . 0) (410 . "Model") (8 . "0"))
      (list (cons '60 v))
      (mapcar (function (lambda (x y) (cons x (v2d y))))
	      '(10 11)
	      dxf
      )
    )
  )
  (entlast)
)

(defun make_arc	(dxf v /)
  (entmake
    (append
      '((0 . "ARC") (67 . 0) (410 . "Model") (8 . "0"))
      (list (cons '60 v))
      (mapcar (function	(lambda	(x y)
			  (cons	x
				(if (listp y)
				  (v2d y)
				  y
				)
			  )
			)
	      )
	      '(10 50 51 40)
	      dxf
      )
    )
  )
  (entlast)
)

(defun make_ray	(dxf v /)
  (entmake
    (append
      (list
	'(0 . "RAY")
	'(100 . "AcDbEntity")
	'(67 . 0)
	'(410 . "Model")
	'(8 . "0")
	(cons '60 v)
	'(100 . "AcDbRay")
      )
      (mapcar (function (lambda (x y) (cons x (v2d y))))
	      '(10 11)
	      dxf
      )
    )
  )
  (entlast)
)


(defun make_point (dxf v /)
  (entmake (append
	     '((0 . "POINT") (410 . "Model") (8 . "0"))
	     (list (cons '60 v))
	     (list (cons '10 (v2d dxf)))
	   )
  )
  (entlast)
)