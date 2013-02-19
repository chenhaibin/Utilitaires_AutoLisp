;;-----------------------=={  Dynamic Information Tool  }==----------------------;;
;;                                                                               ;;
;;  Information about an object is displayed upon the user moving the cursor     ;;
;;  over the object. The cursor size may be altered using the +/- keys, and the  ;;
;;  program mode may be changed using the TAB key.                               ;;
;;                                                                               ;;
;;  Click or Hit Enter to Exit the Program                                       ;;
;;  +/- Increase or Decrease Cursor Pick Box respectively.                       ;;
;;                                                                               ;;
;;  Hit 'TAB' to switch modes:-                                                  ;;
;;  DINFO Mode    ~    Information about an object is displayed.                 ;;
;;  LAYISO Mode   ~    Isolate a Layer by clicking an object on that layer,      ;;
;;                     Shift-click to turn on all layers.                        ;;
;;                                                                               ;;
;;-------------------------------------------------------------------------------;;
;;  FUNCTION SYNTAX:  DINFO                                                      ;;
;;-------------------------------------------------------------------------------;;
;;  Author:  Lee Mac, Copyright © 2011 - www.lee-mac.com                         ;;
;;-------------------------------------------------------------------------------;;
;;  Version 1.0    -    09-05-2009                                               ;;
;;                                                                               ;;
;;  First Release                                                                ;;
;;-------------------------------------------------------------------------------;;
;;  Version 1.1    -    04-04-2010                                               ;;
;;                                                                               ;;
;;  Complete program upgrade.                                                    ;;
;;  Removed Object Editing functionality (unnecessary).                          ;;
;;-------------------------------------------------------------------------------;;
;;  Version 1.2    -    06-04-2010                                               ;;
;;                                                                               ;;
;;  Added additional properties to list.                                         ;;
;;  Accounted for Radian measurements.                                           ;;
;;  Added 'Belongs to' Field.                                                    ;;
;;-------------------------------------------------------------------------------;;
;;  Version 1.3    -    07-04-2010                                               ;;
;;                                                                               ;;
;;  Fixed Dynamic Block SelectionSet error.                                      ;;
;;-------------------------------------------------------------------------------;;
;;  Version 1.4    -    07-04-2010                                               ;;
;;                                                                               ;;
;;  Added Dynamic Block Flag.                                                    ;;
;;-------------------------------------------------------------------------------;;
;;  Version 1.5    -    06-01-2011                                               ;;
;;                                                                               ;;
;;  Updated code formatting.                                                     ;;
;;  Fixed bug pertaining to Polylines within Blocks/XRefs.                       ;;
;;-------------------------------------------------------------------------------;;

(defun c:SInfo


					;...............................................................................;



	       (/
		;;         --=={ Local Functions }==--          ;
		*error*	 _Display dxf	   _GetColour	     _GetLW
		_GetName _GetScale	   PurgeLayer	     _PutDXF
		ss->lst	 _Stringify	   _Text    _Update
		;;         --=={ Local Variables }==--          ;
		-pi/4	 5pi/4	  alignment	    attachment
		celst	 cent	  cobj	   code	    data     doc
		e	 express  gr	   inter    layers   mode
		modelst	 msg	  msglst   on	    owner    pi/4
		r	 rad	  spc	   ss	    telst    tent
		vs	 x
			 ;;         --=={ Global Variables }==--         ;
					; DInfo:Mode    -- Global: Function Number
					; DInfo:cRad    -- Global: Cursor Aperture
	       )

  (vl-load-com)



  ;;-------------------------------------------------------------------------------;;
  ;;                           --=={  Preliminaries  }==--                         ;;
  ;;-------------------------------------------------------------------------------;;

  (setq	spc
	 (vlax-get-property
	   (setq doc
		  (vla-get-ActiveDocument (vlax-get-acad-object))
	   )
	   (if (= 1 (getvar 'CVPORT))
	     'PaperSpace
	     'ModelSpace
	   )
	 )
  )

  (vlax-for l (vla-get-layers doc)
    (setq layers (cons l layers))
    (if	(eq :vlax-true (vla-get-Layeron l))
      (setq on (cons l on))
    )
  )

  (setq	Express
	 (and (vl-position "acetutil.arx" (arx))
	      (not
		(vl-catch-all-error-p
		  (vl-catch-all-apply
		    (function (lambda nil (acet-sys-shift-down)))
		  )
		)
	      )
	 )
  )

  (setq ModeLst '("DINFO" "LAYISO"))

  (or DInfo:Mode (setq DInfo:Mode 0))
  (or DInfo:cRad (setq DInfo:cRad 30))

  (setq	Alignment
	 (list
	   (cons acAlignmentLeft "Left")
	   (cons acAlignmentCenter "Center")
	   (cons acAlignmentRight "Right")
	   (cons acAlignmentAligned "Aligned")
	   (cons acAlignmentMiddle "Middle")
	   (cons acAlignmentFit "Fit")
	   (cons acAlignmentTopLeft "Top-Left")
	   (cons acAlignmentTopCenter "Top-Center")
	   (cons acAlignmentTopRight "Top-Right")
	   (cons acAlignmentMiddleLeft "Middle-Left")
	   (cons acAlignmentMiddleCenter "Middle-Center")
	   (cons acAlignmentMiddleRight "Middle-Right")
	   (cons acAlignmentBottomLeft "Bottom-Left")
	   (cons acAlignmentBottomCenter "Bottom-Center")
	   (cons acAlignmentBottomRight "Bottom-Right")
	 )
  )

  (setq	Attachment
	 (list
	   (cons acAttachmentPointTopLeft "Top-Left")
	   (cons acAttachmentPointTopCenter "Top-Center")
	   (cons acAttachmentPointTopRight "Top-Right")
	   (cons acAttachmentPointMiddleLeft "Middle-Left")
	   (cons acAttachmentPointMiddleCenter "Middle-Center")
	   (cons acAttachmentPointMiddleRight "Middle-Right")
	   (cons acAttachmentPointBottomLeft "Bottom-Left")
	   (cons acAttachmentPointBottomCenter "Bottom-Center")
	   (cons acAttachmentPointBottomRight "Bottom-Right")
	 )
  )

  ;;-------------------------------------------------------------------------------;;
  ;;                           --=={  Local Functions  }==--                       ;;
  ;;-------------------------------------------------------------------------------;;


  ;;  --=={  Error Handler  }==--

  (defun *error* (msg)
    (if	cEnt
      (entdel cEnt)
    )
    (if	tEnt
      (entdel tEnt)
    )

    (if	on
      (mapcar '(lambda (x) (vla-put-layeron x :vlax-true)) on)
    )

    (PurgeLayer "LMAC_DINFO")

    (or	(wcmatch (strcase msg) "*BREAK,*CANCEL*,*EXIT*")
	(princ (strcat "\n** Error: " msg " **"))
    )
    (princ)
  )

					;.................................................................................;

  (defun _PutDXF (e x r)
    (entmod (subst (cons x r) (assoc x e) e))
  )

					;.................................................................................;

  (defun _Update (e)
    (entupd (cdr (assoc -1 e)))
  )

					;.................................................................................;

  (defun _Text (tx) (strcat "{\\fArial|b1|i0|c0|p34;" tx "}"))

					;.................................................................................;

  (defun ss->lst (ss / i ent lst)
    (setq i -1)
    (while (setq ent (ssname ss (setq i (1+ i))))
      (setq lst (cons (vlax-ename->vla-object ent) lst))
    )
    lst
  )

					;.................................................................................;

  (defun _GetColour (e / c)
    (if	(setq c (cdr (assoc 62 e)))
      (cond
	(
	 (cdr
	   (assoc c
		  '(
		    (0 . "ByBlock")
		    (1 . "Red")
		    (2 . "Yellow")
		    (3 . "Green")
		    (4 . "Cyan")
		    (5 . "Blue")
		    (6 . "Magenta")
		    (7 . "White")
		   )
	   )
	 )
	)
	((itoa c))
      )
      "ByLayer"
    )
  )

					;.................................................................................;

  (defun _GetScale (e)
    (vl-princ-to-string
      (mapcar
	(function
	  (lambda (x)
	    (rtos (dxf x e) (getvar 'LUNITS) 2)
	  )
	)
	'(41 42 43)
      )
    )
  )

					;.................................................................................;

  (defun _GetName (obj)
    (if	(vlax-property-available-p obj 'EffectiveName)
      (vla-get-EffectiveName obj)
      (vla-get-name obj)
    )
  )

					;.................................................................................;

  (defun _GetLW	(e / w)
    (if	(setq w (cdr (assoc 370 e)))
      (cond
	(
	 (cdr
	   (assoc w
		  '(
		    (-1 . "ByLayer")
		    (-2 . "ByBlock")
		    (-3 . "Default")
		   )
	   )
	 )
	)
	((strcat (rtos (/ w 100.) 2 2) "mm"))
      )
      "ByLayer"
    )
  )

					;.................................................................................;

  (defun _Stringify (data / typ)

    (setq data
	   (cond
	     ((eq :vlax-true data) "Yes")
	     ((eq :vlax-false data) "No")
	     (data)
	   )
    )

    (cond
      ((eq 'STR (setq typ (type data)))

       data
      )
      ((eq 'INT typ)

       (itoa data)
      )
      ((eq 'REAL typ)

       (rtos data)
      )
      ((vl-princ-to-string data))
    )
  )

					;.................................................................................;

  (defun _Display
	 (Cir Tx ss mode / cObj cSs iObj aStr iLst tStr cnt tStr)
    (setq cObj (vlax-ename->vla-object Cir)
	  aStr ""
    )

    (cond
      (
       (or (not ss) (= (sslength ss) 0))
      )
      ((setq iObj
	      (vl-some
		(function
		  (lambda (obj)
		    (if	(vlax-invoke obj 'IntersectWith cObj acExtendNone)
		      obj
		    )
		  )
		)
		(ss->lst ss)
	      )
       )

       (setq iLst (entget (vlax-vla-object->ename iObj)))
       (vla-put-Color cObj acRed)

       (cond
	 (
	  (zerop mode)

	  (setq tStr (strcat "{\\C4;" (dxf 0 iLst) "}"))

	  (setq	Courbe (vlax-vla-object->ename iObj)
		Point  (vlax-curve-getClosestPointTo
			 Courbe
			 (dxf 10 (entget Cir))
		       )
		Param (vlax-curve-getParamAtPoint Courbe Point)
		Tangente (vlax-curve-getFirstDeriv Courbe Param)
		Lg_Tangente (* (distance '(0 0 0) Tangente) 10000)
		Perp (vlax-curve-getSecondDeriv Courbe Param)
		Lg_Perp (* (distance '(0 0 0) Perp) 10000)
	  )

	  (setq	tStr
		 (strcat
		   tStr
		   (strcat
		     "\n Position :  "
		     (_Stringify Point)
		   )
		   (strcat
		     "\n Param :  "
		     (_Stringify Param)
		   )
		   (strcat
		     "\n Tangente :  "
		     (_Stringify Tangente)
		   )
		   (strcat
		     "\n Lg Tangente :  "
		     (_Stringify Lg_Tangente)
		   )
		   (strcat
		     "\n Perp :  "
		     (_Stringify Perp)
		   )
		   (strcat
		     "\n Lg Perp :  "
		     (_Stringify Lg_Perp)
		   )
		   
		 )
	  )

	  (_Update
	    (_PutDxf
	      (_PutDxf (entget tx) 62 251)
	      1
	      (_Text tStr)
	    )
	  )
	  t
	 )
       )
      )
      (t)
    )

    iObj
  )

					;.................................................................................;

  (defun dxf (code lst) (cdr (assoc code lst)))

					;.................................................................................;

  (defun PurgeLayer (layer)
    (if
      (not
	(vl-catch-all-error-p
	  (setq	layer
		 (vl-catch-all-apply
		   'vla-item
		   (list
		     (vla-get-layers
		       (vla-get-ActiveDocument (vlax-get-acad-object))
		     )
		     layer
		   )
		 )
	  )
	)
      )
       (vl-catch-all-apply 'vla-delete (list layer))
    )
  )

					;.................................................................................;

  (defun RedrawSS (ss mode)
    (
     (lambda (i)
       (while (setq e (ssname ss (setq i (1+ i))))
	 (redraw e mode)
       )
     )
      -1
    )
  )

  ;;-------------------------------------------------------------------------------;;
  ;;                           --=={  Main Function  }==--                         ;;
  ;;-------------------------------------------------------------------------------;;

  (setq	cEnt
	      (entmakex
		(list
		  (cons 0 "CIRCLE")
		  (cons 8 "LMAC_DINFO")
		  (cons 10 (getvar 'VIEWCTR))
		  (cons	40
			(setq rad (/ (getvar 'VIEWSIZE) (float DInfo:cRad)))
		  )
		  (cons 62 3)
		)
	      )
	cELst (entget cEnt)
  )

  (setq	tEnt
	      (entmakex
		(list
		  (cons 0 "MTEXT")
		  (cons 100 "AcDbEntity")
		  (cons 100 "AcDbMText")
		  (cons 8 "LMAC_DINFO")
		  (cons 1 (_Text (nth DInfo:Mode ModeLst)))
		  (cons 10 (getvar 'VIEWCTR))
		  (cons 40 (/ (getvar 'VIEWSIZE) 60.0))
		  (cons 50 0.0)
		  (cons 62 71)
		  (cons 71 1)
		  (cons 90 3)
		  (cons 63 256)
		  (cons 45 1.2)
		)
	      )
	tElst (entget tEnt)
	-pi/4 (/ pi -4.)
	pi/4  (/ pi 4.)
	5pi/4 (/ (* 5 pi) 4.)
  )

  (setq	msgLst
	 '("\n[TAB Mode] [+/- Cursor Size] Move Cursor Over Objects to Retrieve Information..."
	   "\n[TAB Mode] [+/- Cursor Size] Click Object to Isolate Layer, Shift+Click to Turn on All Layers..."
	  )
  )

  (princ (setq msg (nth DInfo:Mode msgLst)))

  (while
    (progn
      (setq gr	 (grread 't 15 1)
	    code (car gr)
	    data (cadr gr)
	    vs	 (getvar 'VIEWSIZE)
      )

      (cond
	(
	 (and (= 5 code) (listp data))

	 (setq r (sqrt (* 2. rad rad)))

	 (setq cEnt
		(_Update
		  (setq	cELst
			 (_PutDxf (_PutDxf cELst 10 data)
				  40
				  (setq rad (/ vs (float DInfo:cRad)))
			 )
		  )
		)
	 )

	 (setq tEnt
		(_Update
		  (setq	tELst
			 (_PutDxf
			   (_PutDxf tELst
				    10
				    (polar (polar data -pi/4 rad) 0 (/ vs 90.0))
			   )
			   40
			   (/ vs 60.0)
			 )
		  )
		)
	 )

	 (if (setq
	       ss (ssget "_C" (polar data pi/4 r) (polar data 5pi/4 r))
	     )
	   (progn

	     (ssdel cEnt ss)
	     (ssdel tEnt ss)

	     (setq Inter (_Display cEnt tEnt ss DInfo:Mode))
	   )
	 )

	 t
	)
	(
	 (= 2 code)

	 (cond
	   (
	    (vl-position data '(43 61))

	    (if	(> DInfo:cRad 1.0)
	      (setq cEnt
		     (_Update
		       (setq cELst
			      (_PutDxf
				cELst
				40
				(setq
				  rad (/ vs
					 (float (setq DInfo:cRad (1- DInfo:cRad)))
				      )
				)
			      )
		       )
		     )
	      )

	      (princ (strcat "\n** Maximum Cursor Size Reached **" msg))
	    )
	   )
	   (
	    (= 45 data)

	    (setq cEnt
		   (_Update
		     (setq cELst
			    (_PutDxf cELst
				     40
				     (setq rad
					    (/ vs (float (setq DInfo:cRad (1+ DInfo:cRad))))
				     )
			    )
		     )
		   )
	    )
	   )
	   (
	    (= 9 data)

	    (setq DInfo:Mode (rem (1+ DInfo:Mode) 2))

	    (setq tEnt
		   (_Update
		     (setq tELst
			    (_PutDxf tELst 1 (_Text (nth DInfo:Mode ModeLst)))
		     )
		   )
	    )

	    (princ (setq msg (nth DInfo:Mode msgLst)))
	   )
	   (
	    (vl-position data '(13 32))
	    nil
	   )
	   (t)
	 )
	)
	(
	 (and (= 3 code) (listp data) (= 1 DInfo:Mode))

	 (if (and Express (acet-sys-shift-down))
	   (mapcar
	     (function
	       (lambda (x) (vla-put-layeron x :vlax-true))
	     )
	     on
	   )

	   (if
	     (and Inter (not (eq "LMAC_DINFO" (vla-get-layer Inter))))

	      (mapcar
		(function
		  (lambda (x)
		    (if	(not (eq (strcase (vla-get-layer Inter))
				 (strcase (vla-get-name x))
			     )
			)
		      (vla-put-layeron x :vlax-false)
		    )
		  )
		)
		layers
	      )
	   )
	 )

	 t
	)
      )
    )
  )

  (mapcar 'entdel (list tEnt cEnt))
  (PurgeLayer "LMAC_DINFO")

  (princ)
)

					;.................................................................................;

(princ)
(princ
  "\n:: DInfo.lsp | Version 1.5 | © Lee Mac 2011 www.lee-mac.com ::"
)
(princ "\n:: Type \"DInfo\" to Invoke ::")
(princ)

;;-------------------------------------------------------------------------------;;
;;                             End of Program Code                               ;;
;;-------------------------------------------------------------------------------;;