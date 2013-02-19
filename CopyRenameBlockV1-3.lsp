;;------------------=={ Copy/Rename Block }==-----------------;;
;;                                                            ;;
;;  Copies or Renames an single selected block reference with ;;
;;  a name specified by the user. The program utilises an     ;;
;;  ObjectDBX Document interface to copy the block definition ;;
;;  of the selected reference, perform the rename operation,  ;;
;;  then copy the renamed definion back to the working        ;;
;;  drawing.                                                  ;;
;;                                                            ;;
;;  Program works with Dynamic Blocks & XRefs.                ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2011 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Version 1.3    -    25-08-2011                            ;;
;;------------------------------------------------------------;;

(defun c:CB nil (RenameBlock   t))

(defun c:RB nil (RenameBlock nil))

;;------------------------------------------------------------;;

(defun RenameBlock ( copy / *error* _Name _ReleaseObject acapp acdoc b1 b2 d1 dbdoc df n1 n2 )

;;------------------------------------------------------------;;

  (defun *error* ( msg )
    (_ReleaseObject dbdoc)
    (or (wcmatch (strcase msg) "*BREAK,*CANCEL*,*EXIT*")
        (princ (strcat "\n** Error: " msg " **")))
    (princ)
  )

  (defun _Name ( obj )
    (if (vlax-property-available-p obj 'EffectiveName)
      (vla-get-EffectiveName obj)
      (vla-get-Name obj)
    )
  )

  (defun _ReleaseObject ( obj )
    (and obj (eq 'VLA-OBJECT (type obj)) (not (vlax-object-released-p obj))
      (not
        (vl-catch-all-error-p
          (vl-catch-all-apply 'vlax-release-object (list obj))
        )
      )
    )
  )
  
;;------------------------------------------------------------;;

  (setq acapp (vlax-get-acad-object)
        acdoc (vla-get-activedocument acapp)
        acblk (vla-get-blocks acdoc)
  )
  
  (if
    (and
      (setq b1
        (car
          (LM:Selectif (strcat "\nSelect Block Reference to " (if copy "Copy" "Rename") ": ")
            (lambda ( x ) (eq "INSERT" (cdr (assoc 0 (entget (car x))))))
            entsel nil
          )
        )
      )
      (LM:CopyBlockDef acdoc (setq dbdoc (LM:ObjectDBXDocument acapp)) (setq n1 (_Name (setq b1 (vlax-ename->vla-object b1))))
        (progn
          (while
            (progn
              (setq n2
                (getstring t
                  (strcat "\nSpecify New Block Name <"
                    (setq df
                      (
                        (lambda ( i / b )
                          (while
                            (tblsearch "BLOCK"
                              (setq b (strcat n1 "_" (itoa (setq i (1+ i)))))
                            )
                          )
                          b
                        )
                        0
                      )
                    )
                    "> : "
                  )
                )
              )
              (cond
                ( (eq "" n2) (setq n2 df)
                  nil
                )
                ( (or (not (snvalid n2)) (tblsearch "BLOCK" n2))
                  (princ "\nBlock Name Invalid or Already Exists.")
                )
              )
            )
          )
          n2
        )
      )
    )
    (progn
      (if (and (vlax-property-available-p b1 'isDynamicBlock) (eq :vlax-true (vla-get-isDynamicBlock b1)))
        (progn
          (setq p1 (mapcar 'vla-get-value (vlax-invoke b1 'GetDynamicBlockProperties)))
          (vla-put-name (if copy (setq b1 (vla-copy b1)) b1) n2)
          (mapcar
            (function
              (lambda ( a b )
                (or (eq "ORIGIN" (strcase (vla-get-PropertyName a))) (vla-put-value a b))
              )
            )
            (vlax-invoke b1 'GetDynamicBlockProperties) p1
          )
        )
        (vla-put-name (if copy (setq b1 (vla-copy b1)) b1) n2)
      )
      (if (eq :vlax-true (vla-get-isxref (setq d1 (vla-item acblk n2))))
        (vla-reload d1)
      )
      (if copy (sssetfirst nil (ssadd (vlax-vla-object->ename b1))))
    )
  )
  (_ReleaseObject dbdoc)
  (princ)
)

;;---------------=={ Copy Block Definition }==----------------;;
;;                                                            ;;
;;  Copies the specified block defintion with new name as     ;;
;;  specified                                                 ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2011 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Arguments:                                                ;;
;;  acdoc - Document Object containing Block to copy          ;;
;;  dbdoc - ObjectDBX Document                                ;;
;;  name1 - Name of block definition to copy                  ;;
;;  name2 - Name to be used for copied definition             ;;
;;------------------------------------------------------------;;
;;  Returns: Copied VLA Block Definition Object, else nil     ;;
;;------------------------------------------------------------;;

(defun LM:CopyBlockDef ( acdoc dbdoc name1 name2 / acblk dbblk b1 b2 )
  (setq acblk (vla-get-blocks acdoc)
        dbblk (vla-get-blocks dbdoc)
  )               
  (if
    (and
      (setq b1 (LM:GetItem acblk name1))
      (not     (LM:GetItem acblk name2))
    )
    (progn  
      (vla-CopyObjects acdoc (LM:SafearrayVariant vlax-vbObject (list b1)) dbblk)
      (vla-put-Name (setq b2 (LM:GetItem dbblk name1)) name2)
      (vla-CopyObjects dbdoc (LM:SafearrayVariant vlax-vbObject (list b2)) acblk)
    )
  )
  (LM:GetItem acblk name2)
)

;;--------------=={ VLA-Collection: Get Item }==--------------;;
;;                                                            ;;
;;  Retrieves the item with index 'item' if present in the    ;;
;;  specified collection, else nil                            ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2011 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Arguments:                                                ;;
;;  collection - the VLA Collection Object                    ;;
;;  item       - the index of the item to be retrieved        ;;
;;------------------------------------------------------------;;
;;  Returns:  the VLA Object at the specified index, else nil ;;
;;------------------------------------------------------------;;

(defun LM:GetItem ( collection item )
  (if
    (not
      (vl-catch-all-error-p
        (setq item (vl-catch-all-apply 'vla-item (list collection item)))
      )
    )
    item
  )
)

;;-----------------=={ ObjectDBX Document }==-----------------;;
;;                                                            ;;
;;  Retrieves a version specific ObjectDBX Document object    ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2011 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Arguments:                                                ;;
;;  acapp - AutoCAD VLA Application Object                    ;;
;;------------------------------------------------------------;;
;;  Returns:  VLA ObjectDBX Document object, else nil         ;;
;;------------------------------------------------------------;;

(defun LM:ObjectDBXDocument ( acapp / acVer )
  (vla-GetInterfaceObject acapp
    (if (< (setq acVer (atoi (getvar "ACADVER"))) 16)
      "ObjectDBX.AxDbDocument" (strcat "ObjectDBX.AxDbDocument." (itoa acVer))
    )
  )
)

;;------------------=={ Safearray Variant }==-----------------;;
;;                                                            ;;
;;  Creates a populated Safearray Variant of a specified      ;;
;;  data type                                                 ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2011 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Arguments:                                                ;;
;;  datatype - variant type enum (eg vlax-vbDouble)           ;;
;;  data     - list of static type data                       ;;
;;------------------------------------------------------------;;
;;  Returns:  VLA Variant Object of type specified            ;;
;;------------------------------------------------------------;;

(defun LM:SafearrayVariant ( datatype data )
  (vlax-make-variant
    (vlax-safearray-fill
      (vlax-make-safearray datatype (cons 0 (1- (length data)))) data
    )
  )
)

;;---------------------=={ Select if }==----------------------;;
;;                                                            ;;
;;  Provides continuous selection prompts until either a      ;;
;;  predicate function is validated or a keyword is supplied. ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2011 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Arguments:                                                ;;
;;  msg  - prompt string                                      ;;
;;  pred - optional predicate function [selection list arg]   ;;
;;  func - selection function to invoke                       ;;
;;  keyw - optional initget argument list                     ;;
;;------------------------------------------------------------;;
;;  Returns:  Entity selection list, keyword, or nil          ;;
;;------------------------------------------------------------;;

(defun LM:SelectIf ( msg pred func keyw / sel ) (setq pred (eval pred))  
  (while
    (progn (setvar 'ERRNO 0) (if keyw (apply 'initget keyw)) (setq sel (func msg))
      (cond
        ( (= 7 (getvar 'ERRNO))
          (princ "\nMissed, Try again.")
        )
        ( (eq 'STR (type sel))
          nil
        )
        ( (vl-consp sel)
          (if (and pred (not (pred sel)))
            (princ "\nInvalid Object Selected.")
          )
        )
      )
    )
  )
  sel
)

;;------------------------------------------------------------;;

(vl-load-com) (princ)

;;------------------------------------------------------------;;
;;                         End of File                        ;;
;;------------------------------------------------------------;;