;;; LIST-DRIVES - Liste des lecteurs disponibles
;;; By Tony Tanzillo
(defun list-drives (/ c i)
  (setq i 66)
  (repeat 24
    (setq c (chr (setq i (1+ i))))
    (if	(findfile (strcat c ":\\."))
      (setq rslt (cons (strcat c ":") rslt))
    )
  )
  (setq rslt (reverse rslt))
)


;;; BROWSEFORFOLDER - retourne le chemin du dossier selectionné
;;; By Tony Tanzillo
(defun BrowseForFolder ( invite / sh folder folderobject result)
  (vl-load-com)
  (setq	sh (vla-getInterfaceObject
	     (vlax-get-acad-object)
	     "Shell.Application"
	   )
  )
  (setq folder (vlax-invoke-method sh 'BrowseForFolder 0 invite 80))
  (vlax-release-object sh)
  (if folder
    (progn (setq folderobject (vlax-get-property folder 'Self))
	   (setq result (vlax-get-property FolderObject 'Path))
	   (vlax-release-object folder)
	   (vlax-release-object FolderObject)
	   result
    )
  )
)


;;; ALE_BROWSEFORFOLDER - retourne le chemin du dossier selectionné avec options
;;; By Tony Tanzillo
;;; 1 RestrictToFilesystem = &H1 ' BIF_RETURNONLYFSDIRS
;;; 2 RestrictToDomain = &H2 ' BIF_DONTGOBELOWDOMAIN
;;; 8 RestrictToSubfolders = &H8 ' BIF_RETURNFSANCESTORS
;;; 16 ShowTextBox = &H10 ' BIF_EDITBOX
;;; 32 ValidateSelection = &H20 ' BIF_VALIDATE
;;; 64 NewDialogStyle = &H40 ' BIF_NEWDIALOGSTYLE
;;; 4096 BrowseForComputer = &H1000 ' BIF_BROWSEFORCOMPUTER
;;; 8192 BrowseForPrinter = &H2000 ' BIF_BROWSEFORPRINTER
;;; 16384 BrowseForEverything = &H4000 ' BIF_BROWSEINCLUDEFILES 
;;;
;;; Example: (ALE_BrowseForFolder "Select a folder:" 48 "C:\\Temp\\") 
;;;

(defun ALE_BrowseForFolder (PrmStr iOptns DefFld /
ShlObj Folder FldObj OutVal)
(vl-load-com)
(setq
ShlObj (vla-getInterfaceObject
(vlax-get-acad-object) "Shell.Application"
)
Folder (vlax-invoke-method ShlObj
'BrowseForFolder 0 PrmStr iOptns DefFld
)
)
(vlax-release-object ShlObj)
(if Folder
(progn
(setq
FldObj (vlax-get-property Folder 'Self)
OutVal (vlax-get-property FldObj 'Path)
)
(vlax-release-object Folder)
(vlax-release-object FldObj)
OutVal
)
)
)

;;; GET-SUBDIRS - retourne tous les sous-dossier d'un dossier
;;;   path = chemin d'accès complet du dossier
(defun get-subdirs (path)
  (apply (quote append)
	 (cons (list path)
	       (mapcar '(lambda (x) (get-subdirs (strcat path x "\\")))
		       (cddr (vl-directory-files path nil -1))
	       )
	 )
  )
)

;;; GET-DIR - retourne le chemin du dossier recherché
;;;   pth = dossier de départ pour la recherche
;;;   che = dossier à rechercher
(defun get-dir (pth che / func file)
  (defun func (path cherche / dir)
    (setq dir (cddr (vl-directory-files path nil -1)))
    (mapcar '(lambda (x)
	       (if (vl-string-search (strcase cherche) (strcase x))
		 (setq file (cons (strcat path x "\\") file)
		       dir  'nil
		 )
		 (func (strcat path x "\\") cherche)
	       )
	     )
	    dir
    )
    file
  )
  (if (and pth che)
    (func pth che)
  )
)

;;; GET-FILES-SUBDIRS - retourne une liste des fichiers contenu dans le dossier et sous-dossier
;;;   RootPath = chemin du dossier
;;;   ext = extension des fichiers à lister
(defun get-files-subdirs (RootPath ext)
  (foreach path	(get-subdirs RootPath)
    (setq ListFilesFullPath
	   (append
	     ListFilesFullPath
	     (mapcar
	       '(lambda (fileName) (strcat path fileName))
	       (vl-directory-files path ext 1)
	     )
	   )
    )
  )
)


;;;================================================================================================;;;
(princ)