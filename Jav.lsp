;=========================================================================
;
; Nettoyer en profondeur un dessin
;
; JAV V3.10
;
; (C) Patrick_35
;
;=========================================================================

(defun patrick:jav( bda / *errjav* app arg att bl bd_appli cde cle dcl dic doc ent img
			  jav lancer lgt lst lst_lay lst_ech lst_maxi maxi old s sel
			  tdi tot tota txt var
			  appli bd bits C30 del_dic eff inputbox msg msgbox opt select
			  liste_echelles RightCleanText)

;=========================================================================
; Gestion des erreurs
;=========================================================================

  (defun *errjav* (msg)
    (or (member (strcase msg) '("FUNCTION CANCELLED" ""QUIT / EXIT ABORT"" "FONCTION ANNULEE" "QUITTER / SORTIR ABANDON"))
      (princ (strcat "\nErreur : " msg))
    )
    (mapcar '(lambda(x) (vla-put-lock (car x) (cdr x))) lst_lay)
    (vla-endundomark doc)
    (setq *error* s)
    (princ)
  )

;=========================================================================
; Afficher un message via une boite de dialogue
;=========================================================================

  (defun msgbox (titre bouttons message / reponse wshshell)
    (acad-push-dbmod)
    (setq wshshell (vlax-create-object "WScript.Shell"))
    (setq reponse (vlax-invoke wshshell 'popup message 0 titre (itoa bouttons)))
    (vlax-release-object wshshell)
    (acad-pop-dbmod)
    reponse
  )

;=========================================================================
; Boite de dialogue Options
;=========================================================================

  (defun opt(titre / dcl fil tmp txt)
    (setq tmp (vl-filename-mktemp "opt" nil ".dcl")
	  fil (open tmp "w")
    )
    (foreach txt (list	"opt : dialog {"
			"  key = titre;"
			"  is_cancel = true;"
			"  fixed_width = true;"
			"  alignment = centered;"
			"  : row {"
			"    : boxed_column {"
			"      label = \"Nettoyer, vérifier\";"
			"      width = 35;"
			(strcat "      : toggle {key = \"" (itoa (expt 2  0)) "\"; label = \"Contrôle des erreurs.\";}")
			(strcat "      : toggle {key = \"" (itoa (expt 2 16)) "\"; label = \"Purger le dessin.\";}")
			(strcat "      : toggle {key = \"" (itoa (expt 2  1)) "\"; label = \"Purger les applications.\";}")
			(strcat "      : toggle {key = \"" (itoa (expt 2 10)) "\"; label = \"Liste d'échelles.\";}")
			(strcat "      : toggle {key = \"" (itoa (expt 2 12)) "\"; label = \"Filtres de calques.\";}")
			(strcat "      : toggle {key = \"" (itoa (expt 2 11)) "\"; label = \"Liens dxe.\";}")
			(strcat "      : toggle {key = \"" (itoa (expt 2 19)) "\"; label = \"Police de texte inconnue.\";}")
			(strcat "      : toggle {key = \"" (itoa (expt 2 17)) "\"; label = \"Images (raster) orphelines.\";}")
			"      spacer;"
			"    }"
			"    : boxed_column {"
			"      label = \"Inutiles dans le dessin\";"
			"      width = 35;"
			(strcat "      : toggle {key = \"" (itoa (expt 2  2)) "\"; label = \"Lignes de longueur 0.\";}")
			(strcat "      : toggle {key = \"" (itoa (expt 2  3)) "\"; label = \"Textes vide.\";}")
			(strcat "      : toggle {key = \"" (itoa (expt 2  4)) "\"; label = \"Blocs vide.\";}")
			(strcat "      : toggle {key = \"" (itoa (expt 2  5)) "\"; label = \"Groupes vide.\";}")
			(strcat "      : toggle {key = \"" (itoa (expt 2 14)) "\"; label = \"Dictionnaires vide.\";}")
			(strcat "      : toggle {key = \"" (itoa (expt 2 18)) "\"; label = \"Caractères non imprimable.\";}")
			(strcat "      : toggle {key = \"" (itoa (expt 2 20)) "\"; label = \"Wipeouts.\";}")
			"      spacer;spacer;spacer;spacer;spacer;"
			"    }"
			"  }"
			"  spacer;"
			"  : row {"
			"    : boxed_column {"
			"      label = \"Effacer objets ARX/Proxys\";"
			"      width = 35;"
			(strcat "      : toggle {key = \"" (itoa (expt 2  7)) "\"; label = \"Proxys.\";}")
			(strcat "      : toggle {key = \"" (itoa (expt 2  8)) "\"; label = \"Zombies Blocs.\";}")
			(strcat "      : toggle {key = \"" (itoa (expt 2  9)) "\"; label = \"Zombies Dictionnaires.\";}")
			"      spacer;"
			"    }"
			"    : boxed_column {"
			"      label = \"Attention, peut-être sensible\";"
			"      width = 35;"
			(strcat "      : toggle {key = \"" (itoa (expt 2  6)) "\"; label = \"Effacer Xdatas.\";}")
			(strcat "      : toggle {key = \"" (itoa (expt 2 13)) "\"; label = \"Nettoyer  Dictionnaires.\";}")
			(strcat "      : toggle {key = \"" (itoa (expt 2 15)) "\"; label = \"Supprimer Dictionnaires.\";}")
			"      spacer;"
			"    }"
			"  }"
			"  spacer;"
			"  : row {"
			"    : boxed_column {"
			"      width = 33;"
			"      label = \"Lancer une/des application(s)\";"
			"      : row {"
			"        : toggle {label = \"Activer\"; key = \"acti\";}"
			"        : button {label = \"Sélection\"; key = \"choix\";}"
			"      }"
			"      spacer;"
			"    }"
			"    : row {"
			"      width = 36;"
			"      spacer;"
			"      : column {"
			"        : button {key = \"tout\";  label = \"Tout\";}"
			"        : button {key = \"aucun\"; label = \"Aucun\";}"
			"        spacer;"
			"      }"
			"      : column {"
			"        ok_button;"
			"        cancel_button;"
			"        spacer;"
			"      }"
			"    }"
			"  }"
			"  spacer;"
			"}"
		 )
      (write-line txt fil)
    )
    (close fil)
    (setq dcl (load_dialog tmp))
    (new_dialog "opt" dcl "")
    (set_tile "titre" titre)
    (mapcar '(lambda(x) (or (zerop (logand var x)) (set_tile (itoa x) "1")))
	    lst_maxi
    )
    (mode_tile "appli" 1)
    (set_tile "acti" lancer)
    (cons dcl tmp)
  )

;=========================================================================
; Boite de dialogue Rapport
;=========================================================================

  (defun bd(titre / dcl fil tmp txt)
    (setq tmp (vl-filename-mktemp "rap" nil ".dcl")
	  fil (open tmp "w")
    )
    (foreach txt '(	"msg : dialog {"
			"  key = titre;"
			"  is_cancel = true;"
			"  fixed_width = true;"
			"  alignment = centered;"
			"  : list_box {key = \"lst\"; multiple_select = true; width = 80; height = 50;}"
			"  ok_button;"
			"}"
		 )
      (write-line txt fil)
    )
    (close fil)
    (setq dcl (load_dialog tmp))
    (new_dialog "msg" dcl "")
    (set_tile "titre" titre)
    (mode_tile "accept" 1)
    (cons dcl tmp)
  )

;=========================================================================
; Boite de dialogue Applications
;=========================================================================

  (defun bd_appli(titre / dcl fil tmp txt)
    (setq tmp (vl-filename-mktemp "app" nil ".dcl")
	  fil (open tmp "w")
    )
    (foreach txt '(	"app : dialog {"
			"  key = titre;"
			"  is_cancel = true;"
			"  fixed_width = true;"
			"  alignment = centered;"
			"  : row {"
			"    : list_box {label =\"Application\"; key = \"lst\"; multiple_select = true; width = 50;}"
			"    : column {"
			"      spacer_1;"
			"      : button {label = \"Ajouter\";   key = \"ajout\";}"
			"      : button {label = \"Modifier\";  key = \"modif\";}"
			"      : button {label = \"Supprimer\"; key = \"suppr\";}"
			"      spacer_1; spacer_1; spacer_1;"
			"      : button {label = \"Haut\"; key = \"haut\";}"
			"      : button {label = \"Bas\";  key = \"bas\";}"
			"      spacer_1; spacer_1; spacer_1;"
			"    }"
			"    spacer;"
			"  }"
			"  : text {label = \"Fichier :\"; key = \"fich\";}"
			"  : text {label = \"Arguments :\"; key = \"args\";}"
			"  spacer;"
			"  ok_cancel;"
			"}"
		 )
      (write-line txt fil)
    )
    (close fil)
    (setq dcl (load_dialog tmp))
    (new_dialog "app" dcl "")
    (set_tile "titre" titre)
    (cons dcl tmp)
  )

;=========================================================================
; Boite de dialogue Commande + arguments
;=========================================================================

  (defun inputbox (titre js1 js2 opt fir / ch dcl fil res tmp txt)
    (setq tmp (vl-filename-mktemp "cmd" nil ".dcl")
	  fil (open tmp "w")
    )
    (foreach txt '(	"lsp : dialog {"
			"  key = \"titre\";"
			"  alignment = centered;"
			"  is_cancel = true;"
			"  allow_accept = true;"
			"  : text {key = \"fir\";}"
			"  spacer;"
			"  : row {"
			"    : column {"
			"      width = 8;"
			"      : text {label = \"Commande\";}"
			"      spacer;"
			"      : text {label = \"Arguments\"; key = \"txt\";}"
			"    }"
			"    : column {"
			"      width = 40;"
			"      : edit_box {key= \"cmd\";}"
			"      spacer;"
			"      : edit_box {key= \"arg\";}"
			"    }"
			"  }"
			"  spacer;"
			"  ok_cancel;"
			"}"
		 )
      (write-line txt fil)
    )
    (close fil)
    (setq dcl (load_dialog tmp))
    (new_dialog "lsp" dcl "")
    (set_tile "titre" titre)
    (set_tile "cmd" js1)
    (set_tile "arg" js2)
    (set_tile "fir" fir)
    (mode_tile "arg" opt)
    (mode_tile "txt" opt)
    (mode_tile "cmd" 2)
    (action_tile "cmd" "(setq js1 $value)")
    (action_tile "arg" "(setq js2 $value)")
    (action_tile "cancel" "(done_dialog 0)")
    (action_tile "accept" "(done_dialog 1)")
    (setq res (start_dialog))
    (unload_dialog dcl)
    (vl-file-delete tmp)
    (if (eq res 1)
      (list js1 js2)
      (list "" "")
    )
  )

;=========================================================================
; Selectionner Tout/Aucun
;=========================================================================
  (defun select(bit)
    (mapcar '(lambda(x) (set_tile (itoa x) bit))
	    lst_maxi
    )
    (if (eq bit "0")
      (setq var 0)
      (setq var maxi)
    )
  )

;=========================================================================
; Afficher un message
;=========================================================================

  (defun msg(msg)
    (setq tot 0)
    (if bda
      (progn
	(start_list "lst" 2)
	  (if (eq (vl-string-elt msg (1- (strlen msg))) 10)
	  (progn
	    (add_list (substr msg 1 (1- (strlen msg))))
	    (add_list "")
	  )
	  (add_list msg)
	)
	(end_list)
      )
      (progn
	(terpri)
	(princ msg)
      )
    )
  )

;=========================================================================
; Ajouter/enlever un bit
;=========================================================================

  (defun bits(bit)
    (setq var (boole 6 var bit))
  )

;=========================================================================
; Effacer un dictionnaire
;=========================================================================

  (defun del_dic(rec / dic tot)
    (if (vl-catch-all-error-p (setq dic (vl-catch-all-apply 'vla-item (list (vla-get-dictionaries doc) rec))))
      (setq tot 0)
      (progn
	(setq tot (vla-get-count dic))
	(vl-catch-all-apply 'vla-delete (list dic))
      )
    )
    tot
  )

;=========================================================================
; Effacer un élement
;=========================================================================

  (defun eff(ent)
    (or (vl-catch-all-error-p (vl-catch-all-apply 'vla-delete (list ent)))
      (setq tot (1+ tot))
    )
  )

;=========================================================================
; Faire un chiffre de 3 caractères (ex : 003)
;=========================================================================

    (defun c30(val)
      (setq val (itoa val))
      (while (< (strlen val) 3) (setq val (strcat "0" val)))
      val
    )

;=========================================================================
; Gerer les applications
;=========================================================================

  (defun appli(/ arg der dcl ext lsp lst n pos_lst rep
		 aff_fichier ajouter bas elem haut modifier selection supprimer)

;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; Afficher correctement le fichier, découpe le texte si nécessaire
;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    (defun aff_fichier(str / txt)
      (if (> (strlen str) 70)
	(setq txt (strcat (substr str 1 (- 62 (strlen (vl-filename-base str))))
			  "...\\"
			  (vl-filename-base str)
			  (vl-filename-extension str)
		  )
	)
	(setq txt str)
      )
      (set_tile "fich" txt)
    )

;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; Affiche le nom de fichier + arguments suivant la sélection
;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    (defun selection(cle / der emp pos pre tmp val)
      (and lst
	(progn
	  (and (> (length lst) 1)
	    (mapcar '(lambda(x) (mode_tile x 0)) '("haut" "bas"))
	  )
	  (if (vl-string-search " " cle)
	    (progn
	      (set_tile "fich" "Fichiers : Varié")
	      (set_tile "args" "Arguments : Varié")
	      (setq emp cle
		    pre (read emp)
	      )
	      (while (setq pos (read emp))
		(setq der pos
		      emp (substr emp (+ 2 (strlen (itoa pos))))
		)
		(and tmp
		     (not (eq pos (1+ tmp)))
		  (setq val T)
		)
		(setq tmp pos)
	      )
	      (or val
		(progn
		  (and (eq pre 0) (mode_tile "haut" 1))
		  (and (eq der (1- (length lst))) (mode_tile "bas" 1))
		)
	      )
	    )
	    (progn
	      (aff_fichier (car (nth (read cle) (reverse lst))))
	      (set_tile "args" (strcat "Argument : " (caddr (nth (read cle) (reverse lst)))))
	      (if (eq (read cle) 0)
		(mode_tile "haut" 1)
	      )
	      (if (eq (read cle) (1- (length lst)))
		(mode_tile "bas" 1)
	      )
	    )
	  )
	  (setq pos_lst cle)
	)
      )
    )

;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; Affiche un élement de la liste
;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    (defun elem(rep)
      (add_list (if (= (cadr rep) "")
		  (strcat (vl-filename-base (car rep))(vl-filename-extension (car rep)))
		  (cadr rep)
		)
      )
    )

;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; Ajouter une application
;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    (defun ajouter(/ ext fic rep)
      (or (setq der (vl-registry-read cle "Dernier"))
	(setq der (findfile "jav.lsp"))
      )
      (and (setq fic (getfiled "Sélectionnez l'application" (vl-filename-directory der) "lsp;dvb;scr;arx;dll" 16))
	(progn
	  (setq der fic
		ext (strcase (vl-filename-extension fic))
	  )
	  (cond
	    ((member ext '(".LSP" ".DVB"))
	      (setq arg 1)
	    )
	    ((eq ext ".SCR")
	      (setq arg 2)
	    )
	    (T
	      (setq arg 0)
	    )
	  )
	  (if (= arg 2)
	    (setq rep (list "" ""))
	    (setq rep (inputbox (strcat "Ajouter une application " jav) "" "" arg fic))
	  )
	  (and	(setq rep (list fic (car rep) (if (eq arg 1) "" (cadr rep))))
		(not (member rep lst))
		(setq lst (cons rep lst))
	    (progn
	      (start_list "lst" 2)
	      (elem rep)
	      (end_list)
	      (set_tile "lst" "")
	      (set_tile "lst" (setq pos_lst (itoa (1- (length lst)))))
	      (mapcar '(lambda(x) (mode_tile x 0)) '("modif" "suppr"))
	      (or (eq (length lst) 1) (mode_tile "haut" 0))
	      (selection pos_lst)
	    )
	  )
	)
      )
    )

;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; Modifier une application
;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    (defun modifier(/ arg emp fic pos rep tab)
      (setq emp pos_lst
	    tab (reverse lst)
      )
      (while (setq pos (read emp))
	(setq rep (nth pos tab)
	      emp (substr emp (+ 2 (strlen (itoa pos))))
	)
	(and (setq fic (getfiled "Sélectionnez l'application" (vl-filename-directory (car rep)) "lsp;dvd;scr;arx;dll" 16))
	  (setq rep (subst fic (car rep) rep)
		der fic
	  )
	)
	(setq fic (car rep))
	(if (eq (strcase (vl-filename-extension fic)) ".SCR")
	  (setq rep (cons fic '("" "")))
	  (progn
	    (if (member (strcase (vl-filename-extension fic)) '(".LSP" ".DVB"))
	      (setq arg 1)
	      (setq arg 0)
	    )
	    (setq rep (inputbox (strcat "Modifier une application " jav) (cadr rep) (if (eq arg 1)
										      ""
										      (caddr rep)
										    )
										    arg
										    fic
		      )
	    )
	    (if (eq (car rep) "")
	      (setq rep (cons fic (cdr (nth pos tab))))
	      (setq rep (cons fic rep))
	    )
	  )
	)
	(setq tab (subst rep (nth pos tab) tab))
	(start_list "lst" 1 pos)
	(elem rep)
	(end_list)
      )
      (setq lst (reverse tab))
      (selection pos_lst)
    )

;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; Supprimer une application
;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    (defun supprimer(/ emp old pos tab)
      (setq emp pos_lst
	    tab (reverse lst)
	    old tab
      )
      (while (setq pos (read emp))
	(setq emp (substr emp (+ 2 (strlen (itoa pos))))
	      tab (vl-remove (nth pos old) tab)
	)
      )
      (setq lst (reverse tab))
      (if lst
	(progn
	  (start_list "lst")
	  (mapcar 'elem (reverse lst))
	  (end_list)
	  (if (vl-string-search " " pos_lst)
	    (setq pos_lst "0")
	    (or (zerop (read pos_lst))
	      (setq pos_lst (itoa (1- (read pos_lst))))
	    )
	  )
	  (selection pos_lst)
	  (set_tile "lst" pos_lst)
	)
	(progn
	  (start_list "lst")
	  (end_list)
	  (mapcar '(lambda(x) (mode_tile x 1)) '("modif" "suppr" "haut" "bas"))
	  (mapcar '(lambda(a b) (set_tile a b)) '("fich" "args") '("Fichier :" "Arguments :"))
	)
      )
    )

;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; Un coup vers le haut/bas
;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    (defun haut(/ emp memo n pos tab tbl str lect)
      (defun lect()
	(setq pos (read emp)
	      emp (substr emp (+ 2 (strlen (itoa pos))))
	)
      )
      (setq tab (reverse lst)
	    emp pos_lst
	    n 0
      )
      (lect)
      (while (nth n tab)
	(if (= (1+ n) pos)
	  (progn
	    (setq tbl (cons (nth (1+ n) tab) tbl))
	    (or memo (setq memo (nth n tab)))
	    (or (= emp "") (lect))
	  )
	  (if memo
	    (setq tbl (cons memo tbl)
		  memo nil
	    )
	    (setq tbl (cons (nth n tab) tbl))
	  )
	)
	(if (eq n pos)
	  (or (= emp "") (lect))
	)
	(setq n (1+ n))
      )
      (start_list "lst")
      (mapcar 'elem (reverse tbl))
      (end_list)
      (setq lst tbl
	    emp pos_lst
	    n 0
      )
      (while (setq pos (read emp))
	(setq emp (substr emp (+ 2 (strlen (itoa pos)))))
	(or (eq n pos) (setq pos (1- pos)))
	(setq n (1+ n))
	(if str
	  (setq str (strcat str " " (itoa pos)))
	  (setq str (itoa pos))
	)
      )
      (selection str)
      (set_tile "lst" "")
      (set_tile "lst" pos_lst)
    )

;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; Un coup vers le bas
;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    (defun bas (/ emp memo n pos tab tbl str lect)
      (defun lect()
	(setq pos (read emp)
	      emp (substr emp (+ 2 (strlen (itoa pos))))
	)
      )
      (setq tab (reverse lst)
	    emp pos_lst
	    n 0
      )
      (lect)
      (while (nth n tab)
	(if (= n pos)
	  (progn
	    (setq memo (cons (nth n tab) memo))
	    (or (= emp "") (lect))
	  )
	  (progn
	    (setq tbl (cons (nth n tab) tbl))
	    (if memo
	      (setq tbl (append memo tbl)
		    memo nil
	      )
	    )
	  )
	)
	(setq n (1+ n))
      )
      (if memo
	(setq tbl (append memo tbl)
	      memo nil
	)
      )
      (start_list "lst")
      (mapcar 'elem (reverse tbl))
      (end_list)
      (setq lst tbl
	    emp pos_lst
	    n 0
      )
      (while (setq pos (read emp))
	(setq emp (substr emp (+ 2 (strlen (itoa pos)))))
	(setq memo (cons pos memo))
      )
      (while (nth n memo)
	(if (eq (+ (nth n memo) n) (1- (length lst)))
	  (setq pos (nth n memo))
	  (setq pos (1+ (nth n memo)))
	)
	(if str
	  (setq str (strcat (itoa pos) " " str))
	  (setq str (itoa pos))
	)
	(setq n (1+ n))
      )
      (selection str)
      (set_tile "lst" "")
      (set_tile "lst" pos_lst)
    )

;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; Routine principale des applications
;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    (setq n 0)
    (and (car (setq dcl (bd_appli (strcat "Gestion des applications " jav))))
      (progn
	(while (setq lsp (vl-registry-read cle (strcat "Appli_" (c30 n))))
	  (setq lst (cons (list lsp
				(vl-registry-read cle (strcat "Local_" (c30 n)))
				(vl-registry-read cle (strcat "Args__" (c30 n)))
			  )
			  lst
		    )
		n (1+ n)
	  )
	)
	(if lst
	  (progn
	    (start_list "lst")
	    (mapcar 'elem (reverse lst))
	    (end_list)
	    (selection (vl-registry-read cle "Selection"))
	    (set_tile "lst" pos_lst)
	  )
	  (mapcar '(lambda(x) (mode_tile x 1)) '("modif" "suppr" "haut" "bas"))
	)
	(action_tile "ajout"  "(ajouter)")
	(action_tile "modif"  "(modifier)")
	(action_tile "suppr"  "(supprimer)")
	(action_tile "haut"   "(haut)")
	(action_tile "bas"    "(bas)")
	(action_tile "lst"    "(selection $value)")
        (action_tile "cancel" "(done_dialog 0)")
        (action_tile "accept" "(done_dialog 1)")
        (and (eq (start_dialog) 1)
	  (progn
	    (setq lst (reverse lst)
		  n 0
	    )
	    (while (and lst (setq lsp (nth n lst)))
	      (vl-registry-write cle (strcat "Appli_" (c30 n)) (car  lsp))
	      (vl-registry-write cle (strcat "Local_" (c30 n)) (cadr   lsp))
	      (vl-registry-write cle (strcat "Args__" (c30 n)) (caddr lsp))
	      (setq n (1+ n))
	    )
	    (and der (vl-registry-write cle "Dernier" der))
	    (and pos_lst (vl-registry-write cle "Selection" pos_lst))
	    (while (setq lsp (vl-registry-read cle (strcat "Appli_" (c30 n))))
	      (vl-registry-delete cle (strcat "Appli_" (c30 n)))
	      (vl-registry-delete cle (strcat "Local_" (c30 n)))
	      (vl-registry-delete cle (strcat "Args__" (c30 n)))
	      (setq n (1+ n))
	    )
	  )
	)
        (unload_dialog (car dcl))
        (vl-file-delete (cdr dcl))
      )
    )
  )

;=========================================================================
; Liste des echelles annotatives
;=========================================================================

  (defun liste_echelles(/ dic ele ent lst obj sel typ)
    (vlax-for blo (vla-get-blocks doc)
      (and (eq (vla-get-isxref blo) :vlax-false)
	(vlax-for ent blo
	  (and (eq (vla-get-hasextensiondictionary ent) :vlax-true)
	    (vlax-for dic (vla-getextensiondictionary ent)
	      (and (vlax-property-available-p dic 'name)
		   (eq (vla-get-name dic) "AcDbContextDataManager")
		(vlax-for ele dic
		  (and (eq (vla-get-name ele) "ACDB_ANNOTATIONSCALES")
		    (vlax-for obj ele
		      (setq typ (cdr (assoc 300 (entget (cdr (assoc 340 (entget (vlax-vla-object->ename obj))))))))
		      (or (member typ lst) (setq lst (cons typ lst)))
		    )
		  )
		)
	      )
	    )
	  )
	)
      )
    )
    lst
  )

;=========================================================================
; Effacer les caractères non imprimable
; Merci à BIGC-ROMU pour le lisp et à celui qui l'a écrit (gile)
;=========================================================================

  (defun RightCleanText	(txt / lst)
    (setq lst (reverse (vl-string->list txt)))
    (while (and	lst
		(or (< (car lst) 33)
		    (and (= (car lst) 80) (= (cadr lst) 92))
		)
	   )
      (if (< (car lst) 33)
	(setq lst (cdr lst))
	(setq lst (cddr lst))
      )
    )
    (vl-list->string (reverse lst))
  )

;-------------------------------------------------------------------------
; Initialisation
;-------------------------------------------------------------------------

  (vl-load-com)
  (setq s *error*
	*error* *errjav*
	doc (vla-get-activedocument (vlax-get-acad-object))
	tota 21
	maxi (1- (expt 2 tota))
	jav "JAV v3.10"
	tdi (vla-get-count (vla-get-dictionaries doc))
	tot 1
	cle "HKEY_CURRENT_USER\\Software\\Autodesk\\Autocad\\Patrick_35\\Jav"
  )
  (while (< tot  maxi)
    (setq lst_maxi (cons tot lst_maxi)
	  tot (* tot 2)
    )
  )
  (if (setq var (getenv "Patrick_35_Jav"))
    (setq var (atoi var))
    (setq var maxi)
  )
  (or (setq lancer (getenv "Patrick_35_Jav_App")) (setq lancer "0"))
  (vla-startundomark doc)
  (and bda (car (setq dcl (opt jav)))
    (progn
      (if (< (atof (substr (getvar "acadver") 1 4)) 17.1)
	(progn
	  (or (zerop (logand var (expt 2 10))) (bits (expt 2 10)))
	  (set_tile (itoa (expt 2 10)) "0")
	  (mode_tile (itoa (expt 2 10)) 1)
	  (setq lst_maxi (vl-remove (expt 2 10) lst_maxi))
	)
      )
      (setq tot 0)
      (repeat tota
	(action_tile (itoa (expt 2 tot)) (strcat "(bits " (itoa (expt 2 tot)) " )"))
	(setq tot (1+ tot))
      )
      (action_tile "choix"  "(appli)")
      (action_tile "acti"   "(setq lancer $value)")
      (action_tile "aucun"  "(select \"0\")")
      (action_tile "tout"   "(select \"1\")")
      (action_tile "cancel" "(done_dialog 0)")
      (action_tile "accept" "(done_dialog 1)")
      (setq ent (start_dialog))
      (unload_dialog (car dcl))
      (vl-file-delete (cdr dcl))
    )
  )

  (and (or (not bda) (eq ent 1))
    (progn
      (setenv "Patrick_35_Jav" (itoa var))
      (setenv "Patrick_35_Jav_App" lancer)

;-------------------------------------------------------------------------
; Lancer les applications
;-------------------------------------------------------------------------

      (and (eq lancer "1")
	   (vl-registry-read cle "Appli_000")
	   (setq ent (vl-registry-read cle "Selection" ))
	(progn
	  (while (setq dic (read ent))
	    (setq ent (substr ent (+ 2 (strlen (itoa dic))))
		  app (vl-registry-read cle (strcat "Appli_" (c30 dic)))
		  cde (vl-registry-read cle (strcat "Local_" (c30 dic)))
		  arg (vl-registry-read cle (strcat "Args__" (c30 dic)))
		  old cde
	    )
	    (if (findfile app)
	      (cond
		((eq (strcase (vl-filename-extension app)) ".SCR")
		  (vl-cmdf "_.script" app)
		)
		((eq (strcase (vl-filename-extension app)) ".LSP")
		  (load app)
		  (or (eq cde "")
		    (progn
		      (or (eq (vl-string-position 40 cde) 0)
			(setq cde (strcat "(c:" cde ")"))
		      )
		      (and (vl-catch-all-error-p (vl-catch-all-apply 'eval (list (read cde))))
			(msgbox jav 32 (strcat "\nFonction inconnue \"" old "\""))
		      )
		    )
		  )
		)
		((eq (strcase (vl-filename-extension app)) ".DVB")
		  (vla-loaddvb (vlax-get-acad-object) app)
		  (or (eq cde "")
		    (and (vl-catch-all-error-p (vl-catch-all-apply 'vla-runmacro (list (vlax-get-acad-object) cde)))
		      (msgbox jav 32 (strcat "\nFonction inconnue \"" cde "\""))
		    )
		  )
		)
		(T
		  (if (eq (strcase (vl-filename-extension app)) ".ARX")
		    (vla-loadarx (vlax-get-acad-object) app)
		    (vl-cmdf "_.netload" app)
		  )
		  (or (eq cde "")
		    (progn
		      (and (vl-catch-all-error-p (vl-catch-all-apply 'eval (list (read (if (eq arg "")
											 cde
											 (strcat cde " " arg)
										       )
										 )
									   )
						 )
			   )
			(progn
			  (or (eq (vl-string-position 40 cde) 0)
			    (progn
			      (setq cde (strcat "(" cde))
			      (if (eq arg "")
				(setq cde (strcat cde ")"))
				(setq cde (strcat cde " " arg ")"))
			      )
			    )
			  )
			  (and (vl-catch-all-error-p (vl-catch-all-apply 'eval (list (read cde))))
			    (msgbox jav 32 (strcat "\nFonction inconnue \"" old " " arg "\""))
			  )
			)
		      )
		    )
		  )
		)
	      )
	      (msgbox jav 16 (strcat "Fichier " app " introuvable."))
	    )
	  )
	)
      )

;-------------------------------------------------------------------------
; Déverrouiller les calques
;-------------------------------------------------------------------------

      (and (or (not bda) (car (setq dcl (bd (strcat "Rapport " jav)))))
	(progn
	  (vlax-for ent (vla-get-layers doc)
	    (setq lst_lay (cons (cons ent (vla-get-lock ent)) lst_lay))
	    (vla-put-lock ent :vlax-false)
	  )

;-------------------------------------------------------------------------
; Contrôle des erreurs
;-------------------------------------------------------------------------

	  (or (zerop (logand var (expt 2 0)))
	    (progn
	      (msg "Contrôle des erreurs.\n")
	      (vla-auditinfo doc :vlax-true)
	    )
	  )

;-------------------------------------------------------------------------
; Purger les applications
;-------------------------------------------------------------------------

	  (or (zerop (logand var (expt 2 1)))
	    (progn
	      (msg "Purger les applications.")
	      (vlax-for dic (vla-get-registeredapplications doc)
		(eff dic)
	      )
	      (msg (strcat "   " (itoa tot) " application(s) effacée(s).\n"))
	    )
	  )

;-------------------------------------------------------------------------
; Effacer les lignes/polylignes/splines/arcs/etc... de longueur 0
;-------------------------------------------------------------------------

	  (or (zerop (logand var (expt 2 2)))
	    (progn
	      (msg "Recherche lignes/polylignes/splines/arcs/etc... de longueur 0.")
	      (vlax-for bl (vla-get-blocks doc)
		(or (wcmatch (vla-get-name bl) "*|*")
		  (vlax-for ent bl
		    (setq lgt 1.0)
		    (cond
		      ((member (vla-get-objectname ent) '("AcDbLine" "AcDb3dPolyline" "AcDbPolyline"))
			(setq lgt (vla-get-length ent))
		      )
		      ((eq (vla-get-objectname ent) "AcDbArc")
			(setq lgt (vla-get-arclength ent))
		      )
		      ((eq (vla-get-objectname ent) "AcDbCircle")
			(setq lgt (vla-get-circumference ent))
		      )
		      ((member (vla-get-objectname ent) '("AcDbSpline" "AcDbEllipse"))
			(setq lgt (vlax-curve-getdistatparam ent (vlax-curve-getendparam ent)))
		      )
		      ((eq (vla-get-objectname ent) "AcDbMline")
			(setq n 0 pt2 nil lgt 0)
			(while (nth n (setq lst (vlax-get ent 'Coordinates)))
			  (setq pt1 (list (nth n lst)(nth (1+ n) lst)(nth (+ n 2) lst)))
			  (and pt2
			    (setq lgt (+ lgt (distance pt1 pt2)))
			  )
			  (setq pt2 pt1
				n (+ n 3)
			  )
			)
		      )
		      ((member (vla-get-objectname ent) '("AcDbMPolygon" "AcDbRegion"))
			(setq lgt (vla-get-perimeter ent))
		      )
		    )
		    (and (zerop lgt)
			 (setq tot (1+ tot))
		      (vla-delete ent)
		    )
		  )
		)
	      )
	      (msg (strcat "   " (itoa tot) " ligne(s)/polyligne(s)/spline(s)/arc(s)/etc... de longueur 0 effacé(s).\n"))
	    )
	  )

;-------------------------------------------------------------------------
; Effacer les textes vide
;-------------------------------------------------------------------------

	  (or (zerop (logand var (expt 2 3)))
	    (progn
	      (msg "Recherche des textes vides.")
	      (vlax-for bl (vla-get-blocks doc)
		(or (wcmatch (vla-get-name bl) "*|*")
		  (vlax-for ent bl
		    (and (member (vla-get-objectname ent) '("AcDbText" "AcDbMText"))
			 (member (vla-get-textstring ent) '("" " " "  "))
			 (setq tot (1+ tot))
		      (vla-delete ent)
		    )
		  )
		)
	      )
	      (msg (strcat "   " (itoa tot) " texte(s) vide effacé(s).\n"))
	    )
	  )

;-------------------------------------------------------------------------
; Effacer les caractères nom imprimable
; Merci à BIGC-ROMU pour le lisp et à celui qui l'a écrit (gile)
; Modifié pour l'adapter au lisp global
;-------------------------------------------------------------------------

	  (or (zerop (logand var (expt 2 18)))
	    (progn
	      (msg "Recherche des caractères non imprimable.")
	      (and (ssget "_x" '((0 . "TEXT,MTEXT,INSERT")))
		(progn
		  (vlax-for ent (setq sel (vla-get-activeselectionset doc))
		    (if (= (vla-get-objectname ent) "AcDbBlockReference")
		      (foreach att (vlax-invoke ent 'getattributes)
			(or (eq (setq txt (RightCleanText (vla-get-textstring att))) (vla-get-textstring att))
			  (vla-put-textstring att txt)
			  (setq tot (1+ tot))
			)
		      )
		      (progn
			(setq txt (RightCleanText (vla-get-textstring ent)))
			(or (eq (vla-get-textstring ent) txt)
			  (if (= "" txt)
			    (vla-delete ent)
			    (vla-put-textstring ent txt)
			  )
			  (setq tot (1+ tot))
			)
		      )
		    )
		  )
		  (vla-delete sel)
		)
	      )
	      (msg (strcat "   " (itoa tot) " attribut(s)/texte(s) modifié(s).\n"))
	    )
	  )

;-------------------------------------------------------------------------
; Effacer les wipeouts
;-------------------------------------------------------------------------

	  (or (zerop (logand var (expt 2 20)))
	    (progn
	      (msg "Recherche des wipeouts.")
	      (vlax-for bl (vla-get-blocks doc)
		(or (wcmatch (vla-get-name bl) "*|*")
		  (vlax-for ent bl
		    (and (eq (vla-get-objectname ent) "AcDbWipeout")
		      (setq tot (1+ tot))
		      (vla-delete ent)
		    )
		  )
		)
	      )
	      (msg (strcat "   " (itoa tot) " wipeout(s) effacé(s).\n"))
	    )
	  )

;-------------------------------------------------------------------------
; Effacer les blocs vides
;-------------------------------------------------------------------------

	  (or (zerop (logand var (expt 2 4)))
	    (progn
	      (msg "Recherche des blocs vides.")
	      (vlax-for bl (vla-get-blocks doc)
		(and (not (wcmatch (vla-get-name bl) "*|*"))
		     (zerop (vla-get-count bl))
		  (progn
		    (and (ssget "_x" (list (cons 0 "insert") (cons 2 (strcat "`**," (vla-get-name bl)))))
		      (progn
			(vlax-map-collection (setq sel (vla-get-activeselectionset doc)) 'vla-delete)
			(vla-delete sel)
		      )
		    )
		    (eff bl)
		  )
		)
	      )
	      (msg (strcat "   " (itoa tot) " bloc(s) vide effacé(s).\n"))
	    )
	  )

;-------------------------------------------------------------------------
; Effacer les groupes vide
;-------------------------------------------------------------------------

	  (or (zerop (logand var (expt 2 5)))
	    (progn
	      (msg "Recherche des groupes vides.")
	      (vlax-for ent (vla-get-groups doc)
		(and (zerop (vla-get-count ent))
		     (setq tot (1+ tot))
		  (vla-delete ent)
		)
	      )
	      (msg (strcat "   " (itoa tot) " groupe(s) vide effacé(s).\n"))
	    )
	  )

;-------------------------------------------------------------------------
; Effacer les images orphelines
; Merci à (gile) pour la trame du lisp
; Modifié pour l'adapter au lisp global
;-------------------------------------------------------------------------

	  (or (zerop (logand var (expt 2 17)))
	    (progn
	      (msg "Recherche des images orphelines.")
	      (or (vl-catch-all-error-p (setq dic (vl-catch-all-apply 'vla-item (list (vla-get-dictionaries doc) "ACAD_IMAGE_DICT"))))
		(progn
		  (vlax-for ent dic
		    (setq img (cons ent img))
		  )
		  (and (ssget "_x" (list (cons 0 "IMAGE")))
		    (progn
		      (vlax-for ent (setq sel (vla-get-activeselectionset doc))
			(setq img (vl-remove (vla-item dic (vla-get-name ent)) img))
		      )
		      (vla-delete sel)
		    )
		  )
		  (and img
		    (vlax-for bl (vla-get-blocks doc)
		      (or  (eq (vla-get-islayout bl) :vlax-true)
			   (wcmatch (vla-get-name bl) "*|*")
			   (zerop (vla-get-count bl))
			(vlax-for ent bl
			  (and (eq (vla-get-objectname ent) "AcDbRasterImage")
			    (setq img (vl-remove (vla-item dic (vla-get-name ent)) img))
			  )
			)
		      )
		    )
		  )
		  (mapcar '(lambda(x)(or (vl-catch-all-error-p (vl-catch-all-apply 'vla-delete (list x)))
				       (setq tot (1+ tot))
				     )
			  )
			  img
		  )
		)
	      )
	      (msg (strcat "   " (itoa tot) " image(s) orpheline(s) effacée(s).\n"))
	    )
	  )

;-------------------------------------------------------------------------
; Effacer les Xdatas
;-------------------------------------------------------------------------

	  (or (zerop (logand var (expt 2 6)))
	    (progn
	      (msg "Recherche des Xdatas.")
	      (and (setq tot 0
			 js (ssget "_x" '((-3 ("*"))))
		   )
		(while (setq ent (ssname js tot))
		  (setq ent (entget ent '("*")))
		  (foreach ele (cdr (assoc -3 ent))
		    (setq ent (subst (cons -3 (list (cons (car ele) nil))) (assoc -3 ent) ent)
			  tot (1+ tot)
		    )
		    (entmod ent)
		  )
		)
	      )
	    (msg (strcat "   " (itoa tot) " Xdata(s) effacé(s).\n"))
	    )
	  )

;-------------------------------------------------------------------------
; Effacer les proxys
;-------------------------------------------------------------------------

	  (or (zerop (logand var (expt 2 7)))
	    (progn
	      (msg "Recherche des proxys.")
	      (and (ssget "_x" (list (cons 0 "ACAD_PROXY_ENTITY")))
		(progn
		  (vlax-for ent (setq sel (vla-get-activeselectionset doc))
		    (or (vl-catch-all-error-p (vl-catch-all-apply 'vla-delete (list ent)))
		      (setq tot (1+ tot))
		    )
		  )
		  (vla-delete sel)
		)
	      )
	      (msg (strcat "   " (itoa tot) " proxy(s) effacé(s).\n"))
	    )
	  )

;-------------------------------------------------------------------------
; Effacer les zombies dans les blocs
;-------------------------------------------------------------------------

	  (or (zerop (logand var (expt 2 8)))
	    (progn
	      (msg "Recherche d'entités zombie dans les blocs.")
	      (vlax-for bl (vla-get-blocks doc)
		(or (wcmatch (vla-get-name bl) "*|*")
		  (vlax-for ent bl
		    (and (eq (vla-get-ObjectName ent) "AcDbZombieEntity")
		      (eff ent)
		    )
		  )
		)
	      )
	      (msg (strcat "   " (itoa tot) " zombie(s) effacé(s).\n"))
	    )
	  )

;-------------------------------------------------------------------------
; Effacer les zombies dans les dictionnaires
;-------------------------------------------------------------------------

	  (or (zerop (logand var (expt 2 9)))
	    (progn
	      (msg "Recherche d'entités zombie dans les dictionnaires.")
	      (vlax-for dic (vla-get-dictionaries doc)
		(if (eq (vla-get-objectname dic) "AcDbZombieObject")
		  (eff dic)
		  (and (vlax-property-available-p dic 'count)
		    (vlax-for ent dic
		      (and (eq (vla-get-objectname ent) "AcDbZombieObject")
			(eff ent)
		      )
		    )
		  )
		)
	      )
	      (msg (strcat "   " (itoa tot) " zombie(s) effacé(s).\n"))
	    )
	  )

;-------------------------------------------------------------------------
; Purger la liste d'échelles
;-------------------------------------------------------------------------

	  (or (zerop (logand var (expt 2 10)))
	    (progn
	      (msg "Recherche des échelles.")
	      (setq lst_ech (liste_echelles))
	      (vlax-for ent (setq dic (vla-item (vla-get-dictionaries doc) "ACAD_SCALELIST"))
		(or (member (cdr (assoc 300 (entget (vlax-vla-object->ename ent)))) lst_ech) (eff ent))
	      )
;;;	      (and (zerop (vla-get-count dic))
;;;		(dictadd (vlax-vla-object->ename dic)
;;;			 "A0"
;;;			 (entmakex (list (cons 0   "SCALE")
;;;					 (cons 100 "AcDbScale")
;;;					 (cons 300 "1:1")
;;;					 (cons 140 1.0)
;;;					 (cons 141 1.0)
;;;				   )
;;;			 )
;;;		)
;;;	      )
	      (msg (strcat "   " (itoa tot) " échelle(s) effacée(s).\n"))
	    )
	  )

;-------------------------------------------------------------------------
; Effacer les liens dxe
;-------------------------------------------------------------------------

	  (or (zerop (logand var (expt 2 11)))
	    (progn
	      (msg "Recherche des liens dxe.")
	      (msg (strcat "   " (itoa (del_dic "ACAD_DATALINK")) " lien(s) dxe effacé(s).\n"))
	    )
	  )

;-------------------------------------------------------------------------
; Effacer les filtres de calques
;-------------------------------------------------------------------------

	  (or (zerop (logand var (expt 2 12)))
	    (progn
	      (msg "Recherche des filtres de calques.")
	      (msg (strcat "   " (itoa (del_dic "ACAD_LAYERFILTERS")) " filtre(s) effacé(s).\n"))
	    )
	  )

;-------------------------------------------------------------------------
; Remplacer les polices de caratères non trouvés
; Merci à (gile) pour la trame du lisp
; Modifié pour l'adapter au lisp global
;-------------------------------------------------------------------------

	  (or (zerop (logand var (expt 2 19)))
	    (progn
	      (msg "Recherche des polices de caractères inconnues du dessin.")
	      (vlax-for ent (vla-get-textstyles doc)
		(and (not (wcmatch (vla-get-name ent) "*|*"))
		     (= (logand 1 (cdr (assoc 70 (tblsearch "STYLE" (vla-get-name ent))))) 1)
		     (setq txt (vl-filename-extension (vla-get-fontFile ent)))
		     (= (strcase txt) ".SHX")
		     (not (findfile txt))
		     (setq tot (1+ tot))
		  (vla-put-fontFile ent "ltypeshp.shx")
		)
	      )
	      (msg (strcat "   " (itoa tot) " police(s) remplacée(s) par \"ltypeshp.shx\".\n"))
	    )
	  )

;-------------------------------------------------------------------------
; Nettoyer les dictionnaires
;-------------------------------------------------------------------------

	  (or (zerop (logand var (expt 2 13)))
	    (progn
	      (msg "Nettoyer les dictionnaires.")
	      (vlax-for dic (vla-get-dictionaries doc)
		(or (not (vlax-property-available-p dic 'name))
		    (eq (vla-get-name dic) "ACAD_SCALELIST")
		  (eff dic)
		)
	      )
	      (msg (strcat "   " (itoa tot) " élément(s) de dictionnaire(s) effacé(s).\n"))
	    )
	  )

;-------------------------------------------------------------------------
; Effacer les dictionnaires vide
;-------------------------------------------------------------------------

	  (or (zerop (logand var (expt 2 14)))
	    (progn
	      (msg "Recherche des dictionnaires vides.")
	      (vlax-for dic (vla-get-dictionaries doc)
		(and (eq (vla-get-objectname dic) "AcDbDictionary")
		     (zerop (vla-get-count dic))
		  (eff dic)
		)
	      )
	      (msg (strcat "   " (itoa tot) " dictionnaire(s) vide effacé(s).\n"))
	    )
	  )

;-------------------------------------------------------------------------
; Effacer les dictionnaires
;-------------------------------------------------------------------------

	  (or (zerop (logand var (expt 2 15)))
	    (progn
	      (msg "Recherche des dictionnaires.")
	      (vlax-for dic (vla-get-dictionaries doc)
		(or (not (vlax-property-available-p dic 'name))
		    (eq (vla-get-name dic) "ACAD_SCALELIST")
		  (eff dic)
		)
	      )
	      (msg (strcat "   " (itoa (- tdi (vla-get-count (vla-get-dictionaries doc)))) " dictionnaire(s) effacé(s).\n"))
	    )
	  )

;-------------------------------------------------------------------------
; Reverrouilles les calques
;-------------------------------------------------------------------------

	  (mapcar '(lambda(x) (vla-put-lock (car x) (cdr x))) lst_lay)

;-------------------------------------------------------------------------
; Purger le dessin
;-------------------------------------------------------------------------

	  (or (zerop (logand var (expt 2 16)))
	    (progn
	      (msg "Purger le dessin.")
	      (or (vl-catch-all-error-p (setq dic (vl-catch-all-apply 'vla-item (list (vla-get-dictionaries doc) "ACAD_TABLESTYLE"))))
		(vlax-for ent dic
		  (or (eq (vla-get-name ent) "Standard") (eff ent))
		)
	      )
	      (or (vl-catch-all-error-p (setq dic (vl-catch-all-apply 'vla-item (list (vla-get-dictionaries doc) "ACAD_MLEADERSTYLE"))))
		(vlax-for ent dic
		  (eff ent)
		)
	      )
	      (and (vlax-property-available-p doc 'MATERIALS)
		(vlax-for ent (vla-get-materials doc)
		  (eff ent)
		)
	      )
	      (repeat 3 (vla-purgeall doc))
	    )
	  )

;-------------------------------------------------------------------------
; Boite de dialogue
;-------------------------------------------------------------------------
	  (and bda
	    (progn
	      (mode_tile "accept" 0)
	      (action_tile "accept" "(done_dialog)")
	      (start_dialog)
	      (unload_dialog (car dcl))
	      (vl-file-delete (cdr dcl))
	    )
	  )
	)
      )
    )
  )
  (vla-endundomark doc)
  (setq *error* s)
  (princ)
)

(defun c:jav()
  (if (zerop (getvar "cmdactive"))
    (patrick:jav T)
    (patrick:jav nil)
  )
)

(defun c:-jav()
  (patrick:jav nil)
)

(setq nom_lisp "JAV")
(if (/= app nil)
  (if (= (strcase (substr app (1+ (- (strlen app) (strlen nom_lisp))) (strlen nom_lisp))) nom_lisp)
    (princ (strcat "..." nom_lisp " chargé."))
    (princ (strcat "\n" nom_lisp ".LSP Chargé.....Tapez " nom_lisp " pour l'éxecuter.")))
  (princ (strcat "\n" nom_lisp ".LSP Chargé......Tapez " nom_lisp " pour l'éxecuter.")))
(setq nom_lisp nil)
(princ)