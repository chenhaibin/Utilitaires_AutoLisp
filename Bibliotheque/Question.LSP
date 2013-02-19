;;;----------------------------------------------------------------------------------------------
;;;                                                                                              
;;;                                  Fonction INPUT                                              
;;;                                                                                              
;;;----------------------------------------------------------------------------------------------

(defun input (n		 prog	    /	       var_type	  var_name   var_name_suff
	      var_sv	 var_sv_value	       query	  text	     choix	i
	      test	 msg_erreur ent
	     )
  (setq	var_type      (car n)
	var_name_suff (cadr n)
	var_sv	      (strcat prog "_" var_name_suff)
	query	      (caddr n)
  )
  (if (eval (read var_sv))
    (setq var_sv_value (eval (read var_sv)))
    (setq var_sv_value nil)
  )
  (cond
;;;-----------------------------------------------
;;;          INTEGER                              
;;;-----------------------------------------------
	((= (car n) "INTEGER")
	 ;; test si la valeur existe déjà et l'insert dans la question comme valeur pas défaut si oui
	 (if var_sv_value
	   (setq query (strcat query " <" (itoa var_sv_value) "> : "))
	   (setq query (strcat query " : "))
	 )
	 ;; question
	 (setq var_name (getint query))
	)
;;;-----------------------------------------------
;;;          REAL                                 
;;;-----------------------------------------------
	((= (car n) "REAL")
	 ;; test si la valeur existe déjà et l'insert dans la question comme valeur pas défaut si oui
	 (if var_sv_value
	   (setq query (strcat query " <" (rtos var_sv_value 2) "> : "))
	   (setq query (strcat query " : "))
	 )
	 ;; question
	 (setq var_name (getdist query))
	)
;;;-----------------------------------------------
;;;          STRING                               
;;;-----------------------------------------------
	((= (car n) "STRING")
	 ;; test si la valeur existe déjà et l'insert dans la question comme valeur pas défaut si oui
	 (if var_sv_value
	   (setq query (strcat query " <" var_sv_value "> : "))
	   (setq query (strcat query " : "))
	 )
	 ;; question
	 (setq var_name (getstring T query))
	)
;;;-----------------------------------------------
;;;          LIST                                 
;;;-----------------------------------------------
	((= (car n) "LIST")
	 (setq query (strcat "\n" query " :")
	       choix (cadddr n)
	       i     0
	 )
	 (foreach text choix
	   (setq i     (1+ i)
		 query (strcat query "\n" (itoa i) " : " text)
	   )
	 )
	 (princ query)
	 ;; test si la valeur existe déjà et l'insert dans la question comme valeur pas défaut si oui
	 (if var_sv_value
	   (setq query (strcat "\nChoix <" (itoa var_sv_value) "> : "))
	   (setq query "\nChoix : ")
	 )
	 ;; question
	 (setq test 'T
	       msg_erreur
		(strcat "La valeur doit être comprise entre 1 et " (itoa i))
	 )
	 (while	test
	   (setq var_name (getint query))
	   (if (setq test (not (or (and (>= var_name 1) (<= var_name i)) (not var_name))))
	     (princ msg_erreur)
	   )
	 )
	)
;;;-----------------------------------------------
;;;          ENTITY                               
;;;-----------------------------------------------
	((= (car n) "ENT")
	 ;; test si la valeur existe déjà et l'insert dans la question comme valeur pas défaut si oui
	 (if (and var_sv_value (entget (car var_sv_value)))
	   (progn (vla-highlight (vlax-ename->vla-object (car var_sv_value)) 1)
		  (setq query (strcat query " <selection précédente> : "))
	   )
	   (setq query (strcat query " : "))
	 )
	 ;; question
	 (setq var_name (entsel query))
	)
;;;-----------------------------------------------
;;;          OUI-NON                              
;;;-----------------------------------------------
	((= (car n) "O-N")
	 ;; test si la valeur existe déjà et l'insert dans la question comme valeur pas défaut si oui
	 (if var_sv_value
	   (setq query (strcat query " (O/N) <" var_sv_value "> : "))
	   (setq query (strcat query " (O/N) : "))
	 )
	 ;; question
	 (setq test	  'T
	       msg_erreur "Repondre par Oui ou par Non\n"
	 )
	 (while	test
	   (setq var_name (strcase (getstring query)))
	   (if (setq test (not (or (member var_name '("O" "N")) (member var_sv_value '("O" "N")))))
	     (princ msg_erreur)
	   )
	 )
	)
;;;-----------------------------------------------
;;;          TYPE de variable inconnu             
;;;-----------------------------------------------
	(t (princ (strcat "Le type de la variable n'est pas pris en charge : " var_type)) (exit))
  )
;;; sortie du programme si la variable est 'nil
  (if (and (not var_name) (not var_sv_value))
    (progn (princ "\nLa valeur n'a pas été renseignée !!! sortie du programme\n") (exit))
;;; test si la valeur par defaut doir être prise
    (progn (cond ((or (= (car n) "INTEGER") (= (car n) "LIST"))
		  (if (not var_name)
		    (progn (setq var_name var_sv_value) (princ var_sv_value) (terpri))
		    (progn (princ var_name) (terpri))
		  )
		 )
		 ((= (car n) "REAL")
		  (if (not var_name)
		    (progn (setq var_name var_sv_value) (princ var_sv_value) (terpri))
		    (progn (princ var_name) (terpri))
		  )
		 )
		 ((= (car n) "ENT")
		  (if (and var_sv_value (entget (car var_sv_value)))
		    (vla-highlight (vlax-ename->vla-object (car var_sv_value)) 0)
		    (terpri)
		  )
		  (terpri)
		  (if (not var_name)
		    (setq var_name var_sv_value)
		  )
		 )
		 ((or (= (car n) "STRING") (= (car n) "O-N"))
		  (if (= var_name "")
		    (progn (setq var_name var_sv_value) (princ var_sv_value) (terpri))
		    (progn (princ var_name) (terpri))
		  )
		 )
	   )
    )
  )
  ;; Ecriture de la variable de sauvegarde
  (set (read var_sv) var_name)
)

;;;----------------------------------------------------------------------------------------------
;;;                                                                                              
;;;                                  Fonction QUESTION                                           
;;;                                                                                              
;;;----------------------------------------------------------------------------------------------

(defun question
       (n prog / var_type var_name var_sv var_sv_value query text choix i test msg_erreur ent)
  (setq	var_type (car n)
	var_name (cadr n)
	var_sv	 (strcat prog "_" var_name)
	query	 (caddr n)
  )
  (if (eval (read var_sv))
    (setq var_sv_value (eval (read var_sv)))
    (setq var_sv_value nil)
  )
  (cond
;;;-----------------------------------------------
;;;          INTEGER                              
;;;-----------------------------------------------
    ((= (car n) "INTEGER")
     ;; test si la valeur existe déjà et l'insert dans la question comme valeur pas défaut si oui
     (if var_sv_value
       (setq query (strcat query " <" (itoa var_sv_value) "> : "))
       (setq query (strcat query " : "))
     )
     ;; question
     (set (read var_name) (getint query))
    )
;;;-----------------------------------------------
;;;          REAl                                 
;;;-----------------------------------------------
    ((= (car n) "REAL")
     ;; test si la valeur existe déjà et l'insert dans la question comme valeur pas défaut si oui
     (if var_sv_value
       (setq query (strcat query " <" (rtos var_sv_value 2) "> : "))
       (setq query (strcat query " : "))
     )
     ;; question
     (set (read var_name) (getdist query))
    )
;;;-----------------------------------------------
;;;          STRING                               
;;;-----------------------------------------------
    ((= (car n) "STRING")
     ;; test si la valeur existe déjà et l'insert dans la question comme valeur pas défaut si oui
     (if var_sv_value
       (setq query (strcat query " <" var_sv_value "> : "))
       (setq query (strcat query " : "))
     )
     ;; question
     (set (read var_name) (getstring T query))
    )
;;;-----------------------------------------------
;;;          LIST                                 
;;;-----------------------------------------------
    ((= (car n) "LIST")
     (setq query (strcat "\n" query " :")
	   choix (cadddr n)
	   i	 0
     )
     (foreach text choix
       (setq i	   (1+ i)
	     query (strcat query "\n" (itoa i) " : " text)
       )
     )
     (princ query)
     ;; test si la valeur existe déjà et l'insert dans la question comme valeur pas défaut si oui
     (if var_sv_value
       (setq query (strcat "\nChoix <" (itoa var_sv_value) "> : "))
       (setq query "\nChoix : ")
     )
     ;; question
     (setq test	'T
	   msg_erreur
	    (strcat "La valeur doit être comprise entre 1 et " (itoa i))
     )
     (while test
       (set (read var_name) (getint query))
       (setq test (not (or (and (>= (eval (read var_name)) 1) (<= (eval (read var_name)) i))
			   (not (eval (read var_name)))
		       )
		  )
       )
       (if test
	 (princ msg_erreur)
       )
     )
    )
;;;-----------------------------------------------
;;;          ENTITY                               
;;;-----------------------------------------------
    ((= (car n) "ENT")
     ;; test si la valeur existe déjà et l'insert dans la question comme valeur pas défaut si oui
     (if (and var_sv_value (entget (car var_sv_value)))
       (progn (vla-highlight (vlax-ename->vla-object (car var_sv_value)) 1)
	      (setq query (strcat query " <selection précédente> : "))
       )
       (setq query (strcat query " : "))
     )
     ;; question
     (set (read var_name) (entsel query))
    )
;;;-----------------------------------------------
;;;          OUI-NON                              
;;;-----------------------------------------------
    ((= (car n) "O-N")
     ;; test si la valeur existe déjà et l'insert dans la question comme valeur pas défaut si oui
     (if var_sv_value
       (setq query (strcat query " (O/N) <" var_sv_value "> : "))
       (setq query (strcat query " (O/N) : "))
     )
     ;; question
     (setq test	      'T
	   msg_erreur "Repondre par Oui ou par Non\n"
     )
     (while test
       (set (read var_name) (strcase (getstring query)))
       (setq test
	      (not (or (member (eval (read var_name)) '("O" "N")) (member var_sv_value '("O" "N"))))
       )
       (if test
	 (princ msg_erreur)
       )
     )
    )
;;;-----------------------------------------------
;;;          TYPE de variable inconnu             
;;;-----------------------------------------------
    (t
     (princ (strcat "Le type de la variable \"" var_name "\" n'est pas pris en charge : " var_type))
     (exit)
    )
  )
  ;; sortie du programme si la variable est 'nil
  (if (and (not (eval (read var_name))) (not var_sv_value))
    (progn (princ "\nLa valeur n'a pas été renseignée !!! sortie du programme\n") (exit))
    ;; test si la valeur par defaut doir être prise
    (progn (cond ((or (= (car n) "INTEGER") (= (car n) "LIST"))
		  (if (not (eval (read var_name)))
		    (progn (set (read var_name) var_sv_value) (princ var_sv_value) (terpri))
		    (progn (princ (eval (read var_name))) (terpri))
		  )
		 )
		 ((= (car n) "REAL")
		  (if (not (eval (read var_name)))
		    (progn (set (read var_name) var_sv_value) (princ var_sv_value) (terpri))
		    (progn (princ (eval (read var_name))) (terpri))
		  )
		 )
		 ((= (car n) "ENT")
		  (if (and var_sv_value (entget (car var_sv_value)))
		    (vla-highlight (vlax-ename->vla-object (car var_sv_value)) 0)
		    (terpri)
		  )
		  (terpri)
		  (if (not (eval (read var_name)))
		    (set (read var_name) var_sv_value)
		  )
		 )
		 ((or (= (car n) "STRING") (= (car n) "O-N"))
		  (if (= (eval (read var_name)) "")
		    (progn (set (read var_name) var_sv_value) (princ var_sv_value) (terpri))
		    (progn (princ (eval (read var_name))) (terpri))
		  )
		 )
	   )
    )
  )
  ;; Ecriture de la variable de sauvegarde
  (set (read var_sv) (eval (read var_name)))
)


;;;================================================================================================;;;
(princ)
