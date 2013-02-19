;; fonction de transformation d'une chaine en liste
;; chaque mot séparé par un espace devient un atom
(defun str_to_lst (str / i mot list_mot)
  (while (/= (strlen str) 0)
    (setq i   1
	  mot ""
    )
    (while (not (or (= (ascii (substr str i 1)) 32) (= (ascii (substr str i 1)) 0)))
      (setq mot	(strcat mot (substr str i 1))
	    i	(1+ i)
      )
    )
    (if	(/= mot "")
      (setq list_mot (append list_mot (list mot)))
    )
    (setq str (substr str (1+ i)))
  )
  list_mot
)

;; STR2LST
;; Transforme un chaine avec séparateur en liste de chaines
;;
;; Arguments
;; str : la chaine à transformer en liste
;; sep : le séparateur

;;
;; Exemples
;; (str2lst "a b c" " ") -> ("a" "b" "c")
;; (str2lst "1,2,3" ",") -> ("1" "2" "3")
;; (mapcar 'read (str2lst "1,2,3" ",")) -> (1 2 3)

(defun str2lst (str sep / pos)
  (if (setq pos (vl-string-search sep str))
    (cons (substr str 1 pos) (str2lst (substr str (+ (strlen sep) pos 1)) sep))
    (list str)
  )
)


;; EDIT_MSG
;;
;; Fonction d'edition de message par remplacement des %!
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun edit_msg	(pat-var_list str preci / i mot list_mot pat var pos pat_list var_list)
  (setq	pat_list     (list (car pat-var_list))
	var_list     (list (cadr pat-var_list))
	pat-var_list (cddr pat-var_list)
  )
  (while pat-var_list
    (setq pat_list     (cons (car pat-var_list) pat_list)
	  var_list     (cons (cadr pat-var_list) var_list)
	  pat-var_list (cddr pat-var_list)
    )
  )
  (setq	pat_list (reverse pat_list)
	var_list (reverse var_list)
  )
  (mapcar '(lambda (x y)
	     (setq pat x
		   var y
	     )
	     (cond ((= (type var) 'INT) (setq var (itoa var)))
		   ((= (type var) 'REAL) (setq var (arr_txt preci var)))
	     )
	     (if (= (type var) 'STR)
	       (progn (setq pos 0)
		      (while (setq pos (vl-string-position (ascii "%") str pos))
			(setq str (vl-string-subst var (strcat "%" pat "!") str pos)
			      pos (1+ pos)
			)
		      )
	       )
	     )
	   )
	  pat_list
	  var_list
  )
  str
)


;; NB_STR
;;
;; Converti un objet affichable en texte
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun nb_str (obj /)
  (cond	((= (type obj) 'INT) (rtos obj 2 0))
	((= (type obj) 'REAL) (rtos obj 2))
	((= (type obj) 'STR) obj)
  )
)


;; IS-STR-NB?
;;
;; Test si une chaine est un nombre
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun is-str-nb? (str /)
  (if (= (type str) 'STR)
    (apply 'and
	   (mapcar '(lambda (x)
		      (if (or (< x 48) (> x 57))
			'nil
			't
		      )
		    )
		   (str_to_chr str)
	   )
    )
  )
)


;; STR_TO_CHR
;;
;; Converti une chaine en liste de code ascii
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun str_to_chr (str /)
  (if (> (strlen str) 1)
    (cons (ascii (substr str 1 1)) (str_to_chr (substr str 2)))
    (list (ascii str))
  )
)


;; CHR_TO_STR
;;
;; Converti une liste de code ascii en chaine de caractère
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun chr_to_str (lst /) (apply 'strcat (mapcar 'chr lst)))

;; STR_REPLACE
;;
;; Remplace une chaine par une autre dans une chaine
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun str_replace (string old new)
  (cond	((= string "") string)
	((wcmatch string (strcat old "*"))
	 (strcat new (str_replace (substr string (1+ (strlen old))) old new))
	)
	((strcat (substr string 1 1) (str_replace (substr string 2) old new)))
  )
)

;;; STRTRIM - Supprime les espaces à gauche et à droite
(defun strtrim (s)
  (cond	((/= (type s) 'str) nil)
	(t (strltrim (strrtrim s)))
  )
)

;;; STRLTRIM - Supprime les espaces à gauche
(defun strltrim	(s)
  (cond	((eq s "") s)
	((/= " " (substr s 1 1)) s)
	(t (strltrim (substr s 2)))
  )
)

;;; STRRTRIM - Supprime les espaces à droite
(defun strrtrim	(s)
  (cond	((eq s "") s)
	((/= " " (substr s (strlen s) 1)) s)
	(t (strrtrim (substr s 1 (1- (strlen s)))))
  )
)



;;;================================================================================================;;;
(princ)
