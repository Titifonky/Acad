;;par Ludwig avec l'aide des membres de CadXP
(defun TOUT_CHARGER
       (mod / get-subdirs get-dir get-files-subdirs  Listerror EXTENSIONS_A_CHARGER EXT Right charg nb_file)

  ;;ListFilesFullPath
  
  (defun get-subdirs  (path)
    (apply (quote append)
	   (cons (list path)
		 (mapcar '(lambda (x) (get-subdirs (strcat path x "\\")))
			 (cddr (vl-directory-files path nil -1))))))
  
  (defun get-dir  (pth che / func file)
    (defun func	 (path cherche / dir)
      (setq dir (cddr (vl-directory-files path nil -1)))
      (mapcar '(lambda (x)
		 (if (vl-string-search (strcase cherche) (strcase x))
		   (setq file (cons (strcat path x "\\") file)
			 dir  'nil)
		   (func (strcat path x "\\") cherche)
		   ))
	      dir)
      file
      )
    (func pth che)
    )

  (defun get-files-subdirs  (RootPath ext)
    (foreach path  (get-subdirs RootPath)
      (setq ListFilesFullPath
	     (append ListFilesFullPath
		     (mapcar '(lambda (fileName) (strcat path fileName))
			     (vl-directory-files path ext 1))))))

  (setq cmde (getvar "cmdecho"))
  (setvar "cmdecho" 0)
  (vl-load-com)
  (setq	ListFilesFullPath
	 '()
	Listerror '())

  (setq	EXTENSIONS_A_CHARGER
	 (list "fas" "dvb" "lsp" "arx")
	nb_file	0)

  (cond
    (mod
     (setq REPERTOIRE_DE_RECHERCHE (strcat (acet-ui-pickdir) "\\")))
    ((vl-bb-ref 'chemin_tcg)
     (setq REPERTOIRE_DE_RECHERCHE (vl-bb-ref 'chemin_tcg)))
    ((not REPERTOIRE_DE_RECHERCHE)
     (setq path "C:\\Documents and Settings\\")
     (foreach n	 (cddr (vl-directory-files path nil -1))
       (if (and (not REPERTOIRE_DE_RECHERCHE) (vl-string-search "canuel e" n))
	 (setq REPERTOIRE_DE_RECHERCHE
		(car (get-dir
		       (strcat "C:\\Documents and Settings\\" n "\\")
		       "1 - Autoload")))
	 )
       )
     (if REPERTOIRE_DE_RECHERCHE
       (vl-bb-set 'chemin_tcg REPERTOIRE_DE_RECHERCHE)
       (progn (princ "\nAucun repertoire \"1 - Autoload\" trouvé") (exit))
       )
     )
    )


  (foreach EXT	EXTENSIONS_A_CHARGER
    (get-files-subdirs
      REPERTOIRE_DE_RECHERCHE
      (strcat "*." EXT)
      )
    )

  (setq ListFilesFullPath (vl-sort ListFilesFullPath '(lambda (x y) (< x y))))
  
  (foreach n  ListFilesFullPath
    ;;(princ "\n/////////////////////////////////")
    (setq Right (substr n (+ (- (strlen n) 3) 1)))
    (cond
      ((= (strcase Right t) "fas") (setq charg 'vl-load-all))
      ((= (strcase Right t) "lsp") (setq charg 'vl-load-all))
      ((= (strcase Right t) "dvb") (setq charg 'vl-vbaload))
      ((= (strcase Right t) "arx") (setq charg 'arxload)))
    ((eval charg) n)
    (setq nb_file (1+ nb_file))
    )
  (princ (strcat "\n\n" (itoa nb_file) " fichiers chargés"))
  (setvar "cmdecho" cmde)
  (princ)
  )

(defun c:tcg  ()
  (tout_charger 'nil)
  )

(if (not (vl-bb-ref 'chemin_tcg))
  (progn
    (tout_charger 'nil)
    (princ "\n===== Tapez TCG pour rechargez les lisp du dossier \"1 - Autoload\" =====")
    )
  (princ "\n===== Fichiers Lisp chargé - TCG pour recharger =====")
  )


 ;|«Visual LISP© Format Options»
(100 2 40 0 nil "Fin de " 100 9 0 0 0 T T nil T)
;*** NE PAS AJOUTER de texte au-dessous du commentaire! ***|;
