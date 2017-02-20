;;NE PAS EFFACER CES LIGNES : CHEMIN DE RECHERCHE
(vl-bb-set
  'chemin_file
  "C:/Documents and Settings/canuel e/Mes documents/Acad - Bibliothèque/Bibliothèque Lisp"
  )
(vl-bb-set
  'chemin_tcg
  "C:/Documents and Settings/canuel e/Mes documents/Acad - Bibliothèque/Bibliothèque Lisp/1 - Autoload"
  )
;;------------------------------------------------------------------------------------
;;------------------------------------------------------------------------------------
;;------------------------------------------------------------------------------------
;;par Ludwig avec l'aide des membres de CadXP
(defun TOUT_CHARGER
		     (mod	     /		    get-subdirs	   get-dir	  get-files-subdirs
		      Listerror	     EXTENSIONS_A_CHARGER	   EXT		  Right
		      charg	     nb_file)

  ;;ListFilesFullPath

  (defun get-subdirs  (path)
    (apply (quote append)
	   (cons (list path)
		 (mapcar '(lambda (x) (get-subdirs (strcat path x "/")))
			 (cddr (vl-directory-files path nil -1))))))

  (defun get-dir  (pth che / func file)
    (defun func	 (path cherche / dir)
      (setq dir (cddr (vl-directory-files path nil -1)))
      (mapcar '(lambda (x)
		 (if (vl-string-search (strcase cherche) (strcase x))
		   (setq file (cons (strcat path x "/") file)
			 dir  'nil)
		   (func (strcat path x "/") cherche)
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
  ;;------------------------------------------------------------------------------------
  (vl-load-com)
  (setq cmde (getvar "cmdecho"))
  (setvar "cmdecho" 0)

  (setq	ListFilesFullPath
	 '()
	Listerror '())

  (setq	EXTENSIONS_A_CHARGER
	 (list "fas" "dvb" "lsp" "arx")
	nb_file	0)
  ;;------------------------------------------------------------------------------------
  (setq	file_T (vl-directory-files (vl-bb-ref 'chemin_file) "TOUT_CHARGER.lsp" 1)
	tcg_T  (vl-file-directory-p (vl-bb-ref 'chemin_tcg)))
  ;;------------------------------------------------------------------------------------
  (setq	file (if (or (not file_T) mod)
	       (getfiled "Emplacement du fichier du fichier TOUT_CHARGER.lsp" "TOUT_CHARGER" "lsp" 8)
	       (strcat (vl-bb-ref 'chemin_file) "/TOUT_CHARGER.lsp"))
	tcg  (if (or (not tcg_T) mod)
	       (vl-string-subst "/" "\\" (acet-ui-pickdir))
	       (vl-bb-ref 'chemin_tcg)))
  (if (or (not file_T) (not tcg_T) mod)
    (progn
      (setq tmp	   (vl-filename-mktemp "tmp.dat")
	    flag_w (open tmp "w")
	    flag_r (open file "r")
	    line   (read-line flag_r)
	    i	   0)
      (while line
	(cond
	  ((= i 4) (write-line (strcat "\"" file "\"") flag))
	  ((= i 8) (write-line (strcat "\"" tcg "\"") flag))
	  ('t (write-line line flag_r)))
	(setq i	   (1+ i)
	      line (read-line flag_r)))
      (close flag_w)
      (close flag_r)
      (vl-file-delete file)
      (vl-file-copy tmp file 'nil)
      (vl-bb-set 'chemin_file (vl-string-subst "" "/TOUT_CHARGER.lsp" file))
      (vl-bb-set 'chemin_tcg tcg)
      )
    )
  ;;------------------------------------------------------------------------------------
  (setq tcg (strcat tcg "/"))
  ;;------------------------------------------------------------------------------------
  (foreach EXT	EXTENSIONS_A_CHARGER
    (get-files-subdirs
      tcg
      (strcat "*." EXT)
      )
    )
  ;;------------------------------------------------------------------------------------
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
;;------------------------------------------------------------------------------------
;;------------------------------------------------------------------------------------
;;------------------------------------------------------------------------------------
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
