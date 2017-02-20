;;; BROWSEFORFOLDER - retourne le chemin du dossier selectionné
(defun BrowseForFolder ( invite / sh folder folderobject result)
  (vl-load-com)
  (setq	sh (vla-getInterfaceObject
	     (vlax-get-acad-object)
	     "Shell.Application"
	   )
  )
  (setq folder (vlax-invoke-method sh 'BrowseForFolder 0 invite 0))
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

;;par Ludwig avec l'aide des membres de CadXP
(defun TOUT_CHARGER  (mod	  /
		      CHARG	  CHE
		      CHEMIN_FILE CHEMIN_TCG
		      CHERCHE	  CMDE
		      DIR	  DOSSIER
		      DU	  EXT
		      EXTENSIONS_A_CHARGER
		      FILE	  FILENAME
		      FILE_T	  FLAG_R
		      FLAG_W	  FUNC
		      GET-DIR	  GET-FILES-SUBDIRS
		      GET-SUBDIRS LINE
		      LISTERROR	  LISTFILESFULLPATH
		      N		  NB_FILE
		      NEW	  OLD
		      PATH	  PTH
		      REPLACE_CH  RIGHT
		      ROOTPATH	  STRING
		      TCG	  TCG_T
		      TMP	  TST_MP
		      TST_REP)
  (vl-load-com)
  (setq	ListFilesFullPath
	 '()
	EXTENSIONS_A_CHARGER
	 '("fas" "dvb" "lsp" "arx")
	nb_file	0
	cmde (getvar "cmdecho"))
  (setvar "cmdecho" 0)
  ;;------------------------------------------------------------------------------------
  ;;Fonctions locales
  ;;------------------------------------------------------------------------------------
  (defun replace_ch  (string new old)
    (while (vl-string-search old string) (setq string (vl-string-subst new old string)))
    string)
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
		   (func (strcat path x "/") cherche)))
	      dir)
      file)
    (func pth che))
  (defun get-files-subdirs  (RootPath ext)
    (foreach path  (get-subdirs RootPath)
      (setq ListFilesFullPath
	     (append ListFilesFullPath
		     (mapcar '(lambda (fileName) (strcat path fileName))
			     (vl-directory-files path ext 1))))))
  ;;------------------------------------------------------------------------------------
  ;;ListFilesFullPath
  ;;------------------------------------------------------------------------------------
;;; Lecture du chemin dans le registre
;;; Si le chemin n'existe pas, ouvrir la boîte de dialogue pour le selectionner
  (if (or (not (setq tcg (vl-registry-read
			   "HKEY_CURRENT_USER\\Software\\Autodesk\\Perso"
			   "chemin_lisp_autoload")))
	  mod)
    (setq
      tcg (strcat (replace_ch
		    (BrowseForFolder "\nEmplacement du dossier à charger automatiquement")
		    "/"
		    "\\")
		  "/")))
;;; Ecrire le chemin dans la base de registre
  (vl-registry-write "HKEY_CURRENT_USER\\Software\\Autodesk\\Perso" "chemin_lisp_autoload" tcg)
;;; Recupère les sous-dossiers du dossier à charger
  (foreach EXT EXTENSIONS_A_CHARGER (get-files-subdirs tcg (strcat "*." EXT)))
;;; Recupère les chemins complets de chaque fichier
  (setq ListFilesFullPath (vl-sort ListFilesFullPath '(lambda (x y) (< x y))))
;;; Charge les fichiers
  (foreach n  ListFilesFullPath
    (setq Right (substr n (+ (- (strlen n) 3) 1)))
    (cond ((= (strcase Right t) "fas") (setq charg 'vl-load-all))
	  ((= (strcase Right t) "lsp") (setq charg 'vl-load-all))
	  ((= (strcase Right t) "dvb") (setq charg 'vl-vbaload))
	  ((= (strcase Right t) "arx") (setq charg 'arxload)))
    ((eval charg) n)
    (setq nb_file (1+ nb_file)))
  (setvar "cmdecho" cmde)
  (princ (strcat "\n\nDossier chargé : " tcg))
  (princ (strcat "\n" (itoa nb_file) " fichiers chargés"))
  (princ "\n===== Fichiers Lisp chargé - TCG pour recharger  =====")
  (princ "\n===== TCGG pour reinitialiser le dossiers de recherche  =====")
  (princ))

(defun c:tcg () (tout_charger 'nil))
(defun c:tcgg () (tout_charger 't))
(tout_charger 'nil)
(princ)



