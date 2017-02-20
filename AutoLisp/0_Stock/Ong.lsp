;;;=================================================================
;;;
;;; ONG.LSP V3.10
;;;
;;; Affichage des Présentations
;;;
;;; Copyright (C) Patrick_35
;;;
;;;=================================================================

(defun c:ong(/ cmd position resultat s *errong* affiche_ong tmp file)

  ;;; =================================================================
  ;;;
  ;;;  ONG.DCL V3.10
  ;;;
  ;;;  Copyright (C) Patrick_35
  ;;;
  ;;; =================================================================

  (setq tmp  (vl-filename-mktemp "Tmp.dcl")
	file (open tmp "w"))

  (write-line
	"ong : dialog {
	  key = \"total\";
	  is_cancel = true;
	  allow_accept = true;
	  : row {
	    : list_box {key = \"present\"; height = 20; width = 20;multiple_select = true;}
	    : boxed_column {
	      width = 25;
	      label = \"Onglets\";
	      : button {key = \"choisir\";  label = \"Choisir\";}
	      : button {key = \"renommer\"; label = \"Renommer\";}
	      : button {key = \"effacer\";  label = \"Effacer\";}
	      : button {key = \"nouveau\";  label = \"Nouveau\";}
	      : button {key = \"gabarit\";  label = \"Gabarit\";}
	      : button {key = \"copier\";   label = \"Copier\";}
	      spacer;
	    }
	    : column {
	      : boxed_column {
	        width = 25;
	      label = \"Mise en page / Impression\";
	        : button {key = \"page\";   label = \"Mise en page\";}
	        : button {key = \"tracer\"; label = \"Imprimer\";}
	        : toggle {key = \"inv\"; 	  label = \"Inverser impression\";}
	      }
	      : boxed_column {
	      label = \"Lisps externes\";
	        : button {key = \"oog\";	  label = \"Organiser Onglets\";}
	        : button {key = \"mim\";	  label = \"Propager Imprimante\";}
	        : button {key = \"mpl\"; 	  label = \"Propager Config.Imp\";}
	      spacer;
	      }
	    }
	  }
	  spacer;
	  ok_button;
	}

	edi : dialog{
	  key = texte;
	  : row {
	    : edit_box {key = select; width = 20; allow_accept = true;}
	  }
	  spacer;
	  ok_cancel;
	}"
	file)
  (close file)

  ;;;---------------------------------------------------------------
  ;;;
  ;;; Gestion des erreurs
  ;;;
  ;;;---------------------------------------------------------------

  (defun *errong* (msg)
    (if (/= msg "Function cancelled")
      (if (= msg "quit / exit abort")
	(princ)
	(princ (strcat "\nErreur : " msg))
      )
      (princ)
    )
    (setq *error* s)
    (setvar "cmdecho" cmd)
    (princ)
  )

  ;;;---------------------------------------------------------------
  ;;;
  ;;; Routine principale
  ;;;
  ;;;---------------------------------------------------------------

  (defun affiche_ong(/ ch choix fi fichier init_ong inv liste_lay n ptxy r resultat sel initialisation)

    (defun initialisation(pos / lay lst)
      (setq n 0 liste_lay nil)
      (vlax-for lay (vla-get-layouts (vla-get-activedocument (vlax-get-acad-object)))
	(setq lst (append lst (list (cons (vla-get-taborder lay) lay))))
      )
      (while (assoc n lst)
	(setq liste_lay (append liste_lay (list (vla-get-name (cdr (assoc n lst))))))
	(if (eq (vla-get-name (cdr (assoc n lst))) (getvar "ctab"))
	  (setq position (itoa n))
	)
	(setq n (1+ n))
      )
    )

    (setq Cle_Patrick_35 "HKEY_CURRENT_USER\\Software\\Autodesk\\Autocad\\Patrick_35")
    (if (setq ch (findfile "Ong.ini"))
      (progn
	(setq fichier (open ch "r"))
	(setq ptxy (list (atoi (read-line fichier))(atoi (read-line fichier))))
	(close fichier)
      )
      (setq ptxy '(-1 -1))
    )
    (setq init_ong (load_dialog tmp))
    (initialisation 1)
    (while (/= resultat 1)
      (new_dialog "ong" init_ong "" ptxy)
      (setq n 0)
      (start_list "present")
      (while (nth n liste_lay)
	(add_list (nth n liste_lay))
	(setq n (1+ n))
      )
      (end_list)
      (set_tile "total" "Gestion des Onglets V3.10")
      (set_tile "present" position)
      (if (not c:oog)
	(mode_tile "oog" 1)
      )
      (if (not c:mim)
	(mode_tile "mim" 1)
      )
      (if (not c:mpl)
	(mode_tile "mpl" 1)
      )
      (mode_tile "present" 2)
      (action_tile "present"   "(setq position $value)")
      (action_tile "inv"       "(setq inv $value)")
      (action_tile "choisir"   "(done_dialog 2)")
      (action_tile "renommer"  "(done_dialog 3)")
      (action_tile "effacer"   "(done_dialog 4)")
      (action_tile "nouveau"   "(done_dialog 5)")
      (action_tile "gabarit"   "(done_dialog 6)")
      (action_tile "copier"    "(done_dialog 7)")
      (action_tile "page"      "(done_dialog 8)")
      (action_tile "tracer"    "(done_dialog 9)")
      (action_tile "trier"     "(done_dialog 10)")
      (action_tile "oog"       "(done_dialog 11)")
      (action_tile "mim"       "(done_dialog 12)")
      (action_tile "mpl"       "(done_dialog 13)")
      (action_tile "accept"    "(setq ptxy (done_dialog 1))")
      (action_tile "cancel"    "(setq ptxy (done_dialog 0))")
      (setq resultat (start_dialog))
      (cond
	((= resultat 2)
	  (setq position (itoa (read position)))
	  (command "_.layout" "_set" (nth (atoi position) liste_lay))
	)
	((= resultat 3)
	  (setq sel position)
	  (if (= (read position) 0)
	    (alert "Impossible de renommer l'espace Objet")
	    (progn
	      (while (setq n (read sel))
		(setq choix (nth n liste_lay))
		(new_dialog "edi" init_ong)
		(set_tile "texte" "Renommer")
		(set_tile "select" choix)
		(mode_tile "select" 2)
		(action_tile "select" "(setq choix $value)")
		(action_tile "accept" "(done_dialog 1)")
		(action_tile "cancel" "(done_dialog 0)")
		(setq r (start_dialog))
		(if (= r 1)
		  (progn
		    (if (vl-position choix liste_lay)
		      (alert (strcat "Présentation " choix " existante"))
		      (if (and choix (/= choix "") (/= choix (nth n liste_lay)))
			(progn
			  (command "_.layout" "_ren" (nth n liste_lay) choix)
			  (setq liste_lay (subst choix (nth n liste_lay) liste_lay))
			)
		      )
		    )
		  )
		)
		(setq sel (substr sel (+ 2 (strlen (itoa n)))))
	      )
	    )
	  )
	)
	((= resultat 4)
	  (if (= (read position) 0)
	    (alert "Impossible d'effacer l'espace Objet")
	    (progn
	      (setq sel position)
	      (while (setq n (read sel))
		(command "_.layout" "_del" (nth n liste_lay))
		(setq sel (substr sel (+ 2 (strlen (itoa n)))))
	      )
	      (initialisation 0)
	      (while (not (nth (atoi position) liste_lay))
		(setq position (itoa (1- (atoi position))))
	      )
	    )
	  )
	)
	((= resultat 5)
	  (setq position (itoa (read position)))
	  (command "_.layout" "_new" "")
	  (initialisation 0)
	)
	((= resultat 6)
	  (if (setq fichier (getfiled "Choisissez un fichier Gabarit" "template\\" "dwt" 4))
	    (progn
	      (command "_.layout" "_temp" fichier "*")
	      (initialisation 0)
	    )
	  )
	)
	((= resultat 7)
	  (setq sel position)
	  (while (setq n (read sel))
	    (command "_.layout" "_copy" (nth n liste_lay) "")
	    (setq sel (substr sel (+ 2 (strlen (itoa n)))))
	  )
	  (initialisation 0)
	)
	((= resultat 8)
	  (setq sel position)
	  (while (setq n (read sel))
	    (command "_.layout" "_set" (nth n liste_lay))
	    (command "_.pagesetup")
	    (setq sel (substr sel (+ 2 (strlen (itoa n)))))
	  )
	)
	((= resultat 9)
	  (setq sel position pos "")
	  (if (eq inv "1")
	    (progn
	      (while (not (eq sel ""))
		(setq pos (strcat (itoa (read sel)) " " pos))
		(setq sel (substr sel (+ 2 (strlen (itoa (read sel)))) (strlen sel)))
	      )
	      (setq sel (substr pos 1 (1- (strlen pos))))
	    )
	  )
	  (while (setq n (read sel))
	    (command "_plot" "n" (nth n liste_lay) "" "" "n" "n" "o")
	    (setq sel (substr sel (+ 2 (strlen (itoa n)))))
	  )
	)
	((= resultat 10)
	  (setq liste_lay (append (list "Model") (acad_strlsort (cdr liste_lay))))
	)
	((= resultat 11)
	  (c:oog)
	)
	((= resultat 12)
	  (c:mim)
	)
	((= resultat 13)
	  (c:mpl)
	)
      )
    )
    (unload_dialog init_ong)
    (setq fichier (open (vl-string-subst "Ong.ini" "Ong.lsp" (findfile "Ong.lsp")) "w"))
    (write-line (itoa (car ptxy)) fichier)
    (write-line (itoa (cadr ptxy)) fichier)
    (close fichier)
  )

  ;;;---------------------------------------------------------------
  ;;;
  ;;; Routine de lancement
  ;;;
  ;;;---------------------------------------------------------------

  (setq s *error*)
  (setq *error* *errong*)
  (setq cmd (getvar "cmdecho"))
  (setvar "cmdecho" 0)
  (command "_.undo" "_group")
  (affiche_ong)
  (vl-file-delete tmp)
  (command "_.undo" "_end")
  (setq *error* s)
  (setvar "cmdecho" cmd)
  (princ)
)

(setq nom_lisp "ONG")
(if (/= app nil)
  (if (= (strcase (substr app (1+ (- (strlen app) (strlen nom_lisp))) (strlen nom_lisp))) nom_lisp)
    (princ (strcat "..." nom_lisp " chargé."))
    (princ (strcat "\n" nom_lisp ".LSP Chargé......Tapez " nom_lisp " pour l'éxecuter.")))
  (princ (strcat "\n" nom_lisp ".LSP Chargé......Tapez " nom_lisp " pour l'éxecuter.")))
(setq nom_lisp nil)
(princ)
