;;;=================================================================
;;;
;;; OOG.LSP V1.10
;;;
;;; Réorganiser les onglets
;;;
;;; Copyright (C) Patrick_35
;;;
;;;=================================================================

(defun c:oog(/ s *erroog* MsgBox organiser_onglets)
  
  ;;; =================================================================
  ;;;
  ;;;  OOG.DCL V1.10
  ;;;
  ;;;  Copyright (C) Patrick_35
  ;;;
  ;;; =================================================================

  (setq tmp  (vl-filename-mktemp "Tmp.dcl")
	file (open tmp "w"))

  (write-line
      "oog : dialog {
	    key = \"titre\";
	    fixed_width = true;
	    alignment = centered;
	    is_cancel = true;
	    width = 50;
	    : list_box {label= \"Liste des Onglets\" ; key=\"oog\"; height = 10;}
	    spacer;
	    : edit_box {label= \"Position\"; key=pos;}
	    : text {key=\"txt\";}
	    spacer;
	    : row {
	      ok_cancel;
	      spacer;
	      : button {label = \"Haut\"; key = \"haut\";}
	      spacer;
	      : button {label = \"Bas\";  key = \"bas\";}
	    }
	  }"
      file
      )
  (close file)

  ;;;---------------------------------------------------------------
  ;;;
  ;;; Gestion des erreurs
  ;;;
  ;;;---------------------------------------------------------------

  (defun *erroog* (msg)
    (if (/= msg "Function cancelled")
      (if (= msg "quit / exit abort")
	(princ)
	(princ (strcat "\nErreur : " msg))
      )
      (princ)
    )
    (setq *error* s)
    (vla-endundomark (vla-get-activedocument (vlax-get-acad-object)))
    (princ)
  )

  ;;;---------------------------------------------------------------
  ;;;
  ;;; Message
  ;;;
  ;;;---------------------------------------------------------------

  (defun MsgBox (Titre Bouttons Message / Reponse WshShell)
    (vl-load-com)  
    (setq WshShell (vlax-create-object "WScript.Shell"))
    (setq Reponse  (vlax-invoke WshShell 'Popup Message 7 Titre (itoa Bouttons)))
    (vlax-release-object WshShell)
    Reponse
  )

  ;;;---------------------------------------------------------------
  ;;;
  ;;; Routine principale
  ;;;
  ;;;---------------------------------------------------------------

  (defun organiser_onglets(/ haut_bas init_oog lay lays lst n old ordre position affiche_liste affiche_position changer_position)

    (defun affiche_liste(/ num str)
      (setq num 0)
      (start_list "oog")
      (while (setq val (nth num ordre))
	(setq num (1+ num)
	      str (itoa num)
	)
	(while (< (strlen str) 3)
	  (setq str (strcat "0" str))
	)
	(add_list (strcat str " - " (vla-get-name val)))
      )
      (end_list)
    )

    (defun affiche_position()
      (set_tile "pos" (itoa (vla-get-taborder (nth (atoi position) ordre))))
      (set_tile "txt" "")
      (if (eq position "0")
	(mode_tile "haut" 1)
	(mode_tile "haut" 0)
      )
      (if (eq (1+ (atoi position)) (length ordre))
	(mode_tile "bas" 1)
	(mode_tile "bas" 0)
      )
    )

    (defun changer_position(ou / lay loc lst)
      (if (not (zerop (setq loc (abs (atoi ou)))))
	(progn
	  (set_tile "txt" "")
	  (if (> loc (length ordre))
	    (setq loc (length ordre))
	  )
	  (setq lay (nth (atoi position) ordre) ordre (vl-remove lay ordre) n 0)
	  (while (< n (1- loc))
	    (setq lst (append lst (list (nth n ordre))))
	    (setq n (1+ n))
	  )
	  (setq lst (append lst (list lay)))
	  (while (nth n ordre)
	    (setq lst (append lst (list (nth n ordre))))
	    (setq n (1+ n))
	  )
	  (vla-put-taborder lay loc)
	  (setq position (itoa (1- loc)) ordre lst)
	  (affiche_liste)
	  (set_tile "oog" position)
	  (affiche_position)
	  (mode_tile "pos" 3)
	)
	(progn
	  (if (eq (type (read ou)) 'INT)
	    (set_tile "txt" "Veuillez saisir une valeur autre que zéro")
	    (set_tile "txt" "Veuillez saisir une valeur numérique")
	  )
	  (set_tile "pos" (itoa (1+ (atoi position))))
	  (mode_tile "pos" 3)
	)
      )
    )

    (defun haut_bas(chg / pos)
      (setq pos (atoi position))
      (cond
	((and chg (/= pos 0))
	  (changer_position (itoa pos))
	)
	((and (not chg) (/= (length ordre) (1+ pos)))
	  (changer_position (itoa (+ pos 2)))
	)
      )
    )

    (if tmp
      (progn
	(setq init_oog (load_dialog tmp)
	      lays (vla-get-layouts (vla-get-activedocument (vlax-get-acad-object)))
	      position "0" n 1 resultat 2)
	(vlax-for lay lays
	  (setq lst (append lst (list (cons (vla-get-taborder lay) lay))))
	)
	(while (assoc n lst)
	  (setq ordre (append ordre (list (cdr (assoc n lst)))))
	  (setq n (1+ n))
	)
	(setq old ordre)
	(new_dialog "oog" init_oog "")
	(set_tile "titre" "Réorganisation des Onglets V1.10")
	(affiche_liste)
	(set_tile "oog" "0")
	(affiche_position)
	(mode_tile "cancel" 2)
	(while (and (/= resultat 1)(/= resultat 0))
	  (action_tile "oog"    "(setq position $value)(affiche_position)")
	  (action_tile "pos"    "(changer_position $value)")
	  (action_tile "haut"   "(haut_bas T)")
	  (action_tile "bas"    "(haut_bas nil)")
	  (action_tile "accept" "(done_dialog 1)")
	  (action_tile "cancel" "(done_dialog 0)")
	  (setq resultat (start_dialog))
	)
	(unload_dialog init_oog)
	(if (and (= resultat 0) (/= old ordre))
	  (progn
	    (setq n 0)
	    (while (nth n old)
	      (vla-put-taborder (nth n old) (1+ n))
	      (setq n (1+ n))
	    )
	  )
	)
      )
      (msgbox "OOG" 16 "Fichier OOG.DCL introuvable")
    )
  )

  ;;;---------------------------------------------------------------
  ;;;
  ;;; Routine de lancement
  ;;;
  ;;;---------------------------------------------------------------

  (vl-load-com)
  (setq s *error*)
  (setq *error* *erroog*)
  (vla-startundomark (vla-get-activedocument (vlax-get-acad-object)))
  (organiser_onglets)
  (vl-file-delete tmp)
  (vla-endundomark (vla-get-activedocument (vlax-get-acad-object)))
  (setq *error* s)
  (princ)
)

(setq nom_lisp "OOG")
(if (/= app nil)
  (if (= (strcase (substr app (1+ (- (strlen app) (strlen nom_lisp))) (strlen nom_lisp))) nom_lisp)
    (princ (strcat "..." nom_lisp " chargé."))
    (princ (strcat "\n" nom_lisp ".LSP Chargé.....Tapez " nom_lisp " pour l'éxecuter.")))
  (princ (strcat "\n" nom_lisp ".LSP Chargé......Tapez " nom_lisp " pour l'éxecuter.")))
(setq nom_lisp nil)
(princ)