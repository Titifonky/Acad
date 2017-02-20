;;;=================================================================
;;;
;;; RINS.LSP V2.10
;;;
;;; Redéfinir le point d'insertion des blocs
;;;
;;; Copyright (C) Patrick_35
;;;
;;;=================================================================

(defun c:rins(/ bl dec deh ent n js lst1 lst2 pt rep)
  (vl-load-com)
  (vla-startundomark (vla-get-activedocument (vlax-get-acad-object)))
  (if (not (setq js (cadr (ssgetfirst))))
    (setq js (ssget (list (cons 0 "INSERT"))))
  )
  (sssetfirst nil)
  (if js
    (progn
      (setq n 0)
      (while (setq ent (ssname js n))
	(setq ent (entget ent))
	(if (eq (cdr (assoc 0 ent)) "INSERT")
	  (if (not (member (cdr (assoc 2 ent)) lst2))
	    (setq lst1 (append lst1 (list (cdr (assoc -1 ent))))
		  lst2 (append lst2 (list (cdr (assoc  2 ent)))))
	  )
	)
	(setq n (1+ n))
      )
      (if lst1
	(progn
	  (initget "Oui Non _Yes _No")
	  (setq rep (getkword "\nDésirez-vous conserver l'emplacement actuel des blocs <O> : "))
	  (if (not rep)
	    (setq rep "Oui")
	  )
	  (foreach ent lst1
	    (redraw ent 3)
	    (while (not (setq pt (getpoint "\nVeuillez sélectionner son nouveau point de base : "))))
	    (redraw ent 4)
	    (setq ent (entget ent)
		  bl  (vla-item (vla-get-blocks (vla-get-activedocument (vlax-get-acad-object))) (cdr (assoc 2 ent)))
		  dec (mapcar '- pt (trans (cdr (assoc 10 ent)) 0 1))
		  deh (mapcar '/ dec (list (cdr (assoc 41 ent)) (cdr (assoc 42 ent)) (cdr (assoc 43 ent))))
	    )
	    (vlax-put bl 'origin (mapcar '+ (vlax-get bl 'origin) deh))
	    (setq js (ssget "x" (list (cons 0 "INSERT") (assoc 2 ent))) n 0)
	    (while (setq ent (ssname js n))
	      (setq ent (entget ent))
	      (if (eq rep "Oui")
		(setq ent (subst (cons 10 (trans (mapcar '+ (trans (cdr (assoc 10 ent)) 0 1) dec) 1 0)) (assoc 10 ent) ent))
	      )
	      (entmod ent)
	      (setq n (1+ n))
	    )
	    (princ (strcat "\nModification du point de base pour le bloc \"" (vla-get-name bl) "\" effectué."))
	  )
	)
	(princ "\nPas de bloc(s) de sélectionné(s)")
      )
    )
  )
  (vla-endundomark (vla-get-activedocument (vlax-get-acad-object)))
  (princ)
)

(setq nom_lisp "RINS")
(if (/= app nil)
  (if (= (strcase (substr app (1+ (- (strlen app) (strlen nom_lisp))) (strlen nom_lisp))) nom_lisp)
    (princ (strcat "..." nom_lisp " chargé."))
    (princ (strcat "\n" nom_lisp ".LSP Chargé.....Tapez " nom_lisp " pour l'éxecuter.")))
  (princ (strcat "\n" nom_lisp ".LSP Chargé......Tapez " nom_lisp " pour l'éxecuter.")))
(setq nom_lisp nil)
(princ)