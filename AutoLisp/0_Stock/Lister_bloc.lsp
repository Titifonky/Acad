;;;=================================================================
;;;
;;; LSTBL.LSP V2.00
;;;
;;; Décompte des blocs
;;;
;;; Copyright (C) Patrick_35
;;;
;;;=================================================================

(defun c:lstbl(/ js bllst ent lstbl nb InputBox)

  (defun InputBox (Titre Message Defaut / users1 valeur)
    (setq users1 (getvar "users1"))
    (acad-push-dbmod)
    (vla-eval (vlax-get-acad-object) (strcat "ThisDrawing.SetVariable \"USERS1\"," "InputBox (\"" Message "\", \"" Titre "\", \"" Defaut "\")"))
    (setq valeur (getvar "users1"))
    (setvar "users1" users1)
    (acad-pop-dbmod)
    valeur
  )

  (if (not (eq (setq js (InputBox "Décompte de blocs" "Veuillez donnez un nom de bloc ou * pour tous" "*")) ""))
    (if (setq js (ssget (list (cons 0 "INSERT") (cons 2 js))))
      (progn
	(setq nb 0)
	(while (setq ent (ssname js nb))
	  (setq ent (vlax-ename->vla-object ent))
	  (if (not (vlax-property-available-p ent 'Path))
	    (if (vlax-property-available-p ent 'EffectiveName)
	      (setq lstbl (append lstbl (list (vla-get-EffectiveName ent))))
	      (setq lstbl (append lstbl (list (vla-get-Name ent))))
	    )
	  )
	  (setq nb (1+ nb))
	)
	(setq lstbl (acad_strlsort lstbl))
	(while (setq ent (car lstbl))
	  (setq nb    (length lstbl)
		lstbl (vl-remove ent lstbl)
		bllst (append bllst (list (cons ent (- nb (length lstbl))))))
	)
	(mapcar '(lambda (x) (princ (strcat "\nIl y a " (itoa (cdr x)) " bloc(s) " (car x)))) bllst)
      )
    )
  )
  (princ)
)

(setq nom_lisp "LSTBL")
(if (/= app nil)
  (if (= (strcase (substr app (1+ (- (strlen app) (strlen nom_lisp))) (strlen nom_lisp))) nom_lisp)
    (princ (strcat "..." nom_lisp " chargé."))
    (princ (strcat "\n" nom_lisp ".LSP Chargé.....Tapez " nom_lisp " pour l'éxecuter.")))
  (princ (strcat "\n" nom_lisp ".LSP Chargé......Tapez " nom_lisp " pour l'éxecuter.")))
(setq nom_lisp nil)
(princ)