;;;=================================================================
;;;
;;; JSD.LSP V2.00
;;;
;;; Jeu de Sélection de ce qui a été créé
;;;
;;; Copyright (C) Patrick_35
;;;
;;;=================================================================

(defun commencer_jeu__selection(Rea Cde)
  (if (member (car Cde) '("COPY" "MIRROR" "GRIP_MIRROR" "ARRAY" "MOVE"))
    (setq $ (ssadd))
  )
)

(defun ajouter_jeu_de_selection (Rea Obj)
  (if (not (eq (cdr (assoc 0 (entget (cadr Obj)))) "ATTRIB"))
    (ssadd (cadr Obj) $)
  )
)

(defun enlever_jeu_de_selection (Rea Obj)
  (ssdel (cadr Obj) $)
)

(defun faire_jeu_de_selection()
  (setq mrea_jsd (vlr-command-reactor nil (list (cons :vlr-commandWillStart (function commencer_jeu__selection))
					  )
		 )
	mrea_fin (vlr-acdb-reactor    nil (list (cons :vlr-objectAppended   (function ajouter_jeu_de_selection))
						(cons :vlr-objectUnErased   (function ajouter_jeu_de_selection))
						(cons :vlr-objectErased     (function enlever_jeu_de_selection))
					  )
		 )
  )
  (princ "\nSélection de ce qui est créé Actif, utilisez !$ sur la ligne de commande")
  (princ)
)

(vl-load-com)
(if (not mrea_jsd)
  (faire_jeu_de_selection)
)
(princ)