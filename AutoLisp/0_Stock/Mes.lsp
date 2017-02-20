;;;=================================================================
;;;
;;; MES.LSP V1.00
;;;
;;; Mesurer en continu et avec cumul
;;;
;;; Copyright (C) Patrick_35
;;;
;;;=================================================================

(defun c:mes(/ di pt1 pt2)
  (setq pt1 (getpoint "\nPremier Point : ") di 0.0)
  (while pt1
    (if (setq pt2 (getpoint pt1 "\nPoint suivant : "))
      (progn
	(setq di (+ di (distance pt1 pt2)))
	(princ (strcat "\nDistance mesurée: " (rtos (distance pt1 pt2)) " -- Distance cumulée : " (rtos di)))
      )
    )
    (setq pt1 pt2)
  )
  (princ)
)

(setq nom_lisp "MES")
(if (/= app nil)
  (if (= (strcase (substr app (1+ (- (strlen app) (strlen nom_lisp))) (strlen nom_lisp))) nom_lisp)
    (princ (strcat "..." nom_lisp " chargé."))
    (princ (strcat "\n" nom_lisp ".LSP Chargé.....Tapez " nom_lisp " pour l'éxecuter.")))
  (princ (strcat "\n" nom_lisp ".LSP Chargé......Tapez " nom_lisp " pour l'éxecuter.")))
(setq nom_lisp nil)
(princ)