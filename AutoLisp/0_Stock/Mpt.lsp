;;;=================================================================
;;;
;;; MPT.LSP V1.00
;;;
;;; Réacteur pour que les coordonnées XYZ d'un bloc correspondent à ses attributs
;;;
;;; Copyright (C) Patrick_35
;;;
;;;=================================================================


(defun Attributs_ini(Rea Cde)
  (setq dernier_ent (entlast))
)

(defun Attributs_xyz(Rea Cde / bl js n xyz)
  (cond
    ((or (eq (car Cde) "MOVE") (eq (car Cde) "STRETCH"))
      (setq js (ssget "_p"))
    )
    ((or (eq (car Cde) "GRIP_MOVE") (eq (car Cde) "GRIP_STRETCH"))
      (setq js (cadr (ssgetfirst)))
    )
    ((eq (car Cde) "INSERT")
      (setq js (ssadd))
      (ssadd (entlast) js)
    )
    ((eq (car Cde) "COPY")
      (setq js (ssadd) n (entnext dernier_ent))
      (while n
        (ssadd n js)
        (setq n (entnext n))
      )
    )
    ((eq (car Cde) "UCS")
      (setq js (ssget "x" (list (cons 0 "INSERT") (cons 2 Nom_bloc))))
    )
  )
  (if js
    (progn
      (setq n 0)
      (while (ssname js n)
        (setq bl (entget (ssname js n)))
        (if (and (eq (cdr (assoc 0 bl)) "INSERT") (eq (strcase (cdr (assoc 2 bl))) (strcase Nom_bloc)))
          (if (cdr (assoc 66 bl))
            (progn
              (setq xyz (trans (cdr (assoc 10 bl)) (cdr (assoc -1 bl)) 1))
              (while (not (eq (cdr (assoc 0 bl)) "SEQEND"))
                (if (eq (cdr (assoc 0 bl)) "ATTRIB")
                  (cond
                    ((eq (strcase (cdr (assoc 2 bl))) (strcase Eti_X))
                      (setq bl (subst (cons 1 (rtos (car xyz))) (assoc 1 bl) bl))
                      (entmod bl)
                      (entupd (cdr (assoc -1 bl)))
                    )
                    ((eq (strcase (cdr (assoc 2 bl))) (strcase Eti_Y))
                      (setq bl (subst (cons 1 (rtos (cadr xyz))) (assoc 1 bl) bl))
                      (entmod bl)
                      (entupd (cdr (assoc -1 bl)))
                    )
                    ((eq (strcase (cdr (assoc 2 bl))) (strcase Eti_Z))
                      (setq bl (subst (cons 1 (rtos (caddr xyz))) (assoc 1 bl) bl))
                      (entmod bl)
                      (entupd (cdr (assoc -1 bl)))
                    )
                  )
                )
                (setq bl (entget (entnext (cdr (assoc -1 bl)))))
              )
            )
          )
        )
        (setq n (1+ n))
      )
    )
  )
  (princ)
)

(defun c:spt(/ i j n)
  (if (setq i (vlr-reactors :vlr-command-reactor))
    (progn
      (setq n 1 i (nth n (car i)))
      (while i
        (setq j nil)
        (if (or (eq (cdr (car (vlr-reactions i))) 'ATTRIBUTS_XYZ) (eq (cdr (car (vlr-reactions i))) 'ATTRIBUTS_INI))
          (setq j i)
        )
        (if j
          (vlr-remove j)
          (setq n (1+ n))
        )
        (if (setq i (vlr-reactors :vlr-Command-Reactor))
          (setq i (nth n (car i)))
        )
      )
      (if mrea_mpt
        (princ (strcat "\n\tDésactivation de la mise à jour XYZ du bloc " Nom_bloc "."))
      )
      (setq mrea_mpt nil)
    )
  )
  (princ)
)

(defun c:mpt()
  (if (not mrea_mpt)
    (progn
      (c:spt)
      (vlr-command-reactor nil '((:vlr-commandwillstart . Attributs_ini)))
      (setq mrea_mpt (vlr-command-reactor nil '((:vlr-commandEnded . Attributs_xyz))))
      (princ (strcat "\n\tActivation de de la mise à jour XYZ du bloc " Nom_bloc "."))
    )
    (princ (strcat "\n\tLa mise à jour XYZ du bloc " Nom_bloc " est déjà activée."))
  )
  (princ)
)

(vl-load-com)
(setq Nom_bloc "XY"
      Eti_X    "X"
      Eti_Y    "Y"
      Eti_Z    "Z")
(princ (strcat "\n\tPour activer la mise à jour XYZ du bloc " Nom_bloc ", lancez la commande MPT.\n\tPour revenir à la normale, faites la commande SPT."))
(princ)