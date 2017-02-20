;;;=================================================================
;;;
;;; Ctr.LSP V1.01
;;;
;;; Ignorer le changement d'état de la variable dbmod avec un ctrl+C
;;;
;;; Copyright (C) Patrick_35
;;;
;;;=================================================================

(defun debut_d_un_command_ctrl(Rea Cde)
  (if (member (car Cde) (list "COPYCLIP" "COPYBASE" ))
    (acad-push-dbmod)
  )
)

(defun fin_d_un_command_ctrl(Rea Cde)
  (if (member (car Cde) (list "COPYCLIP" "COPYBASE" ))
    (acad-pop-dbmod)
  )
)

(defun effacer_reacteur_ctrl+c(Rea Cde / i j k l n)
  (if (setq i (vlr-reactors :vlr-editor-reactor))
    (progn
      (setq n 1 i (nth n (car i)) l 0)
      (while i
        (setq j nil)
        (if (or (eq (cdr (nth l (vlr-reactions i))) 'DEBUT_D_UN_COMMAND_CTRL)
                (eq (cdr (nth l (vlr-reactions i))) 'FIN_D_UN_COMMAND_CTRL))
          (setq j i)
          (setq l (1+ l))
        )
        (if j
          (progn
            (setq k (vl-remove (nth l (vlr-reactions i)) (vlr-reactions i)))
            (vlr-remove j)
            (if k
              (vlr-editor-reactor nil k)
            )
          )
          (if (not (nth l (vlr-reactions i)))
            (setq n (1+ n) l 0)
          )
        )
        (if (setq i (vlr-reactors :vlr-editor-reactor))
          (setq i (nth n (car i)))
        )
      )
    )
  )
)

(defun creation_reacteur_ctrl+c()
  (effacer_reacteur_ctrl+c nil nil)
  (setq mrea_ctrl+c (vlr-editor-reactor nil (list (cons :vlr-CommandWillStart (function debut_d_un_command_ctrl))
                                                  (cons :vlr-CommandEnded     (function fin_d_un_command_ctrl))
                                                  (cons :vlr-BeginClose       (function effacer_reacteur_ctrl+c)))))
  (princ "\nIgnorer le changement d'état de la variable dbmod avec un Ctrl+C ou un Copier base ACTIF.")
)

(vl-load-com)
(if (not mrea_ctrl+c)
  (creation_reacteur_ctrl+c)
)
(princ)
