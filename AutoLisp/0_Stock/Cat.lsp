;;;=================================================================
;;;
;;; CAT.LSP V1.20
;;;
;;; Copier des attributs
;;;
;;; Copyright (C) Patrick_35
;;;
;;;=================================================================

(defun c:cat(/ chemin fichier nom_bloc s p *errcat* changer_texte_attributs)
  
  ;;;---------------------------------------------------------------
  ;;;
  ;;; Gestion des erreurs
  ;;;
  ;;;---------------------------------------------------------------

  (defun *errcat* (msg)
    (if (/= msg "Function cancelled")
      (if (= msg "quit / exit abort")
        (princ)
        (princ (strcat "\nErreur : " msg))
      )
      (princ)
    )
    (setvar "pickadd" p)
    (vla-endundomark (vla-get-activedocument (vlax-get-acad-object)))
    (setq *error* s)
    (princ)
  )

  ;;;---------------------------------------------------------------
  ;;;
  ;;; Routine principale
  ;;;
  ;;;---------------------------------------------------------------

  (defun changer_texte_attributs(/ a b c n r s ttt u v)

    ;;;-------------------------------------------------------------
    ;;;
    ;;; Changement de texte de l'entit�
    ;;;
    ;;;-------------------------------------------------------------

    (princ "\nS�lectionner le Bloc de R�f�rence.")
    (setvar "pickadd" 0)
    (setq s (ssget '((0 . "INSERT"))))
    (setvar "pickadd" 1)
    (if s
      (progn
        (setq r (entget (ssname s 0)))
        (if (= (cdr (assoc 66 r)) 1)
          (progn
            (princ "\nS�lection des Blocs � Mofifier.")
            (if (setq s (ssget '((0 . "INSERT"))))
              (progn
                (setq n 0 u 0)
                (while (/= (ssname s n) nil)
                  (setq a r v 0 b (entget (ssname s n)))
                  (if (= (cdr (assoc 0 b)) "INSERT")
                    (setq ttt 1)
                  )
                  (while (and (/= (cdr (assoc 0 a)) "SEQEND") (/= (cdr (assoc 0 b)) "SEQEND"))
                    (setq c (subst (assoc 1 a) (assoc 1 b) b))
                    (entmod c)
                    (setq a (entget (entnext (cdr (assoc -1 a))))
                          b (entget (entnext (cdr (assoc -1 b))))
                          v 1)
                  )
                  (if (/= v 0)
                    (progn
                      (entupd (cdr (assoc -1 (entget (ssname s n)))))
                      (setq u (1+ u))
                    )
                  )
                  (setq n (1+ n))
                )
                (if (and (= ttt 1) (= u 0))
                  (alert "Pas de blocs correspondant au bloc de r�f�rence.")
                )
                (if (= ttt nil)
                  (alert "Pas de blocs s�lectionn�s.")
                )
                (if (/= u 0)
                  (princ (strcat "\n" (itoa u) " blocs modifi�s..."))
                )
              )
            )
          )
          (alert "Bloc sans Attributs")
        )
      )
      (princ "\nAucune S�lection...")
    )
  )

  ;;;---------------------------------------------------------------
  ;;;
  ;;; Routine de lancement
  ;;;
  ;;;---------------------------------------------------------------

  (vl-load-com)
  (setq s *error*)
  (setq *error* *errcat*)
  (vla-startundomark (vla-get-activedocument (vlax-get-acad-object)))
  (setq p (getvar "pickadd"))
  (changer_texte_attributs)
  (setvar "pickadd" p)
  (vla-endundomark (vla-get-activedocument (vlax-get-acad-object)))
  (setq *error* s)
  (princ)
)


(setq nom_lisp "CAT")
(if (/= app nil)
  (if (= (strcase (substr app (1+ (- (strlen app) (strlen nom_lisp))) (strlen nom_lisp))) nom_lisp)
    (princ (strcat "..." nom_lisp " charg�."))
    (princ (strcat "\n" nom_lisp ".LSP Charg�.....Tapez " nom_lisp " pour l'�xecuter.")))
  (princ (strcat "\n" nom_lisp ".LSP Charg�......Tapez " nom_lisp " pour l'�xecuter.")))
(setq nom_lisp nil)
(princ)
