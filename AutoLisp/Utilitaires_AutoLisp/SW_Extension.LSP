(defun c:mmm () (command "_MIRROR" (ssget) "" (setq pt1 (getpoint)) (getpoint pt1) "_y") (princ))

(defun c:rrr () (command "_ROTATE" (ssget) "" (getpoint) 180.0) (princ))

(defun c:ddd ()
  (and (setq ss	(ssget '((0 . "MTEXT") (410 . "Model")))
	     e	(ssname ss 0)
       )
       (setq pt (cdr (assoc 10 (entget e))))
       (setq pt1 (getpoint "\nPoint"))
       (setq ag (/ (* (getangle pt1 "\nAngle") 180.0) pi))
       (progn (command "_MOVE" ss "" pt pt1) (command "_ROTATE" ss "" pt1 ag))
  )
  (princ)
)


;|«Visual LISP© Format Options»
(100 2 40 2 nil "Fin de " 100 9 0 0 0 T nil nil T)
;*** NE PAS AJOUTER de texte au-dessous du commentaire! ***|;
