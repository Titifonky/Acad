(defun c:ge ()
  (while (= (car (princ (setq pt (grread 1 5 0)))) 5))
  (princ "\nLe point est ")
  (princ (cadr pt))
  (princ)
) ;_ Fin de defun
