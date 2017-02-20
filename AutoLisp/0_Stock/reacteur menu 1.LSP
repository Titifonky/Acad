(defun choix_reac3 (c d) (setq point (princ (car d))))

(vlr-remove-all :vlr-mouse-Reactor)

(vlr-mouse-reactor
  nil
  '((:vlr-beginRightClick . choix_reac3))
) ;_ Fin de vlr-mouse-reactor