(defun trace_vecteur
       (ent / nb_div start end div param point vecteur pt_fin)
  (setq	start  (vlax-curve-getStartParam ent)
	end    (vlax-curve-getEndParam ent)
	nb_div 30
	div    (/ (- end (float start)) nb_div)
	param  (- 0 div)
  ) ;_ Fin de setq
  (repeat (1+ nb_div)
    (setq param (+ param div))
    (setq point	  (vlax-curve-getPointAtParam ent param)
	  vecteur (vlax-curve-getfirstDeriv ent param)
    ) ;_ Fin de setq
    (setq vecteur (mapcar '(lambda (x) (* x 0.5)) vecteur)
	  vecteur (list (* -1 (cadr vecteur)) (car vecteur) (last vecteur))
	  pt_fin  (mapcar '+ vecteur point)
    ) ;_ Fin de setq
    (grdraw point pt_fin 3)
  ) ;_ Fin de repeat
) ;_ Fin de defun
