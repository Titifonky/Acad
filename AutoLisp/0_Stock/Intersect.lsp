(defun VxGetInters (/ IntLst PntLst)
  (vl-load-com)
  (princ "\nChoisissez le premier objet :")
  (setq fst (vlax-ename->vla-object (car (entsel))))
  (princ "\nChoisissez le deuxième objet :")
  (setq	nxt (vlax-ename->vla-object (car (entsel)))
	mde acExtendNone
  )
  (setq IntLst (vlax-invoke Fst 'IntersectWith Nxt Mde))
  (cond
    (IntLst
     (repeat (/ (length IntLst) 3)
       (setq PntLst (cons
		      (list
			(car IntLst)
			(cadr IntLst)
			(caddr IntLst)
		      )
		      PntLst
		    )
	     IntLst (cdddr IntLst)
       )
     )
     (reverse PntLst)
    )
    (T nil)
  )
)
(defun c:tst ()
  (VxGetInters)
)