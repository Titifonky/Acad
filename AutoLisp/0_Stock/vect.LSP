
(defun c:cv ()
  (setq	util (vla-get-utility
	       (vla-get-activedocument (vlax-get-acad-object))
	     ) ;_ Fin de vla-get-utility
  ) ;_ Fin de setq

  (vla-getentity
    util
    'ent
    'ip
    "\nSelectionnez l'objet à diviser :"
  ) ;_ Fin de vla-getentity

  (setq	entname	(vlax-vla-object->ename ent)
	sel	(ssadd entname)
  ) ;_ Fin de setq

  (sssetfirst nil sel)

  (setq sel_ent (cadr (sssetfirst nil sel)))
(trace_vecteur entname)
) ;_ Fin de defun


(defun vect ()
;;;  (if (equal entname (ssname (ssgetfirst) 0))
  (trace_vecteur)
;;;  ) ;_ Fin de if
) ;_ Fin de defun

(defun flagf (a b c)
  (princ a)
  (princ b)
  (princ c)
  (setq flag 1)
  (redraw)
  (trace_vecteur)
) ;_ Fin de defun
(vlr-remove-all :vlr-object-reactor)

(vlr-object-reactor (list ent) nil '((:vlr-modified . flagf)))
