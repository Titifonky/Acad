(defun c:cvs (/ sset i ent)
  (defun CreerLigne (pt1 pt2 /)
    (entmake
      (append
	(list '(0 . "LINE") '(67 . 0) '(410 . "Model") (cons '8 (getvar "CLAYER")) (cons '60 '0))
	(mapcar	(function (lambda (x y) (cons x (v2d y))))
		'(10 11)
		(list (cdr pt1) (cdr pt2))
	)
      )
    )
    (entlast)
  )

  (setq	sset (ssget '((0 . "SPLINE")))
	i    0
  )
  (repeat (sslength sset)
    (setq ent (ssname sset i)
	  i   (1+ i)
	  l   (massoc '10 (entget ent))
    )
    (mapcar 'CreerLigne l (cdr l))
    (entdel ent)
  )
  (setq sset nil)
)
 ;|«Visual LISP© Format Options»
(100 2 40 2 nil "Fin de " 100 9 0 0 0 T T nil T)
;*** NE PAS AJOUTER de texte au-dessous du commentaire! ***|;
