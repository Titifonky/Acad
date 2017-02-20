(defun c:ef (/ js_off n ent dxf_ent)
  (princ "\nChoix des entités à inactiver: ")
  (setq	js_off (ssget)
	n      0
  ) ;_ Fin de setq
  (cond
    (js_off
     (repeat (sslength js_off)
       (setq
	 ent	 (ssname js_off n)
	 dxf_ent (entget ent)
       ) ;_ Fin de setq
       (if (not (member '(60 . 0) dxf_ent))
	 (entmod (append dxf_ent '((60 . 1))))
	 (entmod (subst '(60 . 1) '(60 . 0) dxf_ent))
       ) ;_ Fin de if
       (setq n (1+ n))
     ) ;_ Fin de repeat
    )
    (T
     (princ "\nSélection vide.")
    )
  ) ;_ Fin de cond
  (princ)
) ;_ Fin de defun
(defun c:en (/ js_off n js_swap n_swap ent ent_dxf js_on nb)
  (setq	js_off (ssget "_X" '((60 . 1)))
	n      0
  ) ;_ Fin de setq
  (cond
    (js_off
     (setq js_swap (ssget "_X" '((-4 . "")))
	   n_swap  0
     ) ;_ Fin de setq
     (cond
       (js_swap
	(repeat	(sslength js_swap)
	  (setq ent (ssname js_swap n_swap))
	  (redraw ent 2)
	  (setq n_swap (1+ n_swap))
	) ;_ Fin de repeat
       )
     ) ;_ Fin de cond
     (repeat (sslength js_off)
       (setq ent (ssname js_off n))
       (setq dxf_ent (entget ent))
       (entmod (subst '(60 . 0) '(60 . 1) dxf_ent))
       (entupd ent)
       (setq n (1+ n))
     ) ;_ Fin de repeat
     (princ "\nChoix des entités à réactiver: ")
     (setq js_on (ssget)
	   n	 0
     ) ;_ Fin de setq
     (cond
       (js_on
	(repeat	(sslength js_on)
	  (if (ssmemb (ssname js_on n) js_off)
	    (setq js_off (ssdel (ssname js_on n) js_off))
	  ) ;_ Fin de if
	  (setq n (1+ n))
	) ;_ Fin de repeat
	(cond
	  (js_off
	   (setq nb 0)
	   (repeat (sslength js_off)
	     (setq ent (ssname js_off nb))
	     (setq dxf_ent (entget ent))
	     (if (not (member '(60 . 0) dxf_ent))
	       (entmod (append dxf_ent '((60 . 1))))
	       (entmod (subst '(60 . 1) '(60 . 0) dxf_ent))
	     ) ;_ Fin de if
	     (entupd ent)
	     (setq nb (1+ nb))
	   ) ;_ Fin de repeat
	   (cond
	     (js_swap
	      (setq n_swap 0)
	      (repeat (sslength js_swap)
		(setq ent (ssname js_swap n_swap))
		(redraw ent 1)
		(setq n_swap (1+ n_swap))
	      ) ;_ Fin de repeat
	     )
	   ) ;_ Fin de cond
	  )
	  (T
	   (princ "\nAucune entité à réactivée!")
	  )
	) ;_ Fin de cond
       )
       (T
	(princ "\nSélection vide.")
       )
     ) ;_ Fin de cond
    )
    (T
     (princ "\nAucune entité à réactivée!")
    )
  ) ;_ Fin de cond
  (princ)
) ;_ Fin de defun
