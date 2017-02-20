(defun list_ss	(sset / i bcl ss nb lst)
  (setq	i   -1
	lst '())
  (defun bcl  (ss nb)
    (if	(setq ent (ssname ss (setq nb (1+ nb))))
      (setq lst (cons ent (bcl ss nb)))))
  (if sset
    (bcl sset i)
    'nil
    )
  )
(setq a (list_ss (ssget "_X" '((0 . "HATCH")))))
(foreach n a
  (entdel n)
  )