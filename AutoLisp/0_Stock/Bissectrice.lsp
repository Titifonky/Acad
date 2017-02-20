;;; C:BISSECTRICE -Gilles Chanteau- (maj 23/04/07)
;;; Cr�e une ligne sur la bissectrice de l'angle form� par les deux segments s�lectionn�s.
;;; La longueur de la ligne est entr�e au clavier ou sp�cifi�e � l'aide du pointeur.

(defun c:bi (/ e1 e2 l1 l2 p1 p1e p1m p2 p2e p2m som ang)

  (while (not
	   (setq e1 (entsel "\nS�lectionnez le premier segment: "))
	 )
  )
  (while (not
	   (setq e2 (entsel "\nS�lectionnez le second segment: "))
	 )
  )
  (setq	l1 (entget (car e1))
	l2 (entget (car e2))
	p1 (osnap (cadr e1) "_near")
	p2 (osnap (cadr e2) "_near")
  )
  (if
    (and
      (or
	(and (member (cdr (assoc 0 l1)) '("XLINE" "RAY"))
	     (setq p1m (mapcar '+ p1 (trans (cdr (assoc 11 l1)) 0 1 T)))
	     (setq p1e (mapcar '- p1 (trans (cdr (assoc 11 l1)) 0 1 T)))
	)
	(and
	  (setq p1m (osnap (cadr e1) "_midpoint"))
	  (setq p1e (osnap (cadr e1) "_endpoint"))
	)
      )
      (or
	(and (member (cdr (assoc 0 l2)) '("XLINE" "RAY"))
	     (setq p2m (mapcar '+ p2 (trans (cdr (assoc 11 l2)) 0 1 T)))
	     (setq p2e (mapcar '- p2 (trans (cdr (assoc 11 l2)) 0 1 T)))
	)
	(and
	  (setq p2m (osnap (cadr e2) "_midpoint"))
	  (setq p2e (osnap (cadr e2) "_endpoint"))
	)
      )
    )
     (if (vl-every (function (lambda (x) (equal (caddr p1) (caddr x) 1e-009)))
		   (list p1m p1e p2 p2m p2e)
	 )
       (if (and	(null (inters p1 p1m p1 p1e))
		(null (inters p2 p2m p2 p2e))
	   )
	 (if (setq som (inters p1 p1e p2 p2e nil))
	   (progn
	     (setq ang (/ (+ (angle som p1) (angle som p2)) 2.0))
	     (vl-cmdf "_.line" "_non" som (strcat "<" (angtos ang)) pause "")
	   )
	   (princ "\nErreur: segments parall�les")
	 )
	 (princ "\nErreur: segment non lin�aire")
       )
       (princ
	 "\nErreur: segments non coplanaires ou non parall�les au plan du SCU courant"
       )
     )
     (princ "\nErreur: entit� non valide")
  )
  (princ)
)