;;;Start Coding Here

(defun tablelist (s / d r)
	(while 
		(setq d (tblnext s (null d)))
		(setq r (cons (cdr (assoc 2 d)) r))
	);while
);defun

;;;End Coding Here
