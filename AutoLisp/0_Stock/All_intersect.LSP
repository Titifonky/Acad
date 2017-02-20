(defun c:gat (/	SS SSL			;length of SS
	      PTS			;returning list
	      aObj1			;Object 1
	      aObj2			;Object 2
	      N1			;Loop counter
	      N2			;Loop counter
	      iPts			;intersects
)
  (setq	SS  (ssget)
	N1  0				;index for outer loop
	SSL (sslength SS)
  )
					; Outer loop, first through second to last
  (while (< N1 (1- SSL))
					; Get object 1, convert to VLA object type
    (setq aObj1	(ssname SS N1)
	  aObj1	(vlax-ename->vla-object aObj1)
	  N2	(1+ N1)
    )					;index for inner loop
					; Inner loop, go through remaining objects
    (while (< N2 SSL)
					; Get object 2, convert to VLA object
      (setq aObj2 (ssname SS N2)
	    aObj2 (vlax-ename->vla-object aObj2)
					; Find intersections of Objects
	    iPts  (vla-intersectwith
		    aObj1
		    aObj2
		    3
		  )
					; variant result
	    iPts  (vlax-variant-value iPts)
      )
					; Variant array has values?
      (if (> (vlax-safearray-get-u-bound iPts 1)
	     0
	  )
	(progn				;array holds values, convert it
	  (setq	iPts			;to a list.
		 (vlax-safearray->list iPts)
	  )
					;Loop through list constructing points
	  (while (> (length iPts) 0)
	    (setq Pts  (cons (list (car iPts)
				   (cadr iPts)
				   (caddr iPts)
			     )
			     Pts
		       )
		  iPts (cdddr iPts)
	    )
	  )
	)
      )
      (setq N2 (1+ N2))
    )					;inner loop end
    (setq N1 (1+ N1))
  )					;outer loop end
  (princ Pts)
  (princ)
)					;return list of points found