(vl-load-com)

					;Sw_NettoyerDXF

(defun ConvertirSpline (/)


;;; On convertis les splines en polyligne compose d'arcs
  (setvar "PLINECONVERTMODE" 1)

  (setq	Sel (ssget "_X" '((0 . "SPLINE") (410 . "Model")))
	i   0
  )

  (if sel
    (repeat (sslength Sel)
      (setq spl (ssname Sel i))
      (command "_splinedit" spl "_p" 1)
      (setq i (1+ i))
    )
  )

  (setq	Sel (ssget "_X" '((0 . "LWPOLYLINE") (410 . "Model")))
	i   0
  )

  (if sel
    (repeat (sslength Sel)
      (setq lw (ssname Sel i))
      (command "_convertpoly" "_h" lw)
      (setq i (1+ i))
    )
  )

  (princ)
)

(defun c:cspl (/)
  (ConvertirSpline)
)