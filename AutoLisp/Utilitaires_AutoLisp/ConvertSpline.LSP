(defun c:cvsp (/ sset i ent)
  (setq	sset (ssget '((0 . "SPLINE")))
	i    0
  )
  (repeat (sslength sset)
    (setq ent (ssname sset i)
	  i   (1+ i)
    )
    (command "_splinedit" ent "P" 2)
  )
  (setq sset nil)
)

(defun c:ada (/ sset i ent aire)
  (setq cmde (getvar "CMDECHO"))
  (terpri)
  (princ "==== Calculer la somme des aires des objets ====")
  (setq	sset (ssget)
	i    0
  )
  (setvar "CMDECHO" 0)
  (command "_AREA" "_a" "_o")
  (repeat (sslength sset)
    (setq ent (ssname sset i)
	  i   (1+ i)
    )
    (command ent)
  )
  (command "" "")
  (setq	sset nil
	aire (getvar "AREA")
	aire (* aire 0.000001)
	aire (strcat "Aire totale : " (rtos aire 2 6) " m²")
  )
  (setvar "CMDECHO" cmde)
  (terpri)
  (princ (strcat (itoa i) " objet(s)"))
  (terpri)
  (princ aire)
  (princ)
)

;;--------------------=={ Total Length }==--------------------;;
;;                                                            ;;
;;  Displays the total length of selected objects at the      ;;
;;  command line. The units and precision format of the       ;;
;;  printed result is dependent upon the settings of the      ;;
;;  LUNITS & LUPREC system variables respectively.            ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2013 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;

(defun c:tlen (/ e i l s)
  (if (setq s
	     (ssget
	       '((0 . "ARC,CIRCLE,ELLIPSE,LINE,*POLYLINE,SPLINE")
		 (-4 . "<NOT")
		 (-4 . "<AND")
		 (0 . "POLYLINE")
		 (-4 . "&")
		 (70 . 80)
		 (-4 . "AND>")
		 (-4 . "NOT>")
		)
	     )
      )
    (progn
      (setq l 0.0)
      (repeat (setq i (sslength s))
	(setq e	(ssname s (setq i (1- i)))
	      l	(+ l
		   (vlax-curve-getdistatparam e (vlax-curve-getendparam e))
		)
	)
      )
      (princ "\nTotal Length: ")
      (princ (rtos l))
    )
  )
  (princ)
)
(vl-load-com)
(princ)

;;---------------------=={ Total Area }==---------------------;;
;;                                                            ;;
;;  Displays the total area of selected objects at the        ;;
;;  command line. The precision of the printed result is      ;;
;;  dependent on the setting of the LUPREC system variable.   ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2013 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;

(defun c:tarea (/ a i s)
  (if (setq s
	     (ssget
	       '((0 . "CIRCLE,ELLIPSE,*POLYLINE,SPLINE")
		 (-4 . "<NOT")
		 (-4 . "<AND")
		 (0 . "POLYLINE")
		 (-4 . "&")
		 (70 . 80)
		 (-4 . "AND>")
		 (-4 . "NOT>")
		)
	     )
      )
    (progn
      (setq a 0.0)
      (repeat (setq i (sslength s))
	(setq a (+ a (vlax-curve-getarea (ssname s (setq i (1- i))))))
      )
      (princ "\nTotal Area: ")
      (princ (rtos a 2))
    )
  )
  (princ)
)
(vl-load-com)
(princ)

