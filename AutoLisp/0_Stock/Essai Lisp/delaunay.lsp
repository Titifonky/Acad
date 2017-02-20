;------------------------------------------------------------------------------
;
; PROGRAMME AUTOLisp : DELAUNAY.LSP
;
;
; OBJET : Maillage 3dFACE suivant la méthode de DELAUNAY
;
; Vincent LUX
;
; DATE    : 06/06/03
; VERSION : 10/06/03
;
;------------------------------------------------------------------------------

(defun 3to2 (PT3D)
  (list (car PT3D) (cadr PT3D))
)


(defun do_delaunay ()
  (setq P1 (cdr (assoc 10 (entget (ssname ss (- I 1))))))
  (setq P2 (cdr (assoc 10 (entget (ssname ss (- J 1))))))
  (setq P3 (cdr (assoc 10 (entget (ssname ss (- K 1))))))
  (setq X1 (car P1))
  (setq Y1 (cadr P1))
  (setq X2 (car P2))
  (setq Y2 (cadr P2))
  (setq X3 (car P3))
  (setq Y3 (cadr P3))

  ;; détermination du centre du cercle passant par P1 P2 et P3 et son rayon

  (setq PE (list (/ (+ X1 X3) 2) (/ (+ Y1 Y3) 2)))
  (setq PF (list (/ (+ X2 X3) 2) (/ (+ Y2 Y3) 2)))

  (setq PG (polar PE (+ (angle P1 P3) (/ PI 2)) 1.00))
  (setq PH (polar PF (+ (angle P2 P3) (/ PI 2)) 1.00))

  (setq PC (inters PE PG PF PH nil))
  (if PC
    (progn
      (setq X0 (car PC))
      (setq Y0 (cadr PC))
      (setq R (distance PC (3to2 P1)))
      (setq L 1)
      (setq FLAG 1)
      (while (and (<= L SSL) FLAG)             ; je regarde s'il y a un point sélectionné à l'intérieur du cercle
        (if (and (/= L I) (/= L J) (/= L K))   ; je ne prends pas les points P1 P2 et P3
          (progn
            (setq PX (3to2 (cdr (assoc 10 (entget (ssname ss (- L 1)))))))
            (if (< (distance PC PX) R)         ; si la distance PC PX < R alors PX est dans le cercle et P1 P2 P3 n'est pas un triangle Delaunay
              (setq FLAG nil)
            )
          )
        )
        (setq L (+ L 1))
      )
      (if FLAG
        (command "_pline" P1 P2 P3 "c")     ; c'est un triangle de Delaunay
      )
    )
    (prompt "\n3 points alignés")
  )
)




(defun c:delaunay ()
  (command "_undo" "_m")
  (setq OLD_CMD (getvar "cmdecho"))
  (setvar "cmdecho" 0)
  (setq OS (getvar "osmode"))
  (setvar "osmode" 0)

  (print "Sélectionner un semis de points")
  (setq ss (ssget '((0 . "POINT")) ))
  (setq SSL (sslength SS))
  (if (>= SSL 3)
    (progn
      (setq I 1)
      (While (<= I (- SSL 2))
        (setq J (+ I 1))
        (while (<= J (- SSL 1))
          (setq K (+ J 1))
          (while (<= K SSL)
            (do_delaunay)
            (setq K (+ K 1))
          )
          (setq J (+ J 1))
        )
        (setq I (+ I 1))
      )
    )
    (alert "Il faut au moins 3 points")
  ) ; fin si
  (setvar "cmdecho" OLD_CMD)
  (setvar "osmode" OS)
  (princ)
)