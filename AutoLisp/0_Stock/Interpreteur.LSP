(defun c:int ()
  (setq equ (getstring "Equation :"))
)

(defun routine (equ /)
  (setq char (car equ))
  (setq equ (cdr equ))
)