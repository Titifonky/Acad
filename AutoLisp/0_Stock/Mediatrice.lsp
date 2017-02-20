;; Mediatrice -Gilles Chanteau- (maj 23/04/07)
;; Cr�e une ligne sur la m�diatrice du segment d�fini par 2 points
;; La longueur de la ligne est entr�e au clavier ou � l'aide du pointeur.

(defun c:mediatrice (/ pt1 pt2 dep ang)
  (initget 1)
  (setq pt1 (getpoint "\nPremier point: "))
  (initget 1)
  (setq pt2 (getpoint pt1 "\nSecond point: "))
  (if (equal (caddr pt1) (caddr pt2) 1e-009)
    (progn
      (setq dep	 (mapcar (function (lambda (x1 x2) (/ (+ x1 x2) 2))) pt1 pt2)
	    ang (+ (angle dep pt1) (/ pi 2))
      )
      (vl-cmdf "_.line" "_non" dep (strcat "<" (angtos ang)) pause "")
    )
    (prompt
      "Les points ne sont pas dans un plan parall�le au plan du SCU courant."
    )
  )
  (princ)
)