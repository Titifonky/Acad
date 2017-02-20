;;
;;
;;reçoit liste de données des entites
;;renvoi une liste avec les noms et les info suivantes
;;0 10 11 40 42 50 51 70
(defun div (listent / )
  (setq listent (vl-remove-if-not '(lambda (x)
        (or
          (= 0 (car x)) 
          (= 10 (car x))
          (= 11 (car x))
          (and (not (= 0.0 (cdr x))) (= 40 (car x)))
          (= 42 (car x))
          (= 50 (car x))
          (= 51 (car x))
        (= 70 (car x)))
      )
    listent)
  )
  (setq listent (list listent))
  (setq listent (mapcar '(lambda (x)
        (cond 
          ((member '(70 . 1) x)
            (progn
              (setq firstpoint (list (cadr (member '(70 . 1) x))))
              (setq x (append x firstpoint))
            )
          )
          (t x)
        )
      )
      listent
    )
  )
  ;;transforme une polyligne en
  ;;arc et en ligne
  ;;
  (setq listent (mapcar '(lambda (x)
        (cond 
          ((member '(0 . "LWPOLYLINE") x)
            (progn
              (setq listpt (cddr x))
              (setq lg (length listpt))
              (setq i 0)
              (while (<= i lg)
                (setq pts (list (nth i listpt) (nth (1+ i) listpt) (nth (+ 2 i) listpt)))
                (if (/= (car (nth 2 pts)) nil)
                  (progn
                    (if (= (cdr (nth 1 pts)) 0.0)
                      (progn
                        (setq newent (list (list (cons 0 "LINE") (nth 1 pts) (subst 11 10 (nth 2 pts)))))
                        (setq listent (append listent newent))
                      )
                      (progn
                        (setq angle (/ (* (atan (cdr (nth 1 pts))) 4) 2))
                        (setq dist (/ (distance (cdr (nth 0 pts)) (cdr (nth 2 pts))) 2))
                        (setq r1 (cons 40 (/ dist (sin (abs angle)))) r2 r1)
                        (setq ptc1 (nth 0 pts) ptc2 (nth 2 pts))
                        (setq ptc1 (cdr ptc1) ptc2 (cdr ptc2) r1 (cdr r1) r2 (cdr r2))
                        (setq a (car ptc1) b (cadr ptc1) c (car ptc2) d (cadr ptc2) r r1 s r2)
                        (setq m (/ (- (+ (expt a 2) (expt b 2)) (expt c 2) (expt d 2)) (* 2 (- b d))))
                        (setq p (/ (- a c) (- b d)))
                        (setq a1 (+ 1 (expt p 2)))
                        (setq b1 (- (* 2 b p) (* 2 m p) (* 2 a)))
                        (setq c1 (- (+ (expt a 2) (expt b 2) (expt m 2)) (expt r 2) (* 2 b m)))
                        (setq delta (- (expt b1 2) (* 4 a1 c1)))
                        (setq x1 (/ (- (* b1 -1) (sqrt delta)) (float (* 2 a1))))
                        (setq x2 (/ (+ (* b1 -1) (sqrt delta)) (float (* 2 a1))))
                        (setq y1 (- m (* x1 p)))
                        (setq y2 (- m (* x2 p)))
                        (setq int1 (list x1 y1) int2 (list x2 y2))
                        (command"point" int1)
                        (command"point" int2) 
                      )
                    )
                  )
                  
                )
                (setq i (+ i 2)) 
              )
            )
          )
          (t x)
        )
      )
      listent
    )
  )
)

;;Recoit info lignes
;;Retourne 0 ou 1 points
;;
(defun intll (pt1 pt2 pt3 pt4 / )
  (setq pt1 (cdr pt1) pt2 (cdr pt2) pt3 (cdr pt3) pt4 (cdr pt4))
  (setq int1 (osnap (inters pt1 pt2 pt3 pt4) "INT") )
)

;;Recoit info ligne et cercle ou arc
;;Retourne 0, 1 ou 2 points
;;
(defun intlca (pt1 pt2 ptc r anglea angleb / a b k m a1 b1 c1 delta x1 x2 y1 y2 angleint1 angleint2 max min )
  (setq pt1 (cdr pt1) pt2 (cdr pt2) ptc (cadr ptc) r (cadr r))
  (setq k (car ptc) m (last ptc))
  (setq a (/ (float (- (cadr pt2) (cadr pt1))) (- (car pt2) (car pt1))))
  (setq b (- (cadr pt1) (* a (car pt1))))
  (setq a1 (1+ (expt a 2)))
  (setq b1 (+ (* -2 k) (- (* (* 2 a) b) (* (* 2 a) m))))
  (setq c1 (- (+ (- (+ (expt k 2) (expt b 2)) (* (* 2 b) m)) (expt m 2)) (expt r 2)))
  (setq delta (- (expt b1 2) (* 4 a1 c1)))
  (if (>= delta 0.0)
    (progn
      (if (= delta 0.0) 
        (progn 
          (setq x1 (/ (- (* b1 -1) (sqrt delta)) (float (* 2 a1))))
          (setq y1 (+ (* a x1) b))
          (setq int1 (osnap (list x1 y1) "INT") int2 nil)
        )
        (progn 
          (setq x1 (/ (- (* b1 -1) (sqrt delta)) (float (* 2 a1))))
          (setq x2 (/ (+ (* b1 -1) (sqrt delta)) (float (* 2 a1))))
          (setq y1 (+ (* a x1) b))
          (setq y2 (+ (* a x2) b))
          (setq int1 (osnap (list x1 y1) "INT") int2 (osnap (list x2 y2) "INT"))
        )
      )
    )
    (setq int1 nil int2 nil)
  )
)

;;Recoit info cercles ou arcs
;;Retourne 0, 1 ou 2 points
;;
(defun intcca (ptc1 ptc2 r1 r2 anglea angleb anglec angled / a b c d r s m p a1 b1 c1 delta x1 x2 y1 y2 angleint1 angleint2 angleint11 angleint22 max1 min1 max2 min2)
  (setq ptc1 (cdr ptc1) ptc2 (cdr ptc2) r1 (cadr r1) r2 (cadr r2))
  (setq a (car ptc1) b (cadr ptc1) c (car ptc2) d (cadr ptc2) r r1 s r2)
  (setq m (/ (- (+ (expt s 2) (expt a 2) (expt b 2)) (expt r 2) (expt c 2) (expt d 2)) (* 2 (- b d))))
  (setq p (/ (- a c) (- b d)))
  (setq a1 (+ 1 (expt p 2)))
  (setq b1 (- (* 2 b p) (* 2 m p) (* 2 a)))
  (setq c1 (- (+ (expt a 2) (expt b 2) (expt m 2)) (expt r 2) (* 2 b m)))
  (setq delta (- (expt b1 2) (* 4 a1 c1)))
  (if (>= delta 0.0)
    (progn
      (if (= delta 0.0) 
        (progn 
          (setq x1 (/ (- (* b1 -1) (sqrt delta)) (float (* 2 a1))))
          (setq y1 (- m (* x1 p)))
          (setq int1 (osnap (list x1 y1) "INT") int2 nil)
        )
        (progn 
          (setq x1 (/ (- (* b1 -1) (sqrt delta)) (float (* 2 a1))))
          (setq x2 (/ (+ (* b1 -1) (sqrt delta)) (float (* 2 a1))))
          (setq y1 (- m (* x1 p)))
          (setq y2 (- m (* x2 p)))
          (setq int1 (osnap (list x1 y1) "INT") int2 (osnap (list x2 y2) "INT"))
        )
      )
    )
    (setq int1 nil int2 nil)
  )
)

;;Recoit info entite à couper CIRCLE et entites coupantes
;;Retourne une liste de points
;;
(defun circle (entdata entitesdata / )
  (setq ptc (cdr (assoc 10 entdata)))
  (setq r (cdr (assoc 40 entdata)))  
  (setq i (length entitesdata))
  
  
  
  
  
  
  
)

;;Recoit info entite à couper ARC et entites coupantes
;;Retourne une liste de points
;;
(defun arc (entdata entitesdata / )
  (setq ptc (cdr (assoc 10 entdata)))
  (setq r (cdr (assoc 40 entdata)))  
  (setq anglea (cdr (assoc 50 entdata)))  
  (setq angleb (cdr (assoc 51 entdata)))  
  (setq i (length entitesdata))
  
  
  
  
  
)

;;Recoit info entite à couper LINE et entites coupantes
;;Retourne une liste de points
;;
(defun line (entdata entitesdata / i lg)
  (setq pt1 (cdr (assoc 10 entdata)))
  (setq pt2 (cdr (assoc 11 entdata)))
  (setq lg (length entitesdata))
  (setq i 0)
)

;;Recoit info entite à couper POLYLINE et entites coupantes
;;Retourne une liste de points
;;
(defun lwpoly (entdata entitesdata / )
  
  
  
  
  
  
  
  
  
)

;;
;;
;;
(defun c:diva ()
  (setvar "flatland" 0)
  (setq nomobj (car (entsel "\nSelectionnez l'objet à couper :")))
  (setq entdata (div (entget nomobj)))
  (princ "\nSelectionnez les objets coupants :")
  (setq objsel (ssget))
  (if (ssmemb nomobj objsel)
    (setq objsel (ssdel nomobj objsel))
  )
  (setq entitesdata (div (entget (ssname objsel 0))))
  (setq i 1)
  (while (ssname objsel i)
    (setq entitesdata (append entitesdata (div (entget (ssname objsel i)))))
    (setq i (1+ i))
  )
  (setq lg (length entdata))
  (setq lgs (length entitesdata))
  (setq entdata (car entdata))
  (setq entname (cdr (car entdata)))
  (cond 
    ((= entname "LWPOLYLINE")  (lwpoly entdata entitesdata))
    ((= entname "LINE")  (line entdata entitesdata))
    ((= entname "ARC")  (arc entdata entitesdata))
    ((= entname "CIRCLE")  (circle entdata entitesdata))
  )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;;
(defun c:test ()
  (setq ptc1 (append '(10) (getpoint "\nptc1 :")))  
  (setq ptc2 (append '(10) (getpoint "\nptc2 :")))  
  (setq r1 (append '(40) (list (getdist "\nr1 :"))))
  (setq r2 (append '(40) (list (getdist "\nr2 :"))))
  (setq anglea (append '(50) (list (getangle "\nanglea :"))))
  (setq angleb (append '(51) (list (getangle "\nangleb :"))))
  (setq anglec (append '(50) (list (getangle "\nanglec :"))))
  (setq angled (append '(51) (list (getangle "\nangled :"))))
  (intcca ptc1 ptc2 r1 r2 anglea angleb anglec angled)
  (command"point" int1)
  (command"point" int2)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;;
(defun intcca (ptc1 ptc2 r1 r2 anglea angleb anglec angled / )
  (setq ptc1 (cdr ptc1) ptc2 (cdr ptc2) r1 (cadr r1) r2 (cadr r2))
  (setq anglea (cadr anglea) angleb (cadr angleb) anglec (cadr anglec) angled (cadr angled))
  (setq a (car ptc1) b (cadr ptc1) c (car ptc2) d (cadr ptc2) r r1 s r2)
  (setq m (/ (- (+ (expt s 2) (expt a 2) (expt b 2)) (expt r 2) (expt c 2) (expt d 2)) (* 2 (- b d))))
  (setq p (/ (- a c) (- b d)))
  (setq a1 (+ 1 (expt p 2)))
  (setq b1 (- (* 2 b p) (* 2 m p) (* 2 a)))
  (setq c1 (- (+ (expt a 2) (expt b 2) (expt m 2)) (expt r 2) (* 2 b m)))
  (setq delta (- (expt b1 2) (* 4 a1 c1)))
  (if (>= delta 0.0)
    (progn
      (if (= delta 0.0) 
        (progn 
          (setq x1 (/ (- (* b1 -1) (sqrt delta)) (float (* 2 a1))))
          (setq y1 (- m (* x1 p)))
          (setq int1 (list x1 y1) int2 nil)
        )
        (progn 
          (setq x1 (/ (- (* b1 -1) (sqrt delta)) (float (* 2 a1))))
          (setq x2 (/ (+ (* b1 -1) (sqrt delta)) (float (* 2 a1))))
          (setq y1 (- m (* x1 p)))
          (setq y2 (- m (* x2 p)))
          (setq int1 (list x1 y1) int2 (list x2 y2))
        )
      )
    )
    (setq int1 nil int2 nil)
  )
  (princ int1)
  (princ int2)
  (if (and (or (/= anglea 0.0) (/= angleb 0.0) (/= anglec 0.0) (/= angled 0.0)) (/= int1 nil))
    (progn
      (setq angleint1 (angle ptc1 int1))
      (setq angleint2 (angle ptc1 int2))
      (setq angleint11 (angle ptc2 int1))
      (setq angleint22 (angle ptc2 int2))
      (if (> anglea angleb)
        (setq max1 anglea min1 angleb)
        (setq max1 angleb min1 anglea)
      )
      (if (> anglec angled)
        (setq max2 anglec min2 angled)
        (setq max2 angled min2 anglec)
      )
      (if (not (and (< angleint1 max1) (> angleint1 min1) (< angleint11 max2) (> angleint11 min2)))
        (setq int1 nil)
      )
      (if (not (and (< angleint2 max2) (> angleint2 min2) (< angleint22 max2) (> angleint22 min2)))
        (setq int2 nil)
      )
    )
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;;
 a b c d r s m p a1 b1 c1 delta x1 x2 y1 y2 angleint1 angleint2 angleint11 angleint22 max1 min1 max2 min2