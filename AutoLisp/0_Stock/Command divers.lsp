(defun c:ps () (command "_.PAGESETUP") (princ))
(defun c:ic () (command "_-layer" "in"))

(defun c:ac () (command "_-layer" "ac" "*" ""))

(defun c:ci () (command "_-layer" "in" "*" "n" ""))

(defun c:gc () (command "_-layer" "g" ))
(defun c:lc () (command "_-layer" "l"))
(defun c:sc () (vl-vbarun "Selection"))
(defun c:sf () (vl-vbarun "SelecInter"))
(defun c:vs () (command "-vues" "sa"))
(defun c:vu () (command "-vues" "r"))