;;; c:ChgGroupStatus
;;; Change le mode de s�letion des groupes associ�s � un objet choisi.
;;; Par Serge Camir� Cad-Novation 2005-02-22

;;; spyDictionnaries
;;; Retourne une liste tri�e de tous les groupes auquel un objet fait partie.
;;; Re�oit un objet (ename)
;;; Retourne une liste de noms.
(defun spyDictionnaries (
object
/ AllDictObjects AllDictObjectsLength AllDictObjectsLength existIn groupDictionnary
isMemberOf MyDictObjs names objectGet pos
)
(setq objectGet (entget object))
(setq isMemberOf nil)

(setq groupDictionnary (dictsearch (namedobjdict) "acad_group"))
(if groupDictionnary
(progn
(setq names (mapcar 'cdr (vl-remove-if-not '(lambda (x) (= 3 (car x))) groupDictionnary)))
(setq AllDictObjects (mapcar 'cdr (vl-remove-if-not '(lambda (x) (= 350 (car x))) groupDictionnary)))
;; Si on ramasse des objets 330 non pertinents, ce n'est pas grave puisqu'on les filtrera
;; mais �a donne du code plus court
(setq MyDictObjs (mapcar 'cdr (vl-remove-if-not '(lambda (x) (= 330 (car x))) objectGet)))
(setq AllDictObjectsLength (length AllDictObjects))
(foreach MyDictObj MyDictObjs
(setq existIn (member MyDictObj AllDictObjects))
(if existIn
(progn
(setq pos (- AllDictObjectsLength (length existIn)))
(setq isMemberOf (cons (nth pos names) isMemberOf))
))
)
))
(acad_strlsort isMemberOf)
)

;;; changeGroupStatus
;;; Change le statut de s�lection.
;;; Re�oit :
;;; dictName: 'STR, le nom du dictionnaire
;;; selectableMode: 0 ou 1
;;; Retourne: rien
(defun changeGroupStatus (
dictName selectableMode
/ groupDictionnary objDictionnary objDictionnaryGet
)
(cond
((not (setq groupDictionnary (dictsearch (namedobjdict) "acad_group"))) nil)
((not (setq groupDictionnary (member (cons 3 dictName) groupDictionnary))) nil)
((not (setq objDictionnary (cdadr groupDictionnary))) nil)
((not (setq objDictionnaryGet (entget objDictionnary))) nil)
(t
(setq objDictionnaryGet (subst (cons 71 selectableMode) (assoc 71 objDictionnaryGet) objDictionnaryGet))
(entmod objDictionnaryGet)
(entupd objDictionnary)
)
)
nil
)

(defun c:ChgGroupStatus (
/ groupes msg object objectSel reponse status
)
(setq objectSel (entsel "\nChoisissez l'objet: "))
(setq object (car objectSel))
(setq groupes (spyDictionnaries object))
(if groupes
(progn
(setq msg (if (> (length groupes) 1)
"Rendre ces groupes s�lectionnables"
"Rendre ce groupe s�lectionnable"
))
(setq msg (strcat msg " [Oui/Non] : "))
(initget "Oui Non")
(setq reponse (getkword msg))
(setq status (if (= "Oui" reponse) (entdel groupes)))
)
(progn
(princ "\nAucun groupe.")
))
(princ)
)