; Programme qui permet d'éteindre le calque courant dans toutes les fenêtres flottantes
; de toutes les présentations, sauf éventuellement dans la fenêtre active.

; Gestion des erreurs et restauration de l'environnement
; Cette procédure est stéreotypée dans mes programmes LISP
(defun *error* (msg)
  (command)                                                                      ; Annulation des commandes en cours
  (restaureEnv)                                                                  ; Appel à la procédure de restauration
  (princ "\nerreur: ")
  (princ msg)
  (princ)
)

; Restauration de l'environnement
; Cette procédure est stéreotypée dans mes programmes LISP
(defun RestaureEnv ()
  (if (/= *old1* nil) (setvar "cmdecho" *old1*))                                 ; restaure la variable CMDECHO
  (if (/= *old2* nil) (setvar "LAYOUTREGENCTL" *old2*))                          ; restaure la variable LAYOUTREGENCTL
)

; Boucle qui permet d'éteindre le calque courant dans la fenêtre en cours.
; Ceci se fait par le biais des données étendues (xDtata) de la fenêtre en cours,
; dans lesquelles il faut incorporer la liste pointée
; (1003 . Nom_du_Calque) entre les codes (1002 . {) et (1002 . })
(Defun Boucle3 ( / xData xType NewData NewType k)
  (vla-getxdata CurrentPViewport0 "" 'xType 'xData)                              ; Récupération des données xData de la fenêtre
  (setq k (vlax-safearray-get-u-bound xType 1))                                  ; Nb d'éléments dans xData
  (setq NewType (vlax-make-safearray vlax-vbInteger (cons 0 (+ k 1))))           ; Création d'un nouveau tableau Type
  (setq NewData (vlax-make-safearray vlax-vbVariant (cons 0 (+ k 1))))           ; Création d'un nouveau tableau Data

  (setq n 0)
  (repeat (+ k 1)                                                                ; boucle pour recopier les éléments des tableaux
    (vlax-safearray-put-element Newtype n (vlax-safearray-get-element xType n))  ; Recopie xType dans NewType
    (vlax-safearray-put-element NewData n (vlax-safearray-get-element xData n))  ; Recopie xData dans NewData
    (setq n (1+ n))                                                              ; Incrémente
  )
  
  (vlax-safearray-put-element Newtype (+ k 1) 1002)                              ; insere le dernier atome de la liste
  (vlax-safearray-put-element NewData (+ k 1) "}")                               ; insere le dernier atome de la liste
  
  (vlax-safearray-put-element Newtype (- k 1) 1003)                              ; insere l'atome en avant dernière position
  (vlax-safearray-put-element NewData (- k 1) ActiveLayer0)                      ; insere l'atome en avant dernière position

  (vla-setxdata CurrentPViewport0 NewType NewData)                               ; Enregistre la nouvelle liste dans la fenêtre

  (if (equal CurrentLayout0 ActiveLayout0)                                       ; Si l'utilisateur se trouve dans une présentation
    (progn                                                                       ; alors il faut régénérer les fenêtres de la présentation
      (vla-display CurrentPViewport0 0)                                          ; Régénère la fenêtre
      (vla-display CurrentPViewport0 1)                                          ; Régénère la fenêtre
    ))
)

; Enumération des fenêtres flottantes
; Les sous-objets d'une présentation représentent tous les éléments dessinés dans la présentation (blocs, fenêtres...),
; mais le premier sous-objet représente toujours la vue active de la présentation elle-même.
; Il faut donc sauter le premier test.
(Defun Boucle2 (CurrentPViewport0)
  (if (and var1 (vlax-method-applicable-p CurrentPViewport0 'Display)            ; Vérification que l'objet est une fenêtre (PViewPort)
    (not (Equal CurrentPViewport0 ActivePViewport0))) (boucle3))                 ; et que la fenêtre en cours n'est pas active
  (setq Var1 T)                                                                  ; Variable qui sert à sauter le 1er test
)

; Enumération des présentations
(Defun Boucle1 (CurrentLayout0)
  (if (Not (Equal CurrentLayout0 ModelSpace0))                                   ; Si la présentation en cours n'est pas l'espace objet
    (progn
      (setq Var1 nil)                                                            ; Variable utile pour la boucle2
      (if (= (vla-get-count (Vla-Get-Block CurrentLayout0)) 0)
        (progn
          (vla-put-activelayout Activedoc CurrentLayout0)
          (vla-put-activelayout Activedoc ActiveLayout0)
        ))
      (vlax-map-collection (Vla-Get-Block CurrentLayout0) 'Boucle2)              ; boucle sur les sous-objets de la présentation
    ))
)

; Programme principal
(Defun C:GelerCalqueP ()
  (vl-load-com)                                                                  ; Chargement des fonctions ActiveX
  (setq *old1* (getvar "cmdecho"))                                               ; Sauvegarde de la variable
  (setq *old2* (getvar "LAYOUTREGENCTL"))                                        ; Sauvegarde de la variable
  (setvar "cmdecho" 0)                                                           ; Messages désactivés
  (setvar "LAYOUTREGENCTL" 0)                                                    ; Régénération systématique des présentations
  (Setq ActiveLayer0 (getvar "CLayer"))                                          ; Nom du calque courant
  (Setq ActiveDoc (vla-get-ActiveDocument (vlax-get-acad-object)))               ; Document actif
  (setq ActiveLayout0 (vla-get-activelayout ActiveDoc))                          ; Présentation courante (y compris espace objet)
  (if (= (getvar "tilemode") 0)                                                  ; si on est dans l'espace papier
    (setq ActivePViewport0 (Vla-Get-ActivePViewport ActiveDoc))                  ; alors il faut sauvegarder la fenêtre active
    (setq ActivePViewport0 nil)                                                  ; sinon pas de fenêtre active
  )
  (setq ModelSpace0 (vla-get-Layout (vla-get-ModelSpace Activedoc)))             ; Identificateur de l'espace objet
  (setq Layouts0 (vla-get-Layouts ActiveDoc))                                    ; liste de toutes les présentations (+ espace objet)
  (vlax-map-collection Layouts0 'Boucle1)                                        ; sous-programme de traitement de chaque présentation
  (setq Var1 nil)                                                                ; Variable qui sert à choisir le message final
  (if (and ActivePViewport0                                                      ; s'il existe une fenêtre active
    (not (equal (Vla-Get-ActivePViewport ActiveDoc) ActivePViewport0)))          ; et que cette fenêtre n'est plus la même qu'au départ
      (progn
        (Vla-put-ActivePViewport ActiveDoc ActivePViewport0)                     ; alors restaurer la fenêtre de départ
        (setq Var1 T)                                                            ; choisir le message final correspondant
      )
  )
  (RestaureEnv)                                                                  ; Appel à la procédure de restauration
  (if var1                                                                       ; choix du message
    (princ (strcat "Le calque " ActiveLayer0 " a été gelé dans les autres fenêtres flottantes"))
    (princ (strcat "Le calque " ActiveLayer0 " a été gelé dans toutes les fenêtres flottantes"))
  )
  (princ)
)

(princ "\nFonction C:GelerCalqueP chargée - tapez GelerCalqueP pour l'éxecuter.")
(princ)

; pour utiliser ce programme, créer un bouton dans une barre d'outils.
; Puis taper la macro : ^C^CGelerCalqueP
; A chaque session Autocad, il faut recharger le programme,
; ou-bien le rajouter à la liste des programmes chargés au démarrage (menu Outils->Charger une application...)

; Pour moi :
; (Princ (= (cdadr (entget (vlax-vla-object->ename CurrentPViewport0))) "VIEWPORT") ; ceci est une autre méthode d'identification
