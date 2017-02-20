; Programme qui permet d'�teindre le calque courant dans toutes les fen�tres flottantes
; de toutes les pr�sentations, sauf �ventuellement dans la fen�tre active.

; Gestion des erreurs et restauration de l'environnement
; Cette proc�dure est st�reotyp�e dans mes programmes LISP
(defun *error* (msg)
  (command)                                                                      ; Annulation des commandes en cours
  (restaureEnv)                                                                  ; Appel � la proc�dure de restauration
  (princ "\nerreur: ")
  (princ msg)
  (princ)
)

; Restauration de l'environnement
; Cette proc�dure est st�reotyp�e dans mes programmes LISP
(defun RestaureEnv ()
  (if (/= *old1* nil) (setvar "cmdecho" *old1*))                                 ; restaure la variable CMDECHO
  (if (/= *old2* nil) (setvar "LAYOUTREGENCTL" *old2*))                          ; restaure la variable LAYOUTREGENCTL
)

; Boucle qui permet d'�teindre le calque courant dans la fen�tre en cours.
; Ceci se fait par le biais des donn�es �tendues (xDtata) de la fen�tre en cours,
; dans lesquelles il faut incorporer la liste point�e
; (1003 . Nom_du_Calque) entre les codes (1002 . {) et (1002 . })
(Defun Boucle3 ( / xData xType NewData NewType k)
  (vla-getxdata CurrentPViewport0 "" 'xType 'xData)                              ; R�cup�ration des donn�es xData de la fen�tre
  (setq k (vlax-safearray-get-u-bound xType 1))                                  ; Nb d'�l�ments dans xData
  (setq NewType (vlax-make-safearray vlax-vbInteger (cons 0 (+ k 1))))           ; Cr�ation d'un nouveau tableau Type
  (setq NewData (vlax-make-safearray vlax-vbVariant (cons 0 (+ k 1))))           ; Cr�ation d'un nouveau tableau Data

  (setq n 0)
  (repeat (+ k 1)                                                                ; boucle pour recopier les �l�ments des tableaux
    (vlax-safearray-put-element Newtype n (vlax-safearray-get-element xType n))  ; Recopie xType dans NewType
    (vlax-safearray-put-element NewData n (vlax-safearray-get-element xData n))  ; Recopie xData dans NewData
    (setq n (1+ n))                                                              ; Incr�mente
  )
  
  (vlax-safearray-put-element Newtype (+ k 1) 1002)                              ; insere le dernier atome de la liste
  (vlax-safearray-put-element NewData (+ k 1) "}")                               ; insere le dernier atome de la liste
  
  (vlax-safearray-put-element Newtype (- k 1) 1003)                              ; insere l'atome en avant derni�re position
  (vlax-safearray-put-element NewData (- k 1) ActiveLayer0)                      ; insere l'atome en avant derni�re position

  (vla-setxdata CurrentPViewport0 NewType NewData)                               ; Enregistre la nouvelle liste dans la fen�tre

  (if (equal CurrentLayout0 ActiveLayout0)                                       ; Si l'utilisateur se trouve dans une pr�sentation
    (progn                                                                       ; alors il faut r�g�n�rer les fen�tres de la pr�sentation
      (vla-display CurrentPViewport0 0)                                          ; R�g�n�re la fen�tre
      (vla-display CurrentPViewport0 1)                                          ; R�g�n�re la fen�tre
    ))
)

; Enum�ration des fen�tres flottantes
; Les sous-objets d'une pr�sentation repr�sentent tous les �l�ments dessin�s dans la pr�sentation (blocs, fen�tres...),
; mais le premier sous-objet repr�sente toujours la vue active de la pr�sentation elle-m�me.
; Il faut donc sauter le premier test.
(Defun Boucle2 (CurrentPViewport0)
  (if (and var1 (vlax-method-applicable-p CurrentPViewport0 'Display)            ; V�rification que l'objet est une fen�tre (PViewPort)
    (not (Equal CurrentPViewport0 ActivePViewport0))) (boucle3))                 ; et que la fen�tre en cours n'est pas active
  (setq Var1 T)                                                                  ; Variable qui sert � sauter le 1er test
)

; Enum�ration des pr�sentations
(Defun Boucle1 (CurrentLayout0)
  (if (Not (Equal CurrentLayout0 ModelSpace0))                                   ; Si la pr�sentation en cours n'est pas l'espace objet
    (progn
      (setq Var1 nil)                                                            ; Variable utile pour la boucle2
      (if (= (vla-get-count (Vla-Get-Block CurrentLayout0)) 0)
        (progn
          (vla-put-activelayout Activedoc CurrentLayout0)
          (vla-put-activelayout Activedoc ActiveLayout0)
        ))
      (vlax-map-collection (Vla-Get-Block CurrentLayout0) 'Boucle2)              ; boucle sur les sous-objets de la pr�sentation
    ))
)

; Programme principal
(Defun C:GelerCalqueP ()
  (vl-load-com)                                                                  ; Chargement des fonctions ActiveX
  (setq *old1* (getvar "cmdecho"))                                               ; Sauvegarde de la variable
  (setq *old2* (getvar "LAYOUTREGENCTL"))                                        ; Sauvegarde de la variable
  (setvar "cmdecho" 0)                                                           ; Messages d�sactiv�s
  (setvar "LAYOUTREGENCTL" 0)                                                    ; R�g�n�ration syst�matique des pr�sentations
  (Setq ActiveLayer0 (getvar "CLayer"))                                          ; Nom du calque courant
  (Setq ActiveDoc (vla-get-ActiveDocument (vlax-get-acad-object)))               ; Document actif
  (setq ActiveLayout0 (vla-get-activelayout ActiveDoc))                          ; Pr�sentation courante (y compris espace objet)
  (if (= (getvar "tilemode") 0)                                                  ; si on est dans l'espace papier
    (setq ActivePViewport0 (Vla-Get-ActivePViewport ActiveDoc))                  ; alors il faut sauvegarder la fen�tre active
    (setq ActivePViewport0 nil)                                                  ; sinon pas de fen�tre active
  )
  (setq ModelSpace0 (vla-get-Layout (vla-get-ModelSpace Activedoc)))             ; Identificateur de l'espace objet
  (setq Layouts0 (vla-get-Layouts ActiveDoc))                                    ; liste de toutes les pr�sentations (+ espace objet)
  (vlax-map-collection Layouts0 'Boucle1)                                        ; sous-programme de traitement de chaque pr�sentation
  (setq Var1 nil)                                                                ; Variable qui sert � choisir le message final
  (if (and ActivePViewport0                                                      ; s'il existe une fen�tre active
    (not (equal (Vla-Get-ActivePViewport ActiveDoc) ActivePViewport0)))          ; et que cette fen�tre n'est plus la m�me qu'au d�part
      (progn
        (Vla-put-ActivePViewport ActiveDoc ActivePViewport0)                     ; alors restaurer la fen�tre de d�part
        (setq Var1 T)                                                            ; choisir le message final correspondant
      )
  )
  (RestaureEnv)                                                                  ; Appel � la proc�dure de restauration
  (if var1                                                                       ; choix du message
    (princ (strcat "Le calque " ActiveLayer0 " a �t� gel� dans les autres fen�tres flottantes"))
    (princ (strcat "Le calque " ActiveLayer0 " a �t� gel� dans toutes les fen�tres flottantes"))
  )
  (princ)
)

(princ "\nFonction C:GelerCalqueP charg�e - tapez GelerCalqueP pour l'�xecuter.")
(princ)

; pour utiliser ce programme, cr�er un bouton dans une barre d'outils.
; Puis taper la macro : ^C^CGelerCalqueP
; A chaque session Autocad, il faut recharger le programme,
; ou-bien le rajouter � la liste des programmes charg�s au d�marrage (menu Outils->Charger une application...)

; Pour moi :
; (Princ (= (cdadr (entget (vlax-vla-object->ename CurrentPViewport0))) "VIEWPORT") ; ceci est une autre m�thode d'identification
