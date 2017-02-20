(defun C:FLATTEN (/       tmpucs  olderr  oldcmd  zeroz   ss1     ss1len
                  i       numchg  numnot  numno0  ssno0   ename   elist
                  etype   yorn    vrt     crz
                 )
  (setq tmpucs "$FLATTEN-TEMP$")        ;temporary UCS

  ;;Error handler
  (setq olderr *error*)
  (defun *error* (msg)
    (if (or
          (= msg "Fonction annulée")
          (= msg "quit / exit abort")
        )
      ;;if user cancelled or program aborted, exit quietly
      (princ)
      ;;otherwise report error message
      (princ (strcat "\nError: " msg))
    )
    (setq *error* olderr)
    (if (tblsearch "UCS" tmpucs)
      (command "._UCS" "_Restore" tmpucs "._UCS" "_Delete" tmpucs)
    )
    (command "._UNDO" "_End")
    (setvar "CMDECHO" oldcmd)
    (princ)
  )

  ;;Function to change Z coordinate to 0

  (defun zeroz (key zelist / oplist nplist)
    (setq oplist (assoc key zelist)
          nplist (reverse (append '(0.0) (cdr (reverse oplist))))
          zelist (subst nplist oplist zelist)
    )
    (entmod zelist)
  )
  ;;Setup
  (setq oldcmd (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)
  (command "._UNDO" "_Group")
  (command "._UCS" "_Delete" tmpucs "._UCS" "_Save" tmpucs "._UCS" "_World")
                                        ;set World UCS

  ;;Get input
  (prompt
    (strcat
      "\nFLATTEN Deplace les coordonnées de l'axe Z des objets selectionnés à 0."
    )
  )

  (initget "Oui Non")
  (setq yorn (getkword "\nVoulez vous continuer <O>: "))
  (cond ((/= yorn "Non")
         (graphscr)
         (prompt "\nChoisissez les objets à aplatir ")
         (prompt
           "\n[Appuiez sur Entrer pour selectionnez tout les objets du dessin]"
         )
         (setq ss1 (ssget))
         (if (null ss1)                 ;if enter...
           (setq ss1 (ssget "_X"))      ;select all entities in database
         )


         ;;*initialize variables
         (setq ss1len (sslength ss1)    ;length of selection set
               i      0                 ;loop counter
               numchg 0                 ;number changed counter
               numnot 0                 ;number not changed counter
               numno0 0                 ;number not changed and Z /= 0 counter
               ssno0  (ssadd)           ;selection set of unchanged entities
         )                              ;setq

         ;;*do the work
         (prompt "\nEn cours.")
         (while (< i ss1len)            ;while more members in the SS
           (if (= 0 (rem i 10))
             (prompt ".")
           )
           (setq ename (ssname ss1 i)   ;entity name
                 elist (entget ename)   ;entity data list
                 etype (cdr (assoc 0 elist)) ;entity type
           )

           ;;*Keep track of entities not flattened
           (if (not (member etype
                            '("3DFACE"     "ARC"        "ATTDEF"
                              "CIRCLE"     "DIMENSION"  "ELLIPSE"
                              "HATCH"      "INSERT"     "LINE"
                              "LWPOLYLINE" "MTEXT"      "POINT"
                              "POLYLINE"   "SOLID"      "TEXT"
                             )
                    )
               )
             (progn                     ;leave others alone
               (setq numnot (1+ numnot))
               (if (/= 0.0 (car (reverse (assoc 10 elist))))
                 (progn                 ;add it to special list if Z /= 0
                   (setq numno0 (1+ numno0))
                   (ssadd ename ssno0)
                 )
               )
             )
           )

           ;;Change group 10 Z coordinate to 0 for listed entity types.
           (if (member etype
                       '("3DFACE"    "ARC"       "ATTDEF"    "CIRCLE"
                         "DIMENSION" "ELLIPSE"   "HATCH"     "INSERT"
                         "LINE"      "MTEXT"     "POINT"     "POLYLINE"
                         "SOLID"     "TEXT"
                        )
               )
             (setq elist  (zeroz 10 elist) ;change entities in list above
                   numchg (1+ numchg)
             )
           )

           ;;Change group 11 Z coordinate to 0 for listed entity types.
           (if (member etype
                       '("3DFACE" "ATTDEF" "DIMENSION" "LINE" "TEXT" "SOLID")
               )
             (setq elist (zeroz 11 elist))
           )

           ;;Change groups 12 and 13 Z coordinate to 0 for SOLIDs and 3DFACEs.
           (if (member etype '("3DFACE" "SOLID"))
             (progn
               (setq elist (zeroz 12 elist))
               (setq elist (zeroz 13 elist))
             )
           )

           ;;Change groups 13, 14, 15, and 16
           ;;Z coordinate to 0 for DIMENSIONs.
           (if (member etype '("DIMENSION"))
             (progn
               (setq elist (zeroz 13 elist))
               (setq elist (zeroz 14 elist))
               (setq elist (zeroz 15 elist))
               (setq elist (zeroz 16 elist))
             )
           )

           ;;Change each polyline vertex Z coordinate to 0.
           ;;Code provided by Vladimir Livshiz, 09-Oct-1998
           (if (= etype "POLYLINE")
             (progn
               (setq vrt ename)
               (while (not (equal (cdr (assoc 0 (entget vrt))) "SEQEND"))
                 (setq elist (entget (entnext vrt)))
                 (setq crz (cadddr (assoc 10 elist)))
                 (if (/= crz 0)
                   (progn
                     (zeroz 10 elist)
                     (entupd ename)
                   )
                 )
                 (setq vrt (cdr (assoc -1 elist)))
               )
             )
           )

           ;;Special handling for LWPOLYLINEs
           (if (member etype '("LWPOLYLINE"))
             (progn
               (setq elist  (subst (cons 38 0.0) (assoc 38 elist) elist)
                     numchg (1+ numchg)
               )
               (entmod elist)
             )
           )

           (setq i (1+ i))              ;next entity
         )
         (prompt " Done.")

         ;;Print results
         (prompt (strcat "\n" (itoa numchg) " objet(s) aplatit."))
         (prompt
           (strcat "\n" (itoa numnot) " object(s) non aplatit.")
         )

         ;;If there any entities in ssno0, show them
         (if (/= 0 numno0)
           (progn
             (prompt (strcat "  ["
                             (itoa numno0)
                             " non plan]"
                     )
             )
             (getstring
               "\nAppuiez sur entrez pour voir tout les objets non plan inchangé... "
             )
             (command "._SELECT" ssno0)
             (getstring "\nAppuiez sur Entrer pour les deselectionnez... ")
             (command "")
           )
         )
        )
  )

  (command "._UCS" "_Restore" tmpucs "._UCS" "_Delete" tmpucs)
  (command "._UNDO" "_End")
  (setvar "CMDECHO" oldcmd)
  (setq *error* olderr)
  (princ)
)