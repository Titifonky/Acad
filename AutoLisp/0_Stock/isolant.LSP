;***	Isolant.LSP
;
;***    adapté à Autocad 14 Français le 16/03/99 par roule.@wanadoo.fr
;
;***	Written by Don J. Buschert (c) 1992, 1997
;
;	Email:	don.buschert@sait.ab.ca
;		buschert@spots.ab.ca
;	AutoCAD Page:	http://www.spots.ab.ca/~buschert/ 
;
;	Disclaimer:
;	Permission to use, copy, modify, and distribute this software 
;	for any purpose and without fee is hereby granted, provided
;	that the above copyright notice appears in all copies and 
;	that both that copyright notice and the limited warranty and 
;	restricted rights notice below appear in all supporting 
;	documentation.
;
;	THIS PROGRAM IS PROVIDED "AS IS" AND WITH ALL FAULTS.  THE AUTHOR
;	SPECIFICALLY DISCLAIMS ANY IMPLIED WARRANTY OF MERCHANTABILITY OR
;	FITNESS FOR A PARTICULAR USE.  THE AUTHOR ALSO DOES NOT WARRANT THAT
;	THE OPERATION OF THE PROGRAM WILL BE UNINTERRUPTED OR ERROR FREE.
;
;	Version 4.1  01/06/97
;	Added OSNAP controls.
;	Version 4.0  08/14/92 
;	Initial release...
; 
;	This program draws a polyline in the shape of insulation.
;	The user enters or picks the insulation width; and enters or 
;	picks the insulation length and angle. 
; 
;***	Function  INSUL
; 
(princ "\nChargement, attendez s'il vous plait...") 
(defun C:ISOLANT (/ 
                svbm		;"BLIPMODE"
                sv_osmode	;"OSMODE"
                poi1	;Points which calculate the arcs and lines of the 
			;first batt. 
                poi2
                poi3
                poi4
                poi5
                poi6
                poi7
                iang	;Angle. 
                idis	;Approximate insulation length
                iinc	;Increment distance.
                ;iwid	;Insulation width (global)
                poic	;Center point of arc.
                stpt	;Start point of insulation 
             ) 
  (graphscr) 
  (setvar "CMDECHO" 0)
  (command "_UNDO" "_M")
  (setq svbm (getvar "BLIPMODE"))
  (setq sv_osmode (getvar "OSMODE"))
  (setq stpt (getpoint "\nPoint de départ de l'isolant: ")) 
  (if iwid 
    (setq prom (strcat " <" (rtos iwid) ">")) 
    (setq prom "") 
  ) 
  (setq flag (getdist (strcat "\nEpaisseur de l'isolant" prom ": "))) 
  (if flag 
    (setq iwid flag) 
  ) 
  (setq endp 
     (getpoint stpt "\nPointer ou donner le point d'arrivée: ") 
  )
  (setq iang (angle stpt endp))
  (setq idis (distance stpt endp))
  (setvar "OSMODE" 0)
  (princ "\nDessin de l'isolant en cours...")
  (setvar "BLIPMODE" 0)
  (setvar "TRACEWID" 0)
  (setq poi1 (polar stpt iang (* iwid 0.05)))
  (setq iinc (distance poi1 stpt))
  (setq poi2 (polar poi1 (+ (dtor 90) iang) (* iwid 0.30)))
  (setq poi2 (polar poi2 (+ (dtor 180) iang) (* iwid 0.05)))
  (command "_PLINE" poi1 poi2)
  (while (< iinc idis)
    (setq poic (polar poi2 iang (* iwid 0.20)))
    (setq iinc (+ (distance poic poi2) iinc))
    (if (< iinc idis)
      (progn
        (setq poi3 (polar poic (+ (dtor 90) iang) (* iwid 0.20)))
        (setq poi4 (polar poic iang (* iwid 0.20)))
        (setq iinc (+ (distance poi4 poic) iinc))
        (if (< iinc idis)
          (progn
            (command "_ARC" "CE" poic "A" "-180")
            (setq poi5 (polar poi4 (+ (dtor -90) iang) (* iwid 0.60)))
            (setq poi5 (polar poi5 (+ (dtor 180) iang) (* iwid 0.10)))
            (command "_Line" poi5)
            (setq poic (polar poi5 iang (* iwid 0.20)))
            (setq iinc (+ (* iwid 0.10) iinc))
            (if (< iinc idis)
              (progn
                (setq poi6 (polar poic (+ (dtor -90) iang) (* iwid 0.20)))
                (setq poi7 (polar poic iang (* iwid 0.20)))
                (setq iinc (+ (* iwid 0.20) iinc))
                (if (< iinc idis)
                  (progn
                    (command "_ARC" "CE" poic poi7)
                    (setq poi2 (polar poi7 (+ (dtor 90) iang) (* iwid 0.60)))
                    (setq poi2 (polar poi2 (+ (dtor 180) iang) (* iwid 0.10)))
                    (command "_Line" poi2)
                    (setq iinc (- iinc (* iwid 0.10)))
                  )
                  (command "_ARC" "CE" poic poi6)  
                )
              )
            )
          )
          (command "_ARC" "CE" poic "A" "-90")
        )
      )
    )
  )
  (command "")
  (princ "\nDessin isolant terminé...") 
  (setvar "BLIPMODE" svbm) 
  (setvar "OSMODE" sv_osmode)
  (princ) 
) 
; 
;***	End of Program
;***	Support Programs
;***  Function  DTOR
;Converts degrees to radians, where degr is degrees; returns a
;radian value.
;
(defun DTOR (degr)
  (/ (* degr pi) 180)
)
