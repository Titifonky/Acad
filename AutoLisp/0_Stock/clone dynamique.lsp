(defun c:dy (/	       sv_shmnu	 loop	   key	     ent
	     dxf_ent   nam_bl	 typ_ent   lay_ent   lin_ent
	     col_ent   wid_ent	 sct_ent   flag	     tabl_dxf
	     cmd_clone
	    )
  (setq
    sv_shmnu (getvar "SHORTCUTMENU")
    loop     0
  ) ;_ Fin de setq
  (setvar "SHORTCUTMENU" 11)
  (while (and (setq key (grread T 4 0))
	      (not (member key '((2 13) (2 32))))
	      (/= (car key) 25)
	 ) ;_ Fin de and
    (cond
      ((and (eq (car key) 3) ent)
       (setq loop (rem (1+ loop) 2))
      )
      (T
       (setq ent (nentselp "" (cadr key)))
      )
    ) ;_ Fin de cond
    (cond
      ((and ent (zerop loop))
       (if (eq (type (car (last ent))) 'ENAME)
	 (setq
	   dxf_ent (entget (car (last ent)))
	   nam_bl  (cdr (assoc 2 dxf_ent))
	 ) ;_ Fin de setq
	 (setq dxf_ent (entget (car ent))
	       nam_bl  nil
	 ) ;_ Fin de setq
       ) ;_ Fin de if
       (if (eq (cdr (assoc 0 dxf_ent)) "VERTEX")
	 (setq dxf_ent (entget (cdr (assoc 330 dxf_ent))))
       ) ;_ Fin de if
       (setq
	 typ_ent (cdr (assoc 0 dxf_ent))
	 lay_ent (cdr (assoc 8 dxf_ent))
	 lin_ent (cdr (assoc 6 dxf_ent))
	 col_ent (cdr (assoc 62 dxf_ent))
	 wid_ent (cdr (assoc 370 dxf_ent))
	 sct_ent (cdr (assoc 48 dxf_ent))
       ) ;_ Fin de setq
       (grtext -2 typ_ent)
      )
    ) ;_ Fin de cond
  ) ;_ Fin de while
  (cond
    ((eq typ_ent "LWPOLYLINE")
     (setq typ_ent "PLINE")
    )
    ((eq typ_ent "POLYLINE")
     (setq flag (rem (cdr (assoc 70 dxf_ent)) 128))
     (cond
       ((< flag 6)
	(setq typ_ent "PLINE")
       )
       ((and (> flag 7) (< flag 14))
	(setq typ_ent "3DPOLY")
       )
       ((> flag 15)
	(setq typ_ent "3DMESH")
       )
     ) ;_ Fin de cond
    )
    ((or (eq typ_ent "HATCH") (eq typ_ent "SHAPE"))
     (setq nam_bl (cdr (assoc 2 dxf_ent)))
    )
    ((eq typ_ent "DIMENSION")
     (setq nam_bl nil)
     (cond
       ((eq (boole 6 (rem (cdr (assoc 70 dxf_ent)) 128) 32) 0)
	(setq typ_ent "DIMLINEAR")
       )
       ((eq (boole 6 (rem (cdr (assoc 70 dxf_ent)) 128) 32) 1)
	(setq typ_ent "DIMALIGNED")
       )
       ((or (eq (boole 6 (rem (cdr (assoc 70 dxf_ent)) 128) 32) 2)
	    (eq (boole 6 (rem (cdr (assoc 70 dxf_ent)) 128) 32) 5)
	) ;_ Fin de or
	(setq typ_ent "DIMANGULAR")
       )
       ((eq (boole 6 (rem (cdr (assoc 70 dxf_ent)) 128) 32) 3)
	(setq typ_ent "DIMDIAMETER")
       )
       ((eq (boole 6 (rem (cdr (assoc 70 dxf_ent)) 128) 32) 4)
	(setq typ_ent "DIMRADIUS")
       )
       ((or (eq (boole 6 (rem (cdr (assoc 70 dxf_ent)) 128) 32) 6)
	    (eq (boole 6 (rem (cdr (assoc 70 dxf_ent)) 128) 32) 70)
	) ;_ Fin de or
	(setq typ_ent "DIMORDINATE")
       )
       (T (setq typ_ent "DIM"))
     ) ;_ Fin de cond
    )
    ((eq typ_ent "VIEWPORT")
     (setq typ_ent "VPORTS")
    )
    ((eq typ_ent "3DSOLID")
     (initget
       1
       "BOîte Sphère CYlindre CÔne BIseau Tore _Box Sphere CYlinder COne Wedge Torus"
     ) ;_ Fin de initget
     (setq typ_ent
	    (getkword "\n[BOîte/Sphère/CYlindre/CÔne/BIseau/Tore]: ")
     ) ;_ Fin de setq
    )
  ) ;_ Fin de cond
  (grtext -2 "")
  (setvar "SHORTCUTMENU" sv_shmnu)
  (cond
    (typ_ent
     (setvar "clayer" lay_ent)
     (if lin_ent
       (setvar "celtype" lin_ent)
       (setvar "celtype" "ByLayer")
     ) ;_ Fin de if
     (if col_ent
       (setvar "cecolor" (itoa col_ent))
       (setvar "cecolor" "256")
     ) ;_ Fin de if
     (if wid_ent
       (setvar "celweight" wid_ent)
       (setvar "celweight" -1)
     ) ;_ Fin de if
     (if sct_ent
       (setvar "celtscale" sct_ent)
       (setvar "celtscale" 1.0)
     ) ;_ Fin de if
     (setq cmd_clone (strcat "_." typ_ent))
     (if nam_bl
       (progn
	 (if (and (setq tabl_dxf (tblsearch "BLOCK" nam_bl))
		  (eq (boole 1 (cdr (assoc 70 tabl_dxf)) 4) 4)
	     ) ;_ Fin de and
	   (command "_.-XREF" "_attach" nam_bl)
	   (command cmd_clone nam_bl)
	 ) ;_ Fin de if
       ) ;_ Fin de progn
       (command cmd_clone)
     ) ;_ Fin de if
    )
    (T (prin1))
  ) ;_ Fin de cond
) ;_ Fin de defun
