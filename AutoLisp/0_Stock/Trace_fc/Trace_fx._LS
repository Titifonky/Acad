;********************* à modifier en fonction du dossier racine ********************* 

(setq racine_dossier "C:/Documents and Settings/etienne c/Mes documents/Bibliothèque Lisp/Trace_fc")

;************************************************************************************ 


(defun c:tf ()
  (vl-load-com)
  (setq	cmde (getvar "cmdecho")
	acc  (getvar "osmode")
	flat (getvar "flatland")
  )
  (setvar "cmdecho" 0)
  (setvar "osmode" 0)
  (setvar "flatland" 0)
  
  (command "_undo" "_m")

  (setq dcl_id (load_dialog (strcat racine_dossier "/Trace_fx_DlgBox.dcl")))

  (if (not (new_dialog "traceur" dcl_id))
    (exit)
    )
  
  (action_tile "intervalle_depart" "(setq depart (atof $value))")
  (action_tile "intervalle_fin" "(setq fin (atof $value))")
  (action_tile "nb_pitch" "(setq nb_pitch (atof $value))")
  (action_tile "equation" "(setq equation $value)")
  (action_tile "echelle" "(setq echelle (atof $value))")
  
  (start_dialog)

  (setq pitch (/ (- fin depart) nb_pitch))
  (setq x depart)

  (command "polylign")

  (while (<= x fin)
    (command
      (list
	(* x echelle)
	(c:cal equation)
      )
    )
    (setq x (+ (float x) (float pitch)))
  )

  (command "")

  (unload_dialog dcl_id)

  (setvar "cmdecho" cmde)
  (setvar "flatland" flat)
  (setvar "osmode" acc)
  (princ)
)