(defun dcc1  ()
  ;; j'efface la liste
  (setq dclist '("1.00"))
  (start_list "dcselec")
  (mapcar 'add_list dclist)
  (end_list))

(defun dcc2  ()
  (setq dec $value)
  (if (= (member dec dclist) nil)
    (setq dclist (cons dec dclist))))

(defun dcc3 () (setq dec (nth (atoi $value) dclist)) (set_tile "dc_selec" dec))

(defun c:dcc  (/ ent pt tmp-dcc-DCL file-DCL)
  (setq	tmp-dcc-DCL (vl-filename-mktemp "tmp-dcc-DCL.dcl")
	file-DCL    (open tmp-dcc-DCL "w"))
  (write-line
"CHOIXDC:dialog
{label = \"Valeur du décalage\";
:button{label=\"Effacer la liste\";key=\"dellist\" ;width=5;}

:boxed_column{alignment = centered;width = 25;fixed_width = true;spacer;
:list_box{label = \"décalage\";key = \"dcselec\";fixed_width = true;width = 25;height = 10;list = dclist;}
//:button{label=\"Pick >\";key=\"pick\" ;width=5;}
	:edit_box{label=\"Décalage\";key=\"dc_selec\";edit_width=10;}
	
spacer;

	:row{alignment = centered;
	:button{label=\"OK\";key=\"ACCEPT\";is_default=true;width=5;	}
	:button{label=\"Annuler\";key=\"CANCEL\";width=5;is_cancel=true;}

	}
}
}"  file-DCL)
  (close file-DCL)
  (if (not dec)
    (setq dec "1.00"))
  (if (not dclist)
    (setq dclist '("0.10" "0.20" "0.30" "0.40" "0.50" "0.52" "0.70" "1.00")))
  (setq id_dcl (load_dialog tmp-dcc-DCL))
  (if (not (new_dialog "CHOIXDC" id_dcl))
    (exit))
  (set_tile "dc_selec" dec)
  (setq dclist (acad_strlsort dclist))
  (start_list "dcselec")
  (mapcar 'add_list dclist)
  (end_list)
  (action_tile "dcselec" "(dcc3)")
  (action_tile "dc_selec" "(dcc2) ")
  (action_tile "dellist" "(dcc1)")
  (start_dialog)
  (unload_dialog id_dcl)
  (setq ent (car (entsel "\nChoix de l'objet à decaler :")))
  (setq pt (getpoint "\nCoté a decaler :"))
  (command "decaler" (atof dec) ent pt)
  (princ))

(princ "\nCommand DCC chargée......")