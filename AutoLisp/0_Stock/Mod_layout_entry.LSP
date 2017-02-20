(defun c:rp (/ indice no_plan)
  (Princ
    "\nAttention !!!, seul les plans commencant par la lettre 'P' seront renommé"
  ) ;_ Fin de Princ
  (setq	no_plan	(getstring "\nNumero des plans :")
	indice	(getstring "\nIndice des plans :")
  ) ;_ Fin de setq
  (mod_layout indice no_plan)
) ;_ Fin de defun
