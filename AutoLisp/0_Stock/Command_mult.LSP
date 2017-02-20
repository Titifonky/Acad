;;; Recherche et suppression des réacteurs existant
;;;(defun remove-reac ()
;;;  (if (not (vlr-reactions reac-un))
;;;    (if	(equal (caar (vlr-reactions reac-un)) :vlr-unknownCommand)
;;;      (vlr-remove reac-un)
;;;    ) ;_ Fin de if
;;;  ) ;_ Fin de if
;;;) ;_ Fin de defun
;;(if (equal (caar (vlr-reactions reac-end)) :vlr-commandEnded) (vlr-remove reac-end))
(vlr-remove-all :vlr-editor-reactor)

(defun find-cmd	(name-cmd / desc-file)
  (setq	pos	  (vl-string-search "'" cmd)
	name-cmd  (strcat (substr cmd 1 pos) ",")
	desc-file (open (findfile "acad.pgp") "r")
	f-cmd	  nil
  ) ;_ Fin de setq
  (while (and (not f-cmd) (setq line (read-line desc-file)))
    (if
      (not
	(equal ";" (substr line 1 1))
      ) ;_ Fin de not
       (if (vl-string-search name-cmd line)
	 (setq
	   f-cmd (getcname (vl-string-trim
			     "\t *"
			     (vl-string-left-trim name-cmd line)
			   ) ;_ Fin de vl-string-trim
		 ) ;_ Fin de getcname
	 ) ;_ Fin de setq
	 (setq f-cmd nil)
       ) ;_ Fin de if
    ) ;_ Fin de if
  ) ;_ Fin de while
  (close desc-file)
) ;_ Fin de defun

(defun command_mult (reac cmd)
  (setq var (getvar "cmdecho"))
  (setvar "cmdecho" 1)
  (if (vl-string-search "'" (setq cmd (car cmd)))
    (progn
      (setq flag nil)
      (princ "\n1")
      (find-cmd cmd)
 ;_ Fin de if
    ) ;_ Fin de progn
  ) ;_ Fin de if
  (if (not (equal f-cmd nil))
    (progn
      (setq flag t)
    ) ;_ Fin de progn
  ) ;_ Fin de if
  (setvar "cmdecho" var)
) ;_ Fin de defun

(defun command_suite (reac cmd-end)
  (princ "\n2")
  (princ)
) ;_ Fin de defun

(setq reac-un (vlr-editor-reactor
		nil
		'((:vlr-unknownCommand . command_mult))
	      ) ;_ Fin de vlr-editor-reactor
) ;_ Fin de setq

(setq reac-end (vlr-editor-reactor
		 nil
		 '((:vlr-commandended . command_suite))
	       ) ;_ Fin de vlr-editor-reactor
) ;_ Fin de setq