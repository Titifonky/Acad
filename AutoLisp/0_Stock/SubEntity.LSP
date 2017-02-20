(defun c:subent ()
;;;  (while   
;;;     (setq Ent (entsel "\nPick an entity: "))
;;;     (print (strcat "Entity handle is: " 
;;;          (cdr (assoc 5 (entget (car Ent))))))
;;;   )
   (while   
      (setq Ent (nentsel "\nPick an entity or subEntity: "))
      (print (strcat "Entity or subEntity handle is:  " 
          (cdr (assoc 5 (entget (car Ent))))))
   )
  (prompt "\nDone.")
  (princ)
)
