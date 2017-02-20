(defun c:di ()

(vl-load-com)

(setq util (vla-get-utility 
                   (vla-get-activedocument 
                        (vlax-get-acad-object))))
                        
(vla-getentity util 'obj1 'ip "\nSelect First Object: ")

(vla-getentity util 'obj2 'ip "\nSelect Second Object: ")

(setq int (vla-IntersectWith obj1 obj2 acExtendBoth))

(princ int)

(princ)

);defun