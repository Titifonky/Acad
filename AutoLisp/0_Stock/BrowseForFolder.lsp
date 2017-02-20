;;;Hmmmm...  I didn't notice the 'Self' property before, which
;;;makes it a bit simpler:

(defun BrowseForFolder ( / sh folder folderobject result)
   (vl-load-com)
   (setq sh
      (vla-getInterfaceObject
         (vlax-get-acad-object)
         "Shell.Application"
      )
   )

   (setq folder
      (vlax-invoke-method
          sh
          'BrowseForFolder
          0
          ""
          0
       )
   )
   (vlax-release-object sh)

   (if folder
      (progn
         (setq folderobject
            (vlax-get-property folder 'Self)
         )
         (setq result
            (vlax-get-property FolderObject 'Path)
         )
         (vlax-release-object folder)
         (vlax-release-object FolderObject)
         result
      )
   )
)

;;;-- 
;;;http://www.caddzone.com
;;;
;;;AcadXTabs: MDI Document Tabs for AutoCAD 2004/2005
;;;http://www.acadxtabs.com
;;;
;;;"j.buzbee" <j...@alfonsoarchitects.com> wrote in message
;;;
;;;news:4117e99a_3@newsprd01...
;;; Yep - got it!  Was hoping for a FileNav dialog . . .
;;;
;;;http://msdn.microsoft.com/library/default.asp?url=/library/en-us/shel...
;;;
;;; "Tony Tanzillo" <tony.tanzillo@U_KNOW_WHERE.com> wrote in message
;;; news:4117e865$1_1@newsprd01...
;;; If you're using (vlax-dump-object) you won't see them.
;;;
;;; You have to look in MSDN to find out what methods/properties are
;;; supported.
;;;
;;; --
;;; http://www.caddzone.com
;;;
;;; AcadXTabs: MDI Document Tabs for AutoCAD 2004/2005
;;; http://www.acadxtabs.com
;;;
;;; "j.buzbee" <j...@alfonsoarchitects.com> wrote in message
;;; news:4117e579$1_1@newsprd01...
;;; Ok Tony, I give up . . . where do you find the methods associated with
;;; an
;;; IShellDispatch2 object?
;;;
;;; jb
;;;
;;; "Tony Tanzillo" <tony.tanzillo/caddzone> wrote in message
;;; news:4113edd2_1@newsprd01...
;;; Ditch the DCL.
;;;
;;; Try this:

 (defun BrowseForFolder ( / sh folder parentfolder folderobject
result)
    (vl-load-com)
    (setq sh
       (vla-getInterfaceObject
          (vlax-get-acad-object)
          "Shell.Application"
       )
    )

    (setq folder
       (vlax-invoke-method
           sh
           'BrowseForFolder
           0
           ""
           0
        )
    )
    (vlax-release-object sh)

    (if folder
       (progn
          (setq parentfolder
             (vlax-get-property folder 'ParentFolder)
          )
          (setq FolderObject
             (vlax-invoke-method
                ParentFolder
                'ParseName
                (vlax-get-property Folder 'Title)
             )
          )
          (setq result
             (vlax-get-property FolderObject 'Path)
          )
          (mapcar 'vlax-release-object
             (list folder parentfolder folderobject)
          )
          result
       )
    )
 )

;;; --
;;; http://www.caddzone.com
;;;
;;; AcadXTabs: MDI Document Tabs for AutoCAD 2004/2005
;;; http://www.acadxtabs.com
;;;
;;; "T.Willey" <nos...@address.withheld> wrote in message
;;; news:738912.1091555492672.JavaMail.jive@jiveforum1.autodesk.com...
;;; I finally got this to work (hopefully it works for other also).  I
;;; am
;;; posting the code and attaching the
;;; ".dcl" file for anyone to use.  The main routine is "directory-dia".
;;; Have fun.
;;; Tim
;;;
;;; ps. erase the ".zip" to use the dcl file.

 (defun list-drives ( / c i)
 ;By Tony Tanzillo

 (setq i 66)
 (repeat 24
   (setq c (chr (setq i (1+ i))))
   (if (findfile (strcat c ":\\."))
     (setq rslt (cons (strcat c ":") rslt))
   )
 )
 (setq rslt (reverse rslt))
 )

 ;==========================================

 (defun directory-dia(/ dplc dsub listvl rslt ddia1)

 (setq ddia1 (load_dialog "DirSelect.dcl"))
 (if (not (new_dialog "Direct" ddia1))
   (exit)
 );-if
 (list-drives)
 (mode_tile "d-save" 1)
 (mode_tile "lbox1" 2)
 (start_list "lbox1" 3); clear the list
 (mapcar 'add_list rslt)
 (end_list)
 (action_tile "lbox1" "(if (= $reason 4) (UPDATE-DIA) )")
 (action_tile "accept" "(done_dialog 1)")
 (action_tile "cancel"
   "(progn
     (setq dpathdir nil)
     (done_dialog 1)
   )"
 )
 (start_dialog)

 )

 ;============================================

 (defun update-dia (/ flag1)

 (setq dplc (atoi $value))
 (if (not dsub)
   (setq listvl (strcat (nth dplc rslt) "\\"))
   (setq listvl (strcat (nth dplc dsub) "\\"))
 )
 (if (= listvl "..\\")
   (step1back)
   (if dpathdir
     (setq dpathdir (strcat dpathdir listvl))
     (setq dpathdir listvl)
   )
 )
 (if (/= flag1 "no")
   (progn
     (setq dsub (vl-directory-files dpathdir nil -1))
     (if (= dsub nil)
       (setq dsub (list ".."))
       (if (not (member ".." dsub))
         (setq dsub (reverse (append (reverse dsub) (list ".."))))
       )
     )
     (setq dsub (vl-remove "." dsub))
     (start_list "lbox1" 3)
     (mapcar 'add_list dsub)
     (end_list)
     (set_tile "choice1" dpathdir)
   )
   (progn
     (start_list "lbox1" 3)
     (mapcar 'add_list rslt)
     (end_list)
     (setq dpathdir nil)
     (setq dsub nil)
     (set_tile "choice1" "")
   )
 )
 )

 ;===============================================

 (defun step1back(/ cnt1)

 (setq cnt1 (strlen dpathdir))
 (setq cnt1 (1- cnt1))
 (while (and (/= (substr dpathdir cnt1 1) "\\") (> cnt1 1))
   (setq dpathdir (substr dpathdir 1 (1- cnt1)))
   (setq cnt1 (1- cnt1))
 )
 (if (<= cnt1 1)
   (setq flag1 "no")
 )

 ) 