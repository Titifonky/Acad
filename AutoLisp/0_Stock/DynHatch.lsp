;|
DynamicHatch V1.6.90210 for KozMos DynamicToolPack
Copyright(C) 1994-2009 by KozMos Inc.
Permission to use, copy, modify, and distribute this software for any purpose and without fee is hereby granted, provided that the above copyright notice appears in all copies and that both that copyright notice and the limited warranty and restricted rights notice below appear in all supporting documentation.
KOZMOS PROVIDES THIS PROGRAM "AS IS" AND WITH ALL FAULTS. KOZMOS SPECIFICALLY DISCLAIMS ANY IMPLIED WARRANTY OF MERCHANTABILITY OR FITNESS FOR A PARTICULAR USE. KOZMOS INC. DOES NOT WARRANT THAT THE OPERATION OF THE PROGRAM WILL BE UNINTERRUPTED OR ERROR FREE.

System Configuration data used:
===============================
AppData/DynamicToolPack/AutoReadPAT:	If load the PAT file each time calling DynamicHatch.
					Check Value: "1".
AppData/DynamicToolPack/PatFile:	The saved PAT file with full path.
AppData/DynamicToolPack/DefaultPattern:	The default hatch pattern to use while new hatch is created by DynamicHatch.

2009/01/05	V0.0	Programming started, ideas and source structure inherited from Andrea's DHatch 4.1;
2009/01/11	V1.0	All functions finished, improvement from DHatch is also done;
2009/01/16	V1.1	Support modifying the Gradient Fill;
2009/01/20	V1.2	Enhanced pattern switch via dialog, ideas by IRNEB;
			If error happened, delete new created hatch or restore editing hatch to former value;
			Support hatch object type switch between GradientHatch and CommonHatch;
			Develop DynamicToolPack-Options function for all dynamic operations and codes optimization;
2009/01/31	V1.5	Add Grdraw-Angle to show the rotation spread angle virtually in run time;
			Fully Description for all functions and optimize the codes;
2009/02/10	V1.6	Bug fix on error
			DCL management
			Add Shade/Tint change dialog control for Gradient Fill;
			Set default hatch pattern for new created hatches by DynamicHatch
|;
(Defun C:DynHatch (/		      ::DynamicToolPack-End
		   ::DynamicToolPack-Options
		   ::DynamicToolPack-Start
		   ::DynamicToolPack-StrCase
		   ::DynamicPoint::   ::DynamicSysVar::
		   Change-Color	      Color-HSL2RGB
		   Color-RGB2HSL      Color-RGB2TCI
		   Color-TCI2RGB      DynKey-A-AboutDynamicHatch
		   DynKey-C-HatchColor
		   DynKey-D-ScaleDynamic
		   DynKey-F9-SnapMode DynKey-L-HatchLayer
		   DynKey-O-HatchOrigin
		   DynKey-R-HatchRotation
		   DynKey-S-PatternSwitch
		   DynKey-V-ScaleValue
		   DynKey-W-HiddenInformation
		   DynKey-Z-Gradient2Hatch
		   Extract-DCL	      Gradient-GetColor
		   Gradient-PutColor  Gradient-Scale
		   Grdraw-Angle	      Modify-Hatch
		   Read-Message	      Read-PatFile
		   Read-Pattern	      Switch-F8
		   Switch-F9	      *HPNAME*
		   ACTANGLE	      ACTCOLOR
		   ACTDCL	      ACTHATCH
		   ACTLAYER	      ACTMTEXT
		   ACTNAME	      ACTORIGIN
		   ACTPOINT	      ACTSAVE
		   ACTSCALE	      BDR
		   DCLMSG1	      DCLMSG2
		   DCLMSG3　	      DCLMSG4
		   DCLMSG5	      DCLMSG6
		   DCLMSG7	      DCLMSG8
		   DYNPOINT	      GRADIENT
		   HATCH	      LL
		   MODE		      MSG0
		   MSG0-T	      MSG1
		   MSG1-G	      MSG1-ON
		   MSG1-OFF	      MSG2
		   MSG3		      MSG4
		   MSG5		      MSG6
		   MSG7		      MSG8
		   MSG9		      MSG10
		   NEW		      OBJ
		   OLD		      OLDHNAME
		   OLDGNAME	      PT
		   SETROTATE	      SYSOLD
		   UR		      VAL
		  )
;;;;;;;;;;;;;;;;;;;;;;; KozMos DynamicToolPack - DynamicHatch ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Name:		::DynamicToolPack-Options(1)						;;;
;;; Description:	Proceed with dynamic catching and call the certain functions when proper;;;
;;;			keys were pressed.							;;;
;;;			All pressed keys was send to capital letters.				;;;
;;;			Dynamic moving point was leading by "Dynamic" in argument list.		;;;
;;; Argu(1):		The CallBack Keys and functions list		LIST			;;;
;;; ------------------------------------------------------------------------------------------- ;;;
;;;			'(("Dynamic" . "(DynKey-ActivePoint)")("A" . "(ToDoByA)")...)		;;;
;;;			If simple functions is called, can directly define them in argument	;;;
;;;			 with "progn".								;;;
;;;			'(...("N" . "(progn(setvar\042Osmode\042 1)(redraw))")...)		;;;
;;; ------------------------------------------------------------------------------------------- ;;;
;;;			Function keys: "F3" ~ "F12"		Enter: "Enter"			;;;
;;;			Mouse Left Click: "LeftClick"		Mouse Right click: "RightClick"	;;;
;;; ------------------------------------------------------------------------------------------- ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 2009/01/20 (Koz Jono Yeoh) ;;;;;;;;
  (Defun ::DynamicToolPack-Options (data / KEY MODE RUN VAL)
    (setq Run T)
    (while (and	Run
		(setq mode (grread t 4 4)
		      val  (cadr mode)
		      mode (car mode)
		)
	   )
      (cond ((= mode 5)
	     (setq ::DynamicPoint::
		    val
		   key "Dynamic"
	     )
	    )
	    ((= mode 2)
	     (setq key (strcase (chr val))
		   val (cdr (assoc key
				   '(("\006" . "F3")
				     ("\024" . "F4")
				     ("\005" . "F5")
				     ("\004" . "F6")
				     ("\007" . "F7")
				     ("\017" . "F8")
				     ("\002" . "F9")
				     ("\025" . "F10")
				     ("\027" . "F11")
				     ("\037" . "F12")
				     ("\r" . "Enter")
				    )
			    )
		       )
	     )
	     (if val
	       (setq key val)
	     )
	    )
	    ((= mode 25)
	     (setq key "RightClick")
	    )
	    ((= mode 3)
	     (setq key "LeftClick")
	    )
	    (t (setq Run nil))
      )
      (if (and (setq run (cdr (assoc key data)))
	       (= (type run) 'str)
	       (= (substr run 1 1) "(")
	       (= (substr run (strlen run)) ")")
	  )
	(eval (read run))
      )
    )
    (princ)
  )
;;;;;;;;;;;;;;;;;;;;;;; KozMos DynamicToolPack - DynamicHatch ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Name:		::DynamicToolPack-StrCase(1)						;;;
;;; Description:	Get capital letter for given string. Used to proceed with double-byte 	;;;
;;;			characters such as Chinese.						;;;
;;; Argu(1):		The source string				STR			;;;
;;; ------------------------------------------------------------------------------------------- ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 2009/01/20 (Koz Jono Yeoh) ;;;;;;;;
  (Defun ::DynamicToolPack-StrCase (str / CHAR IDX RTN)
    (setq idx 0
	  rtn ""
    )
    (while (<= (setq idx (1+ idx)) (strlen str))
      (setq char (substr str idx 1))
      (if (= (strcase char) "\000")
	(setq rtn (strcat rtn (substr str idx 2))
	      idx (1+ idx)
	)
	(setq rtn (strcat rtn (strcase char)))
      )
    )
    rtn
  )
;;;;;;;;;;;;;;;;;;;;;;; KozMos DynamicToolPack - DynamicHatch ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Name:		::DynamicToolPack-Start(0)						;;;
;;; Description:	Standard system initialization code for DynamicToolPack.		;;;
;;; ------------------------------------------------------------------------------------------- ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 2009/01/20 (Koz Jono Yeoh) ;;;;;;;;
  (Defun ::DynamicToolPack-Start ()
    (while (= (logand (getvar "undoctl") 8) 8)
      (command "_.undo" "_end")
    )
    (if	(null vl-load-com)
      (command "_.Undo" "_Group")
      (progn
	(vl-load-com)
	(vla-startundomark
	  (vla-Get-ActiveDocument (vlax-Get-Acad-Object))
	)
      )
    )
    (setq ::DynamicSysVar::
	   (list (getvar "OsMode")
		 (getvar "CmdEcho")
		 (getvar "AngDir")
		 (getvar "Expert")
		 (getvar "Dimzin")
		 *error*
	   )
    )
    (setvar "cmdecho" 0)
    (setvar "expert" 5)
    (setvar "Dimzin" 3)
    (Defun *error* (msg)
      (cond ((= (type ActSave) 'ename) (entdel ActSave))
	    ((= (type ActSave) 'list) (entmod ActSave))
      )
      (::DynamicToolPack-End)
    )
    (princ)
  )
;;;;;;;;;;;;;;;;;;;;;;; KozMos DynamicToolPack - DynamicHatch ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Name:		::DynamicToolPack-Start(0)						;;;
;;; Description:	Standard system restor code for DynamicToolPack.			;;;
;;; ------------------------------------------------------------------------------------------- ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 2009/01/22 (Koz Jono Yeoh) ;;;;;;;;
  (Defun ::DynamicToolPack-End ()
    (if	(null vl-load-com)
      (command "_.undo" "_end")
      (vla-endundomark
	(vla-Get-ActiveDocument (vlax-Get-Acad-Object))
      )
    )
    (while (= (logand (GETVAR "undoCTL") 8) 8)
      (command "_.undo" "_end")
    )
    (if	(= (type (car ActSS)) 'ename)
      (mapcar 'entdel ActSS)
    )
    (if	(and (= (type ActMText) 'list)
	     (entget (cdr (assoc -1 ActMText)))
	)
      (entdel (cdr (assoc -1 ActMText)))
    )
    (if	(= (type ActDCL) 'int)
      (unload_dialog ActDCL)
    )
    (if
      (setq
	ActDCL (findfile
		 (strcat (vl-filename-directory (findfile "ACAD.EXE"))
			 "\\DynamicToolPack.DCL"
		 )
	       )
      )
       (vl-file-delete ActDCL)
    )
    (if	::DynamicSysVar::
      (progn
	(setvar "OsMode" (nth 0 ::DynamicSysVar::))
	(setvar "CmdEcho" (nth 1 ::DynamicSysVar::))
	(setvar "AngDir" (nth 2 ::DynamicSysVar::))
	(setvar "Expert" (nth 3 ::DynamicSysVar::))
	(setvar "Dimzin" (nth 4 ::DynamicSysVar::))
	(setq *error* (nth 5 ::DynamicSysVar::))
      )
    )
    (setq ActSS	nil
	  ActMText nil
	  ::DynamicSysVar::
	   nil
    )
    (redraw)
    (princ)
  )
;;;;;;;;;;;;;;;;;;;;;;; KozMos DynamicToolPack - DynamicHatch ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Name:		Read-Message(0)								;;;
;;; Description:	Get the prompt messages definition based on the proper language		;;;
;;; ------------------------------------------------------------------------------------------- ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 2009/01/20 (Koz Jono Yeoh) ;;;;;;;;
  (Defun Read-Message ()
    (cond ((= (getvar "syscodepage") "ANSI_936")
	   ;; This is for Simplied Chinese platform.
	   (setq msg0	  (strcat
			    "\042KozMos 动态工具集\n"
			    "\n*****************\n"
			    "动态填充 版本1.6.90210#TRINNOLOV3.2009\n"
			    "原始程序设计思路：  Andrea Andreetti\n"
			    "程序重设计及扩充：  Koz Jono Yeoh\n\n"
			    "特别说明:\n" "========\n"
			    "按 [F8] 键可切换正交\n"
			    "按 [F9] 键可切换点捕捉(普通填充)\n"
			    "按 [F9] 键可切换修改颜色(渐变填充)\n"
			    "按 [R ] 可切换动态/固定旋转\n\n"
			    "切换图案列表文件: DynHatch.TXT\n"
			    "图案列表文件格式: 每行一个填充名称\n\n"
			    "缺省切换图案列表: ANSI3x\042")
		 msg0-t	  "\042关于动态填充 版本1.5\042"
		 msg1	  " 填充角度= %Angle%%%d\\P 填充比例= %Scale%\\P 填充图案= %Pattern%\\P 点 捕 捉= %Snap%"
		 msg1-g	  " 渐变角度= %Angle%%%d\\P 阴影比例= %Scale%\\P 渐变类型= %Pattern%\\P 原点居中= %Center%"
		 msg1-on  "开"
		 msg1-off "关"
		 msg2	  "\n 请点取单一边界实体/内部点来创建填充或点取已有填充 <退出>:"
		 msg3	  "\n 请选择动态填充的操作选项:\n"
		 msg4	  "\r 动态填充 > [填充种类Z/输入比例V/动态比例D/填充图案S/基点O/旋转R/颜色C/图层L/关于A] <退出>:"
		 msg5	  "\r 动态填充>> 动态修改填充比例[%Data%]，按[V]输入角度值，其他任意键返回...                                        "
		 msg6	  "\n 动态填充>> 请输入新的填充比例因子 <%Data%>:"
		 msg7	  "\n 动态填充>> 请输入新的旋转角度 <%Data%>:"
		 msg8	  "\r 动态填充>> 请点取新的填充基准点 <不修改>:                                                 "
		 msg9	  "\r 动态填充>> 请点取目标图层的任意实体 <返回>:                                               "
		 msg9a	  "\r 动态填充>> 填充图层变更到 [%Data%]         \n"
		 msg10	  "\042动态填充错误:\n=============\n所选操作对渐变填充无效!\042"
		 DCLMSG1  "请选择AutoCAD填充文件"
		 DCLMSG2  "选择填充类型"
		 DCLMSG3  "读取填充文件..."
		 DCLMSG4  "自动读取填充文件"
		 DCLMSG5  "原填充类型:"
		 DCLMSG6  "渐深/渐浅"
		 DCLMSG7  "渐深  <------->  渐浅"
		 DCLMSG8  "设当前图案为缺省图案"
	   )
	  )
	  (t
	   ;; This is default English version, other languages can be added from it
	   (setq msg0	  (strcat
			    "\042Dynamic ToolPack Family\n"
			    "\n***********************\n"
			    "DynamicHatch V1.6.90210#TRINNOLOV3.2009\n"
			    "Original design idea:\tAndrea Andreetti\n"
			    "Reprogram and improve:\tKoz Jono Yeoh\n\n"
			    "Additional notes:\n" "=================\n"
			    "Press [F8] to switch orthomode\n"
			    "Press [F9] to switch point snap (Common Hatch)\n"
			    "Press [F9] to switch 2 colors (Gradient Fill)\n"
			    "Press [R ] to switch dynamic/locked rotation\n\n"
			    "Hatch patterns list file: DynHatch.TXT\n"
			    "File format: Hatch names line by line\n\n"
			    "Default pattern switching: ANSI3x\042")
		 msg0-t	  "\042About DynamicHatch V1.5\042"
		 msg1	  " Angle= %Angle%%%d\\P Scale= %Scale%\\P Pattern= %Pattern%\\P Snap= %Snap%"
		 msg1-g	  " GradientAngle= %Angle%%%d\\P GradientName= %Pattern%\\P ShadowScale= %Scale%\\P Centered= %Center%"
		 msg1-on  "ON"
		 msg1-off "OFF"
		 msg2	  "\n Please pick boundary/point to create hatch and existing hatch <Exit>:"
		 msg3	  "\n Options for DynamicHatch V1.0b:\n"
		 msg4	  "\r DynHatch > [Style(Z)/scale(V)alue/scale(D)ynamic/Pattern(S)/(O)rigin/(R)otate/(C)olor/(L)ayer/(A)bout] <Quit>:"
		 msg5	  "\r DynHatch>> Dynamic hatch scaling [%Data%], press V to enter scale, other keys to return...                    "
		 msg6	  "\n DynHatch>> Enter new hatch scale factor <%Data%>:"
		 msg7	  "\n DynHatch>> Enter rotation angle in degree <%Data%>:"
		 msg8	  "\r DynHatch>> Please pick new hatch origin point <No Change>:                                                    "
		 msg9	  "\r DynHatch>> Pick an object on the destination layer <Return>:                                                  "
		 msg9a	  "\r DynHatch>> Hatch layer changed to [%Data%]                  \n"
		 msg10	  "\042DynamicHatch Error:\n===================\nInvalid selected option for Gradient Fill!\042"
		 DCLMSG1  "Select an AutoCAD Hatch Pattern File"
		 DCLMSG2  "Select Pattern"
		 DCLMSG3  "Read PAT file..."
		 DCLMSG4  "Read PAT in startup"
		 DCLMSG5  "OldPattern:"
		 DCLMSG6  "Shade/Tint"
		 DCLMSG7  "Shade  <------->  Tint"
		 DCLMSG8  "Set pattern default"
	   )
	  )
    )
  )
;;;;;;;;;;;;;;;;;;;;;;; KozMos DynamicToolPack - DynamicHatch ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Name:		Extract-DCL(0)								;;;
;;; Description:	Extract and load the Dialog File.					;;;
;;; ------------------------------------------------------------------------------------------- ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 2009/02/05 (Koz Jono Yeoh) ;;;;;;;;
  (Defun Extract-DCL (/ DCL DCL_ID FIL NEW)
    (if	(and (setq dcl (strcat (vl-filename-directory (findfile "ACAD.EXE"))
			       "\\DynamicToolPack.DCL"
		       )
		   fil (open dcl "w")
	     )
	)
      (progn
	(foreach new
		 (list
		   "dcl_settings : default_dcl_settings { audit_level=0; }"
		   "DynHatch : dialog {"
		   (strcat "   label=\042" DCLMSG2 "\042 ;")
		   "   : button {"
		   "      key=\042BtnRead\042 ;"
		   (strcat "      label=\042" DCLMSG3 "\042 ;")
		   "   }"
		   "   : toggle {"
		   "      key=\042TglRead\042 ;"
		   (strcat "      label=\042" DCLMSG4 "\042 ;")
		   "   }"
		   "   : list_box {"
		   "      key=\042LstRead\042 ;"
		   "   }"
		   "   : edit_box {"
		   "      key=\042EdtRead\042 ;"
		   (strcat "      label=\042" DCLMSG5 "\042 ;")
		   "      is_enabled=false ;"
		   "   }"
		   "   : toggle {"
		   "      key=\042TglDefault\042 ;"
		   (strcat "      label=\042" DCLMSG8 "\042 ;")
		   "   }"
		   "   : button {"
		   "      key=\042accept\042 ;"
		   "      label=\042OK\042 ;"
		   "      is_default=true ;"
		   "   }"
		   "   : button {"
		   "      key=\042cancel\042 ;"
		   "      label=\042Cancel\042 ;"
		   "      is_cancel=true ;"
		   "   }"
		   "}"
		   "Shade : dialog {"
		   (strcat "      label=\042" DCLMSG6 "\042 ;")
		   "   : boxed_column {"
		   "      : edit_box {"
		   "         alignment=centered ;"
		   "         key=\042EdtValue\042 ;"
		   "         fixed_width=true ;"
		   "         width=8 ;"
		   "      }"
		   "      : slider {"
		   "         big_increment=5 ;"
		   "         max_value=100 ;"
		   "         min_value=0 ;"
		   "         small_increment=1 ;"
		   "         key=\042SldValue\042 ;"
		   "         width=22 ;"
		   "      }"
		   "      : text {"
		   "         alignment=centered ;"
		   "         key=\042Text\042 ;"
		   (strcat "      label=\042" DCLMSG7 "\042 ;")
		   "      }"
		   "   }"
		   "   ok_cancel;"
		   "}"
		 )
	  (write-line new fil)
	)
	(setq fil    (close fil)
	      dcl_id (load_dialog dcl)
	)
      )
    )
    dcl_id
  )
;;;;;;;;;;;;;;;;;;;;;;; KozMos DynamicToolPack - DynamicHatch ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Name:		Read-Pattern(0)								;;;
;;; Description:	If AutoReadPAT is enabled and the default PAT file is found, read the	;;;
;;;			PAT file to gather all hatch patterns, orthwise, use ANSI31~ANSI37 and	;;;
;;;			what saved in DynHatch.TXT.						;;;
;;; ------------------------------------------------------------------------------------------- ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 2009/01/20 (Koz Jono Yeoh) ;;;;;;;;
  (Defun Read-Pattern (/ LINE OB PAT RTN)
    (if
      (and (equal (getcfg "AppData/DynamicToolPack/AutoReadPAT")
		  "1"
	   )
	   (setq pat (getcfg "AppData/DynamicToolPack/PatFile"))
	   (= (type pat) 'str)
	   (setq pat (findfile pat))
      )
       (setq rtn (Read-PatFile pat))
       (progn
	 (setq rtn '("ANSI31"	  "ANSI32"     "ANSI33"
		     "ANSI34"	  "ANSI35"     "ANSI36"
		     "ANSI37"
		    )
	 )
	 (if (and (setq pat (findfile "DynHatch.txt"))
		  (setq pat (open pat "r"))
	     )
	   (progn
	     (while (setq line (read-line pat))
	       (setq rtn (cons (::DynamicToolPack-StrCase line) rtn))
	     )
	     (close pat)
	   )
	 )
	 (setq rtn (acad_strlsort rtn))
       )
    )
    rtn
  )
;;;;;;;;;;;;;;;;;;;;;;; KozMos DynamicToolPack - DynamicHatch ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Name:		Read-PatFile(1)								;;;
;;; Description:	Read the PAT file to gather all hatch patterns defined in. If the PAT	;;;
;;;			is NIL, prompt users to choose one and then refresh the pattern list in	;;;
;;;			internal pattern switch dialog.						;;;
;;; Argu(1):		The PAT file name with full path		STR			;;;
;;; ------------------------------------------------------------------------------------------- ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 2009/01/20 (Koz Jono Yeoh) ;;;;;;;;
  (Defun Read-PatFile (val / LINE INDCL PATFILE POS)
    (if	(null val)
      (setq indcl T
	    val
	     (getfiled
	       DCLMSG1
	       (strcat (vl-filename-directory (findfile "ACAD.PAT")) "/")
	       "pat"
	       8
	     )
      )
    )
    (if	(and (= (type val) 'str)
	     (setq PatFile (findfile val))
	     (setq val (setcfg "AppData/DynamicToolPack/PATFile" PatFile)
		   val (open PatFile "r")
	     )
	)
      (progn
	(while (setq line (read-line val))
	  (if (= (substr line 1 1) "*")
	    (progn
	      (if (null (setq pos (vl-string-search "," line)))
		(setq line (::DynamicToolPack-StrCase (substr line 2)))
		(setq line (::DynamicToolPack-StrCase
			     (substr line 2 (1- pos))
			   )
		)
	      )
	      (if (null (member line *HPName*))
		(setq *HPName* (cons line *HPName*))
	      )
	    )
	  )
	)
	(setq val      (close val)
	      *HPName* (reverse *HPName*)
	)
      )
    )
    (if	indcl
      (progn
	(start_list "LstRead")
	(mapcar 'add_list *HPName*)
	(end_list)
	(set_tile "LstRead" (itoa (vl-position ActName *HPName*)))
      )
    )
    *HPName*
  )
;;;;;;;;;;;;;;;;;;;;;;; KozMos DynamicToolPack - DynamicHatch ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Name:		Dynamic-Scale(1)							;;;
;;; Description:	Calculate the ratio of distance between give point (and saved ActPoint)	;;;
;;;			as the instant scale factor within the range of 0.1x~10x. This function	;;;
;;;			is used for common hatch only.						;;;
;;; Argu(1):		Run-time mouse position point			LIST			;;;
;;; ------------------------------------------------------------------------------------------- ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 2009/01/20 (Koz Jono Yeoh) ;;;;;;;;
  (Defun Dynamic-Scale (ptx / RTN)
    (setq
      rtn (/ (distance ActOrigin ptx) (distance ActOrigin ActPoint))
    )
    (cond ((< rtn 0.1) (setq rtn 0.1))
	  ((> rtn 10.0) (setq rtn 10.0))
    )
    rtn
  )
;;;;;;;;;;;;;;;;;;;;;;; KozMos DynamicToolPack - DynamicHatch ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Name:		Switch-F9(1)								;;;
;;; Description:	If SnapMode is 1, recalculate the position of given point using the snap;;;
;;;			settings of INT and END.						;;;
;;; Argu(1):		Run-time mouse position point			LIST			;;;
;;; ------------------------------------------------------------------------------------------- ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 2009/01/20 (Koz Jono Yeoh) ;;;;;;;;
  (Defun Switch-F9 (ptx / RTN)
    (setq rtn ptx)
    (if	(= (getvar "snapmode") 1)
      (if (null (setq rtn (osnap ptx "_int,_end")))
	(setq rtn ptx)
      )
    )
    rtn
  )
;;;;;;;;;;;;;;;;;;;;;;; KozMos DynamicToolPack - DynamicHatch ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Name:		Switch-F8(0)								;;;
;;; Description:	If OrthoMode is 1, reset the position of ActPoint to the nearest Ortho	;;;
;;;			point.									;;;
;;; ------------------------------------------------------------------------------------------- ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 2009/01/20 (Koz Jono Yeoh) ;;;;;;;;
  (Defun Switch-F8 (/ DIST NEW PT-E PT-N PT-S PT-W RTN SNAP)
    (setq snap (if SetRotate
		 SetRotate
		 (getvar "SNAPANG")
	       )
	  dist (distance ActOrigin ActPoint)
	  pt-n (polar ActOrigin (+ snap (* 0.5 pi)) dist)
	  pt-w (polar ActOrigin (+ snap pi) dist)
	  pt-e (polar ActOrigin snap dist)
	  pt-s (polar ActOrigin (- snap (* 0.5 pi)) dist)
	  dist 1e99
    )
    (foreach snap (list pt-n pt-w pt-e pt-s)
      (if (< (setq new (distance snap ActPoint)) dist)
	(setq dist new
	      rtn  snap
	)
      )
    )
    rtn
  )
;;;;;;;;;;;;;;;;;;;;;;; KozMos DynamicToolPack - DynamicHatch ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Name:		Grdraw-Angle(0)								;;;
;;; Description:	Grdraw the rotation spread angle with dashed magenta color.		;;;
;;;			This is also idea from Andrea's new release DHatch V4.5			;;;
;;; ------------------------------------------------------------------------------------------- ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 2009/01/22 (Koz Jono Yeoh) ;;;;;;;;
  (Defun Grdraw-Angle (/ ANG ANX DST PTX)
    (setq dst (grdraw ActOrigin
		      (polar ActOrigin 0 (distance ActOrigin ActPoint))
		      4
		      0
	      )
	  dst (* 0.5 (distance ActOrigin ActPoint))
	  anx (* (angle ActOrigin ActPoint) 0.02)
	  ang (- 0.0 anx)
    )
    (repeat 51
      (setq ptx (cons (polar ActOrigin (setq ang (+ ang anx)) dst) ptx))
    )
    (while (cadr ptx)
      (grdraw (car ptx) (cadr ptx) 6 0)
      (setq ptx (cdr ptx))
    )
  )
;;;;;;;;;;;;;;;;;;;;;;; KozMos DynamicToolPack - DynamicHatch ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Name:		Modify-Hatch(0)								;;;
;;; Description:	Create the hatch object in run time.					;;;
;;; ------------------------------------------------------------------------------------------- ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 2009/01/20 (Koz Jono Yeoh) ;;;;;;;;
  (Defun Modify-Hatch (/ HATCH ITEM)
    (if	(and (= (getvar "orthomode") 1)
	     ActOrigin
	     ActPoint
	)
      (setq ActPoint (Switch-F8))
    )
    (setq ActPoint (Switch-F9 ActPoint)
	  hatch	   (entget (vlax-vla-object->ename ActHatch))
    )
    (if	Gradient
      ;; For Gradient Fill
      (foreach item (list (cons 8 ActLayer)
			  (cons 470 ActName)
		    )
	(setq hatch (subst item (assoc (car item) hatch) hatch))
      )
      (progn
	;; For common hatch
	(foreach item
		      (list (cons 2 ActName)
			    (cons 8 ActLayer)
			    (cons 41 ActScale)
			    (cons 43 (car ActOrigin))
			    (cons 44 (cadr ActOrigin))
		      )
	  (setq hatch (subst item (assoc (car item) hatch) hatch))
	)
	(if (< ActColor 0)
	  (if (cdr (assoc 420 hatch))
	    (setq hatch	(subst (cons 420 (abs ActColor))
			       (assoc 420 hatch)
			       hatch
			)
	    )
	    (setq hatch (append hatch (list (cons 420 (abs ActColor)))))
	  )
	  (progn
	    (if	(cdr (assoc 420 hatch))
	      (setq hatch (vl-remove (assoc 420 hatch) hatch))
	    )
	    (if	(cdr (assoc 62 hatch))
	      (setq hatch
		     (subst (cons 62 ActColor) (assoc 62 hatch) hatch)
	      )
	      (setq hatch (append hatch (list (cons 62 ActColor))))
	    )
	  )
	)
      )
    )
    (if	(entmod hatch)
      (progn
	(redraw)
	(grdraw ActOrigin ActPoint 4 1)
	(setq item     (vlax-ename->vla-object (cdr (assoc -1 hatch)))
	      ActAngle (angle ActOrigin ActPoint)
	)
	(if (null Gradient)
	  (vla-put-PatternAngle item ActAngle)
	  (progn
	    (Gradient-PutColor)
	    (vla-put-GradientAngle item ActAngle)
	  )
	)
      )
    )
    ;; start modify the info MTEXT
    (if	(and (null ActMText)
	     (entmake (list (cons 0 "MTEXT")
			    (cons 8 (getvar "CLayer"))
			    (cons 100 "AcDbEntity")
			    (cons 100 "AcDbMText")
			    (list 10 0.0 0.0 0.0)
			    (cons 40 1.0)
			    (cons 41 0.0)
			    (cons 71 4)
			    (cons 72 5)
			    (cons 1 msg1)
			    (cons 7 (getvar "TextStyle"))
			    (list 11 1.0 0.0 0.0)
			    (cons 50 0.0)
			    (cons 62 6)
		      )
	     )
	)
      (setq ActMText (entget (entlast)))
      (setq ActMtext (entget (cdr (assoc -1 ActMText))))
    )
    (setq item (rtos (* 180 (/ ActAngle pi)) 2 3)
	  item (vl-string-subst
		 item
		 "%Angle%"
		 (if Gradient
		   msg1-g
		   msg1
		 )
	       )
	  item (vl-string-subst (rtos ActScale 2 3) "%Scale%" item)
	  item (vl-string-subst ActName "%Pattern%" item)
    )
    (if	(null Gradient)
      (setq item (vl-string-subst
		   (if (= (getvar "SnapMode") 1)
		     msg1-on
		     msg1-off
		   )
		   "%Snap%"
		   item
		 )
      )
      (setq item (vl-string-subst
		   (if (equal (vla-get-GradientCentered ActHatch)
			      :vlax-true
		       )
		     msg1-on
		     msg1-off
		   )
		   "%Center%"
		   item
		 )
      )
    )
    (setq ActMText (subst (cons 10 ActPoint) (assoc 10 ActMText) ActMText)
	  ActMText (subst (cons 40 (* 0.02 (getvar "Viewsize")))
			  (assoc 40 ActMText)
			  ActMText
		   )
	  ActMText (subst (cons 1 item) (assoc 1 ActMText) ActMText)
    )
    (if	DynPoint
      (setq ActMText
	     (subst (cons 10 DynPoint) (assoc 10 ActMText) ActMText)
      )
    )
    (if	(< ActColor 0)
      (if (cdr (assoc 420 ActMText))
	(setq ActMText (subst (cons 420 (abs ActColor))
			      (assoc 420 ActMText)
			      ActMText
		       )
	)
	(setq
	  ActMText (append ActMText (list (cons 420 (abs ActColor))))
	)
      )
      (progn
	(if (cdr (assoc 420 ActMText))
	  (setq ActMtext (vl-remove (assoc 420 ActMText) ActMText))
	)
	(if (cdr (assoc 62 ActMText))
	  (setq	ActMText
		 (subst (cons 62 ActColor) (assoc 62 ActMText) ActMText)
	  )
	  (setq ActMText (append ActMText (list (cons 62 ActColor))))
	)
      )
    )
    (entmod ActMText)
  )
;;;;;;;;;;;;;;;;;;;;;;; KozMos DynamicToolPack - DynamicHatch ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Name:		Gradient-GetColor(0)							;;;
;;; Description:	Get color from Gradient Fill. SnapMode is used to switch from 2 colors.	;;;
;;; ------------------------------------------------------------------------------------------- ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 2009/01/20 (Koz Jono Yeoh) ;;;;;;;;
  (Defun Gradient-GetColor (/ MOD RTN VAL)
    (if	(= (getvar "SnapMode") 0)
      (setq val (vla-get-GradientColor1 ActHatch))
      (setq val (vla-get-GradientColor2 ActHatch))
    )
    (setq mod (vla-get-ColorMethod val))
    (cond ((= mod acColorMethodByACI)
	   (setq rtn (vla-get-ColorIndex val))
	  )
	  ((= mod acColorMethodByBlock)
	   (setq rtn 0)
	  )
	  ((= mod acColorMethodByLayer)
	   (setq rtn 256)
	  )
	  ((= mod acColorMethodByRGB)
	   (setq rtn (*	-1
			(+ (lsh (fix (vla-get-Red val)) 16)
			   (lsh (fix (vla-get-Green val)) 8)
			   (fix (vla-get-Blue val))
			)
		     )
	   )
	  )
    )
    rtn
  )
;;;;;;;;;;;;;;;;;;;;;;; KozMos DynamicToolPack - DynamicHatch ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Name:		Gradient-PutColor(0)							;;;
;;; Description:	Set color to Gradient Fill. SnapMode is used to switch from 2 colors.	;;;
;;; ------------------------------------------------------------------------------------------- ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 2009/01/20 (Koz Jono Yeoh) ;;;;;;;;
  (Defun Gradient-PutColor ()
    (if	(= (getvar "SnapMode") 0)
      (vla-put-GradientColor1
	ActHatch
	(Change-Color (vla-get-GradientColor1 ActHatch))
      )
      (vla-put-GradientColor2
	ActHatch
	(Change-Color (vla-get-GradientColor2 ActHatch))
      )
    )
  )
;;;;;;;;;;;;;;;;;;;;;;; KozMos DynamicToolPack - DynamicHatch ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Name:		Change-Color(1)								;;;
;;; Description:	Change color of the AutoCAD TrueColor Object to ActColor.		;;;
;;; Argu(1):		AutoCAD TrueColor Object		VLA-OBJECT			;;;
;;; ------------------------------------------------------------------------------------------- ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 2009/01/20 (Koz Jono Yeoh) ;;;;;;;;
  (Defun Change-Color (cc / VAL)
    (setq val ActColor)
    (cond ((= val 0)
	   (vla-put-ColorMethod cc acColorMethodByBlock)
	  )
	  ((= val 256)
	   (vla-put-ColorMethod cc acColorMethodByLayer)
	  )
	  ((< val 0)
	   (setq val (abs val))
	   (vla-put-ColorMethod cc acColorMethodByRGB)
	   (vla-setRGB
	     cc
	     (lsh (fix val) -16)
	     (lsh (lsh (fix val) 16) -24)
	     (lsh (lsh (fix val) 24) -24)
	   )
	  )
	  (t
	   (vla-put-ColorMethod cc acColorMethodByACI)
	   (vla-put-ColorIndex cc val)
	  )
    )
    cc
  )
;;;;;;;;;;;;;;;;;;;;;;; KozMos DynamicToolPack - DynamicHatch ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Name:		Color-RGB2HSL(3)							;;;
;;; Description:	Convert color from RGB mode into HSL mode.				;;;
;;;			Codes from http://www.mjtd.com/Functions/ArticleShow.asp?ArticleID=1197	;;;
;;; Argu(1):		Red color number			INT (0 ~ 255)			;;;
;;; Argu(2):		Green color number			INT (0 ~ 255)			;;;
;;; Argu(3):		Blue color number			INT (0 ~ 255)			;;;
;;; ------------------------------------------------------------------------------------------- ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 2009/02/06 (Koz Jono Yeoh) ;;;;;;;;
  (Defun Color-RGB2HSL (R	G	B	/	DEL_B	DEL_G
			DEL_MAX	DEL_R	H	L	S	VAR_B
			VAR_G	VAR_MAX	VAR_MIN	VAR_R
		       )
    (setq var_R	  (/ R 255.0)
	  var_G	  (/ G 255.0)
	  var_B	  (/ B 255.0)
	  var_min (min var_R var_G var_B)
	  var_max (max var_R var_G var_B)
	  del_max (- var_max var_min)
	  L	  (/ (+ var_max var_min) 2)
    )
    (if	(= del_max 0)
      (setq H 0
	    S 0
      )
      (progn
	(setq S	    (if	(< L 0.5)
		      (/ del_max (+ var_max var_min))
		      (/ del_max (- 2 var_max var_min))
		    )
	      del_R (/ (+ (/ (- var_max var_R) 6) (/ del_Max 2)) del_max)
	      del_G (/ (+ (/ (- var_max var_G) 6) (/ del_Max 2)) del_max)
	      del_B (/ (+ (/ (- var_max var_B) 6) (/ del_Max 2)) del_max)
	)
	(cond
	  ((= var_R var_max)
	   (setq H (- del_B del_G))
	  )
	  ((= var_G var_max)
	   (setq H (+ (/ 1.0 3) del_R (- del_B)))
	  )
	  ((= var_B var_max)
	   (setq H (+ (/ 2.0 3) del_G (- del_R)))
	  )
	)
	(cond
	  ((< H 0) (setq H (1+ H)))
	  ((> H 1) (setq H (1- H)))
	)
      )
    )
    (setq h (* 360 H)
	  S (* 100 S)
	  l (* 100 l)
    )
    (list (fix H) (fix S) (fix L))
  )
;;;;;;;;;;;;;;;;;;;;;;; KozMos DynamicToolPack - DynamicHatch ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Name:		Color-HSL2RGB(3)							;;;
;;; Description:	Convert color from HSL mode into RGB mode.				;;;
;;;			Codes from http://www.mjtd.com/Functions/ArticleShow.asp?ArticleID=1198	;;;
;;; Argu(1):		Hue number				INT (0 ~ 360)			;;;
;;; Argu(2):		Saturation number			INT (0 ~ 360)			;;;
;;; Argu(3):		Luminance number			INT (0 ~ 100)			;;;
;;; ------------------------------------------------------------------------------------------- ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 2009/02/06 (Koz Jono Yeoh) ;;;;;;;;
  (Defun Color-HSL2RGB (Hue Saturation Light / B G H L R S VAR1 VAR2)
    (defun _HUE2RGB (v1 v2 vHue / VH)
      (cond
	((< vHue 0) (setq vH (1+ vHue)))
	((> vHue 1) (setq vH (1- vHue)))
	(t (setq vH vHue))
      )
      (cond
	((< (* 6 vH) 1) (+ v1 (* (- v2 v1) 6 vH)))
	((< (* 2 vH) 1) v2)
	((< (* 3 vH) 2) (+ v1 (* (- v2 v1) 6 (- 0.66666667 vH))))
	(t v1)
      )
    )
    (setq h (/ Hue 360.0)
	  s (/ Saturation 100.0)
	  l (/ Light 100.0)
    )
    (if	(= s 0)
      (setq r (* l 255)
	    g (* l 255)
	    b (* l 255)
      )
      (setq var2 (if (< l 0.5)
		   (* l (1+ s))
		   (+ l s (* s l -1))
		 )
	    var1 (- (* 2 l) var2)
	    r	 (* 255 (_HUE2RGB var1 var2 (+ h 0.33333333)))
	    g	 (* 255 (_HUE2RGB var1 var2 h))
	    b	 (* 255 (_HUE2RGB var1 var2 (- h 0.33333333)))
      )
    )
    (list (fix r) (fix g) (fix b))
  )
;;;;;;;;;;;;;;;;;;;;;;; KozMos DynamicToolPack - DynamicHatch ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Name:		Color-TCI2RGB(3)							;;;
;;; Description:	Convert AutoCAD TrueColorIndex number into RGB.				;;;
;;; Argu(1):		TrueColorIndex number			INT (0 ~ 16.7M)			;;;
;;; ------------------------------------------------------------------------------------------- ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 2009/02/06 (Koz Jono Yeoh) ;;;;;;;;
  (defun Color-TCI2RGB (val)
    (list (lsh (fix val) -16)
	  (lsh (lsh (fix val) 16) -24)
	  (lsh (lsh (fix val) 24) -24)
    )
  )
;;;;;;;;;;;;;;;;;;;;;;; KozMos DynamicToolPack - DynamicHatch ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Name:		Color-RGB2TCI(3)							;;;
;;; Description:	Cclculate the AutoCAD TrueColorIndex number from RGB.			;;;
;;; Argu(1):		Red color number			INT (0 ~ 255)			;;;
;;; Argu(2):		Green color number			INT (0 ~ 255)			;;;
;;; Argu(3):		Blue color number			INT (0 ~ 255)			;;;
;;; ------------------------------------------------------------------------------------------- ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 2009/02/06 (Koz Jono Yeoh) ;;;;;;;;
  (defun Color-RGB2TCI (R0 G0 B0)
    (+ B0 (* G0 256) (* R0 65536))
  )
;;;;;;;;;;;;;;;;;;;;;;; KozMos DynamicToolPack - DynamicHatch ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Name:		DynKey-A-AboutDynamicHatch(0)						;;;
;;; Description:	Callback function of "A" to show about dialog.				;;;
;;; ------------------------------------------------------------------------------------------- ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 2009/01/20 (Koz Jono Yeoh) ;;;;;;;;
  (Defun DynKey-A-AboutDynamicHatch ()
    (vla-eval
      (vlax-get-acad-object)
      (strcat "MsgBox " msg0 ",vbInformation," msg0-t)
    )
  )
;;;;;;;;;;;;;;;;;;;;;;; KozMos DynamicToolPack - DynamicHatch ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Name:		DynKey-C-HatchColor(0)							;;;
;;; Description:	Callback function of "C" to change hatch color.	AutoCAD truecolor is	;;;
;;;			supported.								;;;
;;; ------------------------------------------------------------------------------------------- ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 2009/01/20 (Koz Jono Yeoh) ;;;;;;;;
  (Defun DynKey-C-HatchColor (/ NEW)
    (if	Gradient
      (setq ActColor (Gradient-GetColor))
    )
    (if	(< ActColor 0)
      (setq new (cons 420 (abs ActColor)))
      (setq new ActColor)
    )
    (if	(null acad_truecolordlg)
      (if (setq new (acad_colordlg ActColor))
	(setq ActColor new)
      )
      (if (setq new (acad_truecolordlg new))
	(cond ((cdr (assoc 420 new))
	       (setq ActColor (* -1 (cdr (assoc 420 new))))
	      )
	      ((cdr (assoc 62 new))
	       (setq ActColor (cdr (assoc 62 new)))
	      )
	)
      )
    )
    (Modify-Hatch)
  )
;;;;;;;;;;;;;;;;;;;;;;; KozMos DynamicToolPack - DynamicHatch ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Name:		DynKey-D-ScaleDynamic(0)						;;;
;;; Description:	Callback function of "D" to change hatch scale dynamically between	;;;
;;;			0.1x ~ 10x.								;;;
;;;			Process shade/Tint change for Gradient Fill.				;;;
;;; ------------------------------------------------------------------------------------------- ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 2009/02/05 (Koz Jono Yeoh) ;;;;;;;;
  (Defun DynKey-D-ScaleDynamic (/	     Get-Input	  Get-Slider
				Update-Gradient		  COLORH
				COLORS	     OLDSCALE	  UPDHATCH
				UR	     XHATCH
			       )
    (Defun Get-Input (val)
      (if (and (setq val (distof val))
	       (<= val 1.0)
	       (>= val 0.0)
	  )
	(setq ActScale val
	      val      (Update-Gradient)
	)
	(set_tile "EdtValue" (rtos ActScale 2 3))
      )
    )
    (Defun Get-Slider (val)
      (setq ActScale (* 0.01 (read $Value)))
      (set_tile "EdtValue" (rtos ActScale 2 3))
      (Update-Gradient)
    )
    (Defun Update-Gradient (/ HSL)
      (if (or (null ColorH) (null ColorH))
	(setq hsl    (Color-TCI2RGB (cdr (assoc 421 hatch)))
	      hsl    (Color-RGB2HSL (car hsl) (cadr hsl) (caddr hsl))
	      ColorH (car hsl)
	      ColorS (cadr hsl)
	)
      )
      (setq UpdHatch (cdr (cdr (cdr (reverse hatch))))
	    hsl	     (Color-HSL2RGB ColorH ColorS (fix (* ActScale 100)))
	    hsl	     (Color-RGB2TCI (car hsl) (cadr hsl) (caddr hsl))
	    UpdHatch (cons (cons 421 hsl) UpdHatch)
	    UpdHatch (cons (assoc 470 hatch) UpdHatch)
	    UpdHatch (reverse UpdHatch)
	    UpdHatch (subst (cons 452 1) (assoc 452 UpdHatch) UpdHatch)
	    UpdHatch (subst (cons 462 ActScale)
			    (assoc 462 UpdHatch)
			    UpdHatch
		     )
      )
      (if (entmod UpdHatch)
	(setq hsl   (vla-update ActHatch)
	      hatch (entget (cdr (assoc -1 Hatch)))
	)
      )
    )
    (if	Gradient
      (if (and ActDCL
	       (setq XHatch   hatch
		     OldScale ActScale
	       )
	       (new_dialog "Shade" ActDCL)
	  )
	(progn
	  (set_tile "EdtValue" (rtos ActScale 2 3))
	  (set_tile "SldValue" (itoa (fix (* 100 ActScale))))
	  (action_tile
	    "EdtValue"
	    "(Get-Input $Value)"
	  )
	  (action_tile
	    "SldValue"
	    "(Get-Slider $Value)"
	  )
	  (if (= (start_dialog) 0)
	    (setq ActScale (entmod XHatch)
		  ActScale OldScale
	    )
	  )
	)
      )
      (progn
	(setq ur ActScale)
	(if SetRotate
	  (setq DynPoint nil)
	)
	(princ (vl-string-subst
		 (strcat (rtos (* ur 0.1) 2 2)
			 " ~ "
			 (rtos (* ur 10) 2 2)
		 )
		 "%Data%"
		 msg5
	       )
	)
	(while (and ur (setq new (grread t 5 0)))
	  (cond	((= (car new) 5)
		 (setq ActScale (* ur (Dynamic-Scale (cadr new))))
		 (Modify-Hatch)
		)
		((and (= (car new) 2) (cadr new) 118)
		 (setq ur nil)
		 (DynKey-V-ScaleValue)
		)
		(t (setq ur nil))
	  )
	)
	(princ (strcat "\n" msg4))
      )
    )
  )
;;;;;;;;;;;;;;;;;;;;;;; KozMos DynamicToolPack - DynamicHatch ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Name:		DynKey-F9-SnapMode(0)							;;;
;;; Description:	Callback function of "F9" to switch the SnapMode.			;;;
;;;			For Gradient Fill, this will also switch between 2 colors.		;;;
;;; ------------------------------------------------------------------------------------------- ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 2009/01/20 (Koz Jono Yeoh) ;;;;;;;;
  (Defun DynKey-F9-SnapMode ()
    (setvar "snapmode" (abs (1- (getvar "snapmode"))))
    (if	Gradient
      (setq ActColor (Gradient-GetColor))
    )
    (Modify-Hatch)
  )
;;;;;;;;;;;;;;;;;;;;;;; KozMos DynamicToolPack - DynamicHatch ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Name:		DynKey-L-HatchLayer(0)							;;;
;;; Description:	Callback function of "L" to match hatch layer with an existing object.	;;;
;;; ------------------------------------------------------------------------------------------- ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 2009/01/20 (Koz Jono Yeoh) ;;;;;;;;
  (Defun DynKey-L-HatchLayer (/ NEW)
    (setq ActMtext (subst (cons 1 "")
			  (assoc 1 ActMText)
			  ActMText
		   )
    )
    (entmod ActMText)
    (if	(setq new (entsel msg9))
      (setq ActLayer (cdr (assoc 8 (entget (car new))))
	    new	     (Modify-Hatch)
	    new	     (princ (vl-string-subst ActLayer "%Data%" msg9a))
	    new	     (princ msg4)
      )
    )
  )
;;;;;;;;;;;;;;;;;;;;;;; KozMos DynamicToolPack - DynamicHatch ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Name:		DynKey-O-HatchOrigin(0)							;;;
;;; Description:	Callback function of "O" to change common hatch origin point dynamiclly	;;;
;;;			For Gradient Fill, switch the "GradientCentered" parameter.		;;;
;;; ------------------------------------------------------------------------------------------- ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 2009/01/20 (Koz Jono Yeoh) ;;;;;;;;
  (Defun DynKey-O-HatchOrigin (/ UR NEW)
    (if	Gradient
      (progn
	(if
	  (equal (vla-get-GradientCentered ActHatch)
		 :vlax-true
	  )
	   (vla-put-GradientCentered ActHatch :vlax-false)
	   (vla-put-GradientCentered ActHatch :vlax-true)
	)
	(Modify-Hatch)
      )
      (progn
	(setq ur       T
	      ActMtext (subst (cons 1 "")
			      (assoc 1 ActMText)
			      ActMText
		       )
	)
	(entmod ActMText)
	(princ "\n")
	(while
	  (and
	    ur
	    (princ msg8)
	    (setq new (grread t 4 4))
	  )
	   (redraw)
	   (if (/= (car new) 5)
	     (setq ur nil)
	     (progn
	       (setq ActPoint (cadr new)
		     hatch    (subst (cons 43 (car ActPoint))
				     (assoc 43 hatch)
				     hatch
			      )
		     hatch    (subst (cons 44 (cadr ActPoint))
				     (assoc 44 hatch)
				     hatch
			      )
	       )
	       (grdraw ActOrigin ActPoint 12 1)
	       (entmod hatch)
	     )
	   )
	)
	(if (= (type (cadr new)) 'list)
	  (setq ActOrigin (cadr new))
	)
	(Modify-Hatch)
	(princ msg4)
      )
    )
  )
;;;;;;;;;;;;;;;;;;;;;;; KozMos DynamicToolPack - DynamicHatch ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Name:		DynKey-R-HatchRotation(0)						;;;
;;; Description:	Callback function of "R" to set and lock/unlock hatch rotation.		;;;
;;; ------------------------------------------------------------------------------------------- ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 2009/01/20 (Koz Jono Yeoh) ;;;;;;;;
  (Defun DynKey-R-HatchRotation	(/ NEW)
    (if	SetRotate
      (setq SetRotate nil
	    DynPoint nil
      )
      (progn
	(setq ActMtext (subst (cons 1 "")
			      (assoc 1 ActMText)
			      ActMText
		       )
	)
	(entmod ActMText)
	(if (setq new (rtos
			(/ (* 180 (angle ActOrigin ActPoint))
			   pi
			)
			2
			2
		      )
		  new (getreal
			(vl-string-subst new "%Data%" msg7)
		      )
	    )
	  (setq	ActPoint  (polar ActOrigin
				 (setq SetRotate (/ (* pi new) 180))
				 (distance ActOrigin ActPoint)
			  )
		new	  (Modify-Hatch)
		new	  (princ msg4)
	  )
	)
      )
    )
  )
;;;;;;;;;;;;;;;;;;;;;;; KozMos DynamicToolPack - DynamicHatch ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Name:		DynKey-S-PatternSwitch(0)						;;;
;;; Description:	Callback function of "S" to switch between common hatch patterns or all	;;;
;;;			gradient styles for Gradient Fill.					;;;
;;; ------------------------------------------------------------------------------------------- ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 2009/01/20 (Koz Jono Yeoh) ;;;;;;;;
  (Defun DynKey-S-PatternSwitch	(/	      Select-Pattern
				 OLDHP	      OLDNAME	   PATFILE
				 TGLDEFAULT   TGLREAD
				)
    (Defun Select-Pattern (val reason / TMP)
      (setq tmp (nth (read val) *HPName*))
      (if (or Gradient
	      (null (vl-catch-all-error-p
		      (vl-catch-all-apply
			'(lambda ()
			   (setvar "HPName" tmp)
			 )
		      )
		    )
	      )
	  )
	(setq ActName tmp
	      tmp     (Modify-Hatch)
	      tmp     (vla-update ActHatch)
	      tmp     (if (= reason 4)
			(done_dialog 1)
		      )
	)
	(set_tile "LstRead" (itoa (vl-position ActName *HPName*)))
      )
    )
    (if	(and ActDCL
	     (setq OldHP   (getvar "HPName")
		   OldName ActName
	     )
	     (new_dialog "DynHatch" ActDCL)
	)
      (progn
	(if (vl-catch-all-error-p
	      (vl-catch-all-apply
		'(lambda ()
		   (setvar
		     "HPName"
		     (getcfg "AppData/DynamicToolPack/DefaultPattern")
		   )
		 )
	      )
	    )
	  (setq TglDefault "0")
	  (setq TglDefault "1")
	)
	(if
	  (not (equal (getcfg "AppData/DynamicToolPack/AutoReadPAT")
		      "1"
	       )
	  )
	   (setq TglRead "0")
	   (setq TglRead "1")
	)
	(start_list "LstRead")
	(mapcar 'add_list *HPName*)
	(end_list)
	(set_tile "LstRead" (itoa (vl-position ActName *HPName*)))
	(set_tile "TglRead" TglRead)
	(set_tile "TglDefault" TglDefault)
	(set_tile "EdtRead" OldName)
	(action_tile "BtnRead" "(Read-PatFile nil)")
	(action_tile "TglRead" "(setq TglRead $Value)")
	(action_tile "TglDefault" "(setq TglDefault $Value)")
	(action_tile "LstRead" "(Select-Pattern $Value $Reason)")
	(if Gradient
	  (progn
	    (mode_tile "BtnRead" 1)
	    (mode_tile "TglRead" 1)
	    (mode_tile "TglDefault" 1)
	  )
	  (progn
	    (mode_tile "BtnRead" 0)
	    (mode_tile "TglRead" 0)
	    (mode_tile "TglDefault" 0)
	  )
	)
	(if (= (start_dialog) 0)
	  (setq ActName OldName)
	  (setq	OldName	(setcfg	"AppData/DynamicToolPack/AutoReadPAT"
				TglRead
			)
		OldName	(if (= TglDefault "1")
			  (setcfg "AppData/DynamicToolPack/DefaultPattern"
				  ActName
			  )
			)
	  )
	)
	(setvar "HPName" OldHP)
      )
    )
  )
;;;;;;;;;;;;;;;;;;;;;;; KozMos DynamicToolPack - DynamicHatch ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Name:		DynKey-V-ScaleValue(0)							;;;
;;; Description:	Callback function of "V" to enter accurate hatch scale.			;;;
;;; ------------------------------------------------------------------------------------------- ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 2009/01/20 (Koz Jono Yeoh) ;;;;;;;;
  (Defun DynKey-V-ScaleValue (/ NEW)
    (if	Gradient
      (vla-eval
	(vlax-get-acad-object)
	(strcat	"MsgBox "
		msg10
		",vbCritical,\042KozMos Inc.\042"
	)
      )
      (progn
	(setq ActMtext (subst (cons 1 "")
			      (assoc 1 ActMText)
			      ActMText
		       )
	)
	(entmod ActMText)
	(if (setq new (getreal (vl-string-subst
				 (rtos ActScale 2 3)
				 "%Data%"
				 msg6
			       )
		      )
	    )
	  (setq ActScale new)
	)
	(Modify-Hatch)
	(princ msg4)
      )
    )
  )
;;;;;;;;;;;;;;;;;;;;;;; KozMos DynamicToolPack - DynamicHatch ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Name:		DynKey-W-HiddenInformation(0)						;;;
;;; Description:	Callback function of "W" to print the detail English information.	;;;
;;;			This key press is hidden.						;;;
;;; ------------------------------------------------------------------------------------------- ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 2009/01/20 (Koz Jono Yeoh) ;;;;;;;;
  (Defun DynKey-W-HiddenInformation ()
    (textpage)
    (princ
      (strcat
	"\n**HIDDEN PROGRAM INFORMATION OF DynamicHatch by Koz Jono Yeoh|KozMos Inc.\n"
	"\n  DynamicHatch is originally designed by Andrea Andreetti, the new coded routine can proceed dynamic edit to both pattern hatch and Gradient Fill. DynamicHatch also uses a multiple-line MTEXT to show four information items of DynamicHatch: Angle, Scale, Pattern and Snap/Centered mode. Besides, the following features are also supported:\n"
	"\n[R] option:\n----------"
	"\n  It is used to lock the hatch rotation, normally, mouse move will cause hatch rotation change and sometimes, an accurate rotation angle may be needed by users. Thus, the R option will be helpful to enable users entering an angle value and then lock it. After that, moving the mouse will not cause hatch rotation changes any more. If we need to unlock it, just press R again.\n"
	"\n[C] option:\n----------"
	"\n  DynamicHatch fully support truecolor, and the info content's color will also synchonize with the hatch color. Only in case that hatch color is \042BYLAYER\042 but the layer's color is not white/black, the two colors are not same.\n"
	"\n[F9] Option to switch point snap:\n--------------------------------"
	"\n  As DynamicHatch use grread, so can not snap point. F9 key will help program to determine if an accurate point is needed (default snap by int and end). This is necessary and useful in changing the hatch origin point.\n"
	"\n  For Gradient Fill, F9 is used to switch the color change between two colors.\n"
	"\n[S] option for hatch pattern switch:\n-----------------------------------"
	"\n  DynamicHatch reads a data file (DynHatch.TXT) recording names of all frequently used patterns.If it is not exist, ANSI31~ANSI37 will then be automatically gathered.\n"
	"\n  In order to maintain reading patterns from user indicated PAT file and enable users a way to jump between patterns. A very simple dialog is introduced. By selecting the pattern names, the hatch in graphic screen will also changed and users can select a PAT file to read and make DynamicHatch to read it automatically each time."
	"\n  Very thankful to \042irneb\042 from AUGI forum for this DCL selection idea."
	"\n  For Gradient Fill, S option will change between all valid Gradient types.\n"
	"\n[D] option for dynamic Hatch Scale:\n----------------------------------"
	"\n  When D is used for dynamic scaling, the instant distance between cursor point and hatch original point will be defined as old scale. Then new distance will be used to divide the the old one. In order to prevent too densy grid, the scale changes were limited between 0.1x ~ 10x.\n"
	"\n\n Press any key to continue...")
    )
    (grread)
    (princ "\n")
    (graphscr)
    (princ msg4)
  )
;;;;;;;;;;;;;;;;;;;;;;; KozMos DynamicToolPack - DynamicHatch ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Name:		DynKey-Z-Gradient2Hatch(1)						;;;
;;; Description:	Callback function of "Z" to switch between GradientHatch and CommonHatch;;;
;;;			and read proper data from final hatch object.				;;;
;;;			Too frequently switch may cause program crash.				;;;
;;; Argu(1):		Flag to switch hatch type			BOOLEAN			;;;
;;; ------------------------------------------------------------------------------------------- ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 2009/01/20 (Koz Jono Yeoh) ;;;;;;;;
  (Defun DynKey-Z-Gradient2Hatch (flag)
    (if	flag
      (if (= (vla-get-HatchObjectType ActHatch) AcHatchObject)
	(setq OldHName (::DynamicToolPack-StrCase
			 (vla-get-PatternName ActHatch)
		       )
	      Gradient (vla-put-HatchObjectType ActHatch AcGradientObject)
	      Gradient (if OldGName
			 (vla-put-GradientName ActHatch OldGName)
		       )
	      Gradient (vla-update ActHatch)
	)
	(progn
	  (setq	OldGName (strcase (vla-get-GradientName ActHatch))
		Gradient (vla-put-HatchObjectType ActHatch AcHatchObject)
	  )
	  (if OldHName
	    (setq hatch	   (subst (cons 2 OldHName) (assoc 2 hatch) hatch)
		  Gradient (entmod hatch)
		  Gradient (vla-update ActHatch)
	    )
	  )
	)
      )
    )
    (if	(= (vla-get-HatchObjectType ActHatch) AcGradientObject)
      (setq Gradient T
	    ActName  (strcase (vla-get-GradientName ActHatch))
	    *HPNAME* '("LINEAR"		  "CYLINDER"
		       "INVCYLINDER"	  "SPHERICAL"
		       "HEMISPHERICAL"	  "CURVED"
		       "INVSPHERICAL"	  "INVHEMISPHERICAL"
		       "INVCURVED"
		      )
	    ActAngle (vla-get-GradientAngle ActHatch)
	    ActColor (Gradient-GetColor)
	    ActScale (if (= (cdr (assoc 452 hatch)) 1)
		       (cdr (assoc 462 hatch))
		       (getvar "GFCLRLUM")
		     )
      )
      (progn
	(setq Gradient nil
	      ActAngle (vla-get-PatternAngle ActHatch)
	      ActName  (::DynamicToolPack-StrCase
			 (vla-get-PatternName ActHatch)
		       )
	      *HPName* nil
	      *HPName* (Read-Pattern)
	      *HPName* (if (null (member ActName *HPName*))
			 (cons ActName *HPName*)
			 *HPName*
		       )
	      ActScale (vla-get-PatternScale ActHatch)
	)
	(if (setq ActColor (cdr (assoc 420 hatch)))
	  (setq ActColor (* -1 ActColor))
	  (if (null (setq ActColor (cdr (assoc 62 hatch))))
	    (setq ActColor 256)
	  )
	)
      )
    )
    (if	flag
      (Modify-Hatch)
    )
  )

;;; Main routine started
  (::DynamicToolPack-Start)
  (Read-Message)
  (setq	ActDCL (Extract-DCL)
	obj    (entsel msg2)
  )
  (vl-catch-all-error-p
    (vl-catch-all-apply
      '(lambda ()
	 (setvar "HPAssoc" 1)
	 (setvar "HPName"
		 (getcfg "AppData/DynamicToolPack/DefaultPattern")
	 )
       )
    )
  )
  (cond	((null obj)
	 (setq pt  (cadr (grread t 4 4))
	       old (entlast)
	 )
	 (command "_.BHatch" pt "")
	 (if (not (eq old (entlast)))
	   (setq obj (entlast)
		 ActSave obj
	   )
	 )
	)
	(t
	 (setq pt  (cadr obj)
	       obj (car obj)
	 )
	 (cond ((= (cdr (assoc 0 (entget obj))) "HATCH")
		(princ)
	       )
	       ((member	(cdr (assoc 0 (entget obj)))
			'("ARC"		"CIRCLE"      "LWPOLYLINE"
			  "POLYLINE"	"SPLINE"      "ELLIPSE"
			 )
		)
		(setq old (entlast)
		      bdr (command "_.Hatch" "_S" obj "" "")
		      bdr (entlast)
		)
		(if (not (eq old bdr))
		  (setq	obj bdr
			ActSave	obj
		  )
		  (setq obj nil)
		)
	       )
	       (t (setq obj nil))
	 )
	)
  )
  (if obj
    (progn
      (if (null ActSave)
	(setq ActSave (entget obj))
      )
      (setq hatch    (entget obj)
	    ActHatch (vlax-ename->vla-object obj)
	    ActLayer (cdr (assoc 8 hatch))
      )
      (vla-getboundingbox ActHatch 'll 'ur)
      (if vlax-safearray->list
	(setq ll (vlax-safearray->list ll)
	      ur (vlax-safearray->list ur)
	)
      )
      (setq ll	      (trans ll 0 1)
	    ur	      (trans ur 0 1)
	    ActOrigin (polar ll (angle ll ur) (* (distance ll ur) 0.5))
      )
      (DynKey-Z-Gradient2Hatch nil)
      (princ (strcat msg3 msg4))
      (::DynamicToolPack-Options
	'(("Dynamic"
	   .
	   "(progn(if(or(null SetRotate)(null ActPoint))(setq ActPoint ::DynamicPoint::)(setq DynPoint ::DynamicPoint::))(Modify-Hatch)(Grdraw-Angle))"
	  )
	  ("A" . "(DynKey-A-AboutDynamicHatch)")
	  ("C" . "(DynKey-C-HatchColor)")
	  ("D" . "(DynKey-D-ScaleDynamic)")
	  ("F8"
	   .
	   "(progn(setvar\042orthomode\042(abs(1-(getvar\042orthomode\042))))(Modify-Hatch))"
	  )
	  ("F9" . "(DynKey-F9-SnapMode)")
	  ("L" . "(DynKey-L-HatchLayer)")
	  ("O" . "(DynKey-O-HatchOrigin)")
	  ("R" . "(DynKey-R-HatchRotation)")
	  ("S" . "(DynKey-S-PatternSwitch)")
	  ("V" . "(DynKey-V-ScaleValue)")
	  ("W" . "(DynKey-W-HiddenInformation)")
	  ("Z" . "(DynKey-Z-Gradient2Hatch T)")
	 )
      )
    )
  )
  (if ActMText
    (entdel (cdr (assoc -1 ActMText)))
  )
  (::DynamicToolPack-End)
  (princ)
)
(princ
  "\n DynamicHatch V1.6.90210 for KozMos DynamicToolPack is loaded\n Start command with DYNHATCH.\tCopyright(C) 1994-2009 KozMos Inc."
)
(princ)
