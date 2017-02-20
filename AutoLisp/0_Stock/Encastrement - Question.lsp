(princ "\nENCASTR - Verifier un encastrement")

(defun c:encastr  (/)
  (setq enc_s1 'nil)
  (vl-load-com)
;;;(textscr)
  (setq	entete	(strcat
		  "\n---------------- Verification d'un encastrement ----------------"
		  "\nSuivant norme NF P 22-460 et Règles professionnelles du CTICM"
		  "\n\n-----------------------------------------------------"
		  "\n|  Utilisez le formulaire pour remplir les valeurs  |"
		  "\n-----------------------------------------------------"
		  "\n!!!! Resultat à verifier à la main !!!!"
		  "\n\n")
	donnees	"")
  (princ entete)
;;;
;;;-----------------------------------------------------------------
;;;-----------------------------------------------------------------
;;;                                                                 
;;;     Entrée des données                                          
;;;                                                                 
;;;-----------------------------------------------------------------
;;;-----------------------------------------------------------------
;;;
;;;
;;;                                                            
;;; Choix du type d'encastrement                               
;;;                                                            
;;;
  (setq	list_choix_type_encastr
	 '("LIST"
	   "enc_type"
	   "Type d'encastrement [Poutre/Poutre --> Ptr/Ptr Poteau/Poutre --> Pt/Ptr]"
	   ("[Ptr/Ptr] - Avec bls ext, sans raid de platine"
	    "[Ptr/Ptr] - Avec bls ext, avec raid de platine"
	    "[Ptr/Ptr] - Sans bls ext"
	    "[Pt/Ptr] - Avec bls ext, sans raid de poteau, sans raid de platine"
	    "[Pt/Ptr] - Avec bls ext, sans raid de poteau, avec raid de platine"
	    "[Pt/Ptr] - Avec bls ext, avec raid de poteau, sans raid de platine"
	    "[Pt/Ptr] - Avec bls ext, avec raid de poteau, avec raid de platine"
	    "[Pt/Ptr] - Sans bls ext, sans raid de poteau"
	    "[Pt/Ptr] - Sans bls ext, avec raid de poteau")))
  (question list_choix_type_encastr "encastr")
  (setq	donnees	(strcat	donnees
			"\n"
			(caddr list_choix_type_encastr)
			" :"
			"\n| Choix "
			(nb_str (eval (read (cadr list_choix_type_encastr))))
			" : "
			(nth (1- (eval (read (cadr list_choix_type_encastr)))) (last list_choix_type_encastr))
			"\n\n"))
;;;
;;;                                                            
;;; definitions des variables                                  
;;;                                                            
;;;
  (setq	list_var '(("REAL" "enc_A" "A -> Aire de la section jarret (mm²)")
		   ("REAL" "enc_Ntrac" "+N -> Effort Normal de Traction (daN)")
		   ("REAL" "enc_Ncomp" "-N -> Effort Normal de Compression (daN)")
		   ("REAL" "enc_T" "T -> Effort Tranchant Maximum (daN)")
		   ("REAL" "enc_M" "M -> Moment Flechissant (daN)")
		   ("STRING" "enc_Tb" "Type de boulons (ex : HR18 10.9 ou M16 8.8)")
		   ("REAL" "enc_jtr" "Jeu dans les perçages des boulons (mm)")
		   ("STRING"
		    "enc_List_Bls"
		    "Liste des dist des bls par rapport au point de rotation\nex : 50 100 150 (mm)")
		   ;;("REAL" "enc_n" "Nb -> nombre total de boulons")
		   ("LIST" "enc_nf" "Nombre de files de boulons" ("2 files" "4 files"))))
  (foreach var	list_var
    (question var "encastr")
    (if	(/= (car var) "LIST")
      (setq donnees (strcat donnees (caddr var) " : " (nb_str (eval (read (cadr var)))) "\n"))
      (setq donnees (strcat donnees
			    "\n"
			    (caddr var)
			    " :"
			    "\n| Choix "
			    (nb_str (eval (read (cadr var))))
			    " : "
			    (nth (1- (eval (read (cadr var)))) (last var))
			    "\n\n")))
    ;; test de condition sur les variables
    (cond ((= (cadr var) "enc_Tb")
	   ;;N_test : strictement sur 0 ou 1
	   (setq enc_Tb (strcase enc_Tb))
	   (if (not (or (= (substr enc_Tb 1 2) "HR") (= (substr enc_Tb 1 1) "M")))
	     (progn (princ "\nLes boulons doivent être soit HR ou M !!!") (exit))))))
;;;
;;;                                                            
;;; Nombre de boulons                                          
;;;                                                            
;;;
  (setq	enc_List_Bls (str_to_lst enc_List_Bls)
	enc_n	     (* 2 enc_nf (length enc_List_Bls))
	donnees	     (strcat donnees "Nb -> nombre total de boulons : " (nb_str enc_n) "\n"))
  (princ (strcat "Nb -> nombre total de boulons : " (nb_str enc_n) "\n"))
;;;
;;;                                                            
;;; choix du nombre de files de boulons                        
;;;                                                            
;;;
  (if (= enc_nf 2)
    (setq list_var '(("REAL" "enc_ss" "s' -> Ecartement entre les files interieurs et exterieurs de boulons")))
    (setq list_var 'nil))
  (setq	list_var (append list_var
			 '(;;("REAL" "enc_s" "s -> Ecartement entre les deux files interieurs de boulons")
			   ("REAL" "enc_uf" "uf -> Coefficient de frottement")
			   ("REAL" "enc_se" "sigma e -> limite elastique de l'acier (daN/mm²)")
			   ("REAL" "enc_e" "e -> Epaisseur de la platine du jarret (mm)")
			   ("REAL" "enc_lplatine" "Largeur de la platine du jarret (mm)")
			   ("REAL" "enc_ea" "ea -> Epaisseur de l'âme du jarret (mm)")
			   ("REAL" "enc_h" "h -> Hauteur du jarret (0 si inutile) (mm)")
			   ("REAL" "enc_es" "es -> Epaisseur de la semelle inférieure du jarret (mm)")
			   ("REAL" "enc_b" "b -> Largeur du jarret (mm)"))))
  (if (> enc_type 3)
    (setq list_var
	   (append list_var
		   '(("REAL" "enc_eaa" "ea' -> Epaisseur de l'âme du poteau (mm)")
		     ("REAL" "enc_ess" "es' -> Epaisseur de la semelle du poteau (mm)")
		     ("REAL" "enc_er" "er -> Epaisseur du raidisseur du poteau (0 si inexistant) (mm)")
		     ("REAL" "enc_r" "r -> Rayon du congé interieur du poteau (mm)")
		     ("REAL"
		      "enc_Ac"
		      "Aire de la section du poteau (Ac) [pour le calcul de la resist. de l'âme] (mm²)")
		     ("REAL"
		      "enc_lp"
		      "Largeur de la section du poteau [pour le calcul de la resist. de l'âme et de b'] (mm)")))))
  (foreach var	list_var
    ;; test de condition sur les variables
    (cond ((= (cadr var) "enc_uf")
	   ;;si enc_uf = 0
	   (if (not enc_uf)
	     (setq encastr_enc_uf 0.3)))
	  ((= (cadr var) "enc_se")
	   ;;si enc_se = nil
	   (if (not encastr_enc_se)
	     (setq encastr_enc_se 23.5))))
    (question var "encastr")
    (setq donnees (strcat donnees (caddr var) " : " (nb_str (eval (read (cadr var)))) "\n")))
;;;
;;;                                                            
;;; Choix du type de poteau                                    
;;;                                                            
;;;
  (setq list_choix_prof_poteau '("O-N" "enc_hprf" "Le poteau est il un PRS"))
  (if (> enc_type 3)
    (progn
      (question list_choix_prof_poteau "encastr")
      (setq donnees (strcat donnees
			    "\n"
			    (caddr list_choix_prof_poteau)
			    " : "
			    (eval (read (cadr list_choix_prof_poteau)))
			    "\n"))
      (if (= enc_hprf "O")
	(progn (question
		 '("REAL" "enc_hh" "Hauteur de la section du poteau [pour le calcul de la resist. de l'âme] (mm)")
		 "encastr")
	       (setq donnees (strcat donnees
				     "Hauteur de la section du poteau [pour le calcul de la resist. de l'âme] (mm)"
				     " : "
				     (nb_str enc_hh)
				     "\n"))))))
;;;
;;;                                                            
;;; Calcul de b'                                               
;;;                                                            
;;;
  (setq enc_bb (/ (+ enc_lplatine enc_b) 2.0))
;;;
;;;-----------------------------------------------------------------
;;;-----------------------------------------------------------------
;;;                                                                 
;;;     Calcul de la précontrainte du boulons                       
;;;     et de l'effort tranchant maximum admissible sur 1 boulon    
;;;                                                                 
;;;-----------------------------------------------------------------
;;;-----------------------------------------------------------------
;;;
  (setq	list_sigma_red '(("4.6" 24) ("4.8" 28) ("5.6" 30) ("5.8" 34) ("6.6" 35) ("6.8" 41) ("8.8" 55) ("10.9" 67))
	list_As_bls    '(("8" 36.6)
			 ("10" 58.0)
			 ("12" 84.3)
			 ("14" 115.0)
			 ("16" 157.0)
			 ("18" 192.0)
			 ("20" 245.0)
			 ("22" 303.0)
			 ("24" 353.0)
			 ("27" 459.0)
			 ("30" 561.0))
	Bls_list       (str_to_lst enc_Tb))
  (cond	((= (substr (car Bls_list) 1 1) "M")
	 (setq enc_Bl  "M"
	       enc_Db  (atoi (substr (car Bls_list) 2))
	       enc_As  (last (assoc (itoa enc_Db) list_As_bls))
	       enc_Qb  (atof (cadr Bls_list))
	       enc_seb (last (assoc (rtos enc_Qb 2) list_sigma_red)))
	 (setq Bl_res	 (* enc_seb enc_As)
	       Tr	 (arr 0 (/ (* enc_seb enc_As) 1.25))
	       Cis	 (arr 0 (/ (* enc_As enc_seb) 1.54))
	       print_Tr	 "N_bls"
	       print_Cis "V_bls"))
	((= (substr (car Bls_list) 1 2) "HR")
	 (setq enc_Bl  "HR"
	       enc_Db  (atoi (substr (car Bls_list) 3))
	       enc_As  (last (assoc (itoa enc_Db) list_As_bls))
	       enc_Qb  (atof (cadr Bls_list))
	       enc_seb (* (fix enc_Qb) (* (- enc_Qb (fix enc_Qb)) 10)))
	 (setq Tr	 (arr 0 (* 0.8 enc_seb enc_As))
	       Cis	 (arr 0 (* 1.1 Tr enc_uf))
	       print_Tr	 "Pv"
	       print_Cis "Qadm")))
;;;
;;;                                                            
;;; Verification du cisaillement si boulons NON hr             
;;; si T_bls > 60% de Cis, verifier l'effort combiné V + H     
;;;                                                            
;;;
  (setq Bl_Cis (/ enc_T enc_n))
;;;
;;;                                                            
;;; Calcul de l'effort de traction max dans le bls             
;;;                                                            
;;;
  (if (> (/ Bl_Cis Cis) 0.6)
    (setq Tr (sqrt (- (carre Bl_res) (* 2.36 (carre Bl_Cis))))))
  (setq	print_Bls_carac
	 (edit_msg (list "TB" enc_Tb "SEB" enc_seb "AS"	enc_As "TR" print_Tr "Tr" Tr "CIS" print_Cis "Cis" Cis "UF"
			 enc_uf)
		   (strcat "Type de boulons : %TB!\nSigma e du boulon : %SEB! daN/mm²"
			   "\nAs du boulon : %AS! mm²"
			   "\n%TR! = %Tr! daN [ 0.8 × %SEB! × %AS! = 0.8 × Seb × As ]"
			   "\n%CIS! = %Cis! daN [ 1.1 × %Tr! × %UF! = 1.1 × %TR! × uf ]")
		   2))
;;;
;;;-----------------------------------------------------------------
;;;-----------------------------------------------------------------
;;;                                                                 
;;;     Article 9.1 Verification des conditions d'emploi de la      
;;;                 méthode                                         
;;;                                                                 
;;;-----------------------------------------------------------------
;;;-----------------------------------------------------------------
;;;
;;;
;;;                                                            
;;; Verification des conditions d'emploi de la méthode         
;;;                                                            
;;;
  (setq	test_A (arr 0 (* 0.15 enc_A enc_se))
	test_B (arr 0 (* 0.15 enc_n Tr))
	test_C (arr 0 (* 0.05 enc_A enc_se)))
;;;
;;;                                                            
;;; Choisi l'effort normal max entre les deux efforts normaux  
;;;                                                            
;;;
  (setq N_test (max (abs enc_Ncomp) (abs enc_Ntrac)))
;;;
;;;                                                            
;;; Verifie si l'effort normal est inferieur à test_A et test_B
;;;                                                            
;;;
  (if (and (<= N_test test_A) (<= N_test test_B))
    (setq ng1	""
	  ng2	""
	  signe	"<=")
    (setq ng1	"NE "
	  ng2	"PAS "
	  signe	">="))
  (setq	print_condition
	 (edit_msg (list "NG1"	  ng1
			 "NG2"	  ng2
			 "SGN"	  signe
			 "NT"	  enc_Ntrac
			 "NC"	  enc_Ncomp
			 "T_A"	  test_A
			 "A"	  enc_A
			 "SE"	  enc_se
			 "T_B"	  test_B
			 "NB"	  enc_n
			 "TR"	  print_Tr
			 "Tr"	  Tr)
		   (strcat
		     "L'assemblage %NG1!verifie %NG2!les conditions d'emploi de la méthode"
		     "\n---------------------"
		     "\n+N = %NT! daN / -N = %NC! daN"
		     "\n±N %SGN! %T_A! daN [ 0.15 × %A! × %SE! = 0.15 × A × Se ]"
		     "\n±N %SGN! %T_B! daN [ 0.15 × %NB! × %Tr! = 0.15 × n × %TR! ]")
		   2))
;;;
;;;                                                                             
;;; Verifie si l'effort normal doit être pris en compte dans les prochain calcul
;;; Si non -> N_test est mis à 0                                                
;;;                                                                             
;;;
  (if (<= N_test test_C)
    (setq ng3 "NON "
	  signe	"<="
	  condition_verif_N
	   'nil
	  N_test 0)
    (setq ng3 ""
	  signe	">="
	  condition_verif_N
	   'T))
  (setq	print_condition_verif_N
	 (edit_msg (list "NG3" ng3 "SGN" signe "T_C" test_C "A" enc_A "SE" enc_se)
		   (strcat "Verification de l'effort normal dans l'assemblage %NG3!necessaire"
			   "\n---------------------"
			   "\n±N %SGN! %T_C! daN [0.05 × %A! × %SE! = 0.05 × A × Se]")
		   2))
;;;
;;;-----------------------------------------------------------------
;;;-----------------------------------------------------------------
;;;                                                                 
;;;     Article 9.2.2.1 Calcul de l'assemblage à l'effort tranchant 
;;;                                                                 
;;;-----------------------------------------------------------------
;;;-----------------------------------------------------------------
;;;
;;;
;;;                                                                   
;;; Verification de la résistance de l'assemblage à l'effort tranchant
;;;                                                                   
;;;
  (setq T_adm (* enc_n Cis))
  (if (< T_adm enc_T)
    (setq ng1	"NE "
	  ng2	"PAS "
	  signe	">=")
    (setq ng1	""
	  ng2	""
	  signe	"<"))
  (setq	print_resit_effort_T
	 (edit_msg (list "NG1" ng1 "NG2" ng2 "SGN" signe "TRCH"	enc_T "TADM" T_adm "NB"	enc_n "CIS" print_Cis "Cis" Cis)
		   (strcat "L'assemblage %NG1!resiste %NG2!à l'effort tranchant"
			   "\n---------------------"
			   "\nT = %TRCH! daN / %CIS! = %Cis! daN"
			   "\nT %SGN! %TADM! [ %NB! × %Cis! = Nb × %CIS! ]")
		   2))
;;;
;;;-----------------------------------------------------------------
;;;-----------------------------------------------------------------
;;;                                                                 
;;;     Article 9.2.2.2.1 Calcul du moment resistant                
;;;                       Calcul de la partie tendue                
;;;                                                                 
;;;-----------------------------------------------------------------
;;;-----------------------------------------------------------------
;;;
;;;
;;;                                                
;;; Calcul du moment résistant                     
;;;                                                
;;;
  (setq	hh    (* (- enc_h enc_es) 0.001)
	M_res (arr 0 (+ enc_M (/ (* N_test hh enc_b enc_es) enc_A))))
  (if (/= N_test 0)
    (setq print_M_res
	   (edit_msg (list "HH" hh "HT" enc_h "ES" enc_es)
		     (strcat "Calcul du moment resistant"
			     "\n---------------------"
			     "\nh' = %HH! m [ %HT! - %ES! = h - es ]")
		     2))
    (setq print_M_res (strcat "Calcul du moment resistant" "\n---------------------")))
  (setq print_M_res (strcat print_M_res (edit_msg (list "MR" M_res) "\nMres = %MR! daN.m" 2)))
;;;
;;;                                                            
;;; Calcul de X pour determiner la partie tendue               
;;;                                                            
;;;
  (setq	LX	(arr 1 (* enc_es (sqrt (/ enc_b enc_ea))))
	print_X	(edit_msg (list "LX" LX "ES" enc_es "B" enc_b "EA" enc_ea)
			  (strcat "Calcul de la partie tendue"
				  "\n---------------------"
				  "\nX = %LX! mm [ %ES! × \\/¯(%B! / %EA!) = es × \\/¯(b / ea)")
			  2))
;;;
;;;-----------------------------------------------------------------
;;;-----------------------------------------------------------------
;;;                                                                 
;;;     Article 9.2.2.2.2 Effort admissible de compression          
;;;                                                                 
;;;     Calcul de Ac Poutre et Ac Poteau en fonction du tableau 3   
;;;                                                                 
;;;-----------------------------------------------------------------
;;;-----------------------------------------------------------------
;;;
;;;
;;;                                      
;;; Calcul de Ac_poutre                  
;;;                                      
;;;
  (setq	Ac_poutre (* enc_es (+ enc_b (sqrt (* enc_b enc_ea))))
	msg_ptr	  (edit_msg (list "ACPR" Ac_poutre "ES" enc_es "B" enc_B "EA" enc_ea)
			    (strcat "\nAc Poutre = %ACPR! mm²"		    "\n---------"
				    "\n[ es × ( b + \\/¯(b × ea)) ]"	    "\n[ %ES! × ( %B! + \\/¯(%B! × %EA!)) ]"
				    "\n---------")
			    2))
;;;
;;;                                                          
;;; Calcul de Ac_poteau avec ou sans raidisseur              
;;; si le type d'encastrement est superieur à 3              
;;;                                                          
;;;
  (if (> enc_type 3)
    (progn (if (= enc_er 0)
	     (setq Ac_poteau (* enc_eaa (+ enc_es (* 2.0 enc_e) (* 5 (+ enc_ess enc_r))))
		   msg_pt    (edit_msg (list "ACPT" Ac_poteau "EAA" enc_eaa "ES" enc_es "E" enc_e "ESS" enc_ess "R" enc_r)
				       (strcat
					 "\nAc Poteau = %ACPT! mm²"
					 "\n---------"
					 "\n[ ea' × ( es + 2e + ( 5 × (es' + r)))]"
					 "\n[ %EAA! × ( %ES! + 2 × %E! + 5 × (%ESS! + %R!)) ]"
					 "\n---------")
				       2))
	     (setq Ac_poteau (+ (* enc_eaa (+ enc_es (* 2.0 enc_e) (* 5 (+ enc_ess enc_r)))) (* enc_er enc_bb))
		   msg_pt    (edit_msg (list "ACPT"   Ac_poteau
					     "EAA"    enc_eaa
					     "ES"     enc_es
					     "E"      enc_e
					     "ESS"    enc_ess
					     "R"      enc_r
					     "ER"     enc_er
					     "BB"     enc_BB)
				       (strcat
					 "\nAc Poteau = %ACPT! mm²"
					 "\n---------"
					 "\n[ ea' × ( es + 2e + ( 5 × (es' + r))) + er × b']"
					 "\n[ %EAA! × ( %ES! + 2 × %E! + 5 × (%ESS! + %R!)) + %ER! × %BB! ]"
					 "\n---------")
				       2)))
	   (setq Ac (arr 1 (min Ac_poutre Ac_poteau))))
;;;
;;;                                                          
;;; Si enc_type < 3                                          
;;;                                                          
;;;
    (setq Ac	 Ac_poutre
	  msg_pt ""))
;;;
;;;-----------------------------------------------------------------
;;;-----------------------------------------------------------------
;;;                                                                 
;;;     Article 9.2.2.2.2 Effort admissible de compression          
;;;                                                                 
;;;     Verification des efforts suivant expression de l'article    
;;;                                                                 
;;;-----------------------------------------------------------------
;;;-----------------------------------------------------------------
;;;
  (if (= N_test 0)
    (setq Nc_adm (* Ac enc_se)
	  msg_Nc (edit_msg (list "NCADM" Nc_adm "AC" AC "SE" enc_se)
			   (strcat "\nNc admissible = %NCADM! daN"
				   "\n---------"		"\n[ Ac × Se ]"
				   "\n[ %AC! × %SE! ]"		"\n---------")
			   2))
    (setq Nc_adm (+ (* Ac enc_se) (/ (* N_test Ac_poutre) enc_A))
	  msg_Nc (edit_msg (list "NCADM" Nc_adm "AC" AC "SE" enc_se "N" N_test "ACP" Ac_poutre "A" enc_A)
			   (strcat "\nNc admissible = %NCADM! daN"	     "\n---------"
				   "\n[ Ac × Se + (N × Ac poutre) / A]"	     "\n[ %AC! × %SE! + (%N! × %ACP!) / %A!]"
				   "\n---------")
			   2)))
  (setq	print_Ac_compression
	 (strcat "Effort admissible de compression" "\n---------------------" msg_ptr msg_pt msg_Nc))
;;;
;;;-----------------------------------------------------------------
;;;-----------------------------------------------------------------
;;;                                                                 
;;;     Calcul de s1 et des différents s2                           
;;;                                                                 
;;;-----------------------------------------------------------------
;;;-----------------------------------------------------------------
;;;
  (setq	enc_List_Bls  (vl-sort (mapcar '(lambda (z) (* (atof z) 0.001)) enc_List_Bls) '>)
	;; supprime les bls dans la partie comprimée
	list_dist_bls (vl-remove-if '(lambda (y) (< (* y 1000.0) LX)) enc_List_Bls)
	;; calcul les ecartements de boulon
	list_bls_ect  (mapcar '(lambda (x y) (arr_n 0.1 (* (- x y) 1000.0) 't)) enc_List_Bls (cdr enc_List_Bls)))
;;;
;;;                                                          
;;; si bls ext, le 1er ect --> s1                            
;;;                                                          
;;;
  (cond	((member enc_type '(1 2 4 5 6 7))
	 (setq enc_s1	    (car list_bls_ect)
	       list_bls_ect (cdr list_bls_ect))))
;;;
;;;                                                          
;;; liste des s2 pour chaque bls en fonction de l'ecartement 
;;;                                                          
;;;
  (setq list_s2 (mapcar '(lambda (x y) (fix (* (+ x y) 0.5)))
			(cons (car list_bls_ect) list_bls_ect)
			(reverse (cons (car (reverse list_bls_ect)) (reverse list_bls_ect)))))
;;;
;;;-----------------------------------------------------------------
;;;-----------------------------------------------------------------
;;;                                                                 
;;;     Calcul des efforts maximum admissible dans les boulons      
;;;                                                                 
;;;-----------------------------------------------------------------
;;;-----------------------------------------------------------------
;;;
  (setq	N1_Bls_Ext
	 'nil
	N1_Bls_Ext_4file
	 'nil
	N1_Bls_Int
	 'nil
	N1_Bls_Int_4file
	 'nil
	N1_Bls_Cent
	 'nil
	print_N1_Ext ""
	print_N1_Ext_4files ""
	print_N1_Int ""
	print_N1_Int_4files ""
	print_N1_Cent "")
  (princ "\n------ Paramètre de l'encastrement ------\n\n")
  (setq donnees (strcat donnees "\n\n-------- Paramètre de l'encastrement --------\n\n"))
;;;
;;;-----------------------------------------------------------------
;;;-----------------------------------------------------------------
;;;                                                                 
;;;     Boulons exterieurs                                          
;;;                                                                 
;;;-----------------------------------------------------------------
;;;-----------------------------------------------------------------
;;;
  (cond	((= enc_type 1)
	 (setq list_var	'(("REAL" "enc_br" "Prolongement platine -> br (mm)")
			  ("REAL" "enc_a1" "a1 (mm)")
			  ("REAL" "enc_a2" "a2 (mm)")
			  ("REAL" "enc_a3" "a3 (mm)")
			  ("REAL" "enc_a4" "a4 (mm)")))
	 (foreach var  list_var
	   (question var "encastr")
	   (setq donnees (strcat donnees (caddr var) " : " (nb_str (eval (read (cadr var)))) "\n")))
	 (setq enc_s   (+ (* enc_a2 2.0) enc_ea)
	       donnees (strcat donnees "s -> Ecartement entre les deux files interieurs de boulons : " (nb_str enc_s) "\n"))
	 (princ (strcat "s -> Ecartement entre les deux files interieurs de boulons : " (nb_str enc_s) "\n"))
	 (setq N1_2	    (* 375.0 enc_e (* (/ enc_a4 enc_a3) (/ enc_s (+ enc_s enc_a4))))
	       N1_Bls_Ext   (list (list Tr N1_2))
	       print_N1_Ext (edit_msg (list "N11" Tr "N12" N1_2 "BLS" print_Tr "E" enc_e "A4" enc_a4 "A3" enc_a3 "S" enc_s)
				      (strcat
					"\n----- Boulons Exterieurs -----"
					"\nN1 <= %N11! = %BLS!"
					"\n---"
					"\nN1 <= %N12!"
					"\n375e × ((a4 / a3) × (s / (s + a4)))"
					"\n375 × %E! × ((%A4! / %A3!) × (%S! / (%S! + %A4!)))"
					"\n")
				      2)))
	((= enc_type 2)
	 (setq list_var	'(("REAL" "enc_br" "Raid platine -> br (mm)")
			  ("REAL" "enc_lr" "Raid platine -> lr (mm)")
			  ("REAL" "enc_eraid" "Raid platine -> ep (mm)")
			  ("REAL" "enc_a1" "a1 (mm)")
			  ("REAL" "enc_a2" "a2 (mm)")
			  ("REAL" "enc_a3" "a3 (mm)")
			  ("REAL" "enc_a4" "a4 (mm)")))
	 (foreach var  list_var
	   (question var "encastr")
	   (setq donnees (strcat donnees (caddr var) " : " (nb_str (eval (read (cadr var)))) "\n")))
	 (setq enc_s   (+ (* enc_a2 2.0) enc_ea)
	       donnees (strcat donnees "s -> Ecartement entre les deux files interieurs de boulons : " (nb_str enc_s) "\n"))
	 (princ (strcat "s -> Ecartement entre les deux files interieurs de boulons : " (nb_str enc_s) "\n"))
	 (setq N1_2	    (* 375.0 enc_e (+ (/ enc_a2 enc_a1) (/ enc_a4 enc_a3)))
	       N1_Bls_Ext   (list (list Tr N1_2))
	       print_N1_Ext (edit_msg (list "N11"   Tr
					    "N12"   N1_2
					    "BLS"   print_Tr
					    "E"	    enc_e
					    "A1"    enc_a1
					    "A2"    enc_a2
					    "A4"    enc_a4
					    "A3"    enc_a3
					    "S"	    enc_s)
				      (strcat "\n----- Boulons Exterieurs -----"
					      "\nN1 <= %N11! = %BLS!"
					      "\n---"
					      "\nN1 <= %N12!"
					      "\n375e × ((a2 / a1) + (a4 / a3)))"
					      "\n375 × %E! × ((%A2! / %A1!) + (%A4! / %A3!))"
					      "\n")
				      2)))
	((= enc_type 4)
	 (setq list_var	'(("REAL" "enc_br" "Prolongement platine -> br (mm)")
			  ("REAL" "enc_a1" "a1 (mm)")
			  ("REAL" "enc_a2" "a2 (mm)")
			  ("REAL" "enc_a3" "a3 (mm)")
			  ("REAL" "enc_a4" "a4 (mm)")
			  ("REAL" "enc_a11" "a1' (mm)")
			  ("REAL" "enc_a22" "a2' (mm)")))
	 (foreach var  list_var
	   (question var "encastr")
	   (setq donnees (strcat donnees (caddr var) " : " (nb_str (eval (read (cadr var)))) "\n")))
	 (setq enc_s   (+ (* enc_a2 2.0) enc_ea)
	       donnees (strcat donnees "s -> Ecartement entre les deux files interieurs de boulons : " (nb_str enc_s) "\n"))
	 (princ (strcat "s -> Ecartement entre les deux files interieurs de boulons : " (nb_str enc_s) "\n"))
	 (setq N1_2	    (* 375.0 enc_es (* (/ enc_a4 enc_a3) (/ enc_s (+ enc_s enc_a4))))
	       N1_3	    (* 375.0 enc_ess (* (/ enc_a22 enc_a11) (/ enc_s1 (+ enc_s1 enc_a22))))
	       N1_4	    (* 0.5 enc_se enc_eaa enc_s1)
	       N1_Bls_Ext   (list (list Tr N1_2 N1_3 N1_4))
	       print_N1_Ext (edit_msg (list "N11"    Tr
					    "N12"    N1_2
					    "N13"    N1_3
					    "N14"    N1_4
					    "BLS"    print_Tr
					    "E"	     enc_e
					    "ESS"    enc_ess
					    "A3"     enc_a3
					    "A4"     enc_a4
					    "A11"    enc_a11
					    "A22"    enc_a22
					    "S1"     enc_s1
					    "S"	     enc_s)
				      (strcat
					"\n----- Boulons Exterieurs -----"
					"\nN1 <= %N11! = %BLS!"
					"\n---"
					"\nN1 <= %N12!"
					"\n375e × ((a4 / a3) × (s / (s + a4)))"
					"\n375 × %E! × ((%A4! / %A3!) × (%S! / (%S! + %A4!)))"
					"\n---"
					"\nN1 <= %N13!"
					"\n375e × ((a2'/ a1') × (s1 / (s1 + a2')))"
					"\n375 × %ESS! × ((%A22! / %A11!) × (%S1! / (%S1! + %A22!)))"
					"\n---"
					"\nN1 <= %N14!"
					"\n0.5 × Se × ea' × s1"
					"\n0.5 × %SE! × %EAA! × %S1!\n"
					"\n")
				      2)))
	((= enc_type 5)
	 (setq list_var	'(("REAL" "enc_br" "Raid platine -> br (mm)")
			  ("REAL" "enc_lr" "Raid platine -> lr (mm)")
			  ("REAL" "enc_eraid" "Raid platine -> ep (mm)")
			  ("REAL" "enc_a1" "a1 (mm)")
			  ("REAL" "enc_a2" "a2 (mm)")
			  ("REAL" "enc_a3" "a3 (mm)")
			  ("REAL" "enc_a4" "a4 (mm)")
			  ("REAL" "enc_a11" "a1' (mm)")
			  ("REAL" "enc_a22" "a2' (mm)")))
	 (foreach var  list_var
	   (question var "encastr")
	   (setq donnees (strcat donnees (caddr var) " : " (nb_str (eval (read (cadr var)))) "\n")))
	 (setq enc_s   (+ (* enc_a2 2.0) enc_ea)
	       donnees (strcat donnees "s -> Ecartement entre les deux files interieurs de boulons : " (nb_str enc_s) "\n"))
	 (princ (strcat "s -> Ecartement entre les deux files interieurs de boulons : " (nb_str enc_s) "\n"))
	 (setq N1_2	    (* 375.0 enc_e (+ (/ enc_a2 enc_a1) (/ enc_a4 enc_a3)))
	       N1_3	    (* 375.0 enc_ess (* (/ enc_a22 enc_a11) (/ enc_s1 (+ enc_s1 enc_a22))))
	       N1_4	    (* 0.5 enc_se enc_eaa enc_s1)
	       N1_Bls_Ext   (list (list Tr N1_2 N1_3 N1_4))
	       print_N1_Ext (edit_msg (list "N11"    Tr
					    "N12"    N1_2
					    "N13"    N1_3
					    "N14"    N1_4
					    "BLS"    print_Tr
					    "E"	     enc_e
					    "ESS"    enc_ess
					    "A1"     enc_a1
					    "A2"     enc_a2
					    "A3"     enc_a3
					    "A4"     enc_a4
					    "A11"    enc_a11
					    "A22"    enc_a22
					    "S1"     enc_s1)
				      (strcat "\n----- Boulons Exterieurs -----"
					      "\nN1 <= %N11! = %BLS!"
					      "\n---"
					      "\nN1 <= %N12!"
					      "\n375e × ((a2 / a1) + (a4 / a3))"
					      "\n375 × %E! × ((%A2! / %A1!) + (%A4! / %A3!))"
					      "\n---"
					      "\nN1 <= %N13!"
					      "\n375e × ((a2'/ a1') × (s1 / (s1 + a2')))"
					      "\n375 × %ESS! × ((%A22! / %A11!) × (%S1! / (%S1! + %A22!)))"
					      "\n---"
					      "\nN1 <= %N14!"
					      "\n0.5 × Se × ea' × s1"
					      "\n0.5 × %SE! × %EAA! × %S1!\n"
					      "\n")
				      2)))
	((= enc_type 6)
	 (setq list_var	'(("REAL" "enc_br" "Prolongement platine -> br (mm)")
			  ("REAL" "enc_a1" "a1 (mm)")
			  ("REAL" "enc_a2" "a2 (mm)")
			  ("REAL" "enc_a3" "a3 (mm)")
			  ("REAL" "enc_a4" "a4 (mm)")
			  ("REAL" "enc_a11" "a1' (mm)")
			  ("REAL" "enc_a22" "a2' (mm)")
			  ("REAL" "enc_a33" "a3' (mm)")
			  ("REAL" "enc_a44" "a4' (mm)")))
	 (foreach var  list_var
	   (question var "encastr")
	   (setq donnees (strcat donnees (caddr var) " : " (nb_str (eval (read (cadr var)))) "\n")))
	 (setq enc_s   (+ (* enc_a2 2.0) enc_ea)
	       donnees (strcat donnees "s -> Ecartement entre les deux files interieurs de boulons : " (nb_str enc_s) "\n"))
	 (princ (strcat "s -> Ecartement entre les deux files interieurs de boulons : " (nb_str enc_s) "\n"))
	 (setq N1_2	    (* 375.0 enc_ess (* (/ enc_a4 enc_a3) (/ enc_s (+ enc_s enc_a4))))
	       N1_3	    (* 375.0 enc_e (+ (/ enc_a22 enc_a11) (/ enc_a44 enc_a33)))
	       N1_Bls_Ext   (list (list Tr N1_2 N1_3))
	       print_N1_Ext (edit_msg (list "N11"    Tr
					    "N12"    N1_2
					    "N13"    N1_3
					    "BLS"    print_Tr
					    "E"	     enc_e
					    "ESS"    enc_ess
					    "A3"     enc_a3
					    "A4"     enc_a4
					    "A11"    enc_a11
					    "A22"    enc_a22
					    "A33"    enc_a33
					    "A44"    enc_a44
					    "S"	     enc_s)
				      (strcat
					"\n----- Boulons Exterieurs -----"
					"\nN1 <= %N11! = %BLS!"
					"\n---"
					"\nN1 <= %N12!"
					"\n375e × ((a4/ a3) × (s / (s + a4)))"
					"\n375 × %ESS! × ((%A4! / %A3!) × (%S! / (%S! + %A4!)))"
					"\n---"
					"\nN1 <= %N13!"
					"\n375es' × ((a2' / a1') + (a4' / a3'))"
					"\n375 × %ESS! × ((%A22! / %A11!) + (%A44! / %A33!))"
					"\n")
				      2)))
	((= enc_type 7)
	 (setq list_var	'(("REAL" "enc_br" "Raid platine -> br (mm)")
			  ("REAL" "enc_lr" "Raid platine -> lr (mm)")
			  ("REAL" "enc_eraid" "Raid platine -> ep (mm)")
			  ("REAL" "enc_a1" "a1 (mm)")
			  ("REAL" "enc_a2" "a2 (mm)")
			  ("REAL" "enc_a3" "a3 (mm)")
			  ("REAL" "enc_a4" "a4 (mm)")
			  ("REAL" "enc_a11" "a1' (mm)")
			  ("REAL" "enc_a22" "a2' (mm)")
			  ("REAL" "enc_a33" "a3' (mm)")
			  ("REAL" "enc_a44" "a4' (mm)")))
	 (foreach var  list_var
	   (question var "encastr")
	   (setq donnees (strcat donnees (caddr var) " : " (nb_str (eval (read (cadr var)))) "\n")))
	 (setq enc_s   (+ (* enc_a2 2.0) enc_ea)
	       donnees (strcat donnees "s -> Ecartement entre les deux files interieurs de boulons : " (nb_str enc_s) "\n"))
	 (princ (strcat "s -> Ecartement entre les deux files interieurs de boulons : " (nb_str enc_s) "\n"))
	 (setq N1_2	    (* 375.0 enc_e (+ (/ enc_a2 enc_a1) (/ enc_a4 enc_a3)))
	       N1_3	    (* 375.0 enc_e (+ (/ enc_a22 enc_a11) (/ enc_a44 enc_a33)))
	       N1_Bls_Ext   (list (list Tr N1_2 N1_3))
	       print_N1_Ext (edit_msg (list "N11"    Tr
					    "N12"    N1_2
					    "N13"    N1_3
					    "BLS"    print_Tr
					    "E"	     enc_e
					    "ESS"    enc_ess
					    "A1"     enc_a1
					    "A2"     enc_a2
					    "A3"     enc_a3
					    "A4"     enc_a4
					    "A11"    enc_a11
					    "A22"    enc_a22
					    "A33"    enc_a33
					    "A44"    enc_a44
					    "S"	     enc_s)
				      (strcat
					"\n----- Boulons Exterieurs -----"
					"\nN1 <= %N11! = %BLS!"
					"\n---"
					"\nN1 <= %N12!"
					"\n375e × ((a2 / a1) + (a4 / a3))"
					"\n375 × %E! × ((%A2! / %A1!) + (%A4! / %A3!))"
					"\n---"
					"\nN1 <= %N13!"
					"\n375es' × ((a2' / a1') + (a4' / a3'))"
					"\n375 × %ESS! × ((%A22! / %A11!) + (%A44! / %A33!))"
					"\n")
				      2))))
;;;
;;;                                                          
;;; Si boulons exterieurs et 4 files de boulons              
;;;                                                          
;;;
  (if (and (member enc_type '(1 2 4 5 6 7)) (= enc_nf 2))
    (setq N1_7		      (* 300.0 enc_e (* (/ enc_a4 enc_a3) (/ enc_ss (+ enc_ss enc_a4))))
	  N1_8		      (* 300.0 enc_ess (* (/ enc_a44 enc_a33) (/ enc_ss (+ enc_ss enc_a4))))
	  N1_Bls_Ext_4file    (list (list Tr N1_7 N1_8))
	  print_N1_Ext_4files (edit_msg	(list "N11"    Tr
					      "N12"    N1_7
					      "N13"    N1_8
					      "BLS"    print_Tr
					      "E"      enc_e
					      "ESS"    enc_ess
					      "SS"     enc_ss
					      "A3"     enc_a3
					      "A4"     enc_a4
					      "A33"    enc_a33
					      "A44"    enc_a44
					      "S"      enc_s)
					(strcat
					  "\n----- Boulons Exterieurs sur File Exterieur-----"
					  "\nN1 <= %N11! = %BLS!"
					  "\n---"
					  "\nN1 <= %N12!"
					  "\n300e × ((a4 / a3) × (s' / (s' + a4)))"
					  "\n300 × %E! × ((%A4! / %A3!) × (%SS! / ( %SS! + %A4!)))"
					  "\n---"
					  "\nN1 <= %N13!"
					  "\n300es' × ((a4' / a3') × (s' / (s' + a4)))"
					  "\n300 × %ESS! × ((%A44! / %A33!) × (%SS! / ( %SS! + %A4!)))"
					  "\n")
					2)))
;;;
;;;-----------------------------------------------------------------
;;;-----------------------------------------------------------------
;;;                                                                 
;;;     Boulons interieurs                                          
;;;                                                                 
;;;-----------------------------------------------------------------
;;;-----------------------------------------------------------------
;;;
  (cond	((or (= enc_type 1) (= enc_type 2))
	 (setq list_var '(("REAL" "enc_a5" "a5 (mm)") ("REAL" "enc_a6" "a6 (mm)")))
	 (foreach var  list_var
	   (question var "encastr")
	   (setq donnees (strcat donnees (caddr var) " : " (nb_str (eval (read (cadr var)))) "\n")))
	 (setq N1_2	    (* 375.0 enc_e (+ (/ enc_a2 enc_a1) (/ enc_a6 enc_a5)))
	       N1_Bls_Int   (list (list Tr N1_2))
	       print_N1_Int (edit_msg (list "N11"    Tr
					    "N12"    N1_2
					    "BLS"    print_Tr
					    "E"	     enc_e
					    "A1"     enc_a1
					    "A2"     enc_a2
					    "A5"     enc_a5
					    "A6"     enc_a6)
				      (strcat "\n----- Boulons Intérieurs -----"
					      "\nN1 <= %N11! = %BLS!"
					      "\n---"
					      "\nN1 <= %N12!"
					      "\n375e × ((a2 / a1) + (a6 / a5)))"
					      "\n375 × %E! × ((%A2! / %A1!) + (%A6! / %A5!))"
					      "\n")
				      2)))
	((= enc_type 3)
	 (setq list_var	'(("REAL" "enc_a1" "a1 (mm)")
			  ("REAL" "enc_a2" "a2 (mm)")
			  ("REAL" "enc_a5" "a5 (mm)")
			  ("REAL" "enc_a6" "a6 (mm)")))
	 (foreach var  list_var
	   (question var "encastr")
	   (setq donnees (strcat donnees (caddr var) " : " (nb_str (eval (read (cadr var)))) "\n")))
	 (setq enc_s   (+ (* enc_a2 2.0) enc_ea)
	       donnees (strcat donnees "s -> Ecartement entre les deux files interieurs de boulons : " (nb_str enc_s) "\n"))
	 (princ (strcat "s -> Ecartement entre les deux files interieurs de boulons : " (nb_str enc_s) "\n"))
	 (setq N1_2	    (* 375.0 enc_e (+ (/ enc_a2 enc_a1) (/ enc_a6 (* 2.0 enc_a5))))
	       N1_Bls_Int   (list (list Tr N1_2))
	       print_N1_Int (edit_msg (list "N11"    Tr
					    "N12"    N1_2
					    "BLS"    print_Tr
					    "E"	     enc_e
					    "A1"     enc_a1
					    "A2"     enc_a2
					    "A5"     enc_a5
					    "A6"     enc_a6)
				      (strcat
					"\n----- Boulons Intérieurs -----"
					"\nN1 <= %N11! = %BLS!"
					"\n---"
					"\nN1 <= %N12!"
					"\n375e × ((a2 / a1) + (a6 / (2 × a5))))"
					"\n375 × %E! × ((%A2! / %A1!) + (%A6! / (2 × %A5!)))"
					"\n")
				      2)))
	((or (= enc_type 4) (= enc_type 5))
	 (setq list_var	'(("REAL" "enc_a1" "a1 (mm)")
			  ("REAL" "enc_a2" "a2 (mm)")
			  ("REAL" "enc_a5" "a5 (mm)")
			  ("REAL" "enc_a6" "a6 (mm)")))
	 (foreach var  list_var
	   (question var "encastr")
	   (setq donnees (strcat donnees (caddr var) " : " (nb_str (eval (read (cadr var)))) "\n")))
	 (setq N1_2	    (* 375.0 enc_e (+ (/ enc_a2 enc_a1) (/ enc_a6 enc_a5)))
	       N1_3	    (* 375.0 enc_ess (* (/ enc_a22 enc_a11) (/ enc_s1 (+ enc_s1 enc_a22))))
	       N1_4	    (* 0.5 enc_se enc_eaa enc_s1)
	       N1_Bls_Int   (list (list Tr N1_2 N1_3 N1_4))
	       print_N1_Int (edit_msg (list "N11"    Tr
					    "N12"    N1_2
					    "N13"    N1_3
					    "N14"    N1_4
					    "BLS"    print_Tr
					    "E"	     enc_e
					    "EAA"    enc_eaa
					    "ESS"    enc_ess
					    "SE"     enc_se
					    "A1"     enc_a1
					    "A2"     enc_a2
					    "A5"     enc_a5
					    "A6"     enc_a6
					    "A11"    enc_a11
					    "A22"    enc_a22
					    "S1"     enc_s1)
				      (strcat "\n----- Boulons Intérieurs -----"
					      "\nN1 <= %N11! = %BLS!"
					      "\n---"
					      "\nN1 <= %N12!"
					      "\n375e × ((a2 / a1) + (a6 / a5)))"
					      "\n375 × %E! × ((%A2! / %A1!) + (%A6! / %A5!))"
					      "\n---"
					      "\nN1 <= %N13!"
					      "\n375es' × ((a2' / a1') × (s1 / (s1 + a2')))"
					      "\n375 × %ESS! × ((%A22! / %A11!) × (%S1! / (%S1! + %A22!)))"
					      "\n---"
					      "\nN1 <= %N14!"
					      "\n0.5 × Se × ea' × s1"
					      "\n0.5 × %SE! × %EAA! × %S1!"
					      "\n")
				      2)))
	((or (= enc_type 6) (= enc_type 7))
	 (setq list_var	'(("REAL" "enc_a5" "a5 (mm)")
			  ("REAL" "enc_a6" "a6 (mm)")
			  ("REAL" "enc_a55" "a5' (mm)")
			  ("REAL" "enc_a66" "a6' (mm)")))
	 (foreach var  list_var
	   (question var "encastr")
	   (setq donnees (strcat donnees (caddr var) " : " (nb_str (eval (read (cadr var)))) "\n")))
	 (setq N1_2	    (* 375.0 enc_e (+ (/ enc_a2 enc_a1) (/ enc_a6 enc_a5)))
	       N1_3	    (* 375.0 enc_ess (+ (/ enc_a22 enc_a11) (/ enc_a66 enc_a55)))
	       N1_Bls_Int   (list (list Tr N1_2 N1_3))
	       print_N1_Int (edit_msg (list "N11"    Tr
					    "N12"    N1_2
					    "N13"    N1_3
					    "BLS"    print_Tr
					    "E"	     enc_e
					    "ESS"    enc_ess
					    "A1"     enc_a1
					    "A2"     enc_a2
					    "A5"     enc_a5
					    "A6"     enc_a6
					    "A11"    enc_a11
					    "A22"    enc_a22)
				      (strcat
					"\n----- Boulons Intérieurs -----"
					"\nN1 <= %N11! = %BLS!"
					"\n---"
					"\nN1 <= %N12!"
					"\n375e × ((a2 / a1) + (a6 / a5)))"
					"\n375 × %E! × ((%A2! / %A1!) + (%A6! / (2 × %A5!)))"
					"\n---"
					"\nN1 <= %N13!"
					"\n375es' × ((a2' / a1') + (a6' / a5')))"
					"\n375 × %ESS! × ((%A22! / %A11!) + (%A66! / (2 × %A55!)))"
					"\n")
				      2)))
	((= enc_type 8)
	 (setq list_var	'(("REAL" "enc_a1" "a1 (mm)")
			  ("REAL" "enc_a2" "a2 (mm)")
			  ("REAL" "enc_a5" "a5 (mm)")
			  ("REAL" "enc_a6" "a6 (mm)")
			  ("REAL" "enc_a11" "a1' (mm)")
			  ("REAL" "enc_a22" "a2' (mm)")))
	 (foreach var  list_var
	   (question var "encastr")
	   (setq donnees (strcat donnees (caddr var) " : " (nb_str (eval (read (cadr var)))) "\n")))
	 (setq enc_s   (+ (* enc_a2 2.0) enc_ea)
	       donnees (strcat donnees "s -> Ecartement entre les deux files interieurs de boulons : " (nb_str enc_s) "\n"))
	 (princ (strcat "s -> Ecartement entre les deux files interieurs de boulons : " (nb_str enc_s) "\n"))
	 (setq enc_s2	    (car list_s2)
	       list_s2	    (cdr list_s2)
	       N1_2	    (* 375.0 enc_e (+ (/ enc_a2 enc_a1) (/ enc_a6 (* 2.0 enc_a5))))
	       N1_3	    (* 375.0 enc_ess (* (/ enc_a22 enc_a11) (/ enc_s2 (+ enc_s2 enc_a22))))
	       N1_4	    (* 0.5 enc_se enc_eaa enc_s2)
	       N1_Bls_Int   (list (list Tr N1_2 N1_3 N1_4))
	       print_N1_Int (edit_msg (list "N11"    Tr
					    "N12"    N1_2
					    "N13"    N1_3
					    "N14"    N1_4
					    "BLS"    print_Tr
					    "E"	     enc_e
					    "EAA"    enc_eaa
					    "ESS"    enc_ess
					    "SE"     enc_se
					    "A1"     enc_a1
					    "A2"     enc_a2
					    "A5"     enc_a5
					    "A6"     enc_a6
					    "A11"    enc_a11
					    "A22"    enc_a22
					    "S2"     enc_s2)
				      (strcat
					"\n----- Boulons Intérieurs -----"
					"\nN1 <= %N11! = %BLS!"
					"\n---"
					"\nN1 <= %N12!"
					"\n375e × ((a2 / a1) + (a6 / (2 × a5))))"
					"\n375 × %E! × ((%A2! / %A1!) + (%A6! / (2 × %A5!)))"
					"\n---"
					"\nN1 <= %N13!"
					"\n375es' × ((a2' / a1') × (s2 / (s2 + a2')))"
					"\n375 × %ESS! × ((%A22! / %A11!) × (%S2! / (%S2! + %A22!)))"
					"\n---"
					"\nN1 <= %N14!"
					"\n0.5 × Se × ea' × s2"
					"\n0.5 × %SE! × %EAA! × %S2!"
					"\n")
				      2)))
	((= enc_type 9)
	 (setq list_var	'(("REAL" "enc_a1" "a1 (mm)")
			  ("REAL" "enc_a2" "a2 (mm)")
			  ("REAL" "enc_a5" "a5 (mm)")
			  ("REAL" "enc_a6" "a6 (mm)")
			  ("REAL" "enc_a11" "a1' (mm)")
			  ("REAL" "enc_a22" "a2' (mm)")
			  ("REAL" "enc_a55" "a5' (mm)")
			  ("REAL" "enc_a66" "a6' (mm)")))
	 (foreach var  list_var
	   (question var "encastr")
	   (setq donnees (strcat donnees (caddr var) " : " (nb_str (eval (read (cadr var)))) "\n")))
	 (setq enc_s   (+ (* enc_a2 2.0) enc_ea)
	       donnees (strcat donnees "s -> Ecartement entre les deux files interieurs de boulons : " (nb_str enc_s) "\n"))
	 (princ (strcat "s -> Ecartement entre les deux files interieurs de boulons : " (nb_str enc_s) "\n"))
	 (setq N1_1	    Tr
	       N1_2	    (* 375.0 enc_e (+ (/ enc_a2 enc_a1) (/ enc_a6 (* 2.0 enc_a5))))
	       N1_3	    (* 375.0 enc_ess (+ (/ enc_a22 enc_a11) (/ enc_a66 (* 2.0 enc_a55))))
	       N1_Bls_Int   (list (list Tr N1_2 N1_3))
	       print_N1_Int (edit_msg (list "N11"    Tr
					    "N12"    N1_2
					    "N13"    N1_3
					    "BLS"    print_Tr
					    "E"	     enc_e
					    "ESS"    enc_ess
					    "A1"     enc_a1
					    "A2"     enc_a2
					    "A5"     enc_a5
					    "A6"     enc_a6
					    "A11"    enc_a11
					    "A22"    enc_a22
					    "A55"    enc_a55
					    "A66"    enc_a66)
				      (strcat
					"\n----- Boulons Intérieurs -----"
					"\nN1 <= %N11! = %BLS!"
					"\n---"
					"\nN1 <= %N12!"
					"\n375e × ((a2 / a1) + (a6 / (2 × a5))))"
					"\n375 × %E! × ((%A2! / %A1!) + (%A6! / (2 × %A5!)))"
					"\n---"
					"\nN1 <= %N13!"
					"\n375es' × ((a2' / a1') + (a6' / (2 × a5'))))"
					"\n375 × %ESS! × ((%A22! / %A11!) + (%A66! / (2 × %A55!)))"
					"\n")
				      2))))
;;;
;;;                                                          
;;; Si boulons interieurs et 4 files de boulons              
;;;                                                          
;;;
  (if (= enc_nf 2)
    (setq N1_7		      (* 300.0 enc_e (* (/ enc_a6 enc_a5) (/ enc_ss (+ enc_ss enc_a6))))
	  N1_8		      (* 300.0 enc_ess (* (/ enc_a66 enc_a55) (/ enc_ss (+ enc_ss enc_a6))))
	  N1_Bls_Int_4file    (list (list Tr N1_7 N1_8))
	  print_N1_Int_4files (edit_msg	(list "N11"    Tr
					      "N12"    N1_7
					      "N13"    N1_8
					      "BLS"    print_Tr
					      "E"      enc_e
					      "ESS"    enc_ess
					      "SS"     enc_ss
					      "A5"     enc_a5
					      "A6"     enc_a6
					      "A55"    enc_a55
					      "A66"    enc_a66
					      "S"      enc_s)
					(strcat
					  "\n----- Boulons Interieurs sur File Exterieur-----"
					  "\nN1 <= %N11! = %BLS!"
					  "\n---"
					  "\nN1 <= %N12!"
					  "\n300e × ((a6 / a5) × (s' / (s' + a6)))"
					  "\n300 × %E! × ((%A6! / %A5!) × (%SS! / ( %SS! + %A6!)))"
					  "\n---"
					  "\nN1 <= %N13!"
					  "\n300es' × ((a6' / a5') × (s' / (s' + a6)))"
					  "\n300 × %ESS! × ((%A66! / %A55!) × (%SS! / ( %SS! + %A6!)))"
					  "\n")
					2)))
;;;
;;;-----------------------------------------------------------------
;;;-----------------------------------------------------------------
;;;                                                                 
;;;     Boulons centraux                                            
;;;                                                                 
;;;-----------------------------------------------------------------
;;;-----------------------------------------------------------------
;;;
  (setq	list_s2_cent  (cdr list_s2)
	print_N1_Cent (edit_msg	(list "N11" Tr "BLS" print_Tr)
				(strcat "\n----- Boulons Centraux -----" "\nN1 <= %N11! = %BLS!" "\n")
				2))
  (cond	((>= enc_type 4)
	 (setq N1_Bls_Cent
		(mapcar	'(lambda (enc_s2)
			   (setq N1_2 (* 375.0 enc_e (* (/ enc_a2 enc_a1) (/ enc_s2 (+ enc_s2 enc_a2))))
				 N1_3 (* 375.0 enc_ess (* (/ enc_a22 enc_a11) (/ enc_s2 (+ enc_s2 enc_a22))))
				 N1_4 (* 0.5 enc_se enc_ea enc_s2)
				 N1_5 (* 0.5 enc_se enc_eaa enc_s2))
			   (list Tr N1_2 N1_3 N1_4 N1_5))
			list_s2_cent))
	 (mapcar '(lambda (enc_s2)
		    (setq N1_2		(* 375.0 enc_e (* (/ enc_a2 enc_a1) (/ enc_s2 (+ enc_s2 enc_a2))))
			  N1_3		(* 375.0 enc_ess (* (/ enc_a22 enc_a11) (/ enc_s2 (+ enc_s2 enc_a22))))
			  N1_4		(* 0.5 enc_se enc_ea enc_s2)
			  N1_5		(* 0.5 enc_se enc_eaa enc_s2)
			  print_N1_Cent	(strcat	print_N1_Cent
						(edit_msg (list	"N12"	 N1_2
								"N13"	 N1_3
								"N14"	 N1_4
								"N15"	 N1_5
								"E"	 enc_e
								"EA"	 enc_ea
								"EAA"	 enc_eaa
								"ESS"	 enc_ess
								"SE"	 enc_se
								"A1"	 enc_a1
								"A2"	 enc_a2
								"A11"	 enc_a11
								"A22"	 enc_a22
								"S2"	 enc_s2)
							  (strcat
							    "\n---- S2 = %S2! ----"
							    "\nN1 <= %N12!"
							    "\n375e × ((a2 / a1) × (s2 / (s2 + a2)))"
							    "\n375 × %E! × ((%A2! / %A1!) × (%S2! / (%S2! + %A2!)))"
							    "\n---"
							    "\nN1 <= %N13!"
							    "\n375es' × ((a2' / a1') × (s2 / (s2 + a2')))"
							    "\n375 × %ESS! × ((%A22! / %A11!) × (%S2! / (%S2! + %A22!)))"
							    "\n---"
							    "\nN1 <= %N14!"
							    "\n0.5 × Se × ea × s2"
							    "\n0.5 × %SE! × %EA! × %S2!"
							    "\n---"
							    "\nN1 <= %N15!"
							    "\n0.5 × Se × ea' × s2"
							    "\n0.5 × %SE! × %EAA! × %S2!\n")
							  2))))
		 (vl-sort (remove_doubles (mapcar '(lambda (x) (arr 1 x)) list_s2_cent)) '>)))
	((< enc_type 4)
	 (setq N1_Bls_Cent
		(mapcar	'(lambda (enc_s2)
			   (setq N1_2 (* 375.0 enc_e (* (/ enc_a2 enc_a1) (/ enc_s2 (+ enc_s2 enc_a2))))
				 N1_3 (* 0.5 enc_se enc_ea enc_s2))
			   (list Tr N1_2 N1_3))
			list_s2_cent))
	 (mapcar '(lambda (enc_s2)
		    (setq N1_2		(* 375.0 enc_e (* (/ enc_a2 enc_a1) (/ enc_s2 (+ enc_s2 enc_a2))))
			  N1_3		(* 0.5 enc_se enc_ea enc_s2)
			  print_N1_Cent	(strcat	print_N1_Cent
						(edit_msg (list	"N12"	N1_2
								"N13"	N1_3
								"E"	enc_e
								"EA"	enc_ea
								"SE"	enc_se
								"A1"	enc_a1
								"A2"	enc_a2
								"S2"	enc_s2)
							  (strcat
							    "\n---- S2 = %S2! ----"
							    "\nN1 <= %N12!"
							    "\n375e × ((a2 / a1) × (s2 / (s2 + a2)))"
							    "\n375 × %E! × ((%A2! / %A1!) × (%S2! / (%S2! + %A2!)))"
							    "\n---"
							    "\nN1 <= %N13!"
							    "\n0.5 × Se × ea × s2"
							    "\n0.5 × %SE! × %EA! × %S2!\n")
							  2))))
		 (vl-sort (remove_doubles (mapcar '(lambda (x) (arr 1 x)) list_s2_cent)) '>))))
  (setq	list_N1	 (append N1_Bls_Ext N1_Bls_Ext_4file N1_Bls_Int N1_Bls_Int_4file N1_Bls_Cent)
	print_N1 (strcat print_N1_Ext print_N1_Ext_4files print_N1_Int print_N1_Int_4files "\n" print_N1_Cent "\n"))
;;;
;;;-----------------------------------------------------------------
;;;-----------------------------------------------------------------
;;;                                                                 
;;;     Liste des efforts dans les boulons                          
;;;                                                                 
;;;-----------------------------------------------------------------
;;;-----------------------------------------------------------------
;;;
  (cond	((and (member enc_type '(1 2 4 5 6 7)) (= enc_nf 2))
	 (setq list_dist_bls
		(append	(list (car list_dist_bls))
			(list (car list_dist_bls))
			(list (cadr list_dist_bls))
			(list (cadr list_dist_bls))
			(cddr list_dist_bls))))
	((= enc_nf 2)
	 (setq list_dist_bls (append (list (car list_dist_bls)) (list (car list_dist_bls)) (cdr list_dist_bls)))))
  (setq	Sm_Mo 0
	list_eff_bls
	 (mapcar '(lambda (dist N1)
		    (setq N1	(apply 'min N1)
			  carac	(list (arr 0 N1) (arr 0 (* 2.0 N1)) dist (arr 0 (* dist (* 2.0 N1))) (- M_res Sm_Mo))
			  Sm_Mo	(+ Sm_Mo (* dist (* 2.0 N1))))
		    carac)
		 list_dist_bls
		 list_N1))
;;;
;;;-----------------------------------------------------------------
;;;-----------------------------------------------------------------
;;;                                                                 
;;;     Verification du Moment Resistant                            
;;;                                                                 
;;;-----------------------------------------------------------------
;;;-----------------------------------------------------------------
;;;
  (setq	tableau		 (list (car list_eff_bls))
	fin		 t
	tableau_erreur	 ""
	tableau_test_inf ""
	M_res_possible	 (apply '+ (mapcar '(lambda (x) (cadddr x)) list_eff_bls))
	M_ass		 (cadddr (car list_eff_bls)))
  (foreach eff_carac  (cdr list_eff_bls)
    (cond ((> (last eff_carac) (cadddr eff_carac))
	   (setq tableau (append tableau (list eff_carac))
		 M_ass	 (+ M_ass (cadddr eff_carac))))
	  (fin
	   (setq tableau (append tableau
				 (list (list (arr 0 (/ (/ (last eff_carac) (caddr eff_carac)) 2.0))
					     (arr 0 (/ (last eff_carac) (caddr eff_carac)))
					     (caddr eff_carac)
					     (arr 0 (last eff_carac))
					     "reste")))
		 fin	 nil
		 M_ass	 (arr 0 (+ M_ass (last eff_carac)))))))
  (if fin
    (setq tableau_erreur   "\nNb de boulons insuffisants"
	  tableau_test_inf (strcat " < " (rtos M_res 2) " daN.m")))
  (setq	Nc 0
	print_reste ""
	tableau_aff
	 (mapcar '(lambda (x)
		    (setq Nc (+ Nc (cadr x)))
		    (if	(= (last x) "reste")
		      (setq print_reste "\n--- Reste ---"))
		    (strcat print_reste
			    "\n2 × "
			    (rtos (arr 1 (car x)) 2)
			    " = "
			    (rtos (arr 1 (cadr x)) 2)
			    " // "
			    (rtos (caddr x) 2)
			    " // "
			    (rtos (arr 1 (cadr x)) 2)
			    " × "
			    (rtos (caddr x) 2)
			    " = "
			    (rtos (arr 1 (cadddr x)) 2)))
		 tableau)
	tableau_aff
	 (strcat (apply 'strcat tableau_aff) tableau_erreur)
	tableau_aff
	 (strcat "\nCapacité maxi de l'assemblage = "
		 (rtos M_res_possible 2)
		 " daN.m"
		 "\nMoment resistant = "
		 (rtos M_res 2)
		 " daN.m"
		 "\n"
		 "\n2Ni // d // Mres\n---------------------"
		 tableau_aff
		 "\n---------------------"
		 "\nCompression = "
		 (rtos Nc 2)
		 " daN / Moment = "
		 (rtos M_ass 2)
		 " daN.m"
		 tableau_test_inf))
;;;
;;;-----------------------------------------------------------------
;;;-----------------------------------------------------------------
;;;                                                                 
;;;     Verification de l'effort admissible en compression          
;;;                                                                 
;;;-----------------------------------------------------------------
;;;-----------------------------------------------------------------
;;;
  (if (<= Nc Nc_adm)
    (setq signe	"<="
	  comp	"inferieur")
    (setq signe	">"
	  comp	"SUPERIEUR"))
  (setq	print_verif_comp
	 (edit_msg (list "NCADM" Nc_adm "NC" Nc "SGN" signe "CP" comp)
		   (strcat "L'effort de compression est %CP! à l'effort admissible"
			   "\n---------------------"
			   "\nNc_adm = %NCADM! daN"
			   "\n%NC! %SGN! Nc_adm")
		   1))
;;;
;;;--------------------------------------------------------------------
;;;--------------------------------------------------------------------
;;;                                                                    
;;;     Verification de l'effort admissible en compression sur l'âme   
;;;     si enc_type > 3                                                
;;;                                                                    
;;;--------------------------------------------------------------------
;;;--------------------------------------------------------------------
;;;
  (if (> enc_type 3)
    (progn (if (= enc_hprf "O")
	     (setq Av		      (* (- enc_hh (* 2.0 enc_ess)) enc_eaa)
		   print_verif_ame_fx (edit_msg	(list "HH" enc_hh "ESS" enc_ess "EAA" enc_eaa)
						"[ (%HH! - 2 × %ESS!) × %EAA! = (h' - 2 × es') × ea' ]"
						2))
	     (setq Av		      (+ (- enc_Ac (* 2.0 enc_lp enc_ess)) (* enc_ess (+ enc_eaa (* 2.0 enc_r))))
		   print_verif_ame_fx (edit_msg
					(list "HH" enc_hh "ESS" enc_ess "EAA" enc_eaa "BB" enc_lp "R" enc_r "AC" enc_Ac)
					(strcat	"[ Ac - 2 × b' × es' + (ea' + 2 × r) × es' ]"
						"\n[ %AC! - 2 × %BB! × %ESS! + (%EAA! + 2 × %R!) × %ESS! ]")
					1)))
	   (setq Vr (* 0.47 Av enc_se))
	   (if (<= Nc Vr)
	     (setq signe "<="
		   comp	 "inferieur")
	     (setq signe ">"
		   comp	 "SUPERIEUR"))
	   (setq print_verif_ame_comp
		  (edit_msg (list "VR"	    Vr
				  "AV"	    Av
				  "NC"	    Nc
				  "SE"	    enc_se
				  "AC"	    enc_Ac
				  "SGN"	    signe
				  "CP"	    comp
				  "MSG"	    print_verif_ame_fx)
			    (strcat
			      "L'effort de cisaillement dans l'âme est %CP! à l'effort admissible"
			      "\n---------------------"
			      "\nAv = %AV! mm²"
			      "\n%MSG!"
			      "\nVr = %VR! daN"
			      "\n[ 0.47 × %AV! × %SE! = 0.47 × Av × Se ]"
			      "\n%NC! %SGN! Vr")
			    2)))
    (setq print_verif_ame_comp ""))
;;;
;;;-----------------------------------------------------------------
;;;-----------------------------------------------------------------
;;;                                                                 
;;;     Verifie les conditions de pas, de pince                     
;;;     et d'épaisseur de platine                                   
;;;                                                                 
;;;-----------------------------------------------------------------
;;;-----------------------------------------------------------------
;;;
  (setq	enc_dtr	    (+ enc_jtr enc_Db)
	pos_e	    (vl-sort-i (list enc_e enc_ess) '<)
	enc_e-min   (min enc_e enc_ess)
	enc_e-max   (max enc_e enc_ess)
	e_max	    (* 2.0 enc_Db)
	pince_a-min (/ (- enc_lplatine enc_s) 2.0)
	pince_a-max pince_a-min)
  (if (member enc_type '(1 2 4 5 6 7))
    (setq pince_a2    (/ (- enc_br enc_a4))
	  pince_a-max (max pince_a2 pince_a-min)
	  pince_a-min (min pince_a2 pince_a-min)))
  (if enc_s1
    (setq list_s (cons enc_s (cons enc_s1 list_s2)))
    (setq list_s (cons enc_s list_s2)))
  (setq enc_s-max (apply 'max (vl-sort (remove_doubles (mapcar '(lambda (x) (arr 1 x)) list_s)) '>)))
  (setq	comp_s	(* 15.0 enc_e-min)
	comp_-a	(* 1.5 enc_dtr)
	comp_+a	(* 6.0 enc_e-min))
  (if (<= enc_s-max comp_s)
    (setq sgn_s	"<="
	  valid_s "")
    (setq sgn_s	  ">"
	  valid_s "INCORRECTE"))
  (if (>= pince_a-min comp_-a)
    (setq sgn_-a ">="
	  valid_-a "")
    (setq sgn_-a   "<"
	  valid_-a "INCORRECTE"))
  (if (<= pince_a-max comp_+a)
    (setq sgn_+a "<="
	  valid_+a "")
    (setq sgn_+a   ">"
	  valid_+a "INCORRECTE"))
  (if (<= enc_e e_max)
    (setq sgn_e	"<="
	  valid_e "")
    (setq sgn_e	  ">"
	  valid_e "INCORRECTE"))
  (setq	disp_valid	  (if (zerop (strlen (strcat valid_s valid_-a valid_+a valid_e)))
			    (setq ng1 ""
				  ng2 "")
			    (setq ng1 "NE "
				  ng2 "PAS "))
	print_e_min	  (nth (car pos_e) '("e" "es'"))
	print_e_max	  (nth (last pos_e) '("e" "es'"))
	print_disposition (edit_msg (list "NG1"	     ng1
					  "NG2"	     ng2
					  "EMIN"     print_e_min
					  "EMAX"     print_e_max
					  "EMN"	     enc_e-min
					  "EMX"	     enc_e-max
					  "S"	     enc_s-max
					  "E"	     enc_e
					  "D"	     enc_Db
					  "EX"	     e_max
					  "SGN_E"    sgn_e
					  "VAL_E"    valid_e
					  "SGN_S"    sgn_s
					  "VAL_S"    valid_s
					  "COMP_S"   comp_s
					  "AMN"	     pince_a-min
					  "AMX"	     pince_a-max
					  "SGN_-A"   sgn_-a
					  "SGN_+A"   sgn_+a
					  "COMP_-A"  comp_-a
					  "COMP_+A"  comp_+a
					  "VAL_-A"   valid_-a
					  "VAL_+A"   valid_+a
					  "DTR"	     enc_dtr)
				    (strcat
				      "L'assemblage %NG1!verifie %NG2!les dispositions constructives"
				      "\n---------------------"
				      "\ne = %E!"
				      "\ne %SGN_E! %EX! [ 2 × %D! = 2 × Db ] %VAL_E!"
				      "\n---------"
				      "\n%EMIN! = %EMN! / %EMAX!  = %EMX!"
				      "\n%EMIN! < %EMAX!"
				      "\n---------"
				      "\ns = %S!"
				      "\ns %SGN_S! %COMP_S! [ 15 × %EMN! = 15 × %EMIN! ] %VAL_S!"
				      "\n---------"
				      "\na_min = %AMN! / a_max = %AMX!"
				      "\na_min %SGN_-A! %COMP_-A! [ 1.5 × %DTR! = 1.5 × Dtr ] %VAL_-A!"
				      "\na_max %SGN_+A! %COMP_+A! [ 6 × %EMN! = 6 × %EMIN! ] %VAL_+A!")
				    2))
;;;
;;;-----------------------------------------------------------------
;;;-----------------------------------------------------------------
;;;                                                                 
;;;     Verifie le raidisseur de la platine                         
;;;                                                                 
;;;-----------------------------------------------------------------
;;;-----------------------------------------------------------------
;;;
  (if (member enc_type '(2 5 7))
    (progn (setq br_min	(+ enc_a4 (* 1.5 enc_dtr))
		 lr_min	(* 2.0 enc_br))
	   (if (>= enc_br br_min)
	     (setq sgn_br ">="
		   raid_valid_br "")
	     (setq sgn_br	 "<"
		   raid_valid_br "INCORRECTE"))
	   (if (>= enc_lr lr_min)
	     (setq sgn_lr ">="
		   raid_valid_lr "")
	     (setq sgn_lr	 "<"
		   raid_valid_lr "INCORRECTE"))
	   (if (>= enc_eraid enc_ea)
	     (setq sgn_ep ">="
		   raid_valid_ep "")
	     (setq sgn_ep	 "<"
		   raid_valid_ep "INCORRECTE"))
	   (setq raid_valid	  (if (zerop (strlen (strcat raid_valid_br raid_valid_lr raid_valid_ep)))
				    "correctes"
				    "INCORRECTES")
		 print_verif_raid (edit_msg (list "VALID"      raid_valid
						  "VALID_BR"   raid_valid_br
						  "VALID_LR"   raid_valid_lr
						  "VALID_EP"   raid_valid_ep
						  "BR"	       enc_br
						  "BR_MIN"     br_min
						  "A4"	       enc_a4
						  "DTR"	       enc_dtr
						  "SGN_BR"     sgn_br
						  "LR"	       enc_lr
						  "LR_MIN"     lr_min
						  "SGN_LR"     sgn_lr
						  "EP_RAID"    enc_eraid
						  "SGN_EP"     sgn_ep
						  "EA"	       enc_ea)
					    (strcat
					      "Les dimensions du raidisseur sont %VALID!"
					      "\n---------------------"
					      "\nbr = %BR!"
					      "\nbr %SGN_BR! %BR_MIN! [ %A4! + 1.5 × %DTR! = a4 + 1.5 × Dtr] %VALID_BR!"
					      "\n---------"
					      "\nlr = %LR!"
					      "\nlr %SGN_LR! %LR_MIN! [ 2 × %BR! = 2 × br] %VALID_LR!"
					      "\n---------"
					      "\nep_raid = %EP_RAID!"
					      "\nep_raid %SGN_EP! %EA! [ %EA% = Epaisseur âme poutre] %VALID_EP!")
					    2)))
    (setq print_verif_raid ""))
;;;
;;;--------------------------------------------------------------------
;;;--------------------------------------------------------------------
;;;                                                                    
;;;     Affichage du résultat                                          
;;;                                                                    
;;;--------------------------------------------------------------------
;;;--------------------------------------------------------------------
;;;
  (setq	resultat (strcat entete		      donnees
			 "\n"		      "----------------- Resultats -----------------"
			 "\n\n"		      print_Bls_carac
			 "\n\n"		      print_condition
			 "\n\n"		      print_disposition
			 "\n\n"		      print_condition_verif_N
			 "\n\n"		      print_resit_effort_T
			 "\n\n"		      print_M_res
			 "\n\n"		      print_X
			 "\n\n"		      print_Ac_compression
			 "\n\n"		      print_N1
			 "\n\n"		      tableau_aff
			 "\n\n\n\n"	      print_verif_comp
			 "\n\n"		      print_verif_ame_comp
			 "\n\n"		      print_verif_raid
			 "\n\n"))
  (princ resultat)
;;;
;;;--------------------------------------------------------------------
;;;--------------------------------------------------------------------
;;;                                                                    
;;;     Enregistrer les resultats dans un fichier                      
;;;                                                                    
;;;--------------------------------------------------------------------
;;;--------------------------------------------------------------------
;;;
  (setq list_fichier '("O-N" "enc_fichier" "Voulez vous enregistrer les données dans un fichier"))
  (question list_fichier "encastr")
  (cond	((= enc_fichier "O")
	 (setq fichier (getfiled "Emplacement du fichier" (strcat "Encastrement - Type " (nb_str enc_type)) "txt" 1))
	 (if (setq flag (open fichier "w"))
	   (progn (write-line resultat flag) (close flag)))))
  (princ))

 ;|«Visual LISP© Format Options»
(120 2 40 0 nil "Fin de " 100 9 0 0 2 T nil nil T)
;*** NE PAS AJOUTER de texte au-dessous du commentaire! ***|;
