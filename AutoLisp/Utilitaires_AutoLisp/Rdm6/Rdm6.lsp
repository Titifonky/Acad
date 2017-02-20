(defun c:rdm6 (/)
  (setq	mdl_fichier  "
   RDM - Ossatures
   Calcul des Structures par la Méthode des Éléments Finis

   Version  - 6.17 - 29 mars 2011

   Utilisateur : SEDACH - Place de l'Expansion - ONNAING ( Nord )

$debut du fichier
$version
6.17
$SI unites
$nom du fichier
%TXT_NOM_FICHIER%
$date
%TXT_DATE%
$heure
%TXT_HEURE%
$ossature
spatiale
$noeuds ( %NB_NOEUDS% )
%TXT_NOEUDS%   0
$poutres ( %NB_POUTRES% )
%TXT_POUTRES%   0
$sections
%TXT_SECTIONS%0
$materiaux
%TXT_MATERIAUX%0
$gpesanteur
10.000
$modes propres
nombre 1
methode sous_espace
precision 1.00000E-02
decalage_spectral 0.00000E+00
////
$maillage
20
$fin du fichier
"	mdl_noeud    "   %NO_NOEUD%  %X%  %Y%  %Z%"
	mdl_poutre   "   %NO_POUTRE% RIRI     %NO_NOEUD1%    %NO_NOEUD2%  %DX% %DY% %DZ% %NO_SECTION% %NO_SECTION%"
	mdl_section  "%NO_SECTION%
TYPE QUELCONQUE
NOM *
DESIGNATION *
LOGO 0
AIRE  0.00000000000E+00
IYY  0.00000000000E+00
IZZ  0.00000000000E+00
alpha  0.00000000000E+00
WPY  0.00000000000E+00
WPZ  0.00000000000E+00
TORSION  0.00000000000E+00
KYY  1.0000000
KZZ  1.0000000
IWW  0.00000000000E+00
YCISAILLEMENT  0.00000000000E+00
ZCISAILLEMENT  0.00000000000E+00
BTY  0.00000000000E+00
BTZ  0.00000000000E+00
BTW  0.00000000000E+00
///"	mdl_materiau " %NO_MATERIAU%
NOM S235
MOD  2.100E+11
POI 0.2850
MAS 7850.00
DIL  1.2500E-05
LIM  2.350E+08
///"
  )
  (defun _calque (ent) (cdr (assoc 8 (entget ent))))
  (defun _type (ent) (cdr (assoc 0 (entget ent))))
  (defun pt_POLYLINE (ent / elst vtx closed lst)
    (setq vtx	 (entnext ent)
	  closed (cdr (assoc 70 (entget ent)))
    )
    (while (/= (cdr (assoc 0 (setq elst (entget vtx)))) "SEQEND")
      (setq lst	(cons (cdr (assoc 10 elst)) lst)
	    vtx	(entnext vtx)
      )
    )
    (setq lst (reverse lst))
    (if	(= closed 9)
      (append lst (list (car lst)))
      lst
    )
  )
  (defun pt_LWPOLYLINE (ent / elst elev closed lst)
    (setq elst	 (entget ent)
	  elev	 (cdr (assoc 38 elst))
	  closed (cdr (assoc 70 elst))
	  lst	 (apply	'append
			(mapcar	(function (lambda (pair)
					    (if	(= 10 (car pair))
					      (list (trans (list (cadr pair) (caddr pair) elev) ent 0))
					    )
					  )
				)
				elst
			)
		 )
    )
    (if	(= closed 1)
      (append lst (list (car lst)))
      lst
    )
  )
  (defun pt_LINE (ent / pt1 pt2)
    (setq elst (entget ent)
	  pt1  (list (trans (cdr (assoc 10 elst)) ent 0))
	  pt2  (list (trans (cdr (assoc 11 elst)) ent 0))
    )
    (append pt1 pt2)
  )
  (defun round (n)
    (fix (+ n
	    (if	(minusp n)
	      -0.5
	      0.5
	    )
	 )
    )
  )
  (defun arrondi (lst) (mapcar '(lambda (x) (mapcar 'round x)) lst))
  ;; Converti une liste '(a b c) -> '((a b) (b c))
  (defun Ent_En_Poutre (lst)
    (mapcar '(lambda (x y) (append (list x) (list y))) (butlast lst) (cdr lst))
  )
  ;; Renvoi le vecteur unitaire ou par defaut si nil
  (defun VunitOrDefaut (vect defaut / tmp)
    (setq tmp (vunit vect))
    (if	tmp
      tmp
      defaut
    )
  )
  ;; Renvoi le vecteur de direction pour le profil
  ;; un vecteur perpendiculaire à la poutre et horizontal
  (defun vect_direction	(x / vd vv vc)
    (setq vd (vect (car x) (cadr x))
	  vv (VunitOrDefaut (list '0.0 '0.0 (caddr vd)) '(0.0 0.0 1.0))
	  vc (VunitOrDefaut (v^v vd vv) '(1.0 0.0 0.0))
    )
  )
  ;; Tri des points selon x y z
  (defun tri_pts (lst)
    (vl-sort (vl-sort (vl-sort lst '(lambda (x y) (< (caddr x) (caddr y))))
		      '(lambda (x y) (< (cadr x) (cadr y)))
	     )
	     '(lambda (x y) (< (car x) (car y)))
    )
  )
  ;; Edition de chaine de caractere en masse
  (defun str_edit (key val string)
    (mapcar '(lambda (k v)
	       (setq string (str_replace string (strcat "%" (strcase (vl-symbol-name k)) "%") v))
	     )
	    key
	    val
    )
    string
  )
  ;; Date sous forme de liste
  (defun heure (/ yr mo day hh mm sec rst)
    (setq cdate	(getvar "CDATE")
	  yr	(fix (/ cdate 10000.0))
	  cdate	(- cdate (* 10000.0 yr))
	  mo	(fix (/ cdate 100.0))
	  cdate	(- cdate (* 100.0 mo))
	  day	(fix cdate)
	  rst	(- cdate day)
	  hh	(fix (* rst 100))
	  rst	(- rst (/ hh 100.0))
	  mm	(fix (* rst 10000))
	  sec	(* 1000000.0 (- rst (/ mm 10000.0)))
    )
    (strcat (itoa (fix hh)) "/" (itoa (fix mm)) "/" (itoa (fix sec)))
  )
  (defun date (/ yr mo day hh mm sec rst)
    (setq cdate	(getvar "CDATE")
	  yr	(fix (/ cdate 10000.0))
	  cdate	(- cdate (* 10000.0 yr))
	  mo	(fix (/ cdate 100.0))
	  cdate	(- cdate (* 100.0 mo))
	  day	(fix cdate)
    )
    (strcat (itoa (fix day)) "/" (itoa (fix mo)) "/" (itoa (fix yr)))
  )
;;;==========================================================;;;
;;;                                                          ;;;
;;;                        Main                              ;;;
;;;                                                          ;;;
;;;==========================================================;;;
  (and (setq fichier (getfiled "Emplacement du fichier" "" "por" 1)
	     flag    (open fichier "w")
       )
       (princ "Selectionnez la structure à exporter :")
       (setq list_ent (SelEnList (ssget '((0 . "LWPOLYLINE,LINE,POLYLINE")))))
  )
  (setq	i_pt 0
	i_ptre 0
	i_sec 0
	points nil
	poutres	nil
	sections nil
	directions nil
  )
  (setq	poutres		(apply 'append
			       (mapcar '(lambda	(e / tpe c lst lste_ptre)
					  (setq	tpe	   (_type e)
						clque	   (_calque e)
						lst	   (arrondi (cond ((= tpe "LWPOLYLINE") (pt_LWPOLYLINE e))
									  ((= tpe "LINE") (pt_LINE e))
									  ((= tpe "POLYLINE") (pt_POLYLINE e))
									  (t nil)
								    )
							   )
						points	   (append lst points)
						lste_ptre  (Ent_En_Poutre lst)
						sections   (append (repeat_n clque (length lste_ptre)) sections)
						directions (append (mapcar 'vect_direction lste_ptre) directions)
					  )
					  lste_ptre
					)
				       list_ent
			       )
			)
	;; on trie les points
	points		(tri_pts (remove_doubles points))
	;; on ajoute le no du point
	points		(mapcar '(lambda (pt) (conslst (setq i_pt (1+ i_pt)) pt)) points)
	;; on remplace les intitules des sections pas un no
	sections_unique	(mapcar	'(lambda (x)
				   (setq i_sec (1+ i_sec))
				   (setq sections (subst i_sec x sections))
				   (itoa i_sec)
				 )
				(remove_doubles sections)
			)
	sections	(mapcar 'itoa sections)
  )
  ;; on remplace les coordonnées des point par leur numero
  (foreach pt points (setq poutres (mapcar '(lambda (ptr) (subst (car pt) (cadr pt) ptr)) poutres)))
  ;; on ajoute un no à chaque poutre
  (setq	poutres	      (mapcar '(lambda (ptr) (cons (setq i_ptre (1+ i_ptre)) ptr)) poutres)
	txt_noeuds    (apply
			'strcat
			(mapcar
			  '(lambda (pt)
			     (strcat (str_edit '(NO_NOEUD X Y Z)
					       (cons (itoa (car pt))
						     (mapcar '(lambda (x) (rtos (* x 0.001) 1 7)) (cadr pt))
					       )
					       mdl_noeud
				     )
				     "\n"
			     )
			   )
			  points
			)
		      )
	txt_poutres   (apply
			'strcat
			(mapcar
			  '(lambda (ptr dir sec)
			     (strcat
			       (str_edit '(NO_POUTRE NO_NOEUD1 NO_NOEUD2 DX DY DZ NO_SECTION NO_SECTION)
					 (append (mapcar 'itoa ptr)
						 (mapcar '(lambda (x) (rtos x 1 7)) dir)
						 (list sec sec)
					 )
					 mdl_poutre
			       )
			       "\n"
			     )
			   )
			  poutres
			  directions
			  sections
			)
		      )
	txt_sections  (apply
			'strcat
			(mapcar
			  '(lambda (sec) (strcat (str_edit '(NO_SECTION) (list sec) mdl_section) "\n"))
			  sections_unique
			)
		      )
	txt_materiaux (apply 'strcat
			     (mapcar '(lambda (sec)
					(strcat (str_edit '(NO_MATERIAU) (list sec) mdl_materiau) "\n")
				      )
				     sections_unique
			     )
		      )
	txt_fichier   (str_edit	'(TXT_NOM_FICHIER  TXT_DATE	    TXT_HEURE
				  NB_NOEUDS	   TXT_NOEUDS	    NB_POUTRES
				  TXT_POUTRES	   TXT_SECTIONS	    TXT_MATERIAUX
				 )
				(list (strcat (vl-filename-base fichier) ".por")
				      (date)
				      (heure)
				      (itoa (length points))
				      txt_noeuds
				      (itoa (length poutres))
				      txt_poutres
				      txt_sections
				      txt_materiaux
				)
				mdl_fichier
		      )
  )
  (write-line txt_fichier flag)
  (close flag)
  (princ)
)
 ;|«Visual LISP© Format Options»
(100 2 40 2 nil "Fin de " 100 9 0 0 0 T nil nil T)
;*** NE PAS AJOUTER de texte au-dessous du commentaire! ***|;
