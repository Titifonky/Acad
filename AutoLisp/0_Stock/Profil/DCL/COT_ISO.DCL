// Programme Lisp �crit par l.Bouquillon le 30.07.97
// Modifi� le 
// Objet : G�n�rateur de profil


editcotiso : dialog {
	label ="Editeur cot iso :";
	: edit_box {
		label = "valeur de la c�te : ";
        	key = /*MSG0*/"val";
		is_enabled = true;
		}
ok_only;
}

//****************************************
cotiso : dialog {
 		   label ="COTATION ISOMETRIQUE";
		: row {
		spacer_1;
		: column {
		: edit_box {
                    label = "Echelle de cotation : ";
                       key = "echcot";
		    alignment = right;
             	}
		: edit_box {
                    label = "Hauteur du texte : ";
                    key = "txtcot";
		    alignment = right;
                	    }
		: edit_box {
                    label = "Taille des fl�che : ";
                    key = "taillecot";
		    alignment = right;
                	    }
		
		:row {

		label = "Choix :";
                : cluster {layout = vertical;
                 : toggle {
                    label = "Texte";
                    key = /*MSG0*/"choix1";
                          }               
                 : toggle {
                    label = "Cotation";
                    key = /*MSG0*/"choix2";
	                  }
    		}

}

			}
		: row {
            layout = vertical;
                    : icon_image {
                        key         = "isobmp1";
			height = 5;
			width = 15;
                    }
            layout = vertical;
                    : icon_image {
                        key         = "isobmp2";
			height = 5;
			width = 15;
                    }

		: button { 
			label = "Choix des c�tes ";
			key = "choixcot";
			width = 1;
			height = 3;
           		} 
		}
}

    ok_cancel_help;
    errtile;
}



