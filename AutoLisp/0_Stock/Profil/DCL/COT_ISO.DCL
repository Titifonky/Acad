// Programme Lisp écrit par l.Bouquillon le 30.07.97
// Modifié le 
// Objet : Générateur de profil


editcotiso : dialog {
	label ="Editeur cot iso :";
	: edit_box {
		label = "valeur de la côte : ";
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
                    label = "Taille des flèche : ";
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
			label = "Choix des côtes ";
			key = "choixcot";
			width = 1;
			height = 3;
           		} 
		}
}

    ok_cancel_help;
    errtile;
}



