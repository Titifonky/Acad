// Programme Lisp écrit par l.Bouquillon le 24.03.97
// Modifié le 23.04.97
// Objet : Générateur de profil

profil : dialog {
    aspect_ratio = 0;
    label ="GENERATEUR DE PROFILS";
                : row {

 : row {
   : cluster {
        layout = vertical;
            : concatenation {
                : text_part {
                    label = "Type de profil: ";
                }
                : text_part {
                    key = /*MSG0*/"fer";
                    width = 4;
                }
                : text_part {
                    key = /*MSG0*/"type_fer";
                    width = 12;
                }
             }
            : concatenation {
                : text_part {
                    key = /*MSG0*/"definition";
                    width = 32;
		}
             }

        : list_box {
            key = "list_col";
            allow_accept = true;
        }




	: column {
                : row {
		: button { 
			label = "IPE ";
			key = "icon1";
			is_default =true;
			height = 2;
			alignment = centered;
           		} 

		: button {
              		label = "IPN";
                	key = "icon2";
			is_default =true;
			height = 2;
			alignment = centered;
           		}
		: button {
              		label = "HEA";
                	key = "icon3";
			is_default =true;
			fixed_width =true;
			height = 2;
			alignment = centered;
           		}
		: button {
              		label = "HEB";
                	key = "icon4";
			is_default =true;
			fixed_width =true;
			height = 2;
			alignment = centered;
           		}                           
                }
                : row {
			: button {
              		label = "UPN";
                	key = "icon5";
			is_default =true;
			fixed_width =true;
			height = 2;
			alignment = centered;
           		}
		: button {
              		label = "UAP";
                	key = "icon6";
			is_default =true;
			fixed_width =true;
			height = 2;
			alignment = centered;
           		} 
		: button {
              		label = "UPE";
                	key = "icon9";
			is_default =true;
			fixed_width =true;
			height = 2;
			alignment = centered;
           		} 
		: button {
              		label = "COR";
                	key = "icon7";
			is_default =true;
			fixed_width =true;
			height = 2;
			alignment = centered;
           		} 
		: button {
              		label = " *L* ";
                	key = "icon8";
			is_default =true;
			fixed_width =true;
			height = 2;
			alignment = centered;
           		}   
                    
                }

}}
    : column {
            layout = vertical;
                    : icon_image {
                        key         = "m_imag";
                    }

           : cluster {layout = vertical;
		label = "Angle de rotation:";
              : cluster {layout = horizontal;
                : cluster {layout = vertical;
                 : toggle {
                    label = "0°";
                    key = /*MSG0*/"rot1";
                }               
                 : toggle {
                    label = "45°";
                    key = /*MSG0*/"rot2";
                }
                 : toggle {
                    label = "90°";
                    key = /*MSG0*/"rot3";
                }

            }
            : cluster {
                layout = vertical;
                 : toggle {
                    label = "180°";
                    key = /*MSG0*/"rot5";
                }               
                 : toggle {
                    label = "-45°";
                    key = /*MSG0*/"rot6";
                }
                 : toggle {
                    label = "-90°";
                    key = /*MSG0*/"rot7";
                }
		}}	
                 : toggle {
                    label = "Rotation suivant:";
                    key = /*MSG0*/"rot4";
                }
		}
 		: toggle {
                label = "Crée un block";
                key = /*MSG0*/"cbloc";
                }
		: concatenation {
                  : text_part {
                    label = "Nom du bloc: ";
                }
                  : text_part {
                    key = /*MSG0*/"nomdubloc";
                    width = 10;
                }}   
		: button {
              		label = "Nom du bloc:";
                	key = "nombloc";
			is_default =true;
			fixed_width =true;
			height = 2;
			alignment = centered;
           		}  

     }
}

    }
    ok_cancel_help_info;
    errtile;
}

list_bloc : dialog {
    label = "Noms des blocs dans ce dessin";

    : edit_box {
        label = "Nom du bloc actuel";
        key = /*MSG0*/"nom_bl";
    }
    : list_box {                                   
        key = "list_bloc";
        width = 32;
        allow_accept = true;
    }
    : edit_box {
        label = "Filtre:";
        key = /*MSG0*/"filtre_b";
    }

    spacer;
    ok_only;
}

redefinir : dialog{
    label = "ATTENTION !!! vous pouvez redefinir tous les blocs du dessin";
    : paragraph {
        : text_part {
            label = "Un bloc avec ce nom existe déjà dans le dessin.";
        }
        : text_part {
            label = "Voulez-vous le redéfinir ?";
        }
    }
    spacer_1;
    : row {
        fixed_width = true;
        alignment = centered;
        : button {
            label = "&Redéfinir";
            key = /*MSG0*/"yes";
            width = 8;
        }
        : spacer {
            width = 2;
        }
        : default_button {
            label = "Annuler";
            key = /*MSG0*/"no";
            width = 8;
        }
    }
}

insert: dialog {
    label ="CHOIX DU POINT D'INSERTION";
        key = "label";
        initial_focus               = "listbox";
        : row {
            : column {
                : row {
                    : icon_image {
                        key         = "imag5";
                    }
                    : icon_image {
                        key         = "imag6";
                    }
                    : icon_image {
                        key         = "imag7";
                    }
                    : icon_image {
                        key         = "imag8";
                    }
                }
                : row {
                    : icon_image {
                        key         = "imag1";
                    }
                    : icon_image {
                        key         = "imag2";
                    }
                    : icon_image {
                        key         = "imag3";
                    }
                    : icon_image {
                        key         = "imag4";
                    }
                }


            }
        }
                  
            spacer;
            ok_cancel;
}
