traceur : dialog {
   	  label = "Traceur d'equation";
	: column {
		: boxed_column {
		  label = "Intervalle";
		        : edit_box {
		    	    label = "Depart :";
		            key = "intervalle_depart";
		            fixed_width = true;
		            width = 10;
		        }
		        : edit_box {
		            label = "Arrivée :";
		            key = "intervalle_fin";
		            fixed_width = true;
		            width = 10;
		        }
		       }
	        : edit_box {
	            label = "Nb de points :";
	            key = "nb_pitch";
	            fixed_width = true;
	            width = 10;
	        }
	        : edit_box {
	            label = "Equation :";
	            key = "equation";
	            height = 1;
	            width = 130;
	        }
	        : edit_box {
	            label = "Echelle :";
	            key = "echelle";
	            fixed_width = true;
	            width = 10;
        	}
       }
        
    ok_only;
}
