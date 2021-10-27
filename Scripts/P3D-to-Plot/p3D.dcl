p3D : dialog {
    label = "p3D - Generate Longsectional Plot from 3DPolyline";
    : column {
      : row {
        : column {
            : radio_row {
              label = "Service";
              : radio_button {
                label = "Roading";
                key = "RD";
                value  = "1";
              }
              : radio_button {
                label = "Stormwater";
                key = "SW";
                value = "0";
              }
              : radio_button {
                label = "Wastewater";
                key = "SS";
                value = "0";
              }
			  : radio_button {
                label = "Water";
                key = "WR";
                value = "0";
              }
            }
          : boxed_row {
            : toggle {
              key = "IP";
              label = "Draw IP's";
              value = "0";
            }
            : toggle {
              key = "MH";
              label = "Draw Manholes";
              value = "0";
            }
            : toggle {
              key = "ST";
              label = "Draw Structures";
              value = "0";
            }
          }
        }   
      }
      : row {
        : boxed_row {
          : button {
            key = "accept";
            label = "  Okay  ";
            is_default = true;
          }
          : button {
            key = "cancel";
            label = "  Cancel  ";
            is_default = false;
            is_cancel = true;
          }
        }
      }
    }    
}