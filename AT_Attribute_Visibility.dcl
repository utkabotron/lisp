// Dialog for controlling attribute visibility
attribute_visibility : dialog {
  label = "Attribute's visibility";
  
  : row {
    : boxed_column {
      label = "Attributes";
      
      : list_box {
        key = "attr_list";
        width = 35;
        height = 8;
        multiple_select = true;
        fixed_width = true;
        fixed_height = true;
      }
    }
    
    : column {
      alignment = top;
      
      : button {
        key = "btn_visible";
        label = "Visible";
        width = 18;
        fixed_width = true;
      }
      
      : spacer { height = 0.1; }
      
      : button {
        key = "btn_invisible";
        label = "Invisible";
        width = 18;
        fixed_width = true;
      }
      
      : spacer { height = 0.5; }
      
      : button {
        key = "btn_exit";
        label = "Exit";
        width = 18;
        fixed_width = true;
        is_cancel = true;
      }
    }
  }
}
