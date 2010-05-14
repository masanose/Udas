;+ 
;NAME: 
; thm_ui_template
;
;PURPOSE:  
;  Top level object to manage the themis gui settings template.  Mainly provides a root for serialization a la, thm_ui_document
;
;CALLING SEQUENCE:
; template = Obj_New("thm_ui_template")
;
;
;METHODS:
;  GetProperty
;  SetProperty
;
;
;HISTORY:
;
;NOTES:
;  This object differs from other gui objects with respect to its getProperty,setProperty,getAll,setAll methods.  These methods are now provided dynamically
;  by thm_ui_getset, so you only need to modify the class definition and the init method to if you want to add or remove a property from the object. 
;  
;
;$LastChangedBy: pcruce $
;$LastChangedDate: 2009-09-14 14:58:32 -0700 (Mon, 14 Sep 2009) $
;$LastChangedRevision: 6722 $
;$URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/thmsoc/tags/tdas_5_21/idl/themis/thm_ui_new/objects/thm_ui_template__define.pro $
;-----------------------------------------------------------------------------------

function thm_ui_template::init

;  self.page =     obj_new('thm_ui_page_settings')
;  self.panel =    obj_new('thm_ui_panel_settings')
;  self.x_axis =   obj_new('thm_ui_axis_settings')
;  self.y_axis =   obj_new('thm_ui_axis_settings')
;  self.z_axis =   obj_new('thm_ui_zaxis_settings')
;  self.line =     obj_new('thm_ui_line_settings')
;  self.variable = obj_new('thm_ui_variable')

  return,1

end

PRO thm_ui_template__define

   struct = { THM_UI_TEMPLATE,    $
              page:obj_new('thm_ui_page_settings'),$
              panel:obj_new('thm_ui_panel_settings'),$
              x_axis:obj_new('thm_ui_axis_settings'),$
              y_axis:obj_new('thm_ui_axis_settings'),$
              z_axis:obj_new('thm_ui_zaxis_settings'),$
              line:obj_new('thm_ui_line_settings'),$
              variable:obj_new('thm_ui_variable'),$
              INHERITS thm_ui_readwrite, $
              INHERITS thm_ui_getset $ 
}

END
