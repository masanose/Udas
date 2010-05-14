;+ 
;NAME: 
; thm_ui_calculate_settings__define
;
;PURPOSE:  
;  Keeps track of state for calculate panel when closed.
;
;CALLING SEQUENCE:
; calcSettings = Obj_New("THM_UI_CALCULATE_SETTINGS")
;
;INPUT:
; none
;
;ATTRIBUTES:
;
;  path:  string, path to the calculate file
;  name: string,name of the calculate file
;  text: string array, containing text of the buffer
;
;OUTPUT:
;  calculate settings object reference
;
;METHODS:
;   
;  GetProperty
;  SetProperty
;
;NOTES:
;  Methods: GetProperty,SetProperty,GetAll,SetAll are now managed automatically using the parent class
;  thm_ui_getset.  You can still call these methods when using objects of type thm_ui_configuration_Settings, and
;  call them in the same way as before
;
;$LastChangedBy:pcruce $
;$LastChangedDate:2009-09-10 09:15:19 -0700 (Thu, 10 Sep 2009) $
;$LastChangedRevision:6707 $
;$URL:svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/thmsoc/trunk/idl/themis/thm_ui_new/objects/thm_ui_calculate_settings__define.pro $
;-----------------------------------------------------------------------------------

pro thm_ui_calculate_settings::setProperty,$
                                      text=text,$
                                      _extra=ex
   
   ;handle general cases using parent class
   self->thm_ui_getset::setProperty,_extra=ex
   
   ;handle special case
   if n_elements(text) ne 0 then begin
          ptr_free,self.text
          self.text = ptr_new(text)
   endif
   

                                      
end

pro thm_ui_calculate_settings::getProperty,$
                                 text=text,$
                                 _ref_extra=ex

  ;handle general case using parent class
  self->thm_ui_getset::getProperty,_extra=ex

  ;handle special case
  if arg_present(text) then begin
    if ptr_valid(self.text) then begin
      text = *self.text
    endif else begin
      test = ''
    endelse
  endif      
                                 
end


function thm_ui_calculate_settings::init

  self.name = '-scratch-'
  text = strarr(1)
  text[0] = ' '
  self.text = ptr_new(text)
  return,1
  
end

pro thm_ui_calculate_settings__define

  struct = { THM_UI_CALCULATE_SETTINGS, $
               path:'',$
               name:'',$
               text:ptr_new(), $
               inherits thm_ui_getset    $ ; generalized setProperty/getProperty/getAll/setAll methods   
               }

end

