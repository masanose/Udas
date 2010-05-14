;+ 
;NAME:
; thm_ui_marker__define
;
;PURPOSE:  
; Marker object, displayed whenever user ctrl-click-drags to highlight an area
;
;CALLING SEQUENCE:
; marker = Obj_New("THM_UI_MARKER")
;
;INPUT:
; none
;
;KEYWORDS:
; name              name for this marker
; range             start and stop value of marker (data coords)
; settings          property settings of the marker
; isSelected        flag set if marker is currently selected
; filename          filename, if saved
;
;OUTPUT:
; marker object reference
;
;METHODS:
; SetProperty   procedure to set keywords 
; GetProperty   procedure to get keywords 
; GetAll        returns the entire structure
;
;
;NOTES:
;  Methods: GetProperty,SetProperty,GetAll,SetAll are now managed automatically using the parent class
;  thm_ui_getset.  You can still call these methods when using objects of type thm_ui_marker, and
;  call them in the same way as before
;
;$LastChangedBy: pcruce $
;$LastChangedDate: 2009-09-10 15:50:56 -0700 (Thu, 10 Sep 2009) $
;$LastChangedRevision: 6711 $
;$URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/thmsoc/tags/tdas_5_21/idl/themis/thm_ui_new/objects/thm_ui_marker__define.pro $
;-----------------------------------------------------------------------------------



FUNCTION THM_UI_MARKER::Copy
   out = Obj_New('THM_UI_MARKER')
   selfClass = Obj_Class(self)
   outClass = Obj_Class(out)
   IF selfClass NE outClass THEN BEGIN
       Print, 'Object classes not identical'
       RETURN, -1
   END
   Struct_Assign, self, out
   newSettings=Obj_New("THM_UI_MARKER_SETTINGS")
   IF Obj_Valid(self.Settings) THEN newSettings=self.settings->Copy() ELSE $
      newSettings=Obj_New()
   out->SetProperty, Settings=newSettings
;   newName=Obj_New("THM_UI_TEXT")
;   IF Obj_Valid(self.name) THEN newName=self.name->Copy() ELSE $
;      newName=Obj_New()
;   out->SetProperty, Name=newName
   RETURN, out
END ;--------------------------------------------------------------------------------
 
PRO THM_UI_MARKER::Cleanup
;Obj_Destroy, self.settings
RETURN
END ;--------------------------------------------------------------------------------



FUNCTION THM_UI_MARKER::Init,     $
      Range=range,                $ ; start and end value of marker (in normalized coordinates) 
      Settings=settings,          $ ; property settings of the marker
      Name=name,                  $ ; name for this marker
      IsSelected=isselected,      $ ; 
      FileName=filename,          $ ; filename if marker is saved
      Debug=debug                   ; flag to debug
      
   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /Cancel
      ok = Error_Message(Traceback=Keyword_Set(debug))
      RETURN, 0
   ENDIF

      ; Check that all parameters have values
   
   IF N_Elements(range) EQ 0 THEN range = [0D,0D]
   IF NOT Obj_Valid(settings) THEN settings = Obj_New("THM_UI_MARKER_SETTINGS")
   IF N_Elements(name) EQ 0 THEN name = ''
   IF N_Elements(isselected) EQ 0 THEN isselected = 0
   IF N_Elements(filename) EQ 0 THEN filename = ''  

      ; Set all parameters

   self.name = name
   self.range = range
   self.settings = settings
   self.isSelected = isselected
   self.filename = filename
                  
RETURN, 1
END ;--------------------------------------------------------------------------------

PRO THM_UI_MARKER__DEFINE

   struct = { THM_UI_MARKER,  $

        name:'',              $ ; name for this marker
        range:[0D,0D],        $ ; start and end of marker in data coordinates
        settings:Obj_New(),   $ ; properties of this marker
        isSelected: 0,        $ ; flag set if this marker is selected
        filename:'',          $ ; filename, if saved
        INHERITS THM_UI_READWRITE, $ ; generalized read/write methods
        inherits thm_ui_getset $ ; generalized setProperty/getProperty/getAll/setAll methods   

}

END ;--------------------------------------------------------------------------------

