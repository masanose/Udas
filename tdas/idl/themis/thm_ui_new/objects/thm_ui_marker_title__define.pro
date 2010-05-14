;+ 
;NAME: 
; thm_ui_marker_title__define
;
;PURPOSE:  
; Marker object, displayed whenever user ctrl-click-drags to highlight an area
;
;CALLING SEQUENCE:
; markerTitle = Obj_New("THM_UI_MARKER_TITLE")
;
;INPUT:
; none
;
;KEYWORDS:
; name             name for this marker
; useDefault       flag set if using default name
; defaultName      default name for marker
; cancelled        flag set if window cancelled
; 
;
;OUTPUT:
; marker object reference
;
;METHODS:
; SetProperty   procedure to set keywords 
; GetProperty   procedure to get keywords 
; GetAll        returns the entire structure
;
;NOTES:
;  Methods: GetProperty,SetProperty,GetAll,SetAll are now managed automatically using the parent class
;  thm_ui_getset.  You can still call these methods when using objects of type thm_ui_marker_title, and
;  call them in the same way as before
;
;$LastChangedBy: pcruce $
;$LastChangedDate: 2009-09-10 15:50:56 -0700 (Thu, 10 Sep 2009) $
;$LastChangedRevision: 6711 $
;$URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/thmsoc/tags/tdas_5_21/idl/themis/thm_ui_new/objects/thm_ui_marker_title__define.pro $
;-----------------------------------------------------------------------------------



FUNCTION THM_UI_MARKER_TITLE::Copy
   out = Obj_New("THM_UI_MARKER_TITLE")
   selfClass = Obj_Class(self)
   outClass = Obj_Class(out)
   IF selfClass NE outClass THEN BEGIN
       Print, 'Object classes not identical'
       RETURN, -1
   END
   Struct_Assign, self, out
   RETURN, out
END ;--------------------------------------------------------------------------------

FUNCTION THM_UI_MARKER_TITLE::Init,     $
      Name=name,                  $ ; name for this marker
      UseDefault=usedefault,      $ ; flag set if user want to use default name
      DefaultName=defaultname,    $ ; default name of marker 
;      DoNotAsk=donotask,          $ ; flag set is user does not want to be asked every time
      Cancelled=cancelled,        $ ; flag set is cancelled the page
      Debug=debug                   ; flag to debug
      
   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /Cancel
      ok = Error_Message(Traceback=Keyword_Set(debug))
      RETURN, 0
   ENDIF

      ; Check that all parameters have values
   
   IF N_Elements(name) EQ 0 THEN name = ''
   IF N_Elements(usedefault) EQ 0 THEN useDefault = 0
   IF N_Elements(defaultname) EQ 0 THEN defaultname = ''
   IF N_Elements(cancelled) EQ 0 THEN cancelled = 1
;   IF N_Elements(donotask) EQ 0 THEN donotask = 0  

      ; Set all parameters

   self.name = name
   self.useDefault = usedefault
   self.defaultName = defaultname
   self.cancelled = cancelled
;   self.doNotAsk = donotask

                  
RETURN, 1
END ;--------------------------------------------------------------------------------



PRO THM_UI_MARKER_TITLE__DEFINE

   struct = { THM_UI_MARKER_TITLE,  $

        name:'',               $ ; name for this marker
        useDefault: 0,         $ ; flag set if user want to use default name
        defaultName: '',       $ ; default name of marker
        cancelled: 0,          $ ; flag to indicate whether the window was canceled
        inherits thm_ui_getset $ ; generalized setProperty/getProperty/getAll/setAll methods

}

END ;--------------------------------------------------------------------------------
