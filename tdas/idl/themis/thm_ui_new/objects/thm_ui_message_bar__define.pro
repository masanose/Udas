;+ 
;NAME: 
; thm_ui_message_bar__define
;
;PURPOSE:
; This is a text bar object used to display textual information for the user
; (such as a status bar for current states, message bar, or informational bar)
;
;CALLING SEQUENCE:
; To Create:    myStatusBar = Obj_New("THM_UI_MESSAGE_BAR", myWidgetBase)
; To Use:       myStatusBar->Update, 'This is a test'
; Or:           result = myStatusBar->GetState()
;
;INPUT:
; parent:       id for the parent widget (must be a base)
;
;KEYWORDS:
; name:   optional name
; state:  set this to one to display
; value:  text to be displayed in the bar 
; xSize:  size of bar in x direction
; ySize:  size of bar in y direction
; debug:  set this value to one for debugging
;
;OUTPUT:
; message bar object reference
;
;METHODS:
; Draw         creates/displays the bar (automatically called by INIT)
; Delete       removes bar from display (object persists)
; Update       updates bar with new message
; SetProperty  procedure to set keywords 
; GetProperty  procedure to get keywords 
; GetState     returns the current state of the bar (on/off) (this is a function)

;HISTORY:
;
;$LastChangedBy: bckerr $
;$LastChangedDate: 2009-06-10 17:21:15 -0700 (Wed, 10 Jun 2009) $
;$LastChangedRevision: 6122 $
;$URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/thmsoc/tags/tdas_5_21/idl/themis/thm_ui_new/objects/thm_ui_message_bar__define.pro $
;-----------------------------------------------------------------------------------


PRO THM_UI_MESSAGE_BAR::Draw

IF self.id EQ 0 THEN BEGIN
   IF self.scroll EQ 0 THEN BEGIN
      self.id = WIDGET_TEXT(self.parent, value=*self.messages, xsize = self.xsize, ysize = self.ysize,/editable)
   ENDIF ELSE BEGIN
      self.id = WIDGET_TEXT(self.parent, value=*self.messages, xsize = self.xsize, ysize = self.ysize,/scroll, /editable)
   ENDELSE
   Widget_Control, self.id, SET_TEXT_TOP_LINE=N_Elements(*self.messages)-1
   self.state = 1
ENDIF
END ;--------------------------------------------------------------------------------



PRO THM_UI_MESSAGE_BAR::Delete
IF self.id NE 0 THEN BEGIN
   WIDGET_CONTROL, self.id, /Destroy
   self.state = 0
   self.id = 0
ENDIF
END ;--------------------------------------------------------------------------------



PRO THM_UI_MESSAGE_BAR::Update, value
IF self.id NE 0 THEN BEGIN
   newId = self.currentMsgId+1
   newValue = strtrim(string(newId), 2)+': '+ strmid(value,0,1000)
   newMessages = [*self.messages, newValue]
   IF N_Elements(newMessages) GT self.msgLimit THEN newMessages = newMessages[1:self.msgLimit]
   WIDGET_CONTROL, self.id, SET_VALUE=newMessages
   WIDGET_CONTROL, self.id, SET_TEXT_TOP_LINE=n_elements(newMessages)-1, /no_newline
   self.messages = Ptr_New(newMessages)
   self.value=newValue
   self.currentMsgId=newId
ENDIF 
END ;--------------------------------------------------------------------------------
  

FUNCTION THM_UI_MESSAGE_BAR::GetState
RETURN, self.state
END ;--------------------------------------------------------------------------------



PRO THM_UI_MESSAGE_BAR::SetProperty,  $ ; The property set method for the object
            XSize=xsize,              $ ; size of bar in x direction
            YSize=ysize,              $ ; size of bar in y direction
            MsgLimit=msglimit,        $ ; max number of messages
            Refresh=refresh             ; set this to one to redisplay  

   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /Cancel
      ok = Error_Message(Traceback=Keyword_Set(self.debug))
      RETURN
   ENDIF

   ; Check for undefined variables.

   IF N_Elements(xsize) NE 0 THEN self.xsize = xsize
   IF N_Elements(ysize) NE 0 THEN self.ysize = ysize
   IF N_Elements(msglimit) NE 0 THEN self.msgLimit= msglimit
   
   IF Keyword_Set(refresh) THEN BEGIN
      self->Delete
      self->Draw
   ENDIF

END ;--------------------------------------------------------------------------------



PRO THM_UI_MESSAGE_BAR::GetProperty,  $
            id=id,                   $ ; widget id
            Name=name,               $ ; optional name of bar
            Messages=messages,       $ ; text to be displayed in the bar 
            State=state,             $ ; flag to indicate whether bar is diplayed
            MsgLimit=msglimit,       $ ; total number of messages to buffer
            XSize=xsize,             $ ; size of bar in x direction
            YSize=ysize,             $ ; size of bar in y direction
            Scroll=scroll              ; size of bar in x direction

   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /Cancel
      ok = Error_Message(Traceback=Keyword_Set(self.debug))
      RETURN
   ENDIF

   IF Arg_Present(id) then id = self.id
   IF Arg_Present(name) THEN name = self.name
   IF Arg_Present(messages) THEN messages = self.messages
   IF Arg_Present(state) THEN state = self.state
   IF Arg_Present(msglimit) THEN msglimit = self.msgLimit
   IF Arg_Present(xsize) THEN xsize = self.xsize
   IF Arg_Present(ysize) THEN ysize = self.ysize
   IF Arg_Present(scroll) THEN scroll = self.scroll

END ;--------------------------------------------------------------------------------



;PRO THM_UI_MESSAGE_BAR::Cleanup 
;   nothing to clean up (yet)
;END ;--------------------------------------------------------------------------------



FUNCTION THM_UI_MESSAGE_BAR::Init,    $ ; The INIT method of the bar object.
            parent,                  $ ; id of parent, required
            Name=name,               $ ; optional name of bar
            Value=value,             $ ; text to be displayed in the bar 
            State=state,             $ ; flag to indicate whether bar is diplayed
            MsgLimit=msglimit,       $ ; total number of messages to buffer
            XSize=xsize,             $ ; size of bar in x direction
            YSize=ysize,             $ ; size of bar in y direction
            Scroll=scroll,           $ ; size of bar in x direction
            Debug=debug                ; set to one for debugging

   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /Cancel
      ok = Error_Message(Traceback=Keyword_Set(debug))
      RETURN, 0
   ENDIF

   self.debug = Keyword_Set(debug)
    
   ; Check that all parameters have values
   
   IF N_Elements(name) EQ 0 THEN name=' '
   IF N_Elements(value) EQ 0 THEN value = '0: Message Bar ' ELSE value='0: '+value
   IF N_Elements(msglimit) EQ 0 THEN msglimit = 21 
   IF N_Elements(state) EQ 0 THEN state=1
   IF N_Elements(xsize) EQ 0 THEN xsize = 10
   IF N_Elements(ysize) EQ 0 THEN ysize = 1
   IF N_Elements(scroll) EQ 0 THEN scroll = 1

  ; Set all parameters
  
   self.parent = parent
   self.name = name
   self.value = value
   self.messages = Ptr_New(value)
   self.msgLimit = msglimit
   self.state = state
   self.xsize = xsize
   self.ysize = ysize
   self.scroll = scroll

  ; If bar is displayed then create the widget
   
   IF self.state EQ 1 THEN self->Draw

   RETURN, 1
END ;--------------------------------------------------------------------------------



PRO THM_UI_MESSAGE_BAR__DEFINE

   struct = { THM_UI_MESSAGE_BAR,   $
              parent: 0L,           $ ; id for the parent widget (must be a base)
              id: 0L,               $ ; widget id for the bar
              name: ' ',            $ ; optional name
              state: 0,             $ ; on/off flag (for display)
              value: ' ',           $ ; text to be displayed in the bar 
              messages:Ptr_New(),   $ ; max number of messages to buffer 
              currentMsgId: 0L,        $ ; current id for messages
              msgLimit: 0,          $ ; number of messages to buffer
              xSize: 0,             $ ; size of bar in x direction
              ySize: 0,             $ ; size of bar in y direction
              scroll: 0,            $ ; flag to set scroll arrows
              debug: 0              $ ; set this value to one for debugging

}

END
