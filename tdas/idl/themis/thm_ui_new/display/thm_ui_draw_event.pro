;+ 
;NAME:  
; thm_ui_draw_event
;
;PURPOSE:
; This routine handles all events that occur in the draw window
;
;CALLING SEQUENCE:
; info=thm_ui_draw_event(event, info)
;
;INPUT:
; event - the structure from the draw window event
; info - the main information structure from splash_gui
;
;OUTPUT:
; info - the updated main information structure
; 
;HISTORY:
;
;$LastChangedBy: cgoethel_new $
;$LastChangedDate: 2009-07-02 12:55:47 -0700 (Thu, 02 Jul 2009) $
;$LastChangedRevision: 6383 $
;$URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/thmsoc/tags/tdas_5_21/idl/themis/thm_ui_new/display/thm_ui_draw_event.pro $
;-

FUNCTION thm_ui_draw_event, event, info

      ; Catch errors here
      
   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /Cancel
      ok = Error_Message(Traceback=1)
      RETURN, info
   ENDIF
   
   ;to desensitize to erroneous events that occur during panels
   if info.drawDisabled then return,info
   
   ;to desensitize to erroneous events that occur after panels
   if systime(/seconds) - info.drawDisableTimer lt .5 then return,info
 
   if info.draw_select eq 1 then begin
     widget_control,info.drawId,/input_focus
   endif
 
    ; Convert cursor location from device to normalized values
    
    info.drawWin->GetProperty, Dimensions=windowDimensions
    info.cursorPosition = [event.x/windowDimensions[0], event.y/windowDimensions[1]]
            
    if event.type eq 0 && event.clicks gt 0 && event.press eq 1 then begin
      info.click = 1
      info.historyWin->update,'Click On'
    endif
      
    if event.type eq 1 && event.release eq 1 then begin
      info.click = 0
      info.historyWin->update,'Click Off'
      info.contextMenuOn = 0
    endif
    
    if info.contextMenuOn then return,info
        
    IF Size(event.release, /Type) ne 0 && event.release EQ 4 THEN BEGIN
   ;   Print, 'Right Click - display context menu'
      
      ;Reset tracking and return if the user is rubberbanding or creating a marker 
      if info.marking ne 0 or info.rubberbanding ne 0 then begin
        thm_ui_reset_tracking, info
        RETURN, info
      endif
      
      Widget_DisplayContextMenu, info.drawID, event.x, event.y, info.drawContextBase
      info.contextMenuOn = 1
      info.prevEvent='RIGHTCLICK'
      info.marking=0
      RETURN, info
    ENDIF
       
   ; help,event,/str
       
    ;turn marking on
    IF event.ch NE 0 OR event.key NE 0 THEN BEGIN
    
      if event.key eq 2 && event.press then begin
        info.ctrl = 1
        info.historyWin->update,'Ctrl On'
      endif
      
      if event.key eq 2 && event.release then begin
        info.ctrl = 0
        info.historyWin->update,'Ctrl Off'
      endif
    endif
    
    if info.marking eq 0 && info.ctrl && info.click then begin
    
      info.marking = 1
      info.markers[0] = info.cursorposition[0]
      
      info.historyWin->update,'Turning Marking On'

      IF info.rubberbanding EQ 1 THEN BEGIN
         info.rubberbanding=0
         info.rubberBandTimer=0D
         info.drawObject->rubberBandOff
         legend = Widget_Info(info.showPositionMenu, /Button_Set)
         
         if info.tracking && legend then begin
           if info.trackAll then info.drawObject->legendOn,/all else info.drawObject->legendOn 
         endif
          
      ENDIF
    
      if info.trackall eq 1 then begin 
        info.drawObject->markerOn,/all,fail=fail
      endif else begin
        info.drawObject->markerOn,fail=fail
      endelse
      
      IF fail EQ 1 THEN BEGIN
      
         info.statusBar->Update, 'Failed to create marker. Cursor was outside panel area'
         info.marking=0
         info.markers=[0.0,0.0]
         thm_ui_reset_tracking,info

      ENDIF ELSE BEGIN
        FOR i=0,N_Elements(info.markerButtons)-1 DO Widget_Control, info.markerButtons[i], sensitive=1   
      ENDELSE
    endif
    
    ;turn marking off
    if info.marking eq 1 && (~info.ctrl || ~info.click) then begin
    
      info.historyWin->update,'Turning Marking Off'
    
      info.drawObject->markerOff
      info.markers[1]=info.cursorPosition[0]
      
      ;only allow selection for markers that have width greater than or equal to 1 pixel 
      ;this prevents markers that are too small for the user to select.  We may be able to remove this
      ;check in the future if we implement a menu that allows selection of markers without an on-screen 
      ;selection on-screen
           
      ;info.markerTitle->GetProperty, UseDefault=useDefault
      if info.markerTitleOn then thm_ui_marker_title, info.master, info.markerTitle, info.historywin, info.statusbar $
         else info.markerTitle=obj_new('THM_UI_MARKER_TITLE')
      ;info.markerTitle->GetProperty, Cancelled=cancelled      
      thm_ui_create_marker, info  ; updates history and status bar internally
   
      ; since marking is done, reset everything
      info.markers=[0.0,0.0]
      info.marking= 2
      info.drawObject->Update, info.windowStorage, info.loadedData
      info.drawObject->Draw
      info.click = 0
      info.ctrl = 0
      
    endif

      ; Done with special cases, check button event type to determine what happened
      ; Button Press - marking in progress, draw rubberband box, or display options panel
      ; Button Release - end marking, drawing box, or displaying panel
      ; Motion Event - draw tracking line, marking or drawing box
      
    CASE event.type OF
    
       ; Button Press
       ; set the coordinates and event information, wait for a button release 
  
      0:BEGIN
      
        ; NOTE: if marking was not on then this is either a press to display options 
        ; panel or the start of a click-drag event. Do nothing for now since we will 
        ; not know which of the two options it is until the button is released                  
            
            
        if event.press eq 1 then begin
          info.prevEventX = event.x
          info.prevEvent='PRESS'
        
          info.drawObject->setCursor, info.cursorPosition
        endif
     ;   tmp = info.drawObject->getClick()
  
       ; print,'Got Click.'
       ; if ~is_struct(tmp) then begin
          ;print,'Clicked Page'
       ; endif else begin
        ;  print,'Clicked Panel: ' + strtrim(string(tmp.panelidx),2)
        
        ;  if tmp.component eq 0 then print,'Component: Panel'
        ;  if tmp.component eq 1 then print,'Component: Xaxis'
         ; if tmp.component eq 2 then print,'Component: Yaxis'
         ; if tmp.component eq 3 then print,'Component: Zaxis'
         ; if tmp.component eq 4 then print,'Component: Variables'
          
         ; print,"marker:",tmp.marker
          
        ;endelse 

        RETURN, info

      END

        ; Button Release, either
        ; 1)marking done, 
        ; 2)rubberband box done, or 
        ; 3)display options panel 
      
      1: BEGIN
        ; Print, 'Button release'  
            ; display options
            
        IF info.marking EQ 0 && info.prevEvent EQ 'PRESS' && event.release eq 1 THEN BEGIN
        
           info.historyWin->update,'Options Click'
          ; print, 'display options'
           thm_ui_display_options, info
           info.marking=0
           info.prevEventX = event.x
           info.prevEvent='RELEASE'
           info.drawingBox=0
           info.rubberBanding=0
           info.drawObject->rubberBandOff
           info.drawObject->markerOff
           RETURN, info
        ENDIF
        
            ; rubber band box
            
        IF info.rubberBanding EQ 1 && info.prevEvent EQ 'MOTION' THEN BEGIN
        ;   print, 'rubber band off'
        
           info.historyWin->update,'Turning Rubber Band Off'
        
           info.drawObject->setCursor, info.cursorPosition
           info.drawObject->rubberBandOff
      
           vBar = widget_info(info.trackVMenu,/button_set)
           if vBar eq 1 then begin
             IF info.trackAll EQ 1 THEN info.drawObject->vBarOn, /all ELSE info.drawObject->vBarOn
           endif
           
           hBar = widget_info(info.trackhMenu,/button_set)
           if hBar eq 1 then begin
           ;  IF info.trackAll EQ 1 THEN info.drawObject->hBarOn, /all ELSE info.drawObject->hBarOn
             info.drawObject->hBarOn
           endif   
         
           legend = Widget_Info(info.showPositionMenu, /Button_Set)
           IF legend EQ 1 && info.tracking THEN BEGIN
              IF info.trackAll EQ 1 THEN info.drawObject->legendOn, /all ELSE info.drawObject->legendOn
           ENDIF 
           
           tm = sysTime(/seconds)
           IF tm - info.rubberBandTimer GT 0.5 THEN thm_ui_rubber_band_box, info
                      
           
           info.drawObject->draw
           
           info.drawingBox=0
           info.marking=0
           info.rubberBanding=0
           info.rubberBandTimer=0D
           info.prevEventX = event.x
           info.prevEvent='RELEASE'
           RETURN, info
        ENDIF
        
      END      

         ; Motion Event
         ; 1)If marking is on -> draw line
         ; 2)If marking is just finishing -> reset marking params and draw line
         ; 3)draw rubberbsand box
      
      2: BEGIN
        ;print, 'motion event'      
        info.prevEventX = event.x
        info.prevEventType = event.type
        
            ; marking is on, draw line

        IF info.marking EQ 1 && (info.prevEvent EQ 'CTRL' OR info.prevEvent EQ 'MOTION') THEN BEGIN
        ;   print, 'marking on'
           info.drawObject->setCursor, info.cursorPosition
           info.prevEvent='MOTION'
           RETURN, info
        ENDIF
        
            ; marking was on and just turned off - needed to wait for cursor 
            ; motion to reset marking variables 

        IF info.marking EQ 2 THEN BEGIN
         ;  print, 'marking officially off'
           info.marking=0
           info.drawObject->setCursor, info.cursorPosition
           info.prevEvent='MOTION'
           RETURN, info
        ENDIF
        
            ; marking was not on, but button was pressed then start drawing box 
            
        IF info.marking EQ 0 && info.prevEvent EQ 'PRESS' THEN BEGIN
        ;   print, 'starting rubber band box'
           info.drawingBox=1 
           info.drawObject->setCursor, info.cursorPosition
           info.drawObject->rubberBandOn
           info.rubberBandTimer=sysTime(/seconds)
           info.drawObject->vBarOff
           info.drawObject->hBarOff
           info.drawObject->legendOff
           info.marking=0
           info.rubberBanding=1
           info.prevEvent='MOTION'
           RETURN, info
        ENDIF
        
           ; drawing box
           
        IF info.drawingBox EQ 1 && info.prevEvent EQ 'MOTION' THEN BEGIN
          ; print, 'drawing box'
           info.drawObject->setCursor, info.cursorPosition
           info.marking=0
           info.prevEvent='MOTION'
           RETURN, info
        ENDIF
        
           ; just tracking
           
        IF info.marking EQ 0 && info.drawingBox EQ 0 THEN BEGIN
        ;   print, 'just tracking'
           info.drawObject->setCursor, info.cursorPosition
           info.marking=0
           info.prevEvent='MOTION'
           RETURN, info
        ENDIF
        
        info.prevEvent='MOTION'
        RETURN, info
      END
      5: BEGIN 
        
        ;this code block manually implements accelerator keys on platforms without accelerator support
        if info.ctrl && event.press && info.marking eq 0 && info.rubberBanding eq 0 then begin
          info.ctrl=0
          if event.ch eq 26 then begin
            thm_ui_close_window,info
          endif else if event.ch eq 14 then begin
            thm_ui_new_window,info
          endif else if event.ch eq 15 then begin
            thm_ui_open,info
          endif else if event.ch eq 19 then begin
            thm_ui_save,info
          endif else if event.ch eq 16 then begin
            thm_ui_print,info
          endif else if event.ch eq 17 then begin
            thm_ui_exit,event,info=info
            return,0
          endif else if event.ch eq 18 then begin
            thm_ui_refresh,info
          endif
        endif 
        
        if event.press then begin
          if event.ch eq 9 then begin
            thm_ui_expand,info
          endif else if event.ch eq 8 then begin
            thm_ui_reduce,info
          endif
        endif
      
      END
      6: BEGIN
      
        ;this code block manually implements accelerator keys on platforms without accelerator support
        if event.press && event.key eq 5 then begin
          thm_ui_scrollb,info
        endif else if event.press && event.key eq 6 then begin
          thm_ui_scrollf,info
        endif
             
      END
      
      ELSE: BEGIN
      END
      
    ENDCASE
    
  RETURN, info
  END
