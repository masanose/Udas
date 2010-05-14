;NAME: 
; thm_ui_document__define
;
;PURPOSE:
; Helper object for save/load THEMIS document.  
;
;CALLING SEQUENCE:
;
;OUTPUT:
;
;ATTRIBUTES:
; Active window object
; callSequence object
;
;METHODS:
; GetDOMElement        (inherited from thm_ui_readwrite)
; BuildFromDOMElement  (inherited from thm_ui_readwrite)
;
;HISTORY:
;
;$LastChangedBy: pcruce $
;$LastChangedDate: 2010-02-17 12:32:15 -0800 (Wed, 17 Feb 2010) $
;$LastChangedRevision: 7290 $
;$URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/thmsoc/tags/tdas_5_21/idl/themis/thm_ui_new/objects/thm_ui_document__define.pro $
;-----------------------------------------------------------------------------------

; thm_ui_document owns the windowContainer object, and the container
; (but not the windows it contains) should be destroyed at cleanup time 
; to prevent a memory leak.
;
; The callSequence object will have other outstanding references
; and should not be destroyed at cleanup time.

pro thm_ui_document::cleanup
   ; Without the following line, it seems that the objects in 
   ; windowContainer get destroyed when the container is destroyed,
   ; invalidating the window object references held by the original
   ; windowStorage object.  So we need to ensure that the container
   ; gets emptied before destroying it.

   self.windowContainer->Remove,/all

   obj_destroy,self.windowContainer
end

pro thm_ui_document::onLoad,windowStorage=windowStorage,windowMenus=windowMenus, loadedData=loadedData,historywin=historywin,statustext=statustext,guiID=guiID

historywin->Update,'thm_ui_document::onLoad: setting loadedData attribute of call_sequence object', dontshow=1
print,'thm_ui_document::onLoad: setting loadedData attribute of call_sequence object'
; Set loadedData attribute of call_sequence object
self.callSequence->setLoadedData,loadedData

historywin->Update,'thm_ui_document::onLoad: Resetting windowStorage object', dontshow=1
print,'thm_ui_document::onLoad: Resetting windowStorage object'
; Reset windowStorage object, passing the new callSequence object
windowStorage->Reset,callSequence=self.callSequence

historywin->Update,'thm_ui_document::onLoad: Replaying callSequence data loading calls', dontshow=1
print,'thm_ui_document::onLoad: Replaying callSequence data loading calls'
; Replay callSequence calls
self.callSequence->reCall,historywin=historywin,statustext=statustext,guiId=guiID

; Remove everything from windowMenus object
historywin->Update,'thm_ui_document::onLoad: Removing old windows from windowMenus object', dontshow=1
print,'thm_ui_document::onLoad: Removing old windows from windowMenus object'

wm_names=windowMenus->GetNames()

for i=0,n_elements(wm_names)-1 do begin
   windowMenus->Remove,wm_names[i]
endfor

historywin->Update,'thm_ui_document::onLoad: Adding new windows to windowStorage object', dontshow=1
print,'thm_ui_document::onLoad: Adding new windows to windowStorage object'
; Add each new window to the windowstorage object
window_array=self.windowContainer->Get(/all,count=window_count)

for i=0,window_count-1 do begin
   w=window_array[i]
   result = windowStorage->AddNewObject(w)
   if (result EQ 0) then begin
     message,'Unknown failure adding window object to windowStorage.'
   endif
   ; Update the window menus
   w->GetProperty, Name=name
   windowMenus->Add, name
 ;  windowMenus->Update, windowStorage  ; FIXME: is this necessary here? NO, in fact due to bug in update method, will sometimes cause a crash.
endfor


; Set first window active in windowStorage
historywin->Update,'thm_ui_document::onLoad: Setting window 1 active in windowStorage', dontshow=1
print,'thm_ui_document::onLoad: Setting window 1 active in windowStorage'

windowStorage->SetActive,id=1

; Update windowMenus again (FIXME: is this necessary?)
historywin->Update,'thm_ui_document::onLoad: Updating windowMenus with newly loaded windows', dontshow=1
print,'thm_ui_document::onLoad: Updating windowMenus with newly loaded windows'

;note, this will crash if the wrong number of names were added to windowMenus
windowMenus->Update, windowStorage


end

FUNCTION THM_UI_DOCUMENT::Init,windowStorage

   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /Cancel
      ok = Error_Message(Traceback=1)
      RETURN, 0
   ENDIF

   ; We may initialize this object with a windowStorage object
   ; (when saving a document), or with no arguments (when loading
   ; a document)

   if (n_elements(windowStorage) NE 0) then begin
      ; Initialize windowContainer from windowStorage object

      self.windowContainer=obj_new('IDL_CONTAINER')
      windowStorage->GetProperty,windowObjs=ptr_window_array
      ; Deference pointer and store each array element in windowContainer
      if (ptr_valid(ptr_window_array) NE 0) then begin
         window_array=*ptr_window_array
         for i=0,n_elements(window_array)-1 do begin
           self.windowContainer->Add,window_array[i]
         endfor
      endif

      ; Initialize callSequence from windowStorage object
      windowStorage->GetProperty,callSequence=ws_call_sequence
      self.callSequence=ws_call_sequence
   endif else begin
      ; Initialize windowContainer to empty container
      self.windowContainer=obj_new('IDL_CONTAINER')

      ; Initialize callSequence to null object
      self.callSequence = obj_new();
   endelse
       
RETURN, 1
END ;--------------------------------------------------------------------------------


PRO THM_UI_DOCUMENT__DEFINE

   struct = { THM_UI_DOCUMENT,    $

              windowContainer: obj_new('IDL_CONTAINER'), $ 
              callSequence: obj_new(), $
              INHERITS thm_ui_readwrite $ 
}

END
