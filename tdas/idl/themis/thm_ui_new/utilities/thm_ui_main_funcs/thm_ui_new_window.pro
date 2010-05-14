 ;+
;
;  Name: THM_UI_NEW_WINDOW
;  
;  Purpose: Opens a new window
;  
;  Inputs: The info structure from the main gui
;
;
;$LastChangedBy: pcruce $
;$LastChangedDate: 2009-08-03 11:29:44 -0700 (Mon, 03 Aug 2009) $
;$LastChangedRevision: 6519 $
;$URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/thmsoc/tags/tdas_5_21/idl/themis/thm_ui_new/utilities/thm_ui_main_funcs/thm_ui_new_window.pro $
;-
pro thm_ui_new_window,info

  compile_opt idl2
   
      result=info.windowStorage->Add(Settings=info.pageSettings)
       activeWindow=info.windowStorage->GetActive()
       activeWindow[0]->GetProperty, Name=name
       info.windowMenus->Add, name
       info.windowMenus->Update, info.windowStorage
       thm_ui_orientation_update,info.drawObject,info.windowStorage
       info.drawObject->update,info.windowStorage, info.loadedData
       info.drawObject->draw
       info.scrollbar->update
RETURN
end