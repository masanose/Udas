PRO thm_ui_close_window, info

      error=0
        ; remove the active window from window storage 
        windowObjs = info.windowStorage->GetObjects()
        prevNumWin = N_Elements(windowObjs)
        activeWindow = info.windowStorage->GetActive()
        activeWindow->GetProperty, Name=activeName
        result = info.windowStorage->RemoveObject(activeWindow)     
        ; check that this wasn't the last window, if so create a blank one
        IF RESULT EQ -1 THEN BEGIN
          statusMessage = 'An error occurred closing the page. ' + name + ' has not been closed.'
          info.statusBar->Update, statusMessage
        ENDIF ELSE BEGIN
          activeWindow = info.windowStorage->GetActive()
          IF ~Obj_Valid(activeWindow) THEN BEGIN
            info.windowMenus->Remove, activeName
            info.windowMenus->Update, info.windowStorage
            result=info.windowStorage->Add(Settings=info.pageSettings)
            activeWindow=info.windowStorage->GetActive()
            activeWindow[0]->GetProperty, Name=name
            info.windowMenus->Add, name
            info.windowMenus->Update, info.windowStorage
          ENDIF ELSE BEGIN
            ; remove the window from the window menus and update the screen
            windowObjs = info.windowStorage->GetObjects()
            info.windowMenus->Remove, activeName
            info.windowMenus->Update, info.windowStorage
          ENDELSE
          windowObjs = info.windowStorage->GetObjects()
          IF N_Elements(windowObjs) LE 1 && activeName EQ 'Page: 1' && prevNumWin EQ 1 THEN statusMessage = activeName + ' has been cleared' $
            ELSE statusMessage = activeName + ' has been closed'
          info.statusBar->Update, statusMessage  
          info.drawObject->update,info.windowStorage, info.loadedData
          info.drawObject->draw     
          info.scrollbar->update     
        ENDELSE  
   ;ENDIF 
END
