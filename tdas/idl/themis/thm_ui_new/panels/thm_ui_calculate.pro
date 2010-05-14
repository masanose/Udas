;+ 
;NAME:
; thm_ui_calculate
;
;PURPOSE:
; A widget interface for selecting data
;
;CALLING SEQUENCE:
; thm_ui_calculate, master_widget_id
;
;INPUT:
; master_widget_id = the id number of the widget that calls this program
;
;OUTPUT:
; none
;
;HISTORY:
;$LastChangedBy: pcruce $
;$LastChangedDate: 2009-08-24 18:44:22 -0700 (Mon, 24 Aug 2009) $
;$LastChangedRevision: 6628 $
;$URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/thmsoc/tags/tdas_5_21/idl/themis/thm_ui_new/panels/thm_ui_calculate.pro $
;
;---------------------------------------------------------------------------------

function thm_ui_calculate_insert_text,programTextID,insertText,offset
 
  Compile_Opt hidden,idl2

  Widget_control,programTextID,get_value=programtext

  ;insert requested constant at cursor location
  xy = widget_info(programTextID,text_offset_to_xy=offset)
      
  ;cursor is allowed to go one element beyond current text
  if xy[1] ge n_elements(programtext) then begin
    temptext = strarr(n_elements(programtext)+1)
    temptext[0:n_elements(programtext)-1] = programtext
    programtext = temptext
  endif
  
  
  textline = programtext[xy[1]] 
  textline = strmid(textline,0,xy[0]) + insertText + strmid(textline,xy[0],strlen(textline)-xy[0])
  outText = programText
  outText[xy[1]] = textline
  
  return,outText
  
end

PRO thm_ui_calculate_event, event

  Compile_Opt hidden,idl2

  Widget_Control, event.TOP, Get_UValue=state, /No_Copy

      ;Put a catch here to insure that the state remains defined

  err_xxx = 0
  Catch, err_xxx
  IF (err_xxx NE 0) THEN BEGIN
    Catch, /Cancel
    Help, /Last_Message, Output = err_msg
    if is_struct(state) then begin
      FOR j = 0, N_Elements(err_msg)-1 DO state.historywin->update,err_msg[j]
      x=state.gui_id
      histobj=state.historywin
    endif
    Print, 'Error--See history'
    ok=error_message('An unknown error occured and the window must be restarted. See console for details.',$
       /noname, /center, title='Error in Calculate')
    Widget_Control, event.TOP, Set_UValue=state, /No_Copy
    widget_control, event.top,/destroy
    if widget_valid(x) && obj_valid(histobj) then begin 
      thm_gui_error,x,histobj
    endif
    RETURN
  ENDIF

      ;kill request block

  IF(Tag_Names(event, /Structure_Name) EQ 'WIDGET_KILL_REQUEST') THEN BEGIN  
    Exit_Sequence:
    widget_control,state.programText,get_value=text
    state.settings->setProperty,text=text,name=state.programName,path=state.programPath
    state.historyWin->update,'Calculate Widget Killed'
    if obj_valid(state.insertTree) then begin
      *state.treeCopyPtr = state.insertTree->getCopy() 
    endif 
    Widget_Control, event.TOP, Set_UValue=state, /No_Copy    
    Widget_Control, event.top, /Destroy
    RETURN      
  ENDIF

  IF(Tag_Names(event, /Structure_Name) EQ 'WIDGET_TAB') THEN BEGIN  
    widget_control,state.programText,get_value=text
    state.settings->setProperty,text=text,name=state.programName,path=state.programPath
    Widget_Control, event.TOP, Set_UValue=state, /No_Copy
    RETURN 
  ENDIF

  Widget_Control, event.id, Get_UValue=uval

  IF Size(uval, /Type) NE 0 THEN BEGIN

  state.historywin->update,'THM_UI_CALCULATE: User value: '+uval  ,/dontshow

  CASE uval OF
    'OK': begin
      state.historyWin->update,'Calculate Widget Closed'
      widget_control,state.programText,get_value=text
      state.settings->setProperty,text=text,name=state.programName,path=state.programPath
      if obj_valid(state.insertTree) then begin
        *state.treeCopyPtr = state.insertTree->getCopy() 
      endif 
      Widget_Control, event.TOP, Set_UValue=state, /No_Copy    
      Widget_Control, event.top, /Destroy
      RETURN
    end
    'INSERT': begin

      selection = state.insertTree->getValue()
    
      if is_string(selection[0]) then begin          
        Widget_control,state.programtext,set_value=thm_ui_calculate_insert_text(state.programText,'"'+selection+'"',state.offset)
      end
          
    end
    'FUNCTION': begin

      Widget_control,state.programtext,set_value=thm_ui_calculate_insert_text(state.programText,state.functions[event.index],state.offset)
    
    end
    'OPERATOR': begin

      Widget_control,state.programtext,set_value=thm_ui_calculate_insert_text(state.programText,state.operators[event.index],state.offset)
    
    
    end
    'CONSTANT': begin

      widget_control,state.constant,get_value=cvalue

      Widget_control,state.programtext,set_value=thm_ui_calculate_insert_text(state.programText,cvalue[event.index],state.offset)
        
    end
    'RUN': begin 
      Widget_control,state.programtext,get_value=programtext
     
      thm_ui_run_calc,programtext,state.loadedData,state.historyWin,state.statusBar,fail=err
 
      widget_control,state.programLabel,set_value=state.programName
      state.insertTree->update
           
      if ~keyword_set(err) then begin
        state.call_sequence->addCalcOp,programText
     ;   ok = dialog_message('Calculation complete',/information,/center)
        state.statusBar->update,'Calculation complete'
        state.historyWin->update,'Calculation complete'
      endif else begin
      
        state.historyWin->update,'Calculation Failed.  Error Follows:'
        printdat,err,output=o
        for i = 0,n_elements(o)-1 do begin
          state.historyWin->update,o[i]
        endfor
        
        if in_set('VALUE',tag_names(err)) then begin
          state.statusBar->update,'Calculation failed with error: ' + err.name + '  :  ' + err.value[0] + '. Check history for more detail.'
          for i = 0,n_elements(err.value)-1 do begin
            state.historyWin->update,'VALUE:' + err.value[i]
          endfor
        endif else begin
          state.statusBar->update,'Calculation failed with error: ' + err.name + '. Check history for more detail.'
        endelse
        
      endelse      
       
     ; Widget_Control, event.TOP, Set_UValue=state, /No_Copy    
    end
    'TEXT': begin
     
      ;keep track of cursor location
      
      ;only store legitimate cursor locations
      xy = widget_info(state.programtext,text_offset_to_xy=event.offset)
      
      if n_elements(xy) eq 2 && $
         xy[0] ge 0 && $
         xy[1] ge 0 then begin
        state.offset = event.offset
      endif
 
    end
    'OPEN': begin
      
      if state.programPath ne '' && state.programName ne '-scratch-' then begin
        file = dialog_pickfile(path=state.programPath,get_path=path, filter = '*.txt')
      endif else begin
        file = dialog_pickfile(get_path=path, filter = '*.txt')
      endelse
       
      ;if the file is not a regular file and it is not a new file
      if ~file_test(file,/regular) && file_test(file) then begin
         
        result = dialog_message('Illegal file type selected, please try again',/center)
      
      endif else if file ne  '' then begin
       
        sep = path_sep()
        files = strsplit(file,sep,/extract)
         
        state.programPath = path
        state.programName = files[n_elements(files)-1]
        
        catch,err
          
        if err then begin
          ok = error_message('Error reading file: ' + state.programName, /center, $
              title='Error in Calculate')
          state.statusBar->update,'Error reading file: ' + state.programName
          state.historyWin->update,'Error reading file: ' + state.programName
          close,lun
          free_lun,lun
        endif else begin
        
          ln_num = file_lines(state.programPath+state.programName)
        
          if ln_num gt 0 then begin
        
            inlines = strarr(ln_num)
        
            get_lun,lun     
            openr,lun,state.programPath+state.programName
            readf,lun,inlines
            close,lun
            free_lun,lun
            
          endif
          
          state.statusBar->update,'Displaying file: ' + state.programName
          state.historyWin->update,'Displaying file: ' + state.programName
          state.offset = 0
          widget_control,state.programLabel,set_value=state.programName
          widget_control,state.programText,set_value=inlines
      
        endelse
        
        catch,/cancel
      
      endif
    
    end
    'HELP':BEGIN

      gethelppath,path
      xdisplayfile,path+'thm_ui_calculate.txt' , group=state.tlb, /modal, done_button='Done', $
                    title='HELP: Calculate Window'
    END
    
    'SAVE': begin
   
      if (state.programPath + state.programName) eq '-scratch-' then begin
      
     ;   ok=dialog_message('Scratch file not a valid save destination, please select a file name.',/center)
      
        state.statusBar->update,'Scratch file not a valid save destination, please select a file name.'
      
        ;file = dialog_pickfile(get_path = path, filter = '*.txt', /write, default_extension='*.txt')
        file = dialog_pickfile(Title='Save Calculation File:', get_path=path, $
             Filter='*.txt', /Write, Dialog_Parent=state.tlb, default_extension='*.txt')
                 
        if file eq '' || $
           path eq '' then begin
           
           state.statusBar->update,'File Save Canceled' 
           
           Widget_Control, event.TOP, Set_UValue=state, /No_Copy
           
           return
             
        endif
             
        sep = path_sep()
        files = strsplit(file,sep,/extract)       
        state.programPath = path
        state.programName = files[n_elements(files)-1]
        
        widget_control,state.programLabel,set_value=state.programName
      
      endif
      
      result = "No"
      
      while result eq "No" && file_test(state.programPath+state.programName) do begin
      
        result = dialog_message('File: ' + state.programName + ' already exists. Are you sure you want to overwrite file?',/question,/center)
      
        if result eq "No" then begin
        
          if state.programPath ne '' && state.programName ne '-scratch-' then begin
            file = dialog_pickfile(path=state.programPath,get_path=path, filter = '*.txt', default_extension='*.txt', /write)
          endif else begin
            file = dialog_pickfile(get_path=path, filter = '*.txt', default_extension='*.txt', /write)
          endelse
          
          if file eq '' || $
            path eq '' then begin
            
            state.statusBar->update,'File Save Canceled' 
             
            Widget_Control, event.TOP, Set_UValue=state, /No_Copy
            return
             
         endif
        
          sep = path_sep()
          files = strsplit(file,sep,/extract)       
          state.programPath = path
          state.programName = files[n_elements(files)-1]
          widget_control,state.programLabel,set_value=state.programName
        endif
      
      endwhile
      
      widget_control,state.programText,get_value=text
      
      catch,err
      
      get_lun,lun
      openw,lun,state.programPath+state.programName
      
      if err then begin
        ok = error_message('Error writing file: ' + state.programName,/center,title='Error in Calculate')
        state.statusBar->update,'Error writing file:'+state.programName
        state.historyWin->update,'Error writing file:'+state.programName
        close,lun
        free_lun,lun
      endif else begin
        
        for i = 0,n_elements(text)-1 do begin
          printf,lun,text[i]
        endfor
        
        close,lun
        free_lun,lun
        
      endelse
      
      catch,/cancel
    
    end
     
    ELSE:
      
    ;add rest of cases here
  ENDCASE
  ENDIF
  
  Widget_Control, event.TOP, Set_UValue=state, /No_Copy
  
  RETURN
END ;--------------------------------------------------------------------------------



Pro thm_ui_calculate, gui_id,loadedData,settings,historywin,treeCopyPtr,call_sequence, $
                      drawObject,windowStorage,scrollbar

  xsize = 360
  ysize = 380

      ;master widget
      
  if ~obj_valid(settings) then begin
    ok = error_message('ERROR: Calculate panel passed illegal settings',/center, $
         title='Error in Calculate')
    return
  endif
  
  err_xxx = 0
  Catch, err_xxx
  IF(err_xxx Ne 0) THEN BEGIN
    Catch, /Cancel
    Help, /Last_Message, Output=err_msg
    FOR j = 0, N_Elements(err_msg)-1 DO historywin->update,err_msg[j]
    Print, 'Error in Calculate Panel--See history'
    ok = error_message('An unknown error occured starting Calculate. See console for details.',$
         /noname, /center, title='Error in Calculate')   
    widget_control, tlb,/destroy
    thm_gui_error, gui_id, historywin
    RETURN
  ENDIF
   
  tlb = Widget_Base(/col, Title='THEMIS: Calculate ', Group_Leader=gui_id, $
    /Modal, /Floating,/tlb_kill_request_events, xpad=3)

  mainBase =  widget_base(tlb,/row,/base_align_left, frame=3)
  buttonBase = widget_base(tlb,/row,/align_center)
  statusBase = widget_base(tlb,/row,/align_center)
  col1base = widget_base(mainBase,/col,/base_align_center)
  col2base = widget_base(mainBase,/col,/base_align_left)
  col3base = widget_base(mainBase,/col,/base_align_left)
  col4base = widget_base(mainBase,/col,/base_align_left)  
  col1row1 = widget_base(col1base,/row, /align_left)
  col1row2 = widget_base(col1base,/row)
  col1row3 = widget_base(col1base,/row,/align_center)
;  col1row4 = widget_base(col1base,/row)
  col2row1 = widget_base(col2base,/row, /align_left)
  col2row2 = widget_base(col2base,/row)
  col2row3 = widget_base(col2base,/row,/base_align_left)
  col3row1 = widget_base(col3base,/row)
  col3row2 = widget_base(col3base,/row)
  col3row3 = widget_base(col3base,/row)
  col3row4 = widget_base(col3base,/row)
  col3row5 = widget_base(col3base,/row)
  col4row1 = widget_base(col4base,/row)
  col4row2 = widget_base(col4base,/row)
 
  settings->getProperty, $
       name=programName, $
       path=programPath, $
       text=programText

  programLabel = Widget_Label(col1row1, Value='Program Area: ')
  programLabel = Widget_Label(col1row1, Value = strjoin(replicate(' ',20))+ programName + strjoin(replicate(' ',20)))
  ; this will be changed to a string array of tplot_names
;  ;tplot_names, name=insertValue
  ;insertValue=tnames()        ;Does not have limit on length of name like TPLOT_NAMES.

  fieldNames = loadedData->getAll()
  if ~is_string(fieldNames) then begin
    fieldNames = ['none']
    fieldPtr = ptr_new()
  endif else begin
    fieldPtr = ptr_new(fieldNames)
  endelse
    
  insertLabel = Widget_Label(col2row1, Value='Insert Field: ',/Align_Left)
  insertTree = obj_new('thm_ui_widget_tree',col2row2,'INSERT',loadedData,xsize=xsize,ysize=ysize,mode=3,multi=0,leafonly=1,showdatetime=1)
  insertTree->update,from_copy=*treeCopyPtr
 
  progText = Widget_Text(col1row2, Value=programText, /Editable, XSize=floor(xsize/(!D.X_CH_SIZE)),ysize=floor(ysize/(!D.Y_CH_SIZE+4)), /Scroll, Uvalue='TEXT',/all_events)
;  progText = Widget_Text(col1row2, Value=programText, /Editable, xsize=xsize, ysize=ysize, /Scroll, Uvalue='TEXT',/all_events)

  calc, function_list=functionNames, operator_list=operatorNames 
  functionLabel= Widget_Label(col3row1, Value='Insert Function: ')
  functionList = Widget_List(col3row2, Value=functionNames, xsize=27, ysize=13, uval='FUNCTION')

  operatorLabel= Widget_Label(col3row3, Value='Insert Operator: ')
  operatorList = Widget_List(col3row4, Value=operatorNames, xsize=27, ysize=13, uval='OPERATOR')

  constantValue = ['pi', 'e']
  constlabel = widget_label(col3row5, value = 'Insert Constant: ')
  constant_droplist = Widget_combobox(col3row5, Value=constantValue, $
  UValue='CONSTANT') 
  
  statusBar = obj_new('thm_ui_message_bar',statusBase,xsize=143,ysize=1)
  
  newButton = Widget_Button(col1row3, Value=' Open ', UValue = 'OPEN', xsize=70)
  saveButton = Widget_Button(col1row3, Value='  Save  ', UValue = 'SAVE', xsize=70)
  runButton = Widget_Button(col1row3, Value=' Run ', UValue = 'RUN', xsize=70)                                                                       
  okButton = Widget_Button(buttonBase, Value=' Done ', UValue='OK', xsize=70)
  helpButton = Widget_Button(buttonBase, Value='Help', XSize=85, UValue='HELP', $
                            tooltip='Open Help Window.')
        
  statusBar->update,'Calculate opened.  Displaying File: ' + programName
  historyWin->update,'Calculate opened.  Displaying File: ' + programName
  
  state = {tlb:tlb, $
           gui_id:gui_id, $
           programtext:progtext, $
           programLabel:programLabel,$
           programPath:programPath,$
           programName:programName,$
           insertTree:insertTree,$
           functions:functionNames,$
           operators:operatorNames,$
           constant:constant_droplist, $
           loadedData:loadedData, $
           offset:0,$
           historywin:historywin, $
           statusBar:statusBar,$
           settings:settings, $
           treeCopyPtr:treeCopyPtr, $
           call_sequence:call_sequence $
           }

  Widget_Control, tlb, Set_UValue = state, /No_Copy
  
  centerTLB,tlb
  
  Widget_Control, tlb, /Realize
  
  ;print,tlb
  ;print,col1row2
  
  XManager, 'thm_ui_calculate', tlb, /No_Block

  ;Update the draw object to refresh any plots
  drawobject->update, windowStorage, loadeddata
  drawobject->draw
  scrollbar->update
  

  historyWin->update,'Calculate panel closed'

  RETURN
END ;--------------------------------------------------------------------------------
