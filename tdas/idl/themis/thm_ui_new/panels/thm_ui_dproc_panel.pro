

;helper function for setting active data/determining tree selection
pro thm_ui_dproc_panel_get_tree_select, state, sobj

    compile_opt idl2, hidden

  widget_control,state.loadedlist,get_value=treeobj
  treeptrarr=treeobj->GetValue()
  if ~is_num(treeptrarr[0]) then begin
      If(ptr_valid(state.data_choice)) Then ptr_free, state.data_choice
      treelist=(*(treeptrarr[0])).groupname               ;Concatenate data names.
      n_treeptrarr=n_elements(treeptrarr)
      if n_treeptrarr gt 1 then begin
        for i=1,n_treeptrarr-1 do treelist=[treelist,(*(treeptrarr[i])).groupname]
      endif
      state.data_choice=ptr_new(treelist)
      sobj -> update, 'Variables chosen: '+strjoin(*state.data_choice, ',', /single)
      state.info.historywin-> update, 'Data Processing: Variables chosen: '+strjoin(*state.data_choice, ',', /single)
  Endif Else begin
    ptr_free, state.data_choice
    sobj -> update, 'Bad Selection, Please try again'
    state.info.historywin-> update, 'Data Processing: Bad Selection, Please try again'
  endelse

end

; helper function for determining the selected active data
pro thm_ui_dproc_panel_get_active_select, state, sobj

    compile_opt idl2, hidden

  pindex = widget_info(state.activelist, /list_select)
  If(pindex[0] Ne -1) Then Begin
      If(ptr_valid(state.act_data)) Then Begin
          If(ptr_valid(state.data_choice)) Then ptr_free, state.data_choice
          state.data_choice = ptr_new((*state.act_data)[pindex])
          sobj -> update, 'Variables chosen: '+strjoin(*state.data_choice, ',', /single)
          state.info.historywin-> update, 'Data Processing: Variables chosen: '+strjoin(*state.data_choice, ',', /single)
      Endif Else begin
        sobj -> update, 'No Active Data Available'
        state.info.historywin-> update, 'Data Processing: No Active Data Available'
      endelse
  Endif Else begin
    sobj -> update, 'Bad Selection, Please try again'
    state.info.historywin-> update, 'Data Processing: Bad Selection, Please try again'
  Endelse
end


;+
;NAME:
; thm_ui_dproc_panel
;PURPOSE:
; A widget interface that can be used to set the active data variables
; and perform various data analysis tasks.
;CALLING SEQURNCE:
; thm_ui_dproc_panel, gui_id
;INPUT:
; gui_id = the id of the calling widget
;OUTPUT:
; No explicit output, processes are done
;HISTORY:
; 21-nov-2008, jmm, jimm@ssl.berkeley.edu
;$LastChangedBy: pcruce $
;$LastChangedDate: 2010-02-10 11:08:23 -0800 (Wed, 10 Feb 2010) $
;$LastChangedRevision: 7261 $
;$URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/thmsoc/tags/tdas_5_21/idl/themis/thm_ui_new/panels/thm_ui_dproc_panel.pro $
;-
Pro thm_ui_dproc_panel_event, event

 ; print, ''
;Catch here to insure that the state remains defined
  err_xxx = 0
  Catch, err_xxx
  IF (err_xxx NE 0) THEN BEGIN
    Catch, /Cancel
    Help, /Last_Message, Output = err_msg
    if is_struct(state) then begin
      FOR j = 0, N_Elements(err_msg)-1 DO state.info.historywin->update,err_msg[j]
      x=state.gui_id
      histobj=state.info.historywin
      if obj_valid(state.treeObj) then begin
        *state.treeCopyPtr = state.treeObj->getCopy() 
      endif  
    endif
    Print, 'Error--See history'
    ok=error_message('An unknown error occured and the window must be restarted. See console for details.',$
       /noname, /center, title='Error in Data Processing')
    If(is_struct(state)) Then Widget_Control, event.TOP, Set_UValue=state, /No_Copy
    widget_control, event.top,/destroy
    if widget_valid(x) && obj_valid(histobj) then begin
      thm_gui_error,x,histobj
    endif
    RETURN
  ENDIF

widget_control, /hourglass

;Get info from state, and data object from gui_id
widget_control, event.top, get_uvalue = state, /no_copy
dproc_id = state.master
dobj = state.info.loadeddata
sobj_main = state.info.statusbar
sobj = state.statusbar
historywin = state.info.historywin
;kill request block
If(TAG_NAMES(event, /STRUCTURE_NAME) EQ 'WIDGET_KILL_REQUEST') Then Begin
    historywin-> update, 'Kill request on Data Processing Panel'
    if obj_valid(state.treeObj) then begin
      *state.treeCopyPtr = state.treeObj->getCopy() 
    endif  
    widget_control, event.top, set_uvalue = state, /no_copy
    widget_control, event.top, /destroy
    sobj_main -> update, 'Data Processing Panel killed'
    Return
Endif
If(obj_valid(dobj) Eq 0) Then message, 'No valid data object'
;now deal with the event
widget_control, event.id, get_uvalue = uval
SPL = strsplit(uval, ':', /extract)
If(spl[0] Eq 'PROC') Then Begin
    ptree = ptr_new(state.treeobj->getcopy())
    otp = thm_ui_new_dproc(state.info, spl[1], out_string = out_string, $
                           ext_statusbar = sobj, group_leader = state.master, $
                           ptree = ptree)
   ; os = strjoin(out_string, ', ')
   ; if ~stregex(os, '^ *$', /bool) then sobj -> update, os
   ; state.info.historywin-> update, os
    if spl[1] eq 'INTERPOL' then begin
      state.treeobj->update,from_copy=*ptree
    endif
    ptr_free, ptree
    widget_control, event.top, set_uvalue = state, /no_copy
    thm_ui_dproc_reset_act_data, dproc_id, /update_tree
Endif Else If(spl[0] Eq 'COTRANS') Then Begin
    active = state.info.loadedData->getActive(/parent)
    thm_ui_cotrans_new, event.top,spl[1],active,state.info.loadedData, sobj,historywin
    state.info.windowStorage->getProperty,callSequence=callSequence
    callSequence->addCotransOp,spl[1],active
    widget_control, event.top, set_uvalue = state, /no_copy
    thm_ui_dproc_reset_act_data, dproc_id, /update_tree
Endif Else Begin
    state.info.historywin->update,'THM_UI_DPROC_PANEL: User Value: '+uval,/dontshow
    sobj->update,string('THM_UI_DPROC_PANEL: User Value: '+uval)
    Case uval of
        'CANC': Begin
            state.info.historywin-> update, 'Closing Data Processing Panel'
            if obj_valid(state.treeObj) then begin
              *state.treeCopyPtr = state.treeObj->getCopy() 
            endif 
            widget_control, event.top, set_uvalue = state, /no_copy
            widget_control, event.top, /destroy
            sobj_main -> update, 'Data Processing Panel Closed'
            Return
        End
        'DATALOADLIST': Begin
          thm_ui_dproc_panel_get_tree_select, state, sobj
        End
        'ACTIVELIST': Begin
          thm_ui_dproc_panel_get_active_select, state, sobj
        End
        'CLEAR':Begin
            dobj -> clearallactive
            sobj -> update, 'All Active variables cleared'
            state.info.historywin-> update, 'All Active variables cleared'
            thm_ui_dproc_reset_act_data, state
        End
        'SETACTIVE':Begin
            thm_ui_dproc_panel_get_tree_select, state, sobj
            If(ptr_valid(state.data_choice)) Then Begin
                dc = *state.data_choice
                For j = 0, n_elements(dc)-1 Do dobj -> setactive, dc[j]
                ptr_free, state.data_choice
                thm_ui_dproc_reset_act_data, state
                sobj -> update, 'Variables set to active: '+strjoin(dc, ',', /single)
                state.info.historywin-> update, 'Variables set to active: '+strjoin(dc, ',', /single)
            Endif Else sobj -> update, 'No Data has been selected'
        End
        'UNSETACTIVE':Begin
            thm_ui_dproc_panel_get_active_select, state, sobj
            If(ptr_valid(state.data_choice)) Then Begin
                dc = *state.data_choice
                For j = 0, n_elements(dc)-1 Do dobj -> clearactive, dc[j]
                ptr_free, state.data_choice
                thm_ui_dproc_reset_act_data, state
                sobj -> update, 'Variables set to inactive: '+strjoin(dc, ',', /single)
                state.info.historywin-> update, 'Variables set to inactive: '+strjoin(dc, ',', /single)
            Endif Else begin
              sobj -> update, 'No Data has been selected'
              state.info.historywin-> update, 'No Data has been selected'
            endelse
        End
;        'PARTSPEC': BEGIN
;            widget_control,state.loadedlist,get_value=treeobj
;            widget_control, event.top, set_uvalue = state, /no_copy    
;            thm_ui_part_getspec_options, dproc_id,treeobj
;            widget_control, event.top, get_uvalue = state, /no_copy
;        END
    Endcase
    widget_control, event.top, set_uvalue = state, /no_copy
Endelse
Return
End

Pro thm_ui_dproc_panel, info

err_xxx = 0
catch, err_xxx
If(err_xxx Ne 0) Then Begin
    catch, /cancel
    Help, /Last_Message, Output=err_msg
    FOR j = 0, N_Elements(err_msg)-1 DO begin
      Print, err_msg[j]
      info.historywin->update,err_msg[j]
    endfor
    Print, 'Error--See history'

    ok = error_message('An unknown error occured starting Data Processing. See console for details.', $
                       traceback=0, /noname, /center, title='Error in Data Processing')
    widget_control,master,/destroy
    thm_gui_error, info.master, info.historywin
    Return
Endif

gui_id = info.master
master = widget_base(/col, title = 'THEMIS: Data Processing ', $
                     group_leader = gui_id, /floating, /tlb_kill_request_events)

;This base widget will hold the lists of loaded and active data:
listsbase = widget_base(master, /row)
loadedbase = widget_base(listsbase, /column)
addbase = widget_base(listsbase, /column, ypad=140)
activebase = widget_base(listsbase, /column, ypad=5)
analysismenu = widget_base(listsbase, /column)

;This base will hold the buttons on the bottom:
buttonbase = widget_base(master, /row, /align_center)

; Create Status Bar Object
statusBar = Obj_New('THM_UI_MESSAGE_BAR', Value = 'Status information is displayed here.', master, Xsize = 80, YSize = 1)

loadedLabel = WIDGET_LABEL(loadedBase, value='Loaded Data')

;get the loaded data and active data:
state = {master:master, gui_id:gui_id, info:info, $
         val_data:ptr_new(), data_choice:ptr_new(), $
         act_data:ptr_new(), val_data_t:ptr_new(), $
         act_data_t:ptr_new(), treeobj:obj_new(), $
         activelist:-1L,loadedlist:-1L,$
         statusbar:statusbar,treeCopyPtr:info.guiTree}

thm_ui_dproc_reset_act_data, state, /update_tree ;this sets up the data lists

screen_size = get_screen_size()
xtree_size = min([300,floor((screen_size[0]/4.5))])
ytree_size = min([300,floor((screen_size[1]/3.5))])
state.treeobj = obj_new('thm_ui_widget_tree', loadedbase, 'DATALOADLIST', $
                        info.loadeddata, xsize = xtree_size, ysize = ytree_size, mode = 0, $
                        /multi, uname = 'dataloadlist', /showdatetime)
                        
state.treeObj->update,from_copy=*state.treeCopyPtr
state.loadedlist=widget_info(master,find_by_uname='dataloadlist')

;set up the buttons to add, subtract active data as in the load data widget
getresourcepath,rpath
leftArrow = read_bmp(rpath + 'arrow_180_medium.bmp', /rgb)
rightArrow = read_bmp(rpath + 'arrow_000_medium.bmp', /rgb)

thm_ui_match_background, master, leftArrow
thm_ui_match_background, master, rightArrow

;plusbmp = filepath('shift_right.bmp', SubDir = ['resource', 'bitmaps'])
;minusbmp = filepath('shift_left.bmp', SubDir = ['resource', 'bitmaps'])
addButton = Widget_Button(addBase, Value = rightArrow, /Bitmap,  UValue = 'SETACTIVE', $
                          ToolTip = 'Set Selected Data to Active')
minusButton = Widget_Button(addBase, Value = leftArrow, /Bitmap, $
                            Uvalue = 'UNSETACTIVE', $
                            ToolTip = 'Unset selected data from Active')
activeLabel = Widget_Label(activeBase, value='Active Data')

state.activelist = widget_list(activebase, value = *state.act_data_t, $
                               uvalue = 'ACTIVELIST', $
                               XSize = 55, YSize = 22, /multiple)

;buttons along the bottom
clear_button = Widget_Button(buttonBase, Value = ' Clear Active ', XSize = 85, $
                             UValue = 'CLEAR')
; Analysis Pull Down Menu, hacked from thm_gui_new.pro
;analysisMenu = Widget_Button(buttonbase, Value='Analysis ',/menu
;xsize = 85)
subAvgMenu = Widget_Button(analysisMenu, Value='Subtract Average ', $
                           UValue='PROC:SUBAVG', tooltip = 'Subtracts average of each trace')
subMedMenu = Widget_Button(analysisMenu, Value='Subtract Median ', $
                           UValue='PROC:SUBMED', tooltip = 'Subtracts median of each trace from each trace')
smoothMenu = Widget_Button(analysisMenu, Value='Smooth Data... ', UValue='PROC:SMOOTH', $
                          tooltip = 'Smooths data in time, using input resolution')
hpfiltMenu = Widget_Button(analysisMenu, Value='High Pass filter... ', UValue='PROC:HPFILT', $
                          tooltip = 'Subtracts smoothed values from each trace')
blkAvgMenu = Widget_Button(analysisMenu, Value='Block Average... ', UValue='PROC:BLKAVG',$
                          tooltip = 'Block time average of each trace')
clipMenu = Widget_Button(analysisMenu, Value='Clip... ', UValue='PROC:CLIP', $
                         tooltip = 'Clips traces at input max and min values')
deflagMenu = Widget_Button(analysisMenu, Value='Deflag... ', UValue='PROC:DEFLAG', $
                           tooltip = 'Replaces NaN values with interpolated or repeated values')
degapMenu = Widget_Button(analysisMenu, Value='Degap... ', UValue='PROC:DEGAP',$
                          tooltip = 'Fills data gaps with NaN values')
interpMenu = Widget_Button(analysisMenu, value='Interpolate...', uvalue='PROC:INTERPOL', $
                           tooltip = 'Performs interpolation on active data')
spikeMenu = Widget_Button(analysisMenu, Value='Clean Spikes ', UValue='PROC:SPIKE', $
                          tooltip = 'Replaces single-point spikes with NaN values')
derivMenu = Widget_Button(analysisMenu, Value='Time Derivative ', UValue='PROC:DERIV', $
                          tooltip = 'Time derivatives of each trace')
waveMenu = Widget_Button(analysisMenu, Value='Wavelet Transform...', UValue='PROC:WAVE', $
                         tooltip = 'Performs wavelet transform for each trace')
pwrspecMenu = Widget_Button(analysisMenu, Value='Power Spectrum...', UValue='PROC:PWRSPC', $
                         tooltip = 'Performs Dynamic power spectrum for each trace')
cotranMenu = Widget_Button(analysisMenu, Value='Coordinate Transform...', UValue='PROC:COTRAN', $
                         tooltip = 'Performs a coordinate transform on active data', /menu)
validCoords = ['DSL', 'SSL', 'GSE', 'GEI', 'SPG', 'GSM', 'GEO', 'SM']
coordMenus = LonArr(N_Elements(validCoords))
FOR i = 0, N_Elements(validCoords)-1 DO $
  coordMenus[i] = Widget_Button(cotranMenu, Value=string(validCoords[i]+'  '),UValue='COTRANS:'+validCoords[i])
;getspecMenu = Widget_Button(analysisMenu, Value='Get Particle Spectra...', UValue='PARTSPEC', $
;                            tooltip = 'Angular and/or energy spectra for ESA, SST')
;Dude...
cancel_button = Widget_Button(buttonBase, Value = '  Done   ', XSize = 85, $
                              UValue = 'CANC')
CenterTlb, master
Widget_Control, master, Set_UValue = state, /No_Copy
Widget_Control, master, /Realize
XManager, 'thm_ui_dproc_panel', master, /No_Block

End



