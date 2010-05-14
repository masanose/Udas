;+
;NAME:
; thm_ui_fileconfig
;
;PURPOSE:
; A widget that allows the user to set some of the fields in the
; !themis system variable: Also allows the user to set the themis
; configuration text file, and save it
;
;HISTORY:
; 17-may-2007, jmm, jimm@ssl.berkeley.edu
; 2-jul-2007, jmm, 'Add trailing slash to data directories, if necessary
; 5-may-2008, cg, removed text boxes and replaced with radio buttons or 
;                 pulldowns, fixed reset to default
;
;$LastChangedBy: pcruce $
;$LastChangedDate: 2009-08-03 10:19:35 -0700 (Mon, 03 Aug 2009) $
;$LastChangedRevision: 6515 $
;$URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/thmsoc/tags/tdas_5_21/idl/themis/thm_ui_new/panels/thm_ui_fileconfig.pro $
;--------------------------------------------------------------------------------

pro thm_ui_fileconfig_set_draw,state,renderer

  if renderer eq 0 && $
     strlowcase(!VERSION.os_family) eq 'windows' then begin
    retain = 2
  endif else begin
    retain = 1
  endelse

  *state.drawWin->getProperty,current_zoom=cz,virtual_dimensions=original_virtual_dimensions
  widget_control,*state.drawID,/destroy
  *state.drawID = WIDGET_DRAW(state.graphBase,/scroll,xsize=original_virtual_dimensions[0],ysize=original_virtual_dimensions[1],$
                            x_scroll_size=state.screenSize[0],y_scroll_size=state.screenSize[1], $
                            Frame=3, Motion_Event=1, /Button_Event,keyboard_events=2,graphics_level=2,$
                            renderer=renderer,retain=retain,/expose_events)
  widget_control,*state.drawID,get_value=drawWin
  
  ;replace the cursor on non-windows system
  ;The cursor also needs to be reset when a new window is created.
  ;ATM This only happens when switching between hardware & software render modes and on init
  if strlowcase(!version.os_family) ne 'windows' then begin
    thm_ui_set_cursor,drawWin
  endif
  
  *state.drawWin = drawWin
  state.drawObject->setProperty,destination=drawWin
  state.drawObject->setZoom,cz
  state.drawObject->draw
  
 ; drawWin->setCurrentZoom,cz
  !THM_GUI_NEW.renderer = renderer

end

pro thm_ui_fileconfig_update_index,state,index

  compile_opt idl2,hidden
  
  if index eq 0 then begin
    struct = !THEMIS
    thm_ui_fileconfig_update_struct,state,struct
    defsysv,'!THEMIS',struct
  endif else if index eq 1 then begin
    struct = !GOES
    thm_ui_fileconfig_update_struct,state,struct
    defsysv,'!GOES',struct
  endif else if index eq 2 then begin
    struct = !WIND
    thm_ui_fileconfig_update_struct,state,struct
    defsysv,'!WIND',struct
  endif else if index eq 3 then begin
    struct = !ISTP
    thm_ui_fileconfig_update_struct,state,struct
    defsysv,'!ISTP',struct 
  endif
;ace unused currently
;   else if index eq 4 then begin
;    struct = !ACE
;    thm_ui_fileconfig_update_struct,state,struct
;    defsysv,'!ACE',struct 
;  endif
end

pro thm_ui_fileconfig_init_index,state,index

  compile_opt idl2,hidden

  if index eq 0 then begin
    thm_ui_fileconfig_init_struct,state,!THEMIS
  endif else if index eq 1 then begin
    thm_ui_fileconfig_init_struct,state,!GOES
  endif else if index eq 2 then begin
    thm_ui_fileconfig_init_struct,state,!WIND
  endif else if index eq 3 then begin
    thm_ui_fileconfig_init_struct,state,!ISTP
  endif
; ace config currently unused
;   else if index eq 4 then begin
;    thm_ui_fileconfig_init_struct,state,!ACE
;  endif
end

pro thm_ui_fileconfig_update_struct,state,struct

  compile_opt idl2,hidden
  
  widget_control,state.localdir,get_value=local_data_dir
  local_data_dir = strtrim(local_data_dir, 2)
  last = strmid(local_data_dir, strlen(local_data_dir)-1, 1)
  If(last Ne '/' && last Ne '\') Then local_data_dir = local_data_dir+'/'
  
  struct.local_data_dir = local_data_dir
  
  widget_control,state.remotedir,get_value=remote_data_dir
  remote_data_dir = strtrim(remote_data_dir, 2)
  last = strmid(remote_data_dir, strlen(remote_data_dir)-1, 1)
  If(last Ne '/' && last Ne '\') Then remote_data_dir = remote_data_dir+'/'
  
  struct.remote_data_dir = remote_data_dir
  
  struct.no_download = widget_info(state.nd_off_button,/button_set)
  
  struct.no_update = widget_info(state.nu_off_button,/button_set)
  
  struct.downloadonly = widget_info(state.ndo_off_button,/button_set)
  
  struct.verbose = long(widget_info(state.v_droplist,/combobox_gettext))
  
end

pro thm_ui_fileconfig_init_struct,state,struct

  compile_opt idl2,hidden
  
  widget_control,state.localdir,set_value=struct.local_data_dir
  widget_control,state.remotedir,set_value=struct.remote_data_dir
  
  if struct.no_download eq 1 then begin
    widget_control,state.nd_off_button,set_button=1
  endif else begin
    widget_control,state.nd_on_button,set_button=1
  endelse
  
  if struct.no_update eq 1 then begin
    widget_control,state.nu_off_button,set_button=1
  endif else begin
    widget_control,state.nu_on_button,set_button=1
  endelse
  
  if struct.downloadonly eq 1 then begin
    widget_control,state.ndo_off_button,set_button=1
  endif else begin
    widget_control,state.ndo_on_button,set_button=1
  endelse
  
  widget_control,state.v_droplist,set_combobox_select=struct.verbose

end

PRO thm_ui_fileconfig_event, event

    ;get the user value of the widget that caused this event
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
       /noname, /center, title='Error in File Config')
    Widget_Control, event.TOP, Set_UValue=state, /No_Copy
    widget_control, event.top,/destroy
    if widget_valid(x) && obj_valid(histobj) then begin 
      thm_gui_error,x,histobj
    endif
    RETURN
  ENDIF
    
  Widget_Control, event.id, Get_UValue = uval
  widget_control, event.top, get_uval = state, /no_copy
  
  CASE uval OF
    'CONFIGSELECT': Begin
      thm_ui_fileconfig_update_index,state,state.current_id
      thm_ui_fileconfig_init_index,state,event.index
      if event.index eq 0 then begin
        widget_control,state.gr_base,sensitive=1
      endif else begin
        widget_control,state.gr_base,sensitive=0
      endelse
      state.current_id = event.index
    end
    'GRHARD':Begin
       if !THM_GUI_NEW.renderer eq 1 then begin     
         thm_ui_fileconfig_set_draw,state,0        
       endif
    end
    'GRSOFT':BEGIN
       if !THM_GUI_NEW.renderer eq 0 then begin
         thm_ui_fileconfig_set_draw,state,1
       endif
    end
    'RESET': Begin

      if state.current_id eq 0 then begin
      
        !thm_gui_new.renderer = state.thm_ui_cfg_sav.renderer
          
        if !thm_gui_new.renderer eq 0 then begin
          widget_control,state.gr_hard_button,/set_button
          thm_ui_fileconfig_set_draw,state,0
        endif else begin
          widget_control,state.gr_soft_button,/set_button
          thm_ui_fileconfig_set_draw,state,1
        endelse
      endif 
      
      defsysv,'!'+state.config_names[state.current_id],state.init_config[state.current_id]
            
      thm_ui_fileconfig_init_index,state,state.current_id 
    End
   'RESETTODEFAULT': Begin

      if state.current_id eq 0 then begin
        thm_init,  /reset
        !themis.no_download = state.def_values[0]
        !themis.no_update = state.def_values[1]      
        !themis.downloadonly = state.def_values[2]
        !themis.verbose = state.def_values[3]
        
        !thm_gui_new.renderer = 1
        widget_control,state.gr_soft_button,/set_button
        thm_ui_fileconfig_set_draw,state,1
        
        state.init_config[0] = !themis
        state.thm_ui_cfg_sav = !thm_gui_new
        
      endif else if state.current_id eq 1 then begin
        goes_init,/reset
        state.init_config[1] = !goes
      endif else if state.current_id eq 2 then begin
        wind_init,/reset
        state.init_config[2] = !wind
      endif else if state.current_id eq 3 then begin
        istp_init,/reset
        state.init_config[3] = !istp
      endif

; ace config currently unused in gui      
;       else if state.current_id eq 4 then begin
;        ace_init,/reset
;        state.init_config[3] = !ace
;      endif   
      
      thm_ui_fileconfig_init_index,state,state.current_id 
      
    End
    'SAVE': Begin
      config = widget_info(event.top,find_by_uname='configselect')
      current = widget_info(config,/combobox_gettext)
      if current eq 'THEMIS' then begin
        thm_ui_fileconfig_update_index,state,state.current_id
        thm_write_config
        state.statusBar->update,'Saved thm_config.txt'
        state.historyWin->update,'Saved thm_config.txt'
      endif else if current eq 'GOES' then begin
        thm_ui_fileconfig_update_index,state,state.current_id
        goes_write_config
        state.statusBar->update,'Saved goes_config.txt'
        state.historyWin->update,'Saved goes_config.txt'
      endif else begin
        state.statusBar->update,'Config Save Not Set Implemented For: ' + current
        state.historyWin->update,'Config Save Not Set Implemented For: ' + current
      endelse
    End
    'EXIT': Begin
       thm_ui_fileconfig_update_index,state,state.current_id
       widget_control, event.top, /destroy
       return
     ;disabled, widget destroy is done after xmanager termination
    End
    Else:
  Endcase
  
  widget_control, event.top, set_uval = state, /no_copy

Return
END ;--------------------------------------------------------------------------------



PRO thm_ui_fileconfig, gui_id,drawId,drawWin,drawObject,screenSize,graphBase,historyWin

;If init structs do not exist, then create them
  thm_init
  goes_init
 ;
 ;ace config currently unused by gui
 ; ace_init
  istp_init
  wind_init
  
 ; config_names = ['THEMIS','GOES','WIND','ISTP','ACE']
 ;  init_config = [!themis,!goes,!wind,!istp,!ace]
 
  config_names = ['THEMIS','GOES','WIND','ISTP']
  init_config = [!themis,!goes,!wind,!istp]
  

  
  thm_ui_cfg_sav = !thm_gui_new

;Build the widget
  master = widget_base(/col, $
                       title = 'Configuration Settings', $
                       /align_top, group_leader = gui_id, $
                       /modal, /floating)
;widget base for values to set
  vmaster = widget_base(master, /col, /align_left, frame=5)
  top = widget_base(vmaster,/row)
  vlabel = widget_label(top, value = 'Configuration Settings:     ')
  vcombo = widget_combobox(top,value=config_names,uvalue='CONFIGSELECT',uname='configselect')

;Widget base for save, reset and exit buttons
  bmaster = widget_base(master, /row, /align_center)
  ll = max(strlen([!themis.local_data_dir, !themis.remote_data_dir]))+12
;Now create directory text widgets

  configbase = widget_base(vmaster,/col,/frame)

  lbase = widget_base(configbase, /row, /align_left)
  localdir = widget_text(lbase, /edit, /all_events, xsiz = ll, $
                         uval = 'LOCALDIR', val = !themis.local_data_dir)
  flabel = widget_label(lbase, value = 'Local data directory')
  rbase = widget_base(configbase, /row, /align_left)
  remotedir = widget_text(rbase, /edit, /all_events, xsiz = ll, $
                          uval = 'REMOTEDIR', val = !themis.remote_data_dir)
  flabel = widget_label(rbase, value = 'Remote data directory')
;Next radio buttions
  nd_base = widget_base(configbase, /row, /align_left)
  nd_label = widget_label(nd_base, value='Download Data: ')
  nd_buttonbase = widget_base(nd_base, /exclusive, /row, uval="ND")
  nd_on_button = widget_button(nd_buttonbase, value='Automatically            ', uval='NDON')
  nd_off_button = widget_button(nd_buttonbase, value='Use Local Data Only', uval='NDOFF')
;  widget_control, nd_on_button, /set_button

  nubase = widget_base(configbase, /row, /align_left)
  nu_label = widget_label(nubase, value='Update Files:      ')
  nu_buttonbase = widget_base(nubase, /exclusive, /row, uval="NU")
  nu_on_button = widget_button(nu_buttonbase, value='Update if Newer       ', uval='NUON')
  nu_off_button = widget_button(nu_buttonbase, value='Use Local Data Only', uval='NUOFF')
;  widget_control, nu_on_button, /set_button

  ndobase = widget_base(configbase, /row, /align_left, sens=0)
  ndo_label = widget_label(ndobase, value='Load Data:         ')
  ndo_buttonbase = widget_base(ndobase, /exclusive, /row, uval="ND")
  ndo_on_button = widget_button(ndo_buttonbase, value='Download and Load ', uval='NDOON')
  ndo_off_button = widget_button(ndo_buttonbase, value='Download Only   ', uval='NDOOFF')
  
 
;  widget_control, ndo_on_button, /set_button

  v_base = widget_base(configbase, /row)
  v_label = widget_label(v_base, value='Verbose (higher value = more comments):      ')
  v_values = ['0', '1', '2','3', '4', '5', '6', '7', '8', '9', '10']
  v_droplist = widget_Combobox(v_base, value=v_values, uval='VERBOSE', /align_center)

  gr_base = widget_base(vmaster, /row, /align_left)
  gr_label = widget_label(gr_base, value='Graphics Mode:     ')
  gr_buttonbase = widget_base(gr_base, /exclusive, /row, uval="GR")
  gr_hard_button = widget_button(gr_buttonbase, value='Hardware Render   ', uval='GRHARD')
  gr_soft_button = widget_button(gr_buttonbase, value='Software Render   ', uval='GRSOFT')
  
  if !THM_GUI_NEW.renderer then begin
    widget_control,gr_soft_button,/set_button
  endif else begin
    widget_control,gr_hard_button,/set_button
  endelse

;buttons
  savebut = widget_button(bmaster, value = '     Save     ', uvalue = 'SAVE')
  resetbut = widget_button(bmaster, value = '     Reset     ', uvalue = 'RESET')
  reset_to_dbutton =  widget_button(bmaster,  value =  '  Reset to Default   ',  uvalue =  'RESETTODEFAULT')
  exitbut = widget_button(bmaster, value = '    Close     ', uvalue = 'EXIT')
  
  statusBar = Obj_New('THM_UI_MESSAGE_BAR', master, XSize=charSize, YSize=1)

  ;defaults for reset:
  def_values=[0,0,0,2]
  
  ;store these guys in pointers so that they
  ;are easy to return from event handler
  idPtr = ptr_new(drawID)
  winPtr = ptr_new(drawWin)

  state = {init_config:init_config,thm_ui_cfg_sav:thm_ui_cfg_sav, $
          config_names:config_names,localdir:localdir, remotedir:remotedir, $
          current_id:0,gr_base:gr_base,$
          nd_on_button:nd_on_button, nd_off_button:nd_off_button, $
          nu_on_button:nu_on_button, nu_off_button:nu_off_button, $
          ndo_on_button:ndo_on_button, ndo_off_button:ndo_off_button, $
          gr_hard_button:gr_hard_button,gr_soft_button:gr_soft_button,$
          v_values:v_values, v_droplist:v_droplist, statusBar:statusBar, $
          def_values:def_values,drawId:idPtr,drawWin:winPtr,drawObject:drawObject,$
          screenSize:screenSize,graphBase:graphBase, $
          historyWin:historyWin, gui_id:gui_id}

  thm_ui_fileconfig_init_struct,state,init_config[0]

  Centertlb, master
  widget_control, master, set_uval = state, /no_copy
  widget_control, master, /realize
  xmanager, 'thm_ui_fileconfig', master, /no_block
  drawId  = *idPtr
  drawWin = *winPtr
  
END ;--------------------------------------------------------------------------------



