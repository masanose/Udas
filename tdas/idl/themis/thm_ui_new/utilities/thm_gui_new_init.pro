;+ 
;NAME:
;
; thm_gui_new_init
;
;PURPOSE:
;the very beginnings of global configuration info
;for thm_gui_new
;
;CALLING SEQUENCE:
; themis_gui_new_init
;
;INPUT:
; none
;
;OUTPUT:
; none
;
;HISTORY:
;
;$LastChangedBy: pcruce $
;$LastChangedDate: 2009-09-14 16:51:22 -0700 (Mon, 14 Sep 2009) $
;$LastChangedRevision: 6725 $
;$URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/thmsoc/tags/tdas_5_21/idl/themis/thm_ui_new/utilities/thm_gui_new_init.pro $
;-----------------------------------------------------------------------------------

pro thm_gui_new_init

  defsysv,'!THM_GUI_NEW',exists=i
  if ~i then begin
    defsysv,'!THM_GUI_NEW',{renderer:1B,$  ;OS specific rendering options should go here
                            guiId:0L,  $ ; the widget id of the main gui, needed as an input to command line.
                            drawObject:obj_new(),$ ;draw object, so that command line can interface with gui
                            windowStorage:obj_new(),$ ; window_storage object, so that command line can interface with gui
                            loadedData:obj_new(), $ ; loaded_data object, so that command line can interface with gui
                            windowMenus:obj_new(), $ ; the window menu object, so that the command line can update the gui window menu
                            historyWin:obj_new() $ ; the history window object, so that we can log the tplot_gui
                           }
  endif

  out = thm_read_config()

  if is_struct(out) && in_set('renderer',strlowcase(tag_names(out))) then begin
    !thm_gui_new.renderer = long(out.renderer)
  endif

end