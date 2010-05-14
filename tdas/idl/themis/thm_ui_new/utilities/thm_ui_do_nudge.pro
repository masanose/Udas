;+ 
;NAME:
; thm_ui_do_nudge
;
;PURPOSE:
; Abstracted nudge data operation
;
;
;CALLING SEQUENCE:
; thm_ui_do_nudge,
;
;INPUT:
;  gui_id = widget id of the widget that called this program
;  info = the info structure of the Main GUI
;OUTPUT:
;  fail=fail
;HISTORY:
;$LastChangedBy: pcruce $
;$LastChangedDate: 2009-05-27 11:57:46 -0700 (Wed, 27 May 2009) $
;$LastChangedRevision: 5972 $
;$URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/thmsoc/tags/tdas_5_21/idl/themis/thm_ui_new/utilities/thm_ui_do_nudge.pro $
;
;---------------------------------------------------------------------------------

pro thm_ui_do_nudge,tn,nshift,shift_unit,shift_scale,use_records,isspec,loadedData,historyWin,statusbar,fail=fail,tn1=tn1

  compile_opt idl2
  
  fail = 1

  tvo = loadeddata -> gettvarobject(tn) 
  loadeddata -> getvardata, name = tn, time = t, $
    data = d, limits = l, dlimits = dl, yaxis = v
  If(ptr_valid(t) Eq 0) Then Begin
      statusbar->update, 'Invalid time pointer: '+tn
      return
  Endif Else Begin
      If(use_records) Then Begin
          t1 = shift(*t, -nshift)
      Endif Else Begin
          t1 = *t+shift_scale*nshift
      Endelse
     ;Update status, history
      nshift_str = strcompress(/remove_all, string(nshift))
      h = 'Nudging: '+tn+' by '+nshift_str+' '+shift_unit
      statusbar->update, h
      historywin->update, h
      tp1 = ptr_new(t1)
    ;Create a new variable name by appending the shift amount
      If((nshift Mod 1.0) Eq 0) Then Begin
          ext = strcompress(string(fix(nshift)), /remove_all)
      Endif Else Begin
          ext = strcompress(string(nshift), /remove_all)
      Endelse
      tn1 = tn+'_'+ext+'_'+shift_unit

        ;Create a tplot variable
      If(isspec && ptr_valid(v)) Then data1 = {x:t1, y:*d, v:*v} $
      Else data1 = {x:t1, y:*d}
      store_data, tn1, data = temporary(data1)
      If(ptr_valid(l)) Then store_data, tn1, limits = *l
      If(ptr_valid(dl)) Then Begin
        dlim = *dl
        If(isspec Eq 0) Then dlim.spec = 0 $
        Else dlim.spec = 1
        store_data, tn1, dlimits = temporary(dlim)
      Endif
      ;Copy variable into loaded_data object
      tvo_new = tvo->copy()
      tvo_new->setproperty, name = tn1
      If(isspec Eq 0) Then tvo_new->setproperty, isspect = 0
      ok = loadeddata->addtvarobject(tvo_new)
      
      if ~ok then begin
        statusbar -> update, 'Trace Nudge to: '+tn1+' Failed'
        historywin -> update, 'Trace Nudge to: '+tn1+' Failed'
        return
      endif
      
      fail = 0
      
   endelse
   
 end