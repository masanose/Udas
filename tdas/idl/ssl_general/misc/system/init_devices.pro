;+
;PROCEDURE:  init_devices
;PURPOSE:    Initializes IDL devices for multiple systems.  Can be called from idl_startup batch file.
;
;KEYWORDS:
;   COLORTABLE:  Colortable number to be used. (defaults to 34)
;
; Typical examples:
;
; Notes:
;     Searches for the environment variable "IDL_DEVICE" and uses its value to define the
;     graphics device using "SET_PLOT"
;
;HISTORY
; Written by Davin Larson
;
;$LastChangedBy: davin-win $
;$LastChangedDate: 2009-07-13 11:29:29 -0700 (Mon, 13 Jul 2009) $
;$LastChangedRevision: 6421 $
;$URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/ssl_general/tags/tdas_5_21/misc/system/init_devices.pro $
;-
pro init_devices,colortable=colortable

  idl_device = getenv('IDL_DEVICE')
  if keyword_set(idl_device) then begin
      dprint,'Warning: Using device: ',idl_device
      set_plot,idl_device
  endif

  ;display = getenv('DISPLAY')
  ;if display eq '' && !version.os_family eq 'unix' then begin
  ;   set_plot,'z'
  ;   print,'Warning: No display!; Using Z buffer.'
  ;endif

  if n_elements(colortable) eq 0 then colortable =34

  old_dev = !d.name   ;  save current device name
  set_plot,'PS'       ;  change to PS so we can edit the font mapping
  loadct2,colortable
  device,/symbol,font_index=19  ;set font !19 to Symbol
  set_plot,'printer'
  loadct2,colortable
  set_plot,old_dev    ;  revert to old device

  if !d.name eq 'WIN' then begin
    device,decompose = 0
  endif

  if !d.name eq 'X' then begin
    ; device,pseudo_color=8  ;fixes color table problem for machines with 24-bit color
    device,decompose = 0
    if !version.os_name eq 'linux' then device,retain=2  ; Linux does not provide backing store by default
  endif

;  !p.font = -1
  loadct2,colortable

  ; black on white
  !p.background = !d.table_size-1
  !p.color=0

end
