;+
;
;Name:
; gui_load_acknowledgement
;
;Purpose:
; Output of the data acknowledgement message for each observation data.
;
;Syntax:
; gui_load_acknowledgement, datatype = datatype, par_names = par_names
;
;Keywords:
;  par_names = tplot names. For example, iug_load_acknowledgement, par_names = 'iug_mf_pam_vwnd'.
;               The default is 'iug_mf_pam_uwnd'.
;  datatype = Observation data type. For example, iug_load_acknowledgement, datatype = '210mm*'.
;             The default is 'thermosphere'. 
;
;Code:
;  A. Shinbori, 13/01/2011.
;  
;Modifications:
; 
;  
;Acknowledgment:
; $LastChangedBy:  $
; $LastChangedDate:  $
; $LastChangedRevision:  $
; $URL $
;-

function gui_load_acknowledgement, datatype = datatype, par_names = par_names

;**************
;keyword check:
;**************
  if (not keyword_set(par_names)) then par_names='iug_mf_pam_uwnd'

;Output of the data acknowledgement window for each instrument:
  if (datatype eq '210mm#') or (datatype eq 'NIPR_mag#') then begin

     get_data, par_names[0], dlimit=str

     Result=acknowledgement_message(str.cdf.gatt.text, /noname, /center, title='Acknowledgement of IUGONET Data Use:')

  endif else if datatype eq 'ionosphere' then begin
     get_data, par_names[0], dlimit=str
     Result=acknowledgement_message(str.cdf.gatt.rules_of_use, /noname, /center, title='Acknowledgement of IUGONET Data Use:')
  endif else begin  

     get_data, par_names[0], dlimit=str
     Result=acknowledgement_message(str.data_att.acknowledgment, /noname, title='Acknowledgement of IUGONET Data Use:') 
     
  endelse
  
  return, Result
  
end