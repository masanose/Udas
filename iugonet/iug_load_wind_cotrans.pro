; +
; NAME:
;     iug_load_wind_cotrans
; 
; PURPOSE:
; Crib sheet showing the use of cotrans.
;        
; Written by: Atsuki Shinbori
; -

pro iug_load_wind_cotrans

; Set the time span of interest
  timespan,'6-10-2',2,/days
  
; load_wi_mfi 
  wi_mfi_load

; Display all variable names
  tplot_names

;================================================== 
; Geophysical coordinate transformations
;         GEI<-->GSE;
;         GSE<-->GSM;
;         GSM<-->SM;
;         GEI<-->GEO;
;Examples:
;
;
;      cotrans,'***_gse','***_gsm',/GSE2GSM
;      cotrans,'***_gsm','***_gse',/GSM2GSE
;
;      cotrans,'***_gse','***_gei',/GSE2GEI
;      cotrans,'***_gei','***_gse',/GEI2GSE
;
;      cotrans,'***_gsm','***_sm',/GSM2SM
;      cotrans,'***_sm','***_gsm',/SM2GSM
;      
;      cotrans,'***_gei','***_geo',/GEI2GEO
;      cotrans,'***_geo','***_gei',/GEO2GEI
;===================================================

; Geophysical coordinate transformation from GSE to GSM
  cotrans,'wi_h0_mfi_B3GSE','wi_h0_mfi_B3GSM',/GSE2GSM

; Geophysical coordinate transformation from GSE to GEI
  cotrans,'wi_h0_mfi_B3GSE','wi_h0_mfi_B3GEI',/GSE2GEI

; Geophysical coordinate transformation from GEI to GEO
  cotrans,'wi_h0_mfi_B3GEI','wi_h0_mfi_B3GEO',/GEI2GEO
  
; Geophysical coordinate transformation from GSM to SM
  cotrans,'wi_h0_mfi_B3GSM','wi_h0_mfi_B3SM',/GSM2SM
  
; Plot the wi_h0_mfi_B3GSE and wi_h0_mfi_B3GSM data  
  tlimit,'2006-10-2/12:00:00','2006-10-2/12:30:00' 
  tplot, ['wi_h0_mfi_B3GSE','wi_h0_mfi_B3GEO','wi_h0_mfi_B3GEI','wi_h0_mfi_B3SM']
  ;erase
  ;wdelete, 1
  
  print,'You success all the coordinate transformations.'

end

