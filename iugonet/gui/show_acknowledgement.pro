;+
;NAME:
;  show_acknowledgement
;
;PURPOSE:
;  Show data usage policy for each observation data.
;
;EXAMPLE:
;  show_acknowledgement, instrument=instrument, datatype=datatype, $
;	par_names=par_names
;
;Code:
;  A. Shinbori, 13/01/2011.
;  
;Modifications:
;  Y.-M. Tanaka, 11/05/2012
;-

function show_acknowledgement, instrument=instrument, datatype=datatype, $
	par_names=par_names

  ;----- Title of popup window -----;
  title='Rules of Data Use:'

  ;----- keyword check -----;
  if (not keyword_set(par_names)) then return,0

  ;----- Get acknowledgement message -----;
  get_data, par_names[0], dlimit=str
  if (instrument eq 'EISCAT_radar') or (instrument eq 'SuperDARN#') or $
	(datatype eq 'NIPR_mag#') then begin
     theMessage=str.cdf.gatt.rules_of_use
  endif else if (datatype eq '210mm#') then begin
     theMessage=str.cdf.gatt.text
  endif else begin
     theMessage=str.data_att.acknowledgment
  endelse

  ;----- If OS is Windows, divide the message into a string array -----;
  if (strlowcase(!version.os_family) ne 'windows') then begin
     theMessage=str2arr_maxlet(theMessage, maxlet=100)
  endif

  Result = dialog_message(theMessage, /cancel, /information, $
                                /center, title=title)

  return, Result

end


