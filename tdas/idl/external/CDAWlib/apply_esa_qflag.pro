;$Author: jimm $
;$Date: 2010-01-12 12:07:14 -0800 (Tue, 12 Jan 2010) $
;$Header:  $
;$Locker:  $
;$Revision: 7091 $
;Function: Apply_esa_qflag
;Purpose: To use the quality variable to "filter out bad themis l2 esa 
;data points"
;Author: Tami Kovalick, Perot Systems, September 2008
;
;
function apply_esa_qflag, astruct, orig_names, index=index

;Input: astruct: the structure, created by read_myCDF that should
;		 contain at least one Virtual variable.
;	orig_names: the list of varibles that exist in the structure.
;	index: the virtual variable (index number) for which this function
;		is being called to compute.  If this isn't defined, then
;		the function will find the 1st virtual variable.

;this code assumes that the Component_0 is the original variable, 
;Component_1 should be the filter/quality variable.

;astruct will contain all of the variables and metadata necessary
;to filter out the bad flux values (based on the filter variables values -
;a value >= 0 (bad). 

atags = tag_names(astruct) ;get the variable names.
vv_tagnames=strarr(1)
vv_tagindx = vv_names(astruct,names=vv_tagnames) ;find the virtual vars

if keyword_set(index) then begin
  index = index
endif else begin ;get the 1st vv

  index = vv_tagindx(0)
  if (vv_tagindx(0) lt 0) then return, -1

endelse

;print, 'In Apply_esa_qflag'
;print, 'original variables ',orig_names

c_0 = astruct.(index).COMPONENT_0 ;1st component var (real flux var)

if (c_0 ne '') then begin ;this should be the real data
  var_idx = tagindex(c_0, atags)
  itags = tag_names(astruct.(var_idx)) ;tags for the real data.

  d = tagindex('DAT',itags)
    if (d(0) ne -1) then  esa_data = astruct.(var_idx).DAT $
    else begin
      d = tagindex('HANDLE',itags)
      handle_value, astruct.(var_idx).HANDLE, esa_data
    endelse
  fill_val = astruct.(var_idx).fillval

endif else print, 'ESA variable not found'


;9/2/2008 - TJK - check for just one record, if found make it (1,*,*) and
;           continue on.  Otherwise it gets thrown out.
;if (data_size(0) eq 2) then begin 
;  dims = size(esa_data, /dimensions)
;  tmp_esa = make_array(1,dims(0), dims(1))
;  tmp_esa(0,*,*) = esa_data
;  esa_data = tmp_esa
;  data_size = size(esa_data)
;endif


c_0 = astruct.(index).COMPONENT_1 ; should be the quality variable

if (c_0 ne '') then begin ;
  var_idx = tagindex(c_0, atags)
  itags = tag_names(astruct.(var_idx)) ;tags for the real data.

  d = tagindex('DAT',itags)
    if (d(0) ne -1) then  quality_data = astruct.(var_idx).DAT $
    else begin
      d = tagindex('HANDLE',itags)
      handle_value, astruct.(var_idx).HANDLE, quality_data
    endelse
  
endif else print, 'Quality variable not found'


;temp = where(quality_data(0,*) gt 0, badcnt)
temp = where(quality_data gt 0, badcnt)
if (badcnt ge 1) then begin
   print, 'found ',badcnt, ' bad ESA data points'
   dims = size(esa_data, /n_dimensions)
;   print, 'size of esa_data = ',dims
;help, quality_data
;help, esa_data
;help, temp
   if (dims eq 1) then esa_data(temp) = fill_val
   if (dims eq 2) then esa_data(*,temp) = fill_val
   if (dims eq 3) then esa_data(*,*,temp) = fill_val
endif



;now, need to fill the virtual variable data structure with this new data array
;and "turn off" the original variable.

temp = handle_create(value=esa_data)

astruct.(index).HANDLE = temp

esa_data = 1B
quality_data = 1B

; Check astruct and reset variables not in orignal variable list to metadata,
; so that variables that weren't requested won't be plotted/listed.

   status = check_myvartype(astruct, orig_names)

return, astruct

;endif else return, -1 ;if there's no esa data return -1

end




