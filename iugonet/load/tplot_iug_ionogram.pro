;+
;
;NAME:
;  tplot_iug_ionogram
;
;PURPOSE:
;  Generate time-sries plots of echo power profiles of several transmitted frequency 
;  from the ionogram data taken by the ionosonde. 
;
;SYNTAX:
;  tplot_iug_ionogram, datatype = datatype, valuename=valuename
;
;KEYWOARDS:
;  datatype = Observation data type. For example, tplot_iug_ionogram, datatype = 'ionosphere'.
;            The default is 'ionosphere'. 
;  valuename = tplot variable names of ionosonde observation data.  
;         For example, tplot_iug_ionogram,valuename = 'iug_ionosonde_sgk_ionogram'.
;         The default is 'iug_ionosonde_sgk_ionogram'.
;
;CODE:
;  A. Shinbori, 11/01/2013.
;  
;MODIFICATIONS:
;
;  
;ACKNOWLEDGEMENT:
; $LastChangedBy:  $
; $LastChangedDate:  $
; $LastChangedRevision:  $
; $URL $
;-

pro tplot_iug_ionogram,datatype=datatype, valuename=valuename

;***************
;Datatype check:
;***************
if not keyword_set(datatype) then datatype='ionosphere'

;****************
;Valuename check:
;****************
if not keyword_set(valuename) then valuename='iug_ionosonde_sgk_ionogram'

;==============================
;Get data from tplot variables:
;==============================
get_data,valuename,data=d

;==============================
;Store data in TPLOT variables:
;==============================
;Acknowlegment string (use for creating tplot vars)
acknowledgstring = 'If you acquire the ionogram data, ' $
                 + 'we ask that you acknowledge us in your use of the data. This may be done by' $
                 + 'including text such as the ionogram data provided by Research Institute' $
                 + 'for Sustainable Humanosphere of Kyoto University. We would also' $
                 + 'appreciate receiving a copy of the relevant publications. The distribution of ' $
                 + 'ionogram data has been partly supported by the IUGONET (Inter-university Upper ' $
                 + 'atmosphere Global Observation NETwork) project (http://www.iugonet.org/) funded ' $
                 + 'by the Ministry of Education, Culture, Sports, Science and Technology (MEXT), Japan.'    
                         
dlimit=create_struct('data_att',create_struct('acknowledgment',acknowledgstring,'PI_NAME', 'M. Yamamoto'))

for i=0, n_elements(d.v1)-1 do begin
   power = fltarr(n_elements(d.x),n_elements(d.v2))
   power[*,*] = d.y[*,i,*]
   if (i mod 10) eq 0 then begin
      store_data,'iug_ionosonde_sgk_freq_'+strtrim(string(i/10+2),2)+'MHz',data={x:d.x,y:power,v:d.v2},dlimit=dlimit
     ;Add options
      options,'iug_ionosonde_sgk_freq_'+strtrim(string(i/10+2),2)+'MHz',ytitle = 'Height [km]', ztitle = 'Echo power at '+strtrim(string(i/10+2),2)+' [MHz]'
      options, 'iug_ionosonde_sgk_freq_'+strtrim(string(i/10+2),2)+'MHz', spec=1
   endif
endfor

end