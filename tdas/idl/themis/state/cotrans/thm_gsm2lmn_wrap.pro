;+
; NAME:
;     THM_GSM2LMN_WRAP
;
; PURPOSE:
;	Wrapper transforms THEMIS-generated vector field from GSM to LMN (boundary-normal)
;	coordinate system for magnetopause with help of routine gsm2lmn.pro.
;	It gets the necessary solar wind data with help of routine
;	get_sw_data.pro and passes all necessary keywords to it.
;	In distinction of GSM2LMN_WRAP, it finds space-time coordinates using
;	reference to a THEMIS probe.
;
; CATEGORY:
;	Coordinate Transformation
;
; CALLING SEQUENCE:
;	thm_gsm2lmn_wrap,data_in,data_out,probe,SWkeywords
;
; INPUTS:
;	data_in: structure {x:time, y:data}
;	probe: string specify which spacecraft caught data_in.
;
; KEYWORDS: Solarwind_load.pro keywords (Any combination of keywords defining
;           output of solarwind_load.pro)
;
; PARAMETERS: none
;
; OUTPUTS:
;	data_out: structure {x:time, y:transformed_data}
;
; DEPENDENCIES: gsm2lmn.pro, solarwind_load.pro. Intermediate-level part of LMN 
;		transform package.
;
; MODIFICATION HISTORY:
;     Written by: Liu Jiang 09/21/2007
;	Modified for new background routines by: Vladimir Kondratovich 2007/12/28
;-
;
; THE CODE BEGINS:

pro thm_gsm2lmn_wrap,data_in,data_out,probe,_Extra=ex

; preparation of data
time  = data_in.x
btgsm = data_in.y
timer = [time(0),time(n_elements(time)-1)]
solarwind_load,swdata,dst,timer,_Extra=ex

; get the position of THEMIS
thm_load_state, probe=probe, trange = timer, /get_supp
statname = 'th'+probe+'_state_pos'
get_data, statname, timep, ptgei
cotrans, ptgei, ptgse, timep, /GEI2GSE
cotrans, ptgse, ptgsm, timep, /GSE2GSM

nout=n_elements(time)
ptgsmout=fltarr(nout,3)
for ii=0,2 do begin
   pti=reform(ptgsm(*,ii))
   ptouti=interpol(pti,timep,time)
   ptgsmout(*,ii)=ptouti
endfor

txyz=[[time],[ptgsmout]]
bxyz=btgsm
gsm2lmn,txyz,bxyz,blmn,swdata

data_out={x:time, y:blmn}

return
end
