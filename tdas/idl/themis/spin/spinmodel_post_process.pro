;+
; Function: spinmodel_post_process
; 
; Purpose: Performs post-load corrections on spinmodel data.
;          
; Notes: Somewhat finicky about data requirements.  Will throw an error that
;  causes interpreter to halt unless the error is explicitly caught.  Error's
;  will be returned in the SPINMODEL_POST_PROCESS message block.
;  
;  If you find execution halted here and you aren't sure why, it is probably
;  because the state support data was not loaded. 
;  This occurs most commonly because
;  1. there is no data available on this day, or
;  2. because of internet connection issues,
;  3. because the !themis.no_download flag is on. 
;               
;   
; 
; 
; $LastChangedBy: jwl $
; $LastChangedDate: 2009-07-14 15:34:26 -0700 (Tue, 14 Jul 2009) $
; $LastChangedRevision: 6432 $
; $URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/thmsoc/tags/tdas_5_21/idl/themis/spin/spinmodel_post_process.pro $
;- 

pro spinmodel_post_process,sname=sname,filetype=filetype,datatype=datatype,$
  suffix=suffix,coord=coord,level=level,verbose=verbose,$
  progobj=progobj,midfix=midfix,_extra=_extra

common spinmodel_common,tha_sm_ptr, thb_sm_ptr, thc_sm_ptr,$
                        thd_sm_ptr, the_sm_ptr, thf_sm_ptr

; define message block
ename = ['INVAL_PROBE', 'NO_TVAR', 'ZERO_SPINPER']
efmt = ['Unrecognized sname value: %s', $
       'spinmodel_post_process: tplot variable not found: %s, This indicates that State Support Data is unavailable, unloaded, or incorrect.', $
       'spinper[0] = 0 for probe %s, probably due to empty tplot variable.']
DEFINE_MSGBLK, 'SPINMODEL_POST_PROCESS', PREFIX='THM_SPINMODEL_POST_PROCESS_', $
               ename, efmt, /ignore_duplicate

if (n_elements(tha_sm_ptr) EQ 0) then begin
   tha_sm_ptr = ptr_new({spinmodel, probe:'a', capacity: 0L, lastseg:-1L,$
       index_n: 0L, index_t: 0L, segs_ptr: ptr_new(), $
       spincorr_times: ptr_new(), spincorr_vals: ptr_new() })
endif

if (n_elements(thb_sm_ptr) EQ 0) then begin
   thb_sm_ptr = ptr_new({spinmodel, probe:'b', capacity: 0L, lastseg:-1L,$
       index_n: 0L, index_t: 0L, segs_ptr: ptr_new(), $
       spincorr_times: ptr_new(), spincorr_vals: ptr_new() })
endif

if (n_elements(thc_sm_ptr) EQ 0) then begin
   thc_sm_ptr = ptr_new({spinmodel, probe:'c', capacity: 0L, lastseg:-1L,$
       index_n: 0L, index_t: 0L, segs_ptr: ptr_new(), $
       spincorr_times: ptr_new(), spincorr_vals: ptr_new() })
endif

if (n_elements(thd_sm_ptr) EQ 0) then begin
   thd_sm_ptr = ptr_new({spinmodel, probe:'d', capacity: 0L, lastseg:-1L,$
       index_n: 0L, index_t: 0L, segs_ptr: ptr_new(), $
       spincorr_times: ptr_new(), spincorr_vals: ptr_new() })
endif

if (n_elements(the_sm_ptr) EQ 0) then begin
   the_sm_ptr = ptr_new({spinmodel, probe:'e', capacity: 0L, lastseg:-1L,$
       index_n: 0L, index_t: 0L, segs_ptr: ptr_new(), $
       spincorr_times: ptr_new(), spincorr_vals: ptr_new() })
endif

if (n_elements(thf_sm_ptr) EQ 0) then begin
   thf_sm_ptr = ptr_new({spinmodel, probe:'f', capacity: 0L, lastseg:-1L,$
       index_n: 0L, index_t: 0L, segs_ptr: ptr_new(), $
       spincorr_times: ptr_new(), spincorr_vals: ptr_new() })
endif

case sname of
'a': mptr=tha_sm_ptr
'b': mptr=thb_sm_ptr
'c': mptr=thc_sm_ptr
'd': mptr=thd_sm_ptr
'e': mptr=the_sm_ptr
'f': mptr=thf_sm_ptr
else: begin
        ;message,'Unrecognized sname value: '+string(sname)
        message, name='thm_spinmodel_post_process_inval_probe', block='spinmodel_post_process', $
        string(sname)
      end
endcase

if (ptr_valid((*mptr).segs_ptr) GT 0) then begin
   ptr_free,(*mptr).segs_ptr
endif

if (ptr_valid((*mptr).spincorr_times) GT 0) then begin
   ptr_free,(*mptr).spincorr_times
endif

if (ptr_valid((*mptr).spincorr_vals) GT 0) then begin
   ptr_free,(*mptr).spincorr_vals
endif

(*mptr).lastseg = -1L
(*mptr).capacity = 0L
(*mptr).index_n = 0L
(*mptr).index_t = 0L
(*mptr).probe = sname

;
; Extract data from tplot variables

; JWL 2008-07-25 
;
; When loading spinmodel data from the state CDF rather than the spin CDF,
; there will be a midfix component '_state' immediately following
; the probe name in all the tplot variables.  This needs to be accounted 
; for here, by checking the existence of the 'midfix' keyword and IDL variable.
;

if (n_elements(midfix) EQ 0) then begin
   munge='' 
endif else begin
   munge=midfix
endelse

if (n_elements(suffix) EQ 0) then begin
   munge2='' 
endif else begin
   munge2=suffix
endelse

prefix='th' + sname + munge + '_spin_'
spinper_var=prefix + 'spinper' + munge2
time_var=prefix + 'time' + munge2
tend_var=prefix + 'tend' + munge2
c_var=prefix + 'c'+ munge2
nspins_var=prefix + 'nspins' + munge2
npts_var=prefix + 'npts' + munge2
maxgap_var=prefix + 'maxgap' + munge2
phaserr_var=prefix + 'phaserr' + munge2
spincorr_var=prefix + 'correction' + munge2


get_data,tend_var,tstart,tend,index=n
if (n EQ 0) then begin
;  message,'spinmodel_post_process: tplot variable not found: '+tend_var
  message, name='thm_spinmodel_post_process_no_tvar', block='spinmodel_post_process', $
           tend_var
endif
get_data,spinper_var,dummy,spinper,index=n
if (n EQ 0) then begin
;  message,'spinmodel_post_process: tplot variable not found: '+spinper_var
  message, name='thm_spinmodel_post_process_no_tvar', block='spinmodel_post_process', $
           spinper_var
endif
get_data,c_var,dummy,c,index=n
if (n EQ 0) then begin
;  message,'spinmodel_post_process: tplot variable not found: '+c_var
  message, name='thm_spinmodel_post_process_no_tvar', block='spinmodel_post_process', $
           c_var
endif
get_data,nspins_var,dummy,nspins,index=n
if (n EQ 0) then begin
;  message,'spinmodel_post_process: tplot variable not found: '+nspins_var
  message, name='thm_spinmodel_post_process_no_tvar', block='spinmodel_post_process', $
           nspins_var
endif
get_data,npts_var,dummy,npts,index=n
if (n EQ 0) then begin
;  message,'spinmodel_post_process: tplot variable not found: '+npts_var
  message, name='thm_spinmodel_post_process_no_tvar', block='spinmodel_post_process', $
           npts_var
endif
get_data,maxgap_var,dummy,maxgap,index=n
if (n EQ 0) then begin
;  message,'spinmodel_post_process: tplot variable not found: '+maxgap_var
  message, name='thm_spinmodel_post_process_no_tvar', block='spinmodel_post_process', $
           maxgap_var
endif
get_data,phaserr_var,dummy,phaserr,index=n
if (n EQ 0) then begin
;  message,'spinmodel_post_process: tplot variable not found: '+phaserr_var
  message, name='thm_spinmodel_post_process_no_tvar', block='spinmodel_post_process', $
           phaserr_var
endif

; Spin phase correction: if missing, not considered an error, just
; assume 0.0 for all times.  Note that the spin correction has its
; own time variable (currently just a single sample in each daily CDF).

get_data,spincorr_var,tp_spincorr_times,tp_spincorr_vals,index=n
if (n EQ 0) then begin
  ; If spin phase correction variable is not present, use dummy values  
  message,/info,'Using dummy values for spin phase correction'
  tp_spincorr_times=[0.0D,1.0D]
  tp_spincorr_vals=[0.0,0.0]
endif else begin
  message,/info,'Found spin phase correction variables'
endelse

; 
(*mptr).spincorr_times = ptr_new(tp_spincorr_times)
(*mptr).spincorr_vals = ptr_new(tp_spincorr_vals)


;help,tstart
;help,tend
;help,spinper
;help,c
;help,nspins
;help,npts
;help,maxgap

; Determine number of segments to make.  

seg_count=n_elements(tstart)

; get_data always returns at least one element, so how do we
; figure out if tplot variables exist, but are empty?
; In that case, get_data returns a scalar value of 0.
; 0 is not a valid value for spinper, so that's our test.

if (spinper[0] EQ 0) then begin
;  mymsg='spinper[0] = 0 for probe ' + sname + ', probably due to empty tplot variable.'
;  message,mymsg
  message, name='thm_spinmodel_post_process_zero_spinper', block='spinmodel_post_process', $
           sname
  return
endif


; Seams may require insertion of additional segments

if (seg_count GT 1) then begin
   shifted_array=tstart[1:seg_count-1]
   seams=where(shifted_array NE tend,seamcount)
endif else begin
   seamcount=0
endelse
;print,seamcount,' seams found.'
rec_count=seg_count
seg_count = seg_count + seamcount
(*mptr).capacity = seg_count
;for i = 0L,seamcount-1L,1L do begin
;  print,FORMAT='(F20.8, F20.8, E20.6)',tstart[seams[i]+1],tend[seams[i]],$
;        tstart[seams[i]+1]-tend[seams[i]]
;endfor

; Make segment array

segs = replicate({spinmodel_segment, t1:0.0D, t2:0.0D, c1:0L, c2:0L,$
          b:120.0D, c:0.0D, npts:0L, maxgap:0.0D, phaserr:0.0D},$
       seg_count)

(*mptr).segs_ptr = ptr_new(segs)

for i = 0L,rec_count-1L,1L do begin
   nextseg={spinmodel_segment,t1:tstart[i], t2:tend[i], c1:0L, c2:nspins[i],$
        b:360.0D/spinper[i], c:c[i], npts:npts[i], maxgap: maxgap[i],$
        phaserr:phaserr[i]}
   spinmodel_addseg,mptr,nextseg
endfor
end
