;+
;procedure: thm_spinfit
;
;Purpose: finds spinfits of field data
;
;         finds A,B, and C fits for each period of data, over the entire day
;
;required parameters:
;  var_name_in = tplot variable name containing data to fit
;
;keywords:
;  sigma = If set, will cause program to output tplot variable with sigma for each period.
;  npoints = If set, will cause program to output tplot variable with number of points in fit.
;  spinaxis = If set, program will output a tplot variable storing the average over the spin axis dimension
;             for each time period.
;  median  = If spinaxis set, program will output a median of each period instead of the average.
;  plane_dim = Tells program which dimension to treat as the plane. 0=x, 1=y, 2=z. Default 0.
;  axis_dim = Tells program which dimension contains axis to average over. Default 0.  Will not
;             create a tplot variable unless used with /spinaxis.
;  min_points = Minimum number of points to fit.  Default = 5.
;  alpha = A parameter for finding fits.  Points outside of sigma*(alpha + beta*i)
;          will be thrown out.  Default 1.4.
;  beta = A parameter for finding fits.  See above.  Default = 0.4
;  phase_mask_starts = Time to start masking data.  Default = 0
;  phase_mask_ends = Time to stop masking data.  Default = -1
;  sun2sensor = Tells how much to rotate data to align with sun sensor.
;
;Example:
;      thm_spinfit,'th?_fg?',/sigma
;
;Notes: under construction!!
;
;Written by Katherine Ramer
; $LastChangedBy: kramer $
; $LastChangedDate: 2007-06-08 10:10:36 -0700 (Fri June 8, 2007) $
; $LastChangedRevision: 0 $
; $URL:
;-

pro thm_spinfit,var_name_in, $
          sigma=sigma, npoints=npoints, spinaxis=spinaxis, median=median, $
          plane_dim=plane_dim, axis_dim=axis_dim,  $
          min_points=min_points,alpha=alpha,beta=beta, $
          phase_mask_starts=phase_mask_starts,phase_mask_ends=phase_mask_ends, $
          sun2sensor=sun2sensor

vprobes = ['a','b','c','d','e']
vdatatypes = ['fgl', 'fgh', 'fge']
if keyword_set(valid_names) then begin
      probe = vprobes
      datatypes = vdatatypes
      return
endif
if not keyword_set(plane_dim) then plane_dim=0

matches=tnames(var_name_in)
n_matches=n_elements(matches)
if n_matches eq 1 then if matches eq '' then n_matches=0

for i=0,n_elements(n_matches)-1 do begin

 probe = strmid(matches[i], 2, 1)
 probes = thm_check_valid_name(strlowcase(probe), vprobes, /include_all)
 thx = 'th'+probes[0]

 sizephase=size(thx_spinphase)
 sizeper=size(thx_spinper)
 if not sizephase[0] or not sizeper[0] then begin
    get_data,thx+'_state_spinphase',data=thx_spinphase
    get_data,thx+'_state_spinper',data=thx_spinper
 endif
 if not (n_elements(thx_spinphase)) or not (n_elements(thx_spinper)) then begin
     thm_load_state,probe=probes, /get_support_data
     get_data,'th'+probes+'_state_spinphase',data=thx_spinphase
     get_data,'th'+probes+'_state_spinper',data=thx_spinper
 endif

 get_data,matches[i],data=thx_xxx_in, dl = dl

 boomfix=''
 if cotrans_get_coord(dl) eq 'spg' then begin
   if undefined(sun2sensor) then begin
     case 1 of
       plane_dim eq 0: begin
         message,/info,'User set PLANE_DIM to "0".  Internally setting SUN2SENSOR keyword to 135 degrees for E12 boom angle offset.'  ;Cite THEMIS doc here.
         sun2sensor = 135.
         boomfix = '_e12'
       end
       plane_dim eq 1: begin
         message,/info,'User set PLANE_DIM to "1".  Internally setting SUN2SENSOR keyword to 45 degrees for E34 boom angle offset.'  ;Cite THEMIS doc here.
         sun2sensor = 45.
         boomfix = '_e34'
       end
     endcase
   endif else message,/info,'*** WARNING: User has overriden standard angle offset for chosen boom (PLANE_DIM keyword) via SUN2SENSOR keyword.';Cite THEMIS doc.
 endif

 thm_sunpulse,thx_spinphase.x,thx_spinphase.y,thx_spinper.y,sunpulse="thx_sunpulse_times"
 get_data, 'thx_sunpulse_times',data=thx_sunpulse_times



 spinfit,thx_xxx_in.x,thx_xxx_in.y,thx_sunpulse_times.x,thx_sunpulse_times.y,$
           a,b,c,spin_axis,med_axis,s,n,sun_data,min_points=min_points,alpha=alpha,beta=beta, $
            plane_dim=plane_dim,axis_dim=axis_dim,phase_mask_starts=phase_mask_starts,$
           phase_mask_ends=phase_mask_ends,sun2sensor=sun2sensor


 sizesun=size(sun_data)
 sun_midpoint=fltarr(sizesun[1])
 ;for j=0,sizesun[1]-2 do sun_midpoint[j]=(sun_data[j]+sun_data[j+1])/2
 sun_midpoint=sun_data

 ; metadata:
 ;
 str_element, dl, 'data_att', data_att, success=has_data_att
 if has_data_att then str_element, data_att, 'boom', boomfix, /add  else  data_att = { data_type: boomfix }
 str_element, dl, 'data_att', data_att, /add
 str_element, dl,'labels',/delete

 store_data,matches[i]+'_spinfit'+boomfix+'_a',data={x:sun_midpoint,y:a}, dl = dl
 store_data,matches[i]+'_spinfit'+boomfix+'_b',data={x:sun_midpoint,y:b}, dl = dl
 store_data,matches[i]+'_spinfit'+boomfix+'_c',data={x:sun_midpoint,y:c}, dl = dl

 if keyword_set(sigma) then store_data,matches[i]+'_spinfit'+boomfix+'_sig',data={x:sun_midpoint,y:s}, dl = dl
 if keyword_set(Npoints) then store_data,matches[i]+'_spinfit'+boomfix+'_npoints',data={x:sun_midpoint,y:n}, dl = dl
 if keyword_set(spinaxis) then begin
   if keyword_set(median)then begin
     store_data,matches[i]+'_spinfit'+boomfix+'_med',data={x:sun_midpoint,y:med_axis}, dl = dl
   endif else store_data,matches[i]+'_spinfit'+boomfix+'_avg',data={x:sun_midpoint,y:spin_axis}, dl = dl
 endif
endfor ;i
end

