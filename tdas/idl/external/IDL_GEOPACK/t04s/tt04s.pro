;+
;Procedure: tt04s
;
;Purpose:  tplot wrapper for the functional interface to the idl
;geopack implementation of the Tsyganenko 2004 Storm and IGRF fields model.
;
;Keywords: 
;          pos_gsm_tvar: the tplot variable storing the position in
;          gsm coordinates
;
;          pdyn(optional): Solar wind pressure(nanoPascals) should either be a
;          string naming a tplot variable or an array or a single
;value.   If a tplot input is used it will
;be interpolated to match the time inputs from the position
;var. Non-tplot array values must match the number of times in the
;tplot input for pos_gsm_tvar
;
;          dsti(optional): DST index(nanoTeslas)  should either be a
;          string naming a tplot variable or an array or a single
;value.   If a tplot input is used it will
;be interpolated to match the time inputs from the position
;var. Non-tplot array values must match the number of times in the
;tplot input for pos_gsm_tvar
;
;         yimf(optional): y component of the interplanetary magnetic field
;         should either be a string naming a tplot variable or an
;         array or a single value.    If a tplot input is used it will
;be interpolated to match the time inputs from the position
;var. Non-tplot array values must match the number of times in the
;tplot input for pos_gsm_tvar
;
;         zimf(optional): z component of the interplanetary magnetic field
;          should either be a string naming a tplot variable or an
;          array or a single value.   If a tplot input is used it will
;be interpolated to match the time inputs from the position
;var. Non-tplot array values must match the number of times in the
;tplot input for pos_gsm_tvar
;
;         w1(optional):  time integral from the beginning of a storm
;         can be an array or a tplot variable(see paper reference
;         below for definitions of w1-w6) or a single value. 
;  If a tplot input is used it will
;be interpolated to match the time inputs from the position
;var. Non-tplot array values must match the number of times in the
;tplot input for pos_gsm_tvar
;
;         w2(optional): time integral from the beginning of a storm
;         can be an array or a tplot variable or a single value
;  If a tplot input is used it will
;be interpolated to match the time inputs from the position
;var. Non-tplot array values must match the number of times in the
;tplot input for pos_gsm_tvar
;
;         w3(optional):  time integral from the beginning of a storm
;         can be an array or a tplot variable or a single value
;  If a tplot input is used it will
;be interpolated to match the time inputs from the position
;var. Non-tplot array values must match the number of times in the
;tplot input for pos_gsm_tvar
;
;         w4(optional): time integral from the beginning of a storm
;         can be an array or a tplot variable or a single value
;  If a tplot input is used it will
;be interpolated to match the time inputs from the position
;var. Non-tplot array values must match the number of times in the
;tplot input for pos_gsm_tvar
;
;         w5(optional):  time integral from the beginning of a storm
;         can be an array or a tplot variable or a single value
;  If a tplot input is used it will
;be interpolated to match the time inputs from the position
;var. Non-tplot array values must match the number of times in the
;tplot input for pos_gsm_tvar
;
;         w6(optional): time integral from the beginning of a storm
;         can be an array or a tplot variable or a single value
;  If a tplot input is used it will
;be interpolated to match the time inputs from the position
;var. Non-tplot array values must match the number of times in the
;tplot input for pos_gsm_tvar;
;
;         parmod(optional): can input the Nx10 parmod array used by the
;         fortran Tsyganenko model instead of inputing parameters as
;         separate arrays. If passed as a raw array it will not be
;         modified or interpolated so be sure its has the correct
;         number of entries. It can also be passed as a tplot variable
;         name in which case it will be interpolated. If values are 
;         passed individually and as par, the par values will be overwritten.
;
;
;         period(optional): the amount of time between recalculations of
;             geodipole tilt in seconds(default: 60)  increase this
;             value to decrease run time
;             
;          get_nperiod(optional): Return the number of periods used in the time interval
;
;          newname(optional):the name of the output variable. 
;          (default: pos_gsm_tvar+'_bt04s') This option is ignored if
;          globbing is used.
;
;          error(optional): named variable in which to return the
;          error state of this procedure call. 1 = success, 0 = failure
;
;          get_tilt(optional):  Set this value to a tplot variable name in which the geodipole tilt for each period will be returned
;                     One sample will be returned for each period with time at the center of the period.
;          
;          set_tilt(optional): Set this to a tplot variable name or an array of values containing the dipole tilt that should be used.
;                              If a tplot input is used it will be interpolated to match the time inputs from the position
;                              var. Non-tplot array values must match the number of times in the tplot input for pos_gsm_tvar
;
;          add_tilt(optional): Set this to a tplot variable name or an array of values containing the values to be added to the dipole tilt
;                              that should be used for each period. If a tplot input is used it will be interpolated to match the time inputs from the position
;                              var. Non-tplot array values must match the number of times in the tplot input for pos_gsm_tvar
;
;
; Output: Stores the result of the field model calculations in tplot variables
;          
; Notes: 1, converts from normal gsm to rgsm by dividing vectors by earth's
; radius(6374 km) ie inputs should be in km
;        2. Input must be in GSM coordinates
;        3. Haje Korth's IDL/Geopack DLM must be installed for this
;        procedure to work
;        4. either the variables setting parmod or the variables
;        setting the individual parameter arrays should be set because
;        the defaults aren't scientifically accurate
;        5. model parameters that are input as tplot variables they
;        will be interpolated to match the time values on the input 
;        position
;        6.Find more documentation on the inner workings of the model,
;        any gotchas, and the meaning of the arguments at:
;        http://modelweb.gsfc.nasa.gov/magnetos/data-based/modeling.html
;        -or-
;        http://dysprosium.jhuapl.edu/idl_geopack/
;        7. Definition of W1-W6 can be found at:
;        N. A. Tsyganenko and M. I. Sitnov, Modeling the dynamics of the
;        inner magnetosphere during strong geomagnetic storms, J. Geophys. 
;        Res., v. 110 (A3), A03208, doi: 10.1029/2004JA010798, 2005
;
; $LastChangedBy: pcruce $
; $LastChangedDate: 2010-04-12 16:55:36 -0700 (Mon, 12 Apr 2010) $
; $LastChangedRevision: 7496 $
; $URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/idl_socware/tags/tdas_5_21/external/IDL_GEOPACK/t04s/tt04s.pro $
;-

;validates input parameters, in_val is the value passed to the main
;function, name string is a name used for error output,pos_name is the
;name of the position tplot variable
;returns -1L on failure

function tt04_valid_param, in_val, name_string, pos_name
  
  COMPILE_OPT HIDDEN, IDL2

  if n_elements(in_val) gt 0 then begin

    ;if in_val is a string, assume in_val is stored in a tplot variable
    if size(in_val, /type) eq 7 then begin
      if tnames(in_val) eq '' then begin
        message, /continue, name_string + ' is of type string but no tplot variable of that name exists'
        return, -1L
      endif
          
      ;make sure there are an appropriate number of pdyn values in the array
      tinterpol_mxn, in_val, pos_name, newname = 'verify_temp', error = e

      if e ne 0 then begin
        get_data, 'verify_temp', data = d_verify
        del_data, 'verify_temp'
        return, d_verify.y
      endif else begin
        message, /continue, 'error interpolating ' + name_string + ' onto position data'
        return, -1L
      endelse
      
    endif else return, in_val
  endif

  get_data, pos_name, data = d

  return, dblarr(n_elements(d.x))

end

pro tt04s, pos_gsm_tvar, pdyn = pdyn,dsti = dsti, yimf = yimf, zimf = zimf,w1=w1,w2=w2,w3=w3,w4=w4,w5=w5,w6=w6, parmod=parmod,period = period,get_nperiod=get_nperiod, newname = newname, error = error,get_tilt=get_tilt,set_tilt=set_tilt,add_tilt=add_tilt
  error = 0

  if not keyword_set(pos_gsm_tvar) then begin
    message, /continue, 'pos_gsm_tvar must be set'
    return
  endif

  var_names = tnames(pos_gsm_tvar)

  if(var_names[0] eq '') then begin
    message, /continue, 'No valid tplot_variables match pos_gsm_tvar'
    return
  endif else if(n_elements(var_names) ne 1) then begin
    message, /continue, 'tt04s only accepts one position tplot variable as input'
    return
  end

  var_name = var_names[0];just in case

  if n_elements(parmod) gt 0 then begin

     if size(parmod,/type) eq 7 then begin
        
        if tnames(parmod) eq '' then message,'parmod variable not valid tplot variable'
        
        tinterpol_mxn,parmod,var_name,newname='par_out'

        get_data,'par_out',data=dat
        
        par_temp = dat.y

     endif else par_temp = parmod
     
     if n_elements(pdyn_dat) gt 0 then begin

        pdyn_dat = tt04_valid_param(pdyn, 'pdyn', var_name)

        if(size(pdyn_dat, /n_dim) eq 0 && pdyn_dat[0] eq -1L) then return

     endif else begin

        pdyn_dat = par_temp[*,0]

     endelse

     if n_elements(dsti_dat) gt 0 then begin
      
        dsti_dat = tt04_valid_param(dsti, 'dsti', var_name)
     
        if(size(dsti_dat, /n_dim) eq 0 && dsti_dat[0] eq -1L) then return

     endif else begin
  
        dsti_dat = par_temp[*,1]

     endelse

     if n_elements(yimf_dat) gt 0 then begin
        
        yimf_dat = tt04_valid_param(yimf, 'yimf', var_name)

        if(size(yimf_dat, /n_dim) eq 0 && yimf_dat[0] eq -1L) then return

     endif else begin
  
        yimf_dat = par_temp[*,2]

     endelse

     if n_elements(zimf_dat) gt 0 then begin
        
        zimf_dat = tt04_valid_param(zimf, 'zimf', var_name)

        if(size(zimf_dat, /n_dim) eq 0 && zimf_dat[0] eq -1L) then return

     endif else begin

        zimf_dat = par_temp[*,3]

     endelse

     if n_elements(w1_dat) gt 0 then begin
          
        w1_dat = tt04_valid_param(w1, 'w1', var_name)

        if(size(w1_dat, /n_dim) eq 0 && w1_dat[0] eq -1L) then return

     endif else begin
  
        w1_dat = par_temp[*,4]

     endelse

     if n_elements(w2_dat) gt 0 then begin
          
        w2_dat = tt04_valid_param(w2, 'w2', var_name)

        if(size(w2_dat, /n_dim) eq 0 && w2_dat[0] eq -1L) then return

     endif else begin

        w2_dat = par_temp[*,5]

     endelse
     
     if n_elements(w3_dat) gt 0 then begin
          
        w3_dat = tt04_valid_param(w3, 'w3', var_name)

        if(size(w3_dat, /n_dim) eq 0 && w3_dat[0] eq -1L) then return

     endif else begin

        w3_dat = par_temp[*,6]

     endelse
     
     if n_elements(w4_dat) gt 0 then begin
          
        w4_dat = tt04_valid_param(w4, 'w4', var_name)

        if(size(w4_dat, /n_dim) eq 0 && w4_dat[0] eq -1L) then return

     endif else begin

        w4_dat = par_temp[*,7]

     endelse
     
     if n_elements(w5_dat) gt 0 then begin
          
        w5_dat = tt04_valid_param(w5, 'w5', var_name)

        if(size(w5_dat, /n_dim) eq 0 && w5_dat[0] eq -1L) then return

     endif else begin

        w5_dat = par_temp[*,8]

     endelse
     
     if n_elements(w6_dat) gt 0 then begin
          
        w6_dat = tt04_valid_param(w6, 'w6', var_name)

        if(size(w6_dat, /n_dim) eq 0 && w6_dat[0] eq -1L) then return

     endif else begin

        w6_dat = par_temp[*,9]

     endelse

  endif else begin

     pdyn_dat = tt04_valid_param(pdyn, 'pdyn', var_name)

     if(size(pdyn_dat, /n_dim) eq 0 && pdyn_dat[0] eq -1L) then return
    
     dsti_dat = tt04_valid_param(dsti, 'dsti', var_name)

     if(size(dsti_dat, /n_dim) eq 0 && dsti_dat[0] eq -1L) then return
  
     yimf_dat = tt04_valid_param(yimf, 'yimf', var_name)

     if(size(yimf_dat, /n_dim) eq 0 && yimf_dat[0] eq -1L) then return
  
     zimf_dat = tt04_valid_param(zimf, 'zimf', var_name)

     if(size(zimf_dat, /n_dim) eq 0 && zimf_dat[0] eq -1L) then return
  
     w1_dat = tt04_valid_param(w1, 'w1', var_name)

     if(size(w1_dat, /n_dim) eq 0 && w1_dat[0] eq -1L) then return
  
     w2_dat = tt04_valid_param(w2, 'w2', var_name)

     if(size(w2_dat, /n_dim) eq 0 && w2_dat[0] eq -1L) then return

     w3_dat = tt04_valid_param(w3, 'w3', var_name)

     if(size(w3_dat, /n_dim) eq 0 && w3_dat[0] eq -1L) then return
  
     w4_dat = tt04_valid_param(w4, 'w4', var_name)

     if(size(w4_dat, /n_dim) eq 0 && w4_dat[0] eq -1L) then return

     w5_dat = tt04_valid_param(w5, 'w5', var_name)

     if(size(w5_dat, /n_dim) eq 0 && w5_dat[0] eq -1L) then return
  
     w6_dat = tt04_valid_param(w6, 'w6', var_name)

     if(size(w6_dat, /n_dim) eq 0 && w6_dat[0] eq -1L) then return

  endelse

  get_data, var_name, data = d, dlimits = dl, limits = l

  ;several nested ifs to check this
  ;variable's coordinate system in a
  ;safe way(ie won't reference any struct elements that don't exist)
  if ~is_struct(dl) then begin
    message, /continue, 'dlimits structure not set, make sure input variables are in gsm coordinates or results will be invalid'
    endif else begin
   
   str_element, dl, 'data_att', success = s
   if s eq 0 then begin
      message, /continue, 'dlimits.data_att structure not set, make sure input variables are in gsm coordinates or results will be invalid'
   endif else begin
     str_element, dl.data_att, 'coord_sys', success = s
     if s eq 0 then begin
       message, /continue, 'dlimits.data_att.coord_sys value not set, make sure input variables are in gsm coordinates or results will be invalid'
     endif else if dl.data_att.coord_sys ne 'gsm' then begin
       message, /continue, 'input variable is in the wrong coordinate system, returning'
       return                ;definitely wrong coordinate system=error
     endif
   endelse
 endelse
 
 if n_elements(add_tilt) gt 0 then begin
   add_tilt_dat = tt04_valid_param(add_tilt, 'add_tilt', var_name)
   if(size(add_tilt, /n_dim) eq 0 && add_tilt_dat[0] eq -1L) then return
 endif
 
 if n_elements(set_tilt) gt 0 then begin
   set_tilt_dat = tt04_valid_param(set_tilt, 'set_tilt', var_name)
   if(size(set_tilt, /n_dim) eq 0 && set_tilt_dat[0] eq -1L) then return
 endif
 
  ;do the calculation, division converts position into earth radii units

 if n_elements(set_tilt) gt 0 then begin
   mag_array = t04s(d.x, d.y/6374, pdyn_dat, dsti_dat, yimf_dat, zimf_dat,w1_dat,w2_dat,w3_dat,w4_dat,w5_dat,w6_dat, period = period,get_nperiod=get_nperiod,get_period_times=period_times_dat,get_tilt=tilt_dat,set_tilt=set_tilt_dat)
 endif else if n_elements(add_tilt) gt 0 then begin
   mag_array = t04s(d.x, d.y/6374, pdyn_dat, dsti_dat, yimf_dat, zimf_dat,w1_dat,w2_dat,w3_dat,w4_dat,w5_dat,w6_dat, period = period,get_nperiod=get_nperiod,get_period_times=period_times_dat,get_tilt=tilt_dat,add_tilt=add_tilt_dat)
 endif else begin
   mag_array = t04s(d.x, d.y/6374, pdyn_dat, dsti_dat, yimf_dat, zimf_dat,w1_dat,w2_dat,w3_dat,w4_dat,w5_dat,w6_dat, period = period,get_nperiod=get_nperiod,get_period_times=period_times_dat,get_tilt=tilt_dat)
 endelse
 
 if size(mag_array, /n_dim) eq 0 && mag_array[0] eq -1L then begin
   message, /continue, 'Tysganenko model query failed, returning'
   return
 endif

 if is_string(get_tilt) then begin
   store_data,get_tilt,data={x:period_times_dat,y:tilt_dat}
 endif

  ;sometimes v element is present, sometimes not 
  ;if it is around it is stored in output so information is not lost 
  str_element, d, 'v', success = s

  if s eq 1 then $
    d_out = {x:d.x, y:mag_array, v:d.v} $
  else $
    d_out = {x:d.x, y:mag_array}

  if keyword_set(newname) then $
    store_data, newname, data = d_out, dlimits = dl, limits = l $
  else $
    store_data, var_name +'_bt04s', data = d_out, dlimits = dl, limits = l

  ;signal success
  error = 1 

  return

end
