;+
;Procedure: thm_cotrans
;Purpose:   Transform between various THEMIS  and geophysical coordinate systems
;keywords:
;  probe = Probe name. The default is 'all', i.e., transform data for all
;          available probes.
;          This can be an array of strings, e.g., ['a', 'b'] or a
;          single string delimited by spaces, e.g., 'a b'
;  datatype = The type of data to be transformed, can take any of the values
;          allowed for datatype for the various thm_load routines. You
;          can use wildcards like ? and [lh].
;          'all' is not accepted. You can use '*', but you may get unexpected
;          results if you are using suffixes.
;  in_coord = 'spg', 'ssl', 'dsl', 'gse', 'gsm','sm', 'gei','geo', or 'sse'
;          coordinate system of input.
;          This keyword is optional if the dlimits.data_att.coord_sys attribute
;          is present for the tplot variable, and if present, it must match
;          the value of that attribute.  See cotrans_set_coord,
;          cotrans_get_coord
;  out_coord = 'spg', 'ssl', 'dsl', 'gse', 'gsm', 'sm', 'gei','geo', or 'sse'
;           coordinate system of output.  This keyword is optional if
;           out_suffix is specified and last 3 characters of suffix specify
;           the output coordinate system.
;  in_suffix = optional suffix needed to generate the input data quantity name:
;           'th'+probe+'_'datatype+in_suffix
;  out_suffix = optional suffix to add to output data quantity name.  If
;           in_suffix is present, then in_suffix will be replaced by out_suffix
;           in the output data quantity name.
; valid_names:return valid coordinate system names in named varibles supplied to
;           in_coord and/or out_coord keywords.
; support_suffix: if support_data is loaded with a suffix you can
; specify it here
;           
; out_vars: return a list of the names of any transformed variables
;
; ignore_dlimits: set this keyword to true so that an error will not
;     be produced if the internal label of the coordinate system clashed
;     with the user provided coordinate system.
; interpolate_state: use interpolation on 1-minute state CDF spinper/spinphase
;     samples for despinning instead of spin model
;
;
;Optional Positional Parameters:
; in_name  Name(s) of input tplot variable(s) (or glob patern)
;          (space-separated list or array of strings.).  If the in_name
;          parameter is provided, the probe and datatype
;          keywords will be ignored.  However, if the input name
;          is not of format 'th[a-e]_*', use the probe keyword to indicate
;          which probe's state data should be used for each input variable.
; out_name Name(s) of output tplot variable(s).  glob patterns not accepted.
;          Number of output names must match number of input names (after glob
;          expansion of input names).  (single string, or array of strings.)
;
;Examples:
;  thm_load_state, /get_support
;
;  thm_cotrans, probe='a', datatype='fgl', out_suffix='_gsm'
;
;  ; or equivalently
;
;  thm_cotrans, 'tha_fgl', 'tha_fgl_gsm', out_coord='gsm'
;
;  ; to transform all th?_fg?_dsl to th?_fg?_gsm
;
;  thm_cotrans, 'th?_fg?', in_suffix='_dsl', out_suffix='_gsm'
;
;  ; for arbitrary input variables, specify in_coord and probe:
;
;  thm_cotrans,'mydslvar1 mydslvar2 mydslvar3', $
;              in_coord='dsl', probe='b c d', out_suff='_gse'
;
; $LastChangedBy: aaflores $
; $LastChangedDate: 2010-03-04 13:30:17 -0800 (Thu, 04 Mar 2010) $
; $LastChangedRevision: 7400 $
; $URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/thmsoc/tags/tdas_5_21/idl/themis/state/cotrans/thm_cotrans.pro $
;-

;helps simplify transformation logic code using a recursive formulation.
;Rather than specifying the set of transformations for each combination of
;in_coord & out_coord, this routine will perform only the nearest transformation
;then make a recursive call to itself, with each call performing one additional
;step in the chain.  This makes it so only neighboring coordinate transforms need be
;specified.
;The set of transformations forms the following graph:
;SPG<->SSL<->DSL<->GSE<->GEI<->GEO
;            SSE<->GSE<->GSM<->SM
pro thm_cotrans_transform_helper,in_name,out_name,in_coord,out_coord, $
                      spinras,spindec,spinper,spinphase,ignore_dlimits=ignore_dlimits,$
                      interpolate_state=interpolate_state,$
                      use_spinphase_correction=use_spinphase_correction,$
                      name_sun_pos=name_sun_pos,name_lun_pos=name_lun_pos,$
                      prb=prb
                      
  compile_opt hidden
  
   ;case select below modified to increase simplicity and maintainability.
  ;#1 Identity transform separated.
  ;#2 Recursive calls to thm_cotrans prevents duplicated code. 
  if in_coord eq out_coord then begin
    if in_name ne out_name then copy_data,in_name,out_name 
  endif else begin
    case in_coord of
      'spg': begin
          spg2ssl,in_name,out_name
          recursive_in_coord='ssl'
        end
      'ssl': switch out_coord of
        'spg': begin
          spg2ssl,in_name,out_name,/ssl2spg
          recursive_in_coord='spg'
          break
        end
        else: begin
          if keyword_set(interpolate_state) then begin
            ssl2dsl, name_input=in_name, name_thx_spinper=spinper, name_thx_spinphase=spinphase, name_output=out_name,ignore_dlimits=ignore_dlimits,/interpolate_state
          endif else begin
            ssl2dsl, name_input=in_name, name_output=out_name,ignore_dlimits=ignore_dlimits,spinmodel_ptr=spinmodel_get_ptr(prb),use_spinphase_correction=use_spinphase_correction
          endelse
          recursive_in_coord='dsl'
        end
      endswitch
      'dsl': switch out_coord of 
        'spg': 
        'ssl': begin
          if keyword_set(interpolate_state) then begin
            ssl2dsl, name_input=in_name, name_thx_spinper=spinper, name_thx_spinphase=spinphase, name_output=out_name, /dsl2ssl,ignore_dlimits=ignore_dlimits,/interpolate_state
          endif else begin
            ssl2dsl, name_input=in_name, name_output=out_name, /dsl2ssl,ignore_dlimits=ignore_dlimits,spinmodel_ptr=spinmodel_get_ptr(prb),use_spinphase_correction=use_spinphase_correction
          endelse
          recursive_in_coord='ssl'
          break
        end
        else: begin
          dsl2gse, in_name, spinras, spindec, out_name,ignore_dlimits=ignore_dlimits      
          recursive_in_coord='gse'           
        end   
      endswitch
      'gse': switch out_coord of
        'spg':
        'ssl':
        'dsl': begin
          dsl2gse, in_name, spinras, spindec, out_name,ignore_dlimits=ignore_dlimits,/gse2dsl
          recursive_in_coord='dsl'
          break
        end
        'sse': begin
          gse2sse,in_name,name_sun_pos,name_lun_pos,out_name,ignore_dlimits=ignore_dlimits
          recursive_in_coord='sse'
          break
        end
        'sm':
        'gsm': begin
          cotrans, in_name, out_name, /gse2gsm,ignore_dlimits=ignore_dlimits
          recursive_in_coord='gsm'
          break
        end
        else: begin
          cotrans, in_name,out_name,/gse2gei, ignore_dlimits=ignore_dlimits
          recursive_in_coord='gei'
        end
      endswitch
      'sse': begin
        gse2sse,in_name,name_sun_pos,name_lun_pos,out_name,ignore_dlimits=ignore_dlimits,/sse2gse
        recursive_in_coord='gse'
        break
       end
      'sm': begin
         cotrans, in_name,out_name,/sm2gsm, ignore_dlimits=ignore_dlimits
         recursive_in_coord='gsm'
      end
      'gsm': switch out_coord of
        'sm': begin
          cotrans, in_name,out_name,/gsm2sm, ignore_dlimits=ignore_dlimits
          recursive_in_coord='sm'
          break
        end
        else: begin
          cotrans, in_name,out_name,/gsm2gse, ignore_dlimits=ignore_dlimits
          recursive_in_coord='gse'
        end
      endswitch
      'gei': switch out_coord of
        'geo': begin
          cotrans,in_name,out_name,/gei2geo,ignore_dlimits=ignore_dlimits
          recursive_in_coord='geo'
          break
        end
        else: begin
          cotrans,in_name,out_name,/gei2gse,ignore_dlimits=ignore_dlimits
          recursive_in_coord='gse'
        end
      endswitch
      'geo': begin
        cotrans,in_name,out_name,/geo2gei,ignore_dlimits=ignore_dlimits
        recursive_in_coord='gei'
      end
      else: begin
        message, /info,"thm_cotrans: don't know how to transform "+in_coord+" to " $
                + out_coord
        recursive_in_coord=out_coord
      end
    endcase
    thm_cotrans_transform_helper,out_name,out_name,recursive_in_coord,out_coord, $
                      spinras,spindec,spinper,spinphase,ignore_dlimits=ignore_dlimits,$ 
                      interpolate_state=interpolate_state,$
                      use_spinphase_correction=use_spinphase_correction,$
                      name_sun_pos=name_sun_pos,name_lun_pos=name_lun_pos,$
                      prb=prb
  endelse          
                      
end
                      
; in_coord is optional if dlimits includs a data_att.coord_sys element.
pro thm_cotrans, probe=probe, datatype=datatype, valid_names=valid_names, $
                 in_coord=in_coord, out_coord=out_coord, verbose=verbose, $
                 in_suffix=in_suf, out_suffix=out_suf, in_name, out_name, $
                 support_suffix=support_suffix,ignore_dlimits=ignore_dlimits,$
                 interpolate_state=interpolate_state,out_vars=out_vars,$
                 use_spinaxis_correction=use_spinaxis_correction, $
                 use_spinphase_correction=use_spinphase_correction, $
                 slp_suffix=slp_suffix

  thm_init
; If verbose keyword is defined, override !themis.verbose
  vb = size(verbose, /type) ne 0 ? verbose : !themis.verbose

   vprobes = ['a','b','c','d','e']
   vcoord = ['spg', 'ssl', 'dsl', 'gse', 'gsm','sm', 'gei','geo','sse']
   if keyword_set(valid_names) then begin
      in_coord = vcoord
      out_coord = vcoord
      probe=vprobes
      if keyword_set(vb) then begin
         message, /info, string(strjoin(vcoord, ','), $
                                format = '( "Valid coords:",X,A,".")')
         message, /info, string(strjoin(vprobes, ','), $
                                format = '( "Valid probes:",X,A,".")')
         message, /info, 'Valid datatypes: Anything goes!'

      endif
      return
   endif

; validate in_coord and out_coord
   if not keyword_set(out_coord) and keyword_set(out_suf) then begin
      out_coord=strmid(out_suf,2,3,/reverse)
      if stregex(out_coord,'sm',/boolean) && ~stregex(out_coord,'gsm',/boolean) then begin
        out_coord = 'sm'
      endif
   endif
   
   if not keyword_set(out_coord) then begin
      dprint, 'thm_cotrans: must specify out_coord or out_suffix'
      return
   endif else out_coord = thm_check_valid_name(strlowcase(out_coord), vcoord)

   if not keyword_set(out_coord) then return

   if n_elements(out_coord) gt 1 then begin
      dprint, 'thm_cotrans: can only specify one out_coord'
      return
   endif

   if ~keyword_set(in_coord) && keyword_set(in_suf) then begin 
      in_coord=strmid(in_suf,2,3,/reverse)
      if stregex(in_coord,'sm',/boolean) && ~stregex(in_coord,'gsm',/boolean) then begin
        in_coord = 'sm'
      endif
   endif

   if keyword_set(in_coord) then begin
      in_coord = thm_check_valid_name(strlowcase(in_coord), vcoord)
      if not keyword_set(in_coord) then return
      if n_elements(in_coord) gt 1 then begin
         dprint, 'thm_cotrans: can only specify one in_coord'
         return
      endif
   endif

   if not keyword_set(in_suf) then in_suf = ''
   if not keyword_set(out_suf) then out_suf = ''

   if (n_elements(use_spinaxis_correction) EQ 0) then begin
      use_spinaxis_correction=1
      message,/info,'Defaulting to enable V03 spin axis correction'
   end

   if (n_elements(use_spinphase_correction) EQ 0) then begin
      use_spinphase_correction=1
      message,/info,'Defaulting to enable V03 spin phase correction'
   end

; do 'standard' THEMIS name conventions to get tplot names
if n_params() eq 0 then begin
   if not keyword_set(probe) then probe = vprobes $
   else probe = thm_check_valid_name(strlowcase(probe), vprobes, /include_all)
   if not keyword_set(probe) then begin
      message, /info, 'probe keyword required if no positional args present'
      return
   endif

   if not keyword_set(datatype) then begin
      message, /info, 'datatype keyword required if no positional args present'
      return
   endif

   if n_elements(datatype) eq 1 then datatype=strsplit(datatype, ' ', /extract)
   datatype=strlowcase(datatype)

   for i = 0, n_elements(probe)-1 do begin
      for j = 0, n_elements(datatype)-1 do begin
         in_name='th'+probe[i]+'_'+datatype[j]
         out_name='th'+probe[i]+'_'+datatype[j]

            thm_cotrans, in_name, in_coord=in_coord, out_coord=out_coord, $
                in_suf=in_suf, out_suf=out_suf, verbose=verbose,$
                ignore_dlimits=ignore_dlimits,$
                interpolate_state=interpolate_state,$
                use_spinphase_correction=use_spinphase_correction,$
                use_spinaxis_correction=use_spinaxis_correction

      endfor
   endfor
   return
endif else if n_params() gt 2 then begin
   dprint, 'usage: thm_cotrans, probe=probe, datatype=datatype, $'
   dprint, '                    in_coord=in_c, out_coord=out_c, $'
   dprint, '                    in_suffix=in_suf, out_suffix=out_suff'
   dprint, 'or: thm_cotrans, in_name[, out_name], in_coord=in_c, out_coord=out_c'
   return
endif

; allow for globbing on the input parameters , deal with in_suf and out_suf,
; figure out probe, if necessary.


in_names = tnames(in_name+in_suf, n)
if n eq 0 then begin
   dprint, 'thm_cotrans: no match: '+in_name+in_suf
   return
endif
if n_params() eq 1 || $
   n_params() eq 2 && n_elements(in_names) ne n_elements(out_name) then begin
   if n_params() eq 2 then dprint, 'thm_cotrans: warning: ignoring out_names'
   ;; generate output names based on out_suffix
   if in_suf ne '' then $
      base_len = strpos(in_names,in_suf,/reverse_search) $
   else $
      base_len = strlen(in_names)
   ;   out_names = strmid(in_names,0,transpose(base_len))+out_suf
;   out_names = strmid(in_names,0,base_len)+out_suf
;the statement with the transpose bombs on scalars, the statment
;without the transpose bombs later, due to undocumented weird behavior
;of the strmid function that causes out_names to be an nXn matrix
   out_names = in_names
   for j = 0, n-1 do out_names[j] = strmid(in_names[j],0,base_len[j])+out_suf
endif else out_names = out_name + out_suf

if n_elements(in_names) ne n_elements(out_names) then begin
   message, 'thm_cotrans: number of input variables does not match number of output variables'
endif

;preprocess coordinate systems, so we can determine if probe keyword is required.
;This code also helps resolve discrepancies between in_coord keyword, and data_att.coord_sys
in_coords = strarr(n_elements(in_names))
for i = 0,n_elements(in_names)-1 do begin

  data_in_coord = cotrans_get_coord(in_names[i])
  
  if ~keyword_set(in_coord) || strmatch(in_coord, 'unknown') then begin
    in_coords[i] = data_in_coord
  endif else if strmatch(data_in_coord,'unknown') then begin
    in_coords[i] = in_coord
  endif else if data_in_coord ne in_coord then begin
    in_coords[i] = 'conflict'
  endif else begin
    in_coords[i] = in_coord
  endelse

endfor

if ~keyword_set(probe) then begin
   standard = strmatch(in_names,'th?_*')
   ;this only stops the routine if a probe is really necessary
   if total(standard) ne n_elements(in_names) then begin 
      idx = where(standard eq 0 and (in_coords eq 'spg' or in_coords eq 'ssl' or in_coords eq 'dsl'),c) 
      if c ne 0 || out_coord eq 'spg' || out_coord eq 'ssl' || out_coord eq 'dsl' then begin
        dprint, 'thm_cotrans: input name(s) do not specify probe according to THEMIS convention:'
        dprint, '            ', in_names[where(~standard)]
        dprint, '             Must specify probe with probe keyword'
        return
      endif else begin
        probe = strmid(in_names, 2,1)
        probe[where(~standard)] = 'a'
      endelse  
   endif else begin
     probe = strmid(in_names, 2,1)
   endelse
endif

for i = 0, n_elements(in_names)-1 do begin
  in_nam = in_names[i]
  out_nam = out_names[i]
  prb = probe[i < (n_elements(probe)-1)]

  get_data,in_nam,data=in,dl=in_dl
  if size(in, /type) ne 8 then begin
    message, /info, 'input tplot variable '+in_nam+' has no data'
    continue
  endif
  sizein=size(in.y)
  if sizein[0] ne 2 or sizein(2) ne 3 then begin
    message,/info,'Input tplot variable '+in_nam+' is not a 3-vector. Skipping'
    continue
  endif

  in_c = in_coords[i]
  
  if in_c eq 'conflict' then begin
    dprint,'Argument input coordinate system and data coordinate system of "' + in_nam + '" do not match. Skipping.'
    continue
  endif else if in_c eq 'unknown' then begin
    dprint,'Tplot variable "' + in_nam + '" has unknown input coordinate system. Skipping'
    continue
  endif

  dprint, 'thm_cotrans: coord. system of input '+in_nam+': '+in_c

  ; Select which tplot variables (with or without spin axis correction)
  ; to use for transforms in and out of DSL, depending on
  ; use_spinaxis_corrections keyword argument

  if keyword_set(use_spinaxis_correction) then begin
    message,/info,'Using spin axis correction'
    corrected='_corrected'
  endif else begin
    corrected=''
    message,/info,'Not using spin axis correction'
  endelse

  ; force support_suffix to '' if not explicitly specified
  if ~keyword_set(support_suffix) then support_suffix=''

  spinras = 'th'+prb+'_state_spinras'+corrected+support_suffix
  spindec = 'th'+prb+'_state_spindec'+corrected+support_suffix
  spinper = 'th'+prb+'_state_spinper'+support_suffix
  spinphase = 'th'+prb+'_state_spinphase'+support_suffix

  ; for spinras/spindec, might need to fall back to uncorrected
  ; versions if _corrected tplot variables don't exist

  if (strlen(corrected) GT 0) then begin
     tn_spinras = tnames(spinras)
     tn_spindec = tnames(spindec)
     
     if ((strlen(tn_spinras) EQ 0) OR (strlen(tn_spindec) EQ 0)) then begin
        spinras_unc = 'th'+prb+'_state_spinras'+support_suffix
        spindec_unc = 'th'+prb+'_state_spindec'+support_suffix
        message,/info,'spinras or spindec corrections not available, falling back to '+spinras_unc+' and '+spindec_unc
        spinras = spinras_unc
        spindec = spindec_unc
     endif
  endif
   
  if ~keyword_set(slp_suffix) then begin
    slp_suffix = ''
  endif
  
  name_sun_pos = 'slp_sun_pos'+slp_suffix
  name_lun_pos = 'slp_lun_pos'+slp_suffix
  
  thm_cotrans_transform_helper,in_nam,out_nam,in_c,out_coord, $
                      spinras,spindec,spinper,spinphase,ignore_dlimits=ignore_dlimits,$
                      interpolate_state=interpolate_state,$
                      use_spinphase_correction=use_spinphase_correction,$
                      name_sun_pos=name_sun_pos,name_lun_pos=name_lun_pos,$
                      prb=prb
   
   
  if n_elements(name_list) eq 0 then begin
    name_list = [out_nam]
  endif else begin
    name_list = [name_list,out_nam]
  endelse

;this statement is superfluous in the presence of new logic 
;validating coordinate systems   
;   if in_c ne out_coord then $
;     dprint, 'thm_cotrans: '+out_coord+' output placed in '+out_nam

endfor

if arg_present(out_vars) && n_elements(name_list) gt 0 then begin
  out_vars = name_list
endif

end
