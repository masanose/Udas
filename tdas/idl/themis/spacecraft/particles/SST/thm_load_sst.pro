
;change Matt D. 6/29
;----------------------

function thm_load_sst_relpath, sname=probe, filetype=ft, $
                               level=lvl, trange=trange, $
                               addmaster=addmaster, _extra=_extra

  relpath = 'th'+probe+'/'+lvl+'/'+ ft+'/'
  prefix = 'th'+probe+'_'+lvl+'_'+ft+'_'
  dir = 'YYYY/'
  ending = '_v01.cdf'

  return, file_dailynames(relpath, prefix, ending, dir=dir, $
                          trange = trange,addmaster=addmaster)
end

;----------------------



;Procedure: THM_LOAD_SST
;
;Purpose:  Loads THEMIS SST data
;
;keywords:
;  probe = Probe name. The default is 'all', i.e., load all available probes.
;          This can be an array of strings, e.g., ['a', 'b'] or a
;          single string delimited by spaces, e.g., 'a b'
;  datatype = The type of data to be loaded, for this case, there is only
;          one option, the default value of 'sst', so this is a
;          placeholder should there be more that one data type. 'all'
;          can be passed in also, to get all variables.
;  TRANGE= (Optional) Time range of interest  (2 element array), if
;          this is not set, the default is to prompt the user. Note
;          that if the input time range is not a full day, a full
;          day's data is loaded
;  level = the level of the data, the default is 'l1', or level-1
;          data. A string (e.g., 'l2') or an integer can be used. 'all'
;          can be passed in also, to get all levels.
;  CDF_DATA: named variable in which to return cdf data structure: only works
;          for a single spacecraft and datafile name.
;  VARNAMES: names of variables to load from cdf: default is all.
;  /GET_SUPPORT_DATA: load support_data variables as well as data variables
;                      into tplot variables.
;  /DOWNLOADONLY: download file but don't read it.
;  /valid_names, if set, then this routine will return the valid probe, datatype
;          and/or level options in named variables supplied as
;          arguments to the corresponding keywords.
;  files   named varible for output of pathnames of local files.
;  /VERBOSE  set to output some useful info
;Example:
;   thg_load_sst,/get_suppport_data,probe=['a', 'b']
;Notes:
; Written by Davin Larson, Dec 2006
; Updated to use thm_load_xxx by KRB, 2007-2-5
; Update removed to not use thm_load_xxx by DEL
;
; $LastChangedBy: jimm $
; $LastChangedDate: 2010-02-08 12:04:51 -0800 (Mon, 08 Feb 2010) $
; $LastChangedRevision: 7225 $
; $URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/thmsoc/tags/tdas_5_21/idl/themis/spacecraft/particles/SST/thm_load_sst.pro $
;-
pro thm_load_sst,probe=probematch, datatype=datatype, trange=trange, $
                 level=level, verbose=verbose, downloadonly=downloadonly, $
                 cdf_data=cdf_data,get_support_data=get_support_data, $
                 varnames=varnames, valid_names = valid_names, files=files, $
                 source_options = source_options, $
                 progobj=progobj, varformat=varformat

tn_pre_proc = tnames()

if not keyword_set(source_options) then begin
   thm_init
   source_options = !themis
endif
my_themis = source_options
;my_themis.remote_data_dir += 'qa/'  ; remove this line after files have moved to proper location.


vb = keyword_set(verbose) ? verbose : 0
vb = vb > my_themis.verbose
dprint,dlevel=4,verbose=vb,'Start; $Id: thm_load_sst.pro 7225 2010-02-08 20:04:51Z jimm $'

vprobes = ['a','b','c','d','e'];,'f']
vlevels = ['l1','l2']
vdatatypes=['sst']

if keyword_set(valid_names) then begin
    probematch = vprobes
    level = vlevels
    datatype = vdatatypes
    return
endif

if n_elements(probematch) eq 1 then if probematch eq 'f' then vprobes = ['f']

;if not keyword_set(probematch) then probematch='*'
;probe = strfilter(vprobes, probematch ,delimiter=' ',/string)

if not keyword_set(probematch) then probematch=vprobes
probe=thm_check_valid_name(strtrim(strlowcase(probematch),2),vprobes,/include_all)

if probe[0] eq '' then begin
  print,"Invalid probes selected.  Valid probes: 'a','b','c','d' or 'e'  (ie, probe='a')"
  return
end

;change Matt D. 6/29
;----------------------
vlevels_str='l1 l2'
deflevel='l1'
lvl = thm_valid_input(level,'Level',vinputs=vlevels_str,definput=deflevel,$
                        format="('l', I1)", verbose=0)

if lvl eq 'l2' then goto, LEVEL2FILELOAD
;----------------------


if not keyword_set(datatype) then datatype='*'
datatype = strfilter(vdatatypes, datatype ,delimiter=' ',/string)

addmaster=0

for s=0,n_elements(probe)-1 do begin
     sc = 'th'+ probe[s]

;     format = sc+'l1/sst/YYYY/'+sc+'_l1_sst_YYYYMMDD_v01.cdf'   ; Won't work! for sst
     relpathnames = file_dailynames(sc+'/l1/sst/',dir='YYYY/',sc+'_l1_sst_','_v01.cdf',trange=trange,addmaster=addmaster)
     files = file_retrieve(relpathnames, _extra=my_themis ) ;, nowait=downloadonly)

     if keyword_set(downloadonly) or my_themis.downloadonly then continue

     cdfi = cdf_load_vars(files,/all,verbose=vb)
     if not keyword_set(cdfi) then begin
        continue
     endif

;     name = sc+'_sst_raw_data'
;     data_cache,name,data,/set,/no_copy
     vns = cdfi.vars.name

;     distdat = {  $
;        data:ptr_new()       ,$
;        time:ptr_new()    ,$
;        cnfg:ptr_new()    ,$
;        emode: ptr_new()    ,$
;        amode: ptr_new()    $
;     }

     cache = {  $
     project_name:    ''     , $
     data_name:  'SST data'  , $
     sc_name:      probe[s]    , $
     sif_064_time  : cdfi.vars[where(vns eq sc+'_sif_064_time')].dataptr   , $
     sif_064_cnfg  : cdfi.vars[where(vns eq sc+'_sif_064_config')].dataptr   , $
     sif_064_nspins: cdfi.vars[where(vns eq sc+'_sif_064_nspins')].dataptr   , $
     sif_064_atten : cdfi.vars[where(vns eq sc+'_sif_064_atten')].dataptr   , $
;     sif_064_hed   : cdfi.vars[where(vns eq sc+'_sif_064_hed')].dataptr   , $
     sif_064_data  : cdfi.vars[where(vns eq sc+'_sif_064')].dataptr  , $

     sef_064_time  : cdfi.vars[where(vns eq sc+'_sef_064_time')].dataptr  , $
     sef_064_cnfg  : cdfi.vars[where(vns eq sc+'_sef_064_config')].dataptr  , $
     sef_064_nspins: cdfi.vars[where(vns eq sc+'_sef_064_nspins')].dataptr   , $
     sef_064_atten : cdfi.vars[where(vns eq sc+'_sef_064_atten')].dataptr   , $
;     sef_064_hed   : cdfi.vars[where(vns eq sc+'_sef_064_hed')].dataptr   , $
     sef_064_data  : cdfi.vars[where(vns eq sc+'_sef_064')].dataptr  , $

     seb_064_time  : cdfi.vars[where(vns eq sc+'_seb_064_time')].dataptr  , $
     seb_064_cnfg  : cdfi.vars[where(vns eq sc+'_seb_064_config')].dataptr  , $
     seb_064_nspins: cdfi.vars[where(vns eq sc+'_seb_064_nspins')].dataptr   , $
     seb_064_atten : cdfi.vars[where(vns eq sc+'_seb_064_atten')].dataptr   , $
;     seb_064_hed   : cdfi.vars[where(vns eq sc+'_seb_064_hed')].dataptr   , $
     seb_064_data  : cdfi.vars[where(vns eq sc+'_seb_064')].dataptr  , $

     sir_001_time  : cdfi.vars[where(vns eq sc+'_sir_001_time')].dataptr  , $
     sir_001_cnfg  : cdfi.vars[where(vns eq sc+'_sir_001_config')].dataptr  , $
     sir_001_nspins: cdfi.vars[where(vns eq sc+'_sir_001_nspins')].dataptr   , $
     sir_001_atten : cdfi.vars[where(vns eq sc+'_sir_001_atten')].dataptr   , $
;     sir_001_hed   : cdfi.vars[where(vns eq sc+'_sir_001_hed')].dataptr   , $
     sir_001_data  : cdfi.vars[where(vns eq sc+'_sir_001')].dataptr  , $
     ser_001_time  : cdfi.vars[where(vns eq sc+'_ser_001_time')].dataptr  , $
     ser_001_cnfg  : cdfi.vars[where(vns eq sc+'_ser_001_config')].dataptr  , $
     ser_001_nspins: cdfi.vars[where(vns eq sc+'_ser_001_nspins')].dataptr   , $
     ser_001_atten : cdfi.vars[where(vns eq sc+'_ser_001_atten')].dataptr   , $
;     ser_001_hed   : cdfi.vars[where(vns eq sc+'_ser_001_hed')].dataptr   , $
     ser_001_data  : cdfi.vars[where(vns eq sc+'_ser_001')].dataptr  , $

     sir_006_time  : cdfi.vars[where(vns eq sc+'_sir_006_time')].dataptr  , $
     sir_006_cnfg  : cdfi.vars[where(vns eq sc+'_sir_006_config')].dataptr  , $
     sir_006_nspins: cdfi.vars[where(vns eq sc+'_sir_006_nspins')].dataptr   , $
     sir_006_atten : cdfi.vars[where(vns eq sc+'_sir_006_atten')].dataptr   , $
     sir_006_data  : cdfi.vars[where(vns eq sc+'_sir_006')].dataptr  , $

     ser_006_time  : cdfi.vars[where(vns eq sc+'_ser_006_time')].dataptr  , $
     ser_006_cnfg  : cdfi.vars[where(vns eq sc+'_ser_006_config')].dataptr  , $
     ser_006_nspins: cdfi.vars[where(vns eq sc+'_ser_006_nspins')].dataptr   , $
     ser_006_atten : cdfi.vars[where(vns eq sc+'_ser_006_atten')].dataptr   , $
     ser_006_data  : cdfi.vars[where(vns eq sc+'_ser_006')].dataptr  , $

     sir_mix_time  : ptr_new() , $
     sir_mix_index : ptr_new() , $
     sir_mix_mode  : ptr_new() , $
     ser_mix_time  : ptr_new() , $
     ser_mix_index : ptr_new() , $
     ser_mix_mode  : ptr_new() , $

     valid : 1 }

     if ptr_valid(cache.sir_006_time) && ptr_valid(cache.sir_001_time) then begin

        sir_mix_time  = [ *cache.sir_006_time, *cache.sir_001_time ] ; this can easily fail!
        sir_mix_index = [lindgen(n_elements(*cache.sir_006_time)), lindgen(n_elements(*cache.sir_001_time)) ]
        sir_mix_mode  = [replicate(0,n_elements(*cache.sir_006_time)), replicate(1,n_elements(*cache.sir_001_time)) ]

     endif else if ptr_valid(cache.sir_001_time) then begin

        sir_mix_time  = [ *cache.sir_001_time ] ; this can easily fail!
        sir_mix_index = [ lindgen(n_elements(*cache.sir_001_time)) ]
        sir_mix_mode  = [ replicate(1,n_elements(*cache.sir_001_time)) ]

     endif else begin

        dprint,dlevel=0,'No valid ion data in interval'
        return

     endelse

     srt = sort(sir_mix_time)
     cache.sir_mix_time  = ptr_new( sir_mix_time[srt] )
     cache.sir_mix_index = ptr_new( sir_mix_index[srt] )
     cache.sir_mix_mode  = ptr_new( sir_mix_mode[srt] )

     if ptr_valid(cache.ser_006_time) && ptr_valid(cache.ser_001_time) then begin

        ser_mix_time  = [ *cache.ser_006_time, *cache.ser_001_time ] ; this can easily fail!
        ser_mix_index = [lindgen(n_elements(*cache.ser_006_time)), lindgen(n_elements(*cache.ser_001_time)) ]
        ser_mix_mode  = [replicate(0,n_elements(*cache.ser_006_time)), replicate(1,n_elements(*cache.ser_001_time)) ]

     endif else if ptr_valid(cache.ser_001_time) then begin

        ser_mix_time  = [*cache.ser_001_time ] ; this can easily fail!
        ser_mix_index = [lindgen(n_elements(*cache.ser_001_time)) ]
        ser_mix_mode  = [replicate(1,n_elements(*cache.ser_001_time)) ]

     endif else begin

        dprint,dlevel=0,'No valid ion data in interval'
        return

     endelse

     srt = sort(ser_mix_time)
     cache.ser_mix_time  = ptr_new( ser_mix_time[srt] )
     cache.ser_mix_index = ptr_new( ser_mix_index[srt] )
     cache.ser_mix_mode  = ptr_new( ser_mix_mode[srt] )


     ptrs = ptr_extract(cdfi,except=ptr_extract(cache))
     ptr_free,ptrs
     name = sc+'_sst_raw_data'

     ; Check for timing problems:
     ctags = tag_names(cache)
     wt = strfilter(ctags,'*TIME',count=c,/index)
     for i=0,c-1 do begin
         if ptr_valid( cache.(wt[i]) ) then begin
             t = *( cache.(wt[i]) )
             dt = t-shift(t,1)
             If(n_elements(dt) Gt 1) Then dt[0] = dt[1]
             w = where(dt le 0,nw)
             if nw gt 0 then begin
;                beep
                dprint,dlevel=0,'Data File Error: ',name,'  ',ctags[wt[i]]
                dprint,dlevel=0,/phelp,w
                dprint,dlevel=0,/phelp,dt[w]  ;,varname='dt'
                dprint,dlevel=0,/phelp,time_string(t[w])  ;,varname='time'
;                wait,1.             ;bp
             endif
         endif
     endfor

     data_cache,name,cache,/set,/no_copy

     thm_sst_to_tplot,probe=probe[s]

     ; make sure tplot_vars created in post_procs get added to list
     tn_post_proc = tnames()

     if ~array_equal(tn_pre_proc, '') then begin

       ; make ssl_set_intersection doesn't get scalar inputs
       if n_elements(tn_pre_proc) eq 1 then tn_pre_proc=[tn_pre_proc]
       if n_elements(tn_post_proc) eq 1 then tn_post_proc=[tn_post_proc]
       
       post_proc_names = ssl_set_complement(tn_pre_proc, tn_post_proc)
       if size(post_proc_names, /type) eq 7 then tplotnames = post_proc_names
     endif else tplotnames = tn_post_proc
     
     ; clip data to requested trange
     If (keyword_set(trange) && n_elements(trange) Eq 2) $
       Then tr = timerange(trange) $
       else tr = timerange()
     for i = 0, n_elements(tplotnames)-1 do begin
       if tnames(tplotnames[i]) eq '' then continue
       
       time_clip, tplotnames[i], min(tr), max(tr), /replace, error = tr_err
       if tr_err then del_data, tplotnames[i]
     endfor

endfor


return


;change Matt D. 6/29
;----------------------

LEVEL2FILELOAD:

  if arg_present(relpathnames_all) then begin
     downloadonly=1
     no_download=1
  end
  if not keyword_set(suffix) then suffix = ''

  vlevels_str = 'l1 l2'
  deflevel = 'l2'
  lvl = thm_valid_input(level,'Level',vinputs=vlevels_str,definput=deflevel,$
                        format="('l', I1)", verbose=0)
  if lvl eq '' then return

  vL2datatypes='psif_en_eflux psef_en_eflux'
;  datatype='*';jmm, feb-8-2010

  thm_load_xxx,sname=probe, datatype=datatype, trange=trange, $
               level=level, verbose=verbose, downloadonly=downloadonly, $
               relpathnames_all=relpathnames_all, no_download=no_download, $
               cdf_data=cdf_data,get_cdf_data=arg_present(cdf_data), $
               get_support_data=get_support_data, $
               varnames=varnames, valid_names = valid_names, files=files, $
               vsnames = 'a b c d e', $
               type_sname = 'probe', $
               vdatatypes = 'sst', $
               file_vdatatypes = 'sst', $
               vlevels = vlevels_str, $
               vL2datatypes = vL2datatypes, $
               vL2coord = '', $
               deflevel = deflevel, $
               version = 'v01', $
               relpath_funct = 'thm_load_sst_relpath', $
               post_process_proc=post_process_proc, $
               delete_support_data=delete_support_data, $
               proc_type=type, coord=coord, suffix=suffix, $
               progobj=progobj,$
               varformat=varformat

  ylim,'*en_eflux*',0,0,1
  zlim,'*en_eflux*',0,0,1

;----------------------


end







