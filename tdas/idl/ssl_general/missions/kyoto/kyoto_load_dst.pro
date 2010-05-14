;+
;
;Name:
;KYOTO_LOAD_DST
;
;Purpose:
;  Queries the Kyoto servers for DST data and loads data into
;  tplot format.  Adapted from KYOTO_AE2TPLOT.PRO.
;
;Syntax:
;  kyoto_load_dst [ ,TRANGE = TRANGE ]
;                  [ ,FILENAMES ]
;                  [ ,< and data keywords below > ]
;
;Keywords:
;  TRANGE (In):
;    Pass a time range a la TIME_STRING.PRO.
;  FILENAMES (In):
;    * PRESENTLY DISABLED * Pass user-defined file names (full paths to local data files).  These will
;      be read a la the Kyoto format, and the Kyoto server will not be queried.
;  DSTDATA, DSTTIME (Out):  Get 'dst' data, time basis.
;
;Code:
;W.M.Feuerstein, 5/15/2008.
;
;Modifications:
;  Changed file format of name (kyoto_dst_YYYY_MM.dat to kyoto_dst_YYYYMM.dat),
;    changed "DST" references to "DST", updated doc'n, WMF, 4/17/2008.
;  Saved new version under new name (old name was KYOTO_DST_LOAD), added
;    DATATYPE kw, validate and loop on datatypes, hardwired /DOWNLOADONLY,
;    up'd data kwd's, up'd doc'n, WMF, 5/15/2008.
;  Tested that the software defaults to local data when ther internet is not
;    available even with /DOWNLOADONLY (yes), added acknowledgement and
;    warning banner, added 'ax' datatype, WMF, 5/19/2008.
;  Changed name from KYOTO_DST2TPLOT.PRO to KYOTO_LOAD_DST.PRO, added
;    VERBOSE kw, added acknowledgement to DLIMITS structure, updated print, and
;    header acknowledgement, WMF, 7/23/08.
;  Changed acknowledgment as per Andreas Keiling, WMF, 8/4/2008.
;  YTITLE reflects whether Provisional or RT data, WMF, 8/12/2008.
;  YTITLE now set by file data (not kw), WMF, 8/15/2008.
;  Updated doc'n, WMF, 8/25/2008.
;
;Acknowledgment:
;  The DST data are provided by the World Data Center for Geomagnetism, Kyoto,  and
;  are not for redistribution (http://wdc.kugi.kyoto-u.ac.jp/). Furthermore, we thank
;  the geomagnetic observatories (Kakioka [JMA], Honolulu and San Juan [USGS], Hermanus
;  [RSA], Alibag [IIG]), NiCT, INTERMAGNET, and many others for their cooperation to
;  make the Dst index available.
;
; $LastChangedBy:  $
; $LastChangedDate:  $
; $LastChangedRevision:  $
; $URL $
;-

compile_opt idl2

pro kyoto_load_dst ,trange=trange, $
  verbose=verbose, $
  ;filenames=fns, $         ;Do not pounce on FILENAMES.
  dstdata=alldst, $
  dsttime=alldsttime;, $
  datatype=datatype;, $     ;Input/output -- will clean inputs or show default.
  ;source=source


;******************
;VERBOSE kw defaut:
;******************
if ~keyword_set(verbose) then verbose=2


;**************************
;Load 'dst' data by default:
;**************************
if ~keyword_set(datatype) then datatype='dst'


;*****************
;Validate datypes:
;*****************
vns=['dst']
if size(datatype,/type) eq 7 then begin
  datatype=thm_check_valid_name(datatype,vns,/include_all)
  if datatype[0] eq '' then return
endif else begin
  message,'DATATYPE kw must be of string type.',/info
  return
endelse


;Get timespan and define FILE_NAMES:
;===================================
get_timespan,t
if ~size(fns,/type) then begin

  ;Get files for ith datatype:
  ;***************************
  file_names = file_dailynames( $
    file_format='YYYYMM/dst'+ $
    'yyMM',trange=t,times=times,/unique)+'.for.request

  ;Define FILE_RETRIEVE structure for Provisional data:
  ;====================================================
  source = file_retrieve(/struct)
  source.verbose=verbose
  source.local_data_dir = root_data_dir() + 'geom_indices/kyoto/dst/'
  source.remote_data_dir = 'http://wdc.kugi.kyoto-u.ac.jp/dst_provisional/'

  ;Get files and local paths, and concatenate local paths:
  ;=======================================================
  local_paths0=file_retrieve(file_names,_extra=source)

  ;Redefine FILE_RETRIEVE structure for Real Time data:
  ;====================================================
  source.remote_data_dir = 'http://wdc.kugi.kyoto-u.ac.jp/dst_realtime/'

  ;Get files and local paths, and concatenate local paths:
  ;=======================================================
  local_paths1=file_retrieve(file_names,_extra=source)

  ;Concatenate and unique possible file names from Provisional and RT data:
  ;========================================================================
  local_paths=[local_paths0,local_paths1]
  local_paths=local_paths(uniq(local_paths,sort(local_paths)))

endif else file_names=fns

;basedate=time_string(times,tformat='YYYY-MM-01')
;baseyear=strmid(basedate,0,4)



;Read the files:
;===============
s=''
alldsttime=0
alldst= 0
alldstversion = 0


;Loop on files:
;==============
for i=0,n_elements(local_paths)-1 do begin
  file= local_paths[i]
  if file_test(/regular,file) then  dprint,'Loading DST file: ',file $
    else begin
      dprint,'DST file ',file,' not found. Skipping'
      continue
    endelse
    openr,lun,file,/get_lun
    ;basetime = time_double(basedate[i])
    ;
    ;Loop on lines (format documented at
    ;http://wdc.kugi.kyoto-u.ac.jp/dstae/format/dstformat.html):
    ;===========================================================
    while(not eof(lun)) do begin
      readf,lun,s
      ok=1
      if strmid(s,0,1) eq '[' then ok=0
      if ok && keyword_set(s) then begin
         dprint,s ,dlevel=5
         year_lower = (strmid(s,3,2))
         year_upper= (strmid(s,14,2))
         month = (strmid(s,5,2))
         day = (strmid(s,8,2))
;         hour = (strmid(s,19,2))
         type = strmid(s,0,3)
         version=strmid(s,13,1)     ;despite online docs, should be only 0 or 1.
         basetime = time_double(year_upper+year_lower+ $
           '-'+month+'-'+day)
         ;
         kdata = fix ( strmid(s, indgen(24)*4 +20 ,4) )
         ;
         ;Append data by type (DST):
         ;===========================================
         case type of
           'DST': begin
	     append_array,alldst,kdata
	     append_array,alldsttime, basetime + dindgen(24)*3600d
             append_array,alldstversion,fix(version)
	     dprint,' ',s,dlevel=5
	   end
         endcase
         continue
      endif

      ;if s eq 'DAY' then ok=1
    endwhile
    free_lun,lun
endfor

acknowledgestring = 'The DST data are provided by the World Data Center for Geomagnetism, Kyoto, and'+ $
  ' are not for redistribution (http://wdc.kugi.kyoto-u.ac.jp/). Furthermore, we thank'+ $
  ' the geomagnetic observatories (Kakioka [JMA], Honolulu and San Juan [USGS], Hermanus'+ $
  ' [RSA], Alibag [IIG]), NiCT, INTERMAGNET, and many others for their cooperation to'+ $
  ' make the Dst index available.'

;Store data in TPLOT variables setting bad data to NaN, and setting ytitle:
;==========================================================================
if keyword_set(alldst) then begin
  alldst= float(alldst)
  wbad = where(alldst eq 99999,nbad)
  if nbad gt 0 then alldst[wbad] = !values.f_nan
  dlimit=create_struct('data_att',create_struct('acknowledgment',acknowledgestring))
  store_data,'kyoto_dst',data={x:alldsttime, y:alldst},dlimit=dlimit

  ;Determine version and set ytitle:
  ;=================================
  prov_ind = where(alldstversion,cprov,ncomplement=crt)
  case 1 of
    cprov && crt: options,'kyoto_dst','ytitle','Kyoto!CRT/Prov. DST!C[nT]'
    logical_true(cprov): options,'kyoto_dst','ytitle','Kyoto!CProv. DST!C[nT]'
    logical_true(crt): options,'kyoto_dst','ytitle','Kyoto!CRT DST!C[nT]'
  endcase
endif
;

print,'**************************************************************************************
print,  'The DST data are provided by the World Data Center for Geomagnetism, Kyoto, and'
print,  ' are not for redistribution (http://wdc.kugi.kyoto-u.ac.jp/). Furthermore, we thank'
print,  ' the geomagnetic observatories (Kakioka [JMA], Honolulu and San Juan [USGS], Hermanus'
print,  ' [RSA], Alibag [IIG]), NiCT, INTERMAGNET, and many others for their cooperation to'
print,  ' make the Dst index available.'
print,'**************************************************************************************


end

