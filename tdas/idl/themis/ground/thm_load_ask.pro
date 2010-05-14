;+
;Procedure: THM_LOAD_ASK
;
;Purpose:  Loads THEMIS All Sky Keograms
;
;keywords:
;  site  = Observatory name, example, thm_load_gmag, site = 'fykn', the
;          default is 'all', i.e., load all available stations . This
;          can be an array of strings, e.g., ['fykn', 'gako'] or a
;          single string delimited by spaces, e.g., 'fykn gako'
;  datatype = request 'ast' or 'asf', default is 'asf', can also be 'all'.
;  TRANGE= (Optional) Time range of interest  (2 element array), if
;          this is not set, the default is to prompt the user. Note
;          that if the input time range is not a full day, a full
;          day's data is loaded
;  level = the level of the data, the default is 'l2', or level-2
;          data. A string (e.g., 'l2') or an integer can be used. 'all'
;          can be passed in also, to get all levels.
;  /VERBOSE : set to output some useful info
;  /downloadonly, if set, then only download the data, do not load it
;                 into variables.
;  /no_download: use only files which are online locally.
;  relpathnames_all: named variable in which to return all files that are
;          required for specified timespan, probe, datatype, and level.
;          If present, no files will be downloaded, and no data will be loaded.
;  /valid_names, if set, then this will return the valid site, datatype
;                and/or level options in named variables, for example,
;
;                thm_load_gmag, site = xxx, /valid_names
;
;                will return the array of valid sites in the
;                variable xxx
;  get_support_data = does nothing.  present only for consistency with other
;                load routines
;Example:
;   thg_load_ask
;Notes:
;  This routine is (should be) platform independent.
;
;
; $LastChangedBy: jimm $
; $LastChangedDate: 2009-11-02 13:42:04 -0800 (Mon, 02 Nov 2009) $
; $LastChangedRevision: Added valid_names output option$
; $URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/thmsoc/tags/tdas_5_21/idl/themis/ground/thm_load_ask.pro $
;-

; find the correct file names, based on trange, datatype, and site
function thm_load_ask_relpath, trange=trange, _extra=_extra

   relpath = 'thg/l1/asi/ask/'
   prefix = 'thg_l1_ask_'
   ending = '_v01.cdf'
;stop
   return, file_dailynames(relpath,prefix,ending,/YEARDIR,trange=trange)

end

pro thm_load_ask,site = site, datatype = datatype, trange = trange, $
                 level = level, verbose = verbose, $
                 downloadonly = downloadonly, $
                 no_download=no_download, relpathnames_all=relpathnames_all, $
                 varformat=varformat, $
                 valid_names = valid_names, $
                 get_support_data=get_support_data, $
                 progobj=progob, files=files, suffix=suffix
;                 _extra = _extra

  if arg_present(relpathnames_all) then begin
     downloadonly=1
     no_download=1
  end

  thm_load_xxx,sname=site, datatype=datatype, trange=trange, $
               level=level, verbose=verbose, downloadonly=downloadonly, $
               no_download=no_download, relpathnames_all=relpathnames_all, $
               cdf_data=cdf_data,get_cdf_data=arg_present(cdf_data), $
               get_support_data=get_support_data, $
               varnames=varnames, valid_names = valid_names, files=files, $
               varformat=varformat, $
               vsnames = 'atha chbg ekat fsmi fsim fykn gako gbay gill '+ $
               'inuv kapu kian kuuj mcgr pgeo pina rank snkq tpas whit yknf '+ $
               'nrsq snap talo', $
               type_sname = 'site', /all_sites_in_one, $
               vdatatypes = 'ask', $
               vlevels = 'l1', $
               deflevel = 'l1', $
               version = 'v01', $
               relpath_funct = 'thm_load_ask_relpath', $
               progobj = progobj, tplotnames=tplotnames, $
               suffix=suffix,$
               _extra = _extra

end
