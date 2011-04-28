;+
; PROCEDURE: IUG_LOAD_EISCAT
;   iug_load_eiscat, site=site, $
;                    pulse_code=pulse_code, $
;                    int_time=int_time, $
;                    ydatatype=ydatatype, $
;                    trange=trange, $
;                    verbose=verbose, $
;                    downloadonly=downloadonly, $
;                    no_download=no_download
;
; PURPOSE:
;   loads the EISCAT radar data.
;
; KEYWORDS:
;   site  = Combination of observatory and antenna, example, 
;           erg_load_eiscat, site='esr_42m',
;           the default is 'all', i.e., load all available sites.
;           This can be an array of strings, e.g., ['esr_32m', 'esr_42m']
;           or a single string delimited by spaces, e.g., 'esr_32m esr_42m'.
;   pulse_code = Pulse code
;   int_time = Integration time
;   ydatatype = data type of y-axis. Default is 'alt', i.e., altitude.
;   trange = (Optional) Time range of interest  (2 element array).
;   /verbose: set to output some useful info
;   /downloadonly: if set, then only download the data, do not load it 
;           into variables.
;   /no_download: use only files which are online locally.
;
; EXAMPLE:
;   iug_load_eiscat, site='esr_42m', $
;                 trange=['2011-2-1/00:00:00','2011-2-3/00:00:00']
;
; NOTE: There is an alias of this procedure named "erg_load_eiscat".
;   Some load procedures for the ground-based observational data 
;   in the  ERG mission, named "erg_load_???", can be also called  
;   by "iug_load_???", because these data are related to the both 
;   ERG and IUGONET projects.
;   For more information, see http://www.iugonet.org/en/ 
;                         and http://gemsissc.stelab.nagoya-u.ac.jp/erg/
;
; Written by Y.-M. Tanaka, March 22, 2011 (ytanaka at nipr.ac.jp)
;-

pro iug_load_eiscat, site=site, pulse_code=pulse_code, int_time=int_time, $
        ydatatype=ydatatype, trange=trange, verbose=verbose, downloadonly=downloadonly, $
	no_download=no_download

;*************************
;***** Keyword check *****
;*************************
;--- verbose
if ~keyword_set(verbose) then verbose=0

;--- all sites (default)
;site_code_all = strsplit('esr_32m esr_42m tro_vhf tro_uhf kir_uhf sod_uhf', $
;			/extract)
;pulse_code_all = strsplit('cp0 cp1 cp2 cp3 cp4 cp5 cp6 cp7 cp8 cp9 '$
;                 +'tau0 tau1 tau2 tau3 tau4 tau5 tau6 tau7 tau8 tau9 '$
;                 +'taro ipy0 beata', /extract)
;int_time_all = strsplit('0000 0060', /extract)

site_code_all = strsplit('esr_42m', /extract)
pulse_code_all = strsplit('ipy0', /extract)
int_time_all = strsplit('0000', /extract)
ydatatype_all = strsplit('alt lat lon', /extract)

;--- check site codes
if(not keyword_set(site)) then site='all'
site_code = thm_check_valid_name(site, site_code_all, /ignore_case, /include_all)
if site_code[0] eq '' then return

print, 'site_code = '+site_code

;--- validate pulse_code
if(not keyword_set(pulse_code)) then pulse_code='all'
pc = thm_check_valid_name(pulse_code, pulse_code_all, /ignore_case, /include_all)
if pc[0] eq '' then return

print, 'pulse_code = '+pc

;--- validate int_time
if(not keyword_set(int_time)) then int_time='all'
intt = thm_check_valid_name(int_time, int_time_all, /ignore_case, /include_all)
if intt[0] eq '' then return

;--- validate ydatatype
if(not keyword_set(ydatatype)) then ydatatype='alt'
ytype = thm_check_valid_name(ydatatype, ydatatype_all, /ignore_case)
if ytype eq '' then return
if ytype eq 'lon' then ytype='long'

print, 'integration time = '+intt

instr='eiscat'

;*************************************************************************
;***** Download files, read data, and create tplot vars at each site *****
;*************************************************************************
;=================================
;=== Loop on downloading files ===
;=================================
;*** load CDF ***
for i=0,n_elements(site_code)-1 do begin
  site1=site_code(i)
  site1=strsplit(site1, '_', /extract)
  stn=site1(0)
  ant=site1(1)
  for j=0,n_elements(pc)-1 do begin
    pc1=pc(j)
    for k=0,n_elements(intt)-1 do begin
      intt1=intt(k)

      ;--- Create (and initialize) a data file structure 
      source = file_retrieve(/struct)
      source.verbose = verbose

      ;--- Set parameters for the data file class
      source.local_data_dir  = root_data_dir() + 'iugonet/'
      source.remote_data_dir = 'http://www.nipr.ac.jp/~ytanaka/data/'
;      source.remote_data_dir = 'http://iugonet0.nipr.ac.jp/data/'
      if keyword_set(no_download) then source.no_download = 1

      relpathnames1 = file_dailynames(file_format='YYYY', trange=trange)
      relpathnames2 = file_dailynames(file_format='YYYYMMDD', trange=trange)
      relpathnames  = instr+'/'+stn+'/'+ant+'/'+$
        relpathnames1 + '/eiscat_kn_'+stn+ant+'_'+pc1+'_'+intt1+'_'+$
        relpathnames2 + '_v??.cdf'

      ;--- Download the designated data files from the remote data server
      ;    if the local data files are older or do not exist.
      files = file_retrieve(relpathnames, _extra=source, /last_version)

      ;--- print PI info and rules of the road
      if(file_test(files[0])) then begin
        gatt = cdf_var_atts(files[0])

        print, '**************************************************************************************'
        ;print, gatt.project
        print, gatt.Logical_source_description
        print, ''
        print, 'Information about ', gatt.Title
        print, 'PI: ', gatt.PI_name
        print, 'Affiliations: ', gatt.PI_affiliation
        print, ''
        print, 'Rules of the Road for EISCAT Radar Data:'
        ;--- Show gatt.text ---;
        linelen=100
        textlen=strlen(gatt.Rules_of_use)
        remtext=gatt.Rules_of_use
        for iline=0, textlen/linelen do begin
          remtextlen=strlen(remtext)
          if remtextlen gt linelen then begin
            line1=strmid(remtext, 0, linelen)
            for istr=0, linelen-1 do begin
              str1=strmid(line1,linelen-istr-1,1)
              if str1 eq ' ' then begin
                line1=strmid(line1, 0, linelen-istr-1)
                remtext=strmid(remtext, linelen-istr, remtextlen-linelen+istr+1)
                break
              endif
            endfor
            print, line1
          endif else begin
            line1=strmid(remtext, 0, remtextlen)
            print, line1
            break
          endelse
        endfor
        ;----------------------;
        print, ''
        print, gatt.LINK_TEXT, ' ', gatt.HTTP_LINK
        print, '**************************************************************************************'
      endif

      ;--- Load data into tplot variables
      if(not keyword_set(downloadonly)) then downloadonly=0

      if(downloadonly eq 0) then begin
        prefix='eiscat_'+stn+ant+'_'+pc1+'_'
        cdf2tplot, file=files, verbose=source.verbose, prefix=prefix

        len=strlen(tnames(prefix+'ne_0'))
        if len eq 0 then begin
          ;--- Quit if no data have been loaded
          print, 'No tplot var loaded for '+site_code[i]+'.'
        endif else begin
          ;----- ydatatype -----;
	  get_data, prefix+ytype+'_0', data=d
	  vdat=average(d.y,1,/NAN)

          ;--- Rename tplot variables -----;
          tplot_name_all=tnames(prefix+'*_0')
	  for itname=0, n_elements(tplot_name_all)-1 do begin
	    tplot_name_tmp=tplot_name_all(itname)
            len=strlen(tplot_name_tmp)
            tplot_name_new=strmid(tplot_name_tmp,0,len-2)
            copy_data, tplot_name_tmp, tplot_name_new
            store_data, tplot_name_tmp, /delete

            ;--- Missing data -1.e+31 --> NaN
;            tclip, tplot_name_new, -1e+5, 1e+5, /overwrite

            ;--- Labels
            case tplot_name_new of
	      prefix+'lat' : options, tplot_name_new, labels='Lat', $
		ytitle='Latitude', ysubtitle = '[deg]'
	      prefix+'long' : options, tplot_name_new, labels='Lon', $
		ytitle='Longitude', ysubtitle = '[deg]'
	      prefix+'alt' : options, tplot_name_new, labels='Alt', $
		ytitle='Altitude', ysubtitle = '[km]'
	      prefix+'ne' : options, tplot_name_new, labels='Ne', $
		ytitle='Ne', ysubtitle = '[m-3]', 'spec'
	      prefix+'ne_err' : options, tplot_name_new, labels='Ne err.', $
		ytitle='Ne err.', ysubtitle = '[m-3]', 'spec'
	      prefix+'te' : options, tplot_name_new, labels='Te', $
		ytitle='Te', ysubtitle = '[K]', 'spec'
	      prefix+'te_err' : options, tplot_name_new, labels='Te err.', $
		ytitle='Te err.', ysubtitle = '[K]', 'spec'
	      prefix+'ti' : options, tplot_name_new, labels='Ti', $
		ytitle='Ti', ysubtitle = '[K]', 'spec'
	      prefix+'ti_err' : options, tplot_name_new, labels='Ti err.', $
		ytitle='Ti err.', ysubtitle = '[K]', 'spec'
	      prefix+'vi' : options, tplot_name_new, labels='Vi', $
		ytitle='Vi', ysubtitle = '[m/s]', 'spec'
	      prefix+'vi_err' : options, tplot_name_new, labels='Vi err.', $
		ytitle='Vi err.', ysubtitle = '[m/s]', 'spec'
	      prefix+'collision_freq' : options, tplot_name_new, labels='col.freq.', $
		ytitle='col.freq.', ysubtitle = '[s-1]', 'spec'
	      prefix+'composition' : options, tplot_name_new, labels='comp', $
		ytitle='composition', ysubtitle = '[%]', 'spec'
	      prefix+'quality' : options, tplot_name_new, labels='quality', $
		ytitle='quality', 'spec'
	      prefix+'quality_flag' : options, tplot_name_new, labels='qual.flag', $
		ytitle='quality flag', 'spec'
              else : print, ''
            endcase

            case tplot_name_new of
	      prefix+'ne' : zlim, tplot_name_new, 1e10, 1e12, 1
              else : print, ''
            endcase

            ;----- Replace d.v with ydatatype -----;
            get_data, tplot_name_new, data=d, dl=dl, lim=lim
            store_data, tplot_name_new, data={x:d.x, y:d.y,v:vdat},dl=dl,lim=lim

;            store_data,'tha_comb',data=['tha_peif_en_eflux',tplot_name_new]

          endfor
        endelse
      endif

    endfor
  endfor
endfor

;---
return
end


