;+
; PROCEDURE: IUG_LOAD_EISCAT
;   iug_load_eiscat, site=site, $
;                    ydatatype=ydatatype, $
;                    get_support_data=get_support_data, $
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
;   ydatatype = data type of y-axis. Default is 'alt', i.e., altitude.
;   trange = (Optional) Time range of interest  (2 element array).
;   /get_support_data : turn this on to load the supporting data.
;   /verbose: set to output some useful info
;   /downloadonly: if set, then only download the data, do not load it 
;           into variables.
;   /no_download: use only files which are online locally.
;
; EXAMPLE:
;   iug_load_eiscat, site='esr_42m', $
;                 trange=['2011-2-1/00:00:00','2011-2-3/00:00:00']
;
;   For more information, see http://www.iugonet.org/en/ 
;                         and http://polaris.nipr.ac.jp/~eiscat/eiscatdata/
;
; Written by: Y.-M. Tanaka, July 25, 2011 (ytanaka at nipr.ac.jp)
; Modified by: Y.-M. Tanaka, August 24, 2011
;   Separated "print_str_maxlet" to another file. 
; Modified by: Y.-M. Tanaka, December 1, 2011
;   to load data with various int_times separately to different tplot
;   vars.
; Modified by: Y.-M. Tanaka, June 6, 2012
;   Integragted all pulse_code and integration_time data for one day 
;   into a CDF file
;-

;********************************************
;*** Load procedure for EISCAT radar data ***
;********************************************
pro iug_load_eiscat, site=site, ydatatype=ydatatype, $
        trange=trange, get_support_data=get_support_data, $
	verbose=verbose, downloadonly=downloadonly, no_download=no_download

;===== Keyword check =====
;----- all codes -----;
site_code_all = strsplit('esr_32m esr_42m tro_vhf tro_uhf kir_uhf sod_uhf', $
			/extract)
ydatatype_all = strsplit('alt lat lon', /extract)

;----- verbose -----;
if ~keyword_set(verbose) then verbose=0

;----- site -----;
if(not keyword_set(site)) then site='all'
site_code = thm_check_valid_name(site, site_code_all, /ignore_case, /include_all)
if site_code[0] eq '' then return
print, 'site_code = ',site_code

;----- ydatatype -----;
if(not keyword_set(ydatatype)) then ydatatype='alt'
ytype = thm_check_valid_name(ydatatype, ydatatype_all, /ignore_case)
if ytype eq '' then return
if ytype eq 'lon' then ytype='long'

;===== Download files, read data, and create tplot vars at each site =====
ack_flg=0

for i=0,n_elements(site_code)-1 do begin
    site1=site_code[i]
    site1=strsplit(site1, '_', /extract)
    stn=site1[0]
    ant=site1[1]
    
    ;----- Set parameters for file_retrieve and download data files -----;
    source = file_retrieve(/struct)
    source.verbose = verbose
    source.local_data_dir  = root_data_dir() + 'iugonet/nipr/eiscat/'
    source.remote_data_dir = 'http://polaris.nipr.ac.jp/~eiscat/eiscatdata/cdf/v2/'
;    source.remote_data_dir = 'http://polaris.nipr.ac.jp/~ytanaka/data/eiscat/'
    if keyword_set(no_download) then source.no_download = 1

    relpathnames1 = file_dailynames(file_format='YYYY', trange=trange)
    relpathnames2 = file_dailynames(file_format='YYYYMMDD', trange=trange)
    relpathnames  = stn+'/'+ant+'/'+$
        relpathnames1 + '/eiscat_kn_'+stn+ant+'_'+relpathnames2+'_v??.cdf'

    files = file_retrieve(relpathnames, _extra=source, /last_version)

    ;----- Print PI info and rules of the road -----;
    if(file_test(files[0]) and (ack_flg eq 0)) then begin
        ack_flg=1
        gatt = cdf_var_atts(files[0])
        print, '**************************************************************************************'
        print, 'Information about EISCAT radar data'
        print, 'PI: ', gatt.PI_name
        print, ''
        print, 'Rules of the Road for EISCAT Radar Data:'
        print, ''
	print_str_maxlet, gatt.Rules_of_use
        print, gatt.LINK_TEXT, ' ', gatt.HTTP_LINK
        print, '**************************************************************************************'
    endif

    ;----- Load data into tplot variables -----;
    if(not keyword_set(downloadonly)) then downloadonly=0
    if(downloadonly eq 0) then begin
        prefix='eiscat_'
        cdf2tplot, file=files, verbose=source.verbose, prefix=prefix, $
            get_support_data=get_support_data

        len=strlen(tnames(prefix+'ne_0'))
        if len eq 0 then begin
            ;--- Quit if no data have been loaded
           print, 'No tplot var loaded for '+site_code[i]+'.'
        endif else begin
            ;----- get y-axis data -----;
	    get_data, prefix+ytype+'_0', data=d
	    vdat=d.y
;	    vdat=average(d.y,1,/NAN)

            ;----- Loop for params -----;
            tplot_name_all=tnames(prefix+'*_0')
	    for itname=0, n_elements(tplot_name_all)-1 do begin
	        tplot_name_tmp=tplot_name_all(itname)

                ;----- Get_data of tplot_name_tmp -----;
                get_data, tplot_name_tmp, data=d, dl=dl, lim=lim 

                ;----- Find param -----;
                len=strlen(tplot_name_tmp)
                pos=strpos(tplot_name_tmp,'_')
                param=strmid(tplot_name_tmp,pos+1,len-pos-3)

                ;----- Replace data.v with y-axis data -----;
                if ((param eq 'lat') or (param eq 'long') or $
                    (param eq 'alt') or (param eq 'ne') or $
                    (param eq 'ne_err') or (param eq 'te') or $
                    (param eq 'te_err') or (param eq 'ti') or $
                    (param eq 'ti_err') or (param eq 'vi') or $
                    (param eq 'vi_err') or (param eq 'composition') or $
                    (param eq 'quality') or (param eq 'quality_flag')) then begin
                    store_data, tplot_name_tmp, data={x:d.x, y:d.y, v:vdat}, $
			dl=dl, lim=lim
                endif

                ;----- Rename tplot variables -----;
                case param of
                    'pulse_code_id' : paramstr='pulse'
                    'int_time_nominal' : paramstr='inttim'
                    'ne_err'       : paramstr='neerr'
                    'te_err'       : paramstr='teerr'
                    'ti_err'       : paramstr='tierr'
                    'vi_err'       : paramstr='vierr'
                    'composition'  : paramstr='comp'
                    'quality'      : paramstr='q'
                    'quality_flag' : paramstr='qflag'
                    'collision_freq'  : paramstr='colf'
                    'int_time_real'  : paramstr='inttimr'
                    'elev_angle'   : paramstr='elev'
                    'number_gate'  : paramstr='ngate'
                    'txpower'     : paramstr='txpow'
                    'mconst'       : paramstr='mcnst'
                    'heating'      : paramstr='heat'
                    else           : paramstr=param
                endcase

                tplot_name_new='eiscat_'+stn+ant+'_'+paramstr
                copy_data, tplot_name_tmp, tplot_name_new
                store_data, tplot_name_tmp, /delete

                ;----- Set options -----;
                titlehead=stn+'_'+ant+'!C'
                case param of
	            'pulse_code_id' : begin
		        options, tplot_name_new, labels='pulse code', $
		          ytitle=titlehead+'pulse code'
		      end
	            'int_time_nominal' : begin
		        options, tplot_name_new, labels='int.time', $
		          ytitle=titlehead+'Integration time', ysubtitle = '[s]'
		        tclip, tplot_name_new, 0, 1e+4, /overwrite
                        ylim, tplot_name_new, -50, 350
		      end
	            'lat' : begin
		        options, tplot_name_new, labels='Lat', $
		          ytitle=titlehead+'Latitude', ysubtitle = '[deg]', spec=1
                      end
	            'long' : begin
		        options, tplot_name_new, labels='Lon', $
		          ytitle=titlehead+'Longitude', ysubtitle = '[deg]', spec=1
                      end
	            'alt' : begin
		        options, tplot_name_new, labels='Alt', $
		          ytitle=titlehead+'Altitude', ysubtitle = '[km]', spec=1
                      end
	            'ne' : begin
		        options, tplot_name_new, labels='Ne', $
		          ytitle=titlehead+'Ne', ysubtitle = '[m-3]', spec=1
                        zlim, tplot_name_new, 1e10, 1e12, 1
		      end
	            'ne_err' : begin
		        options, tplot_name_new, labels='Ne err.', $
		          ytitle=titlehead+'Ne err.', ysubtitle = '[m-3]', spec=1
                        zlim, tplot_name_new, 1e10, 1e12, 1
		      end
	            'te' : begin
		        options, tplot_name_new, labels='Te', $
		          ytitle=titlehead+'Te', ysubtitle = '[K]', spec=1
		        zlim, tplot_name_new, 0, 4000, 0
		      end
	            'te_err' : begin
		        options, tplot_name_new, labels='Te err.', $
		          ytitle=titlehead+'Te err.', ysubtitle = '[K]', spec=1
		        zlim, tplot_name_new, 0, 4000, 0
		      end
	            'ti' : begin
		        options, tplot_name_new, labels='Ti', $
		          ytitle=titlehead+'Ti', ysubtitle = '[K]', spec=1
		        zlim, tplot_name_new, 0, 3000, 0
		      end
	            'ti_err' : begin
		        options, tplot_name_new, labels='Ti err.', $
		          ytitle=titlehead+'Ti err.', ysubtitle = '[K]', spec=1
		        zlim, tplot_name_new, 0, 3000, 0
		      end
	            'vi' : begin
		        options, tplot_name_new, labels='Vi', $
		          ytitle=titlehead+'Vi', ysubtitle = '[m/s]', spec=1
		        zlim, tplot_name_new, -200, 200, 0
		      end
	            'vi_err' : begin
		        options, tplot_name_new, labels='Vi err.', $
		          ytitle=titlehead+'Vi err.', ysubtitle = '[m/s]', spec=1
		        zlim, tplot_name_new, -200, 200, 0
		      end
	            'composition' : begin
		        options, tplot_name_new, labels='comp', $
		          ytitle=titlehead+'Composition', ysubtitle = '[%]', spec=1
		      end
	            'quality' : begin
		        options, tplot_name_new, labels='quality', $
		          ytitle=titlehead+'Quality', spec=1
		      end
	            'quality_flag' : begin
		        options, tplot_name_new, labels='qual.flag', $
		          ytitle=titlehead+'Quality flag', spec=1
		      end
	            'collision_freq' : begin
		        options, tplot_name_new, labels='col.freq.', $
		          ytitle=titlehead+'Col.freq.', ysubtitle = '[s-1]', spec=1
		      end
	            'int_time_real' : begin
		        options, tplot_name_new, labels='int.time!C   (real)', $
		          ytitle=titlehead+'Integration time (real)', ysubtitle = '[s]'
		        tclip, tplot_name_new, 0, 1e+4, /overwrite
                        ylim, tplot_name_new, -50, 350
		      end
	            'azim' : begin
		        options, tplot_name_new, labels='azim', $
		          ytitle=titlehead+'Azimuth angle', ysubtitle = '[deg]'
		      end
	            'elev_angle' : begin
		        options, tplot_name_new, labels='elev.angle', $
		          ytitle=titlehead+'Elevation angle', ysubtitle = '[deg]'
		      end
	            'number_gate' : begin
		        options, tplot_name_new, labels='nrange max', $
		          ytitle=titlehead+'Nrange max'
		      end
	            'txpower' : begin
		        options, tplot_name_new, labels='txpower', $
		          ytitle=titlehead+'Tx power'
		      end
	            'mconst' : begin
		        options, tplot_name_new, labels='magic const.', $
		          ytitle=titlehead+'Magic const.'
		      end
	            'heating' : begin
		        options, tplot_name_new, labels='heating', $
		          ytitle=titlehead+'Heating on/off'
		      end
                    else : dumm=0
                endcase
            endfor
        endelse
    endif
endfor

return
end


