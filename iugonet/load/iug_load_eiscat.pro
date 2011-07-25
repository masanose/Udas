;+
; PROCEDURE: IUG_LOAD_EISCAT
;   iug_load_eiscat, site=site, $
;                    pulse_code=pulse_code, $
;                    int_time=int_time, $
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
;   pulse_code = Pulse code
;   int_time = Integration time
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
; Written by Y.-M. Tanaka, July 25, 2011 (ytanaka at nipr.ac.jp)
;-

;*****************************************************************
;*** show_text_fixlet is procedure for showing acknowledgement ***
;*****************************************************************
pro show_text_fixlet, txt, maxlet

if ~keyword_set(maxlet) then maxlet=100
textlen=strlen(txt)
remtext=txt
for iline=0, textlen/maxlet+100 do begin
  remtextlen=strlen(remtext)
  if remtextlen gt maxlet then begin
    line1=strmid(remtext, 0, maxlet)

    ispace=strpos(line1, ' ')	; Check if space exists.
    if ispace lt 0 then begin
      line1=strmid(line1, 0, maxlet)
      remtext=strmid(remtext, maxlet, remtextlen-maxlet+1)
    endif else begin
      ;--- Find space ---;
      for istr=0, maxlet-1 do begin
        str1=strmid(line1,maxlet-istr-1,1)
        if str1 eq ' ' then begin
          line1=strmid(line1, 0, maxlet-istr-1)
          remtext=strmid(remtext, maxlet-istr, remtextlen-maxlet+istr+1)
          break
        endif
      endfor
    endelse
    print, line1
  endif else begin
    line1=strmid(remtext, 0, remtextlen)
    print, line1
    break
  endelse
endfor

end


;********************************************
;*** Load procedure for EISCAT radar data ***
;********************************************
pro iug_load_eiscat, site=site, pulse_code=pulse_code, int_time=int_time, $
        ydatatype=ydatatype, trange=trange, get_support_data=get_support_data, $
	verbose=verbose, downloadonly=downloadonly, no_download=no_download

;===== Keyword check =====
;----- all codes -----;
site_code_all = strsplit('esr_32m esr_42m tro_vhf tro_uhf kir_uhf sod_uhf', $
			/extract)
pulse_code_all = strsplit('cp0 cp1 cp2 cp3 cp4 cp5 cp6 cp7 cp8 cp9 '$
                 +'tau0 tau1 tau2 tau3 tau4 tau5 tau6 tau7 tau8 tau9 '$
                 +'taro ipy0 beat folk arc1 mand stef hild pia0 t2pl '$
                 +'gup0 gup1 gup2 gup3 '$
                 +'cp0e cp0f cp0g cp0h '$
                 +'cp1c cp1d cp1e cp1f cp1h cp1i cp1j cp1k cp1l '$
                 +'cp3f cp4a cp4b', /extract)
int_time_all = strsplit('0000 0060 0120 0300', /extract)
ydatatype_all = strsplit('alt lat lon', /extract)

;----- verbose -----;
if ~keyword_set(verbose) then verbose=0

;----- site -----;
if(not keyword_set(site)) then site='all'
site_code = thm_check_valid_name(site, site_code_all, /ignore_case, /include_all)
if site_code[0] eq '' then return
print, 'site_code = ',site_code

;----- pulse code -----;
if(not keyword_set(pulse_code)) then pulse_code='all'
pc = thm_check_valid_name(pulse_code, pulse_code_all, /ignore_case, /include_all)
if pc[0] eq '' then return
print, 'pulse_code = ', pc

;----- int_time -----;
if(not keyword_set(int_time)) then int_time='all'
intt = thm_check_valid_name(int_time, int_time_all, /ignore_case, /include_all)
if intt[0] eq '' then return
print, 'integration time = ', intt

;----- ydatatype -----;
if(not keyword_set(ydatatype)) then ydatatype='alt'
ytype = thm_check_valid_name(ydatatype, ydatatype_all, /ignore_case)
if ytype eq '' then return
if ytype eq 'lon' then ytype='long'

;===== Download files, read data, and create tplot vars at each site =====
ack_flg=0

;----- Loop -----
for i=0,n_elements(site_code)-1 do begin
  site1=site_code(i)
  site1=strsplit(site1, '_', /extract)
  stn=site1(0)
  ant=site1(1)
  for j=0,n_elements(pc)-1 do begin
    pc1=pc(j)
    for k=0,n_elements(intt)-1 do begin
      intt1=intt(k)

      ;----- Set parameters for file_retrieve and download data files -----;
      source = file_retrieve(/struct)
      source.verbose = verbose
      source.local_data_dir  = root_data_dir() + 'iugonet/nipr/eiscat/'
      source.remote_data_dir = 'http://polaris.nipr.ac.jp/~eiscat/eiscatdata/cdf/'
;      source.remote_data_dir = 'http://iugonet0.nipr.ac.jp/data/'
      if keyword_set(no_download) then source.no_download = 1

      relpathnames1 = file_dailynames(file_format='YYYY', trange=trange)
      relpathnames2 = file_dailynames(file_format='YYYYMMDD', trange=trange)
      relpathnames  = stn+'/'+ant+'/'+$
        relpathnames1 + '/eiscat_kn_'+stn+ant+'_'+pc1+'_'+intt1+'_'+$
        relpathnames2 + '_v??.cdf'

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
	show_text_fixlet, gatt.Rules_of_use
        print, gatt.LINK_TEXT, ' ', gatt.HTTP_LINK
        print, '**************************************************************************************'
      endif

      ;----- Load data into tplot variables -----;
      if(not keyword_set(downloadonly)) then downloadonly=0

      if(downloadonly eq 0) then begin
        prefix='eiscat_'+stn+ant+'_'+pc1+'_'
        cdf2tplot, file=files, verbose=source.verbose, prefix=prefix, $
          get_support_data=get_support_data

        len=strlen(tnames(prefix+'ne_0'))
        if len eq 0 then begin
          ;--- Quit if no data have been loaded
;          print, 'No tplot var loaded for '+site_code[i]+'.'
        endif else begin
          ;----- get y-axis data -----;
	  get_data, prefix+ytype+'_0', data=d
	  vdat=d.y
;	   vdat=average(d.y,1,/NAN)

          ;----- Rename tplot variables -----;
          tplot_name_all=tnames(prefix+'*_0')
	  for itname=0, n_elements(tplot_name_all)-1 do begin
	    tplot_name_tmp=tplot_name_all(itname)
            len=strlen(tplot_name_tmp)
            tplot_name_new=strmid(tplot_name_tmp,0,len-2)
            copy_data, tplot_name_tmp, tplot_name_new
            store_data, tplot_name_tmp, /delete

            ;----- Set options -----;
            case tplot_name_new of
	      prefix+'lat' : begin
		  options, tplot_name_new, labels='Lat', $
		    ytitle='Latitude', ysubtitle = '[deg]', spec=1
                end
	      prefix+'long' : begin
		  options, tplot_name_new, labels='Lon', $
		    ytitle='Longitude', ysubtitle = '[deg]', spec=1
                end
	      prefix+'alt' : begin
		  options, tplot_name_new, labels='Alt', $
		    ytitle='Altitude', ysubtitle = '[km]', spec=1
                end
	      prefix+'ne' : begin
		  options, tplot_name_new, labels='Ne', $
		    ytitle='Ne', ysubtitle = '[m-3]', spec=1
                  zlim, tplot_name_new, 1e10, 1e12, 1
		end
	      prefix+'ne_err' : begin
		  options, tplot_name_new, labels='Ne err.', $
		    ytitle='Ne err.', ysubtitle = '[m-3]', spec=1
                  zlim, tplot_name_new, 1e10, 1e12, 1
		end
	      prefix+'te' : begin
		  options, tplot_name_new, labels='Te', $
		    ytitle='Te', ysubtitle = '[K]', spec=1
		  zlim, tplot_name_new, 0, 4000, 0
		end
	      prefix+'te_err' : begin
		  options, tplot_name_new, labels='Te err.', $
		    ytitle='Te err.', ysubtitle = '[K]', spec=1
		  zlim, tplot_name_new, 0, 4000, 0
		end
	      prefix+'ti' : begin
		  options, tplot_name_new, labels='Ti', $
		    ytitle='Ti', ysubtitle = '[K]', spec=1
		  zlim, tplot_name_new, 0, 3000, 0
		end
	      prefix+'ti_err' : begin
		  options, tplot_name_new, labels='Ti err.', $
		    ytitle='Ti err.', ysubtitle = '[K]', spec=1
		  zlim, tplot_name_new, 0, 3000, 0
		end
	      prefix+'vi' : begin
		  options, tplot_name_new, labels='Vi', $
		    ytitle='Vi', ysubtitle = '[m/s]', spec=1
		  zlim, tplot_name_new, -200, 200, 0
		end
	      prefix+'vi_err' : begin
		  options, tplot_name_new, labels='Vi err.', $
		    ytitle='Vi err.', ysubtitle = '[m/s]', spec=1
		  zlim, tplot_name_new, -200, 200, 0
		end
	      prefix+'collision_freq' : begin
		  options, tplot_name_new, labels='col.freq.', $
		    ytitle='Col.freq.', ysubtitle = '[s-1]', spec=1
		end
	      prefix+'composition' : begin
		  options, tplot_name_new, labels='comp', $
		    ytitle='Composition', ysubtitle = '[%]', spec=1
		end
	      prefix+'quality' : begin
		  options, tplot_name_new, labels='quality', $
		    ytitle='Quality', spec=1
		end
	      prefix+'quality_flag' : begin
		  options, tplot_name_new, labels='qual.flag', $
		    ytitle='Quality flag', spec=1
		end
	      prefix+'int_time' : begin
		  options, tplot_name_new, labels='int.time', $
		    ytitle='Integration time', ysubtitle = '[s]'
		  tclip, tplot_name_new, 0, 1e+4, /overwrite
		end
	      prefix+'azim' : begin
		  options, tplot_name_new, labels='azim', $
		    ytitle='Azimuth angle', ysubtitle = '[deg]'
		end
	      prefix+'elev_angle' : begin
		  options, tplot_name_new, labels='elev', $
		    ytitle='Elevation angle', ysubtitle = '[deg]'
		end
	      prefix+'number_gate' : begin
		  options, tplot_name_new, labels='nrange max', $
		    ytitle='Nrange max'
		end
	      prefix+'txpower' : begin
		  options, tplot_name_new, labels='txpower', $
		    ytitle='Tx power'
		end
	      prefix+'mconst' : begin
		  options, tplot_name_new, labels='magic const.', $
		    ytitle='Magic const.'
		end
	      prefix+'heating' : begin
		  options, tplot_name_new, labels='heating', $
		    ytitle='Heating on/off'
		end
              else : dumm=0
            endcase

            ;----- Replace data.v with y-axis data -----;
            if ((tplot_name_new eq prefix+'lat') or (tplot_name_new eq prefix+'long') or $
              (tplot_name_new eq prefix+'alt') or (tplot_name_new eq prefix+'ne') or $
              (tplot_name_new eq prefix+'ne_err') or (tplot_name_new eq prefix+'te') or $
              (tplot_name_new eq prefix+'te_err') or (tplot_name_new eq prefix+'ti') or $
              (tplot_name_new eq prefix+'ti_err') or (tplot_name_new eq prefix+'vi') or $
              (tplot_name_new eq prefix+'vi_err') or (tplot_name_new eq prefix+'composition') or $
              (tplot_name_new eq prefix+'quality') or (tplot_name_new eq prefix+'quality_flag')) then begin
              get_data, tplot_name_new, data=d, dl=dl, lim=lim
              store_data, tplot_name_new, data={x:d.x, y:d.y, v:vdat}, dl=dl, lim=lim
            endif

          endfor
        endelse
      endif

    endfor
  endfor
endfor

return
end


