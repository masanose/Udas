pro iug_load_gmag_wdc_create_tplot_vars, $
   sname = sname, $
   element = element, $
   resolution = res, $
   level = level, $
   tplot_name, $
   tplot_ytitle, tplot_ysubtitle, $
   tplot_dlimit

  ; for acknowledgment
  acknowledg_str_dst = $
    'The DST data are provided by the World Data Center for Geomagnetism, Kyoto, and'+ $
    ' are not for redistribution (http://wdc.kugi.kyoto-u.ac.jp/). Furthermore, we thank'+ $
    ' the geomagnetic observatories (Kakioka [JMA], Honolulu and San Juan [USGS], Hermanus'+ $
    ' [RSA], Alibag [IIG]), NiCT, INTERMAGNET, and many others for their cooperation to'+ $
    ' make the Dst index available.'
  acknowledg_str = $
    'The rules for the data use and exchange are defined'+ $
    ' by the Guide on the World Data Center System '+ $
    ' (ICSU Panel on World Data Centers, 1996).'+$
    ' Note that information on the appropriate institution(s)'+$
    ' is also supplied with the WDC data sets.'+$
    ' If the data are used in publications and presentations,'+$
    ' the data suppliers and the WDC for Geomagnetism, Kyoto'+$
    ' must properly be acknowledged.'+$
    ' Commercial use and re-distribution of WDC data are, in general, not allowed.'+$
    ' Please ask for the information of each observatory to the WDC.'

  if strlowcase(sname) eq 'dst' then begin
     tplot_dlimit = create_struct('data_att', $
                                  create_struct('acknowledgment', acknowledg_str_dst))
  endif else begin
     tplot_dlimit = create_struct('data_att', $
                                  create_struct('acknowledgment', acknowledg_str))
  endelse


  ; create tplot variable options
  if strlowcase(sname) eq 'dst' then begin

     if strlowcase(strmid(level,0,4)) eq 'prov' then begin
        tplot_name = 'iug_gmag_wdc_hour_index_dst_prov'
        tplot_ytitle = 'Prov. Dst'
     endif else begin
        tplot_name = 'iug_gmag_wdc_hour_index_dst'
        tplot_ytitle = 'Dst'
     endelse
     tplot_ysubtitle = '[nT]'

  endif $
  else if strlowcase(sname) eq 'sym' or strlowcase(sname) eq 'asy' then begin

     tplot_name = 'iug_gmag_wdc_min_index_' + $
                  strlowcase(sname+'-'+element)
     tplot_ytitle = strupcase(sname+'-'+element)
     tplot_ysubtitle = '[nT]'

  endif $
  else if strlowcase(sname) eq 'ae' then begin

     if (~keyword_set(res)) then res = 'min'
     if (~keyword_set(element)) then element = 'E'

     if strlowcase(strmid(level,0,4)) eq 'prov' then begin
        tplot_name = 'iug_gmag_wdc_'+res+'_index_a'+element+'_prov'
        tplot_name = strlowcase(tplot_name)
        tplot_ytitle = 'Prov. A' + strupcase(element)
     endif else begin
        tplot_name = 'iug_gmag_wdc_'+res+'_index_a'+element
        tplot_name = strlowcase(tplot_name)
        tplot_ytitle = 'A' + strupcase(element)
     endelse

     if strupcase(element) eq 'X' then begin
        tplot_ysubtitle = '[#]'
     endif else begin
        tplot_ysubtitle = '[nT]'
     endelse

  endif $
  else begin

     tplot_name = 'iug_gmag_wdc_'+res+'_'+sname+'_'+element
     tplot_name = strlowcase(tplot_name)
     tplot_ytitle = strupcase(sname+' '+element)

     if strlowcase(strmid(level,0,4)) eq 'prov' then begin
        tplot_name = tplot_name + '_prov'
        tplot_ytitle = tplot_ytitle + ' (Prov.)'
     endif else if strlowcase(strmid(level,0,2)) eq 'ql' or $
        strlowcase(level) eq 'quicklook' then begin
        tplot_name = tplot_name + '_ql'
        tplot_ytitle = tplot_ytitle + ' (QL)'
     endif

     if strupcase(element) eq 'D' or $
        strupcase(element) eq 'I' then begin
        tplot_ysubtitle = '[deg]'
     endif else begin
        tplot_ysubtitle = '[nT]'
     endelse

  endelse



end





