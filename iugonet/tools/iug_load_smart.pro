pro iug_load_smart, datatype=datatype, lst=lst
  
  ;Initialize 
  thm_init
  
  ;Check the arguments
  if ~keyword_set(datatype) then datatype='halpha'
  
  ;Get the time range 
  get_timespan, tr 
  tr_str = time_string(tr)
  strput, tr_str, "T", 10 ;yyyy-mm-dd/hh:mm:ss --> yyyy-mm-ddThh:mm:ss
  
  ;Set keywords for MDDB query
  mddb_base_url = 'http://search.iugonet.org/iugonet/open-search/'
  keyword='smart+AND+'+datatype+'+AND+fits'
  startdate = tr_str[0]
  enddate = tr_str[1]
  query = 'request?query='+keyword+'&ts='+startdate+'&te='+enddate+'&Granule=granule&'
  url_in = mddb_base_url+query
  
  ;Obtain the list of URLs of data files by throwing a query to MDDB  
  lst = get_source_url_list( url_in )
  if n_elements(lst) eq 0 and strlen(lst[0]) eq 0 then begin
    print, 'iug_load_smart: No data file was hit by MDDB query!'
    return
  endif
  
  ;;;;;;Download then load each data files ...
  for i=0L, n_elements(lst)-1 do begin
    
    print, lst[i]
    
  endfor
  
  
  return
end

