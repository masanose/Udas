;+
; PROCEDURE iug_load_smart
;
; :Description:
;		Load the solar image data in the FITS format obtained by
;		the SMART telescope at the Hida Observatory, Kyoto Univ.
;
;
;
;	:Keywords:
;    datatype:  basically set the telescope (e.g., halpha)
;    filter:    filter name(s) (e.g., m05 )
;    maxfile:   set the maximum number of files to be (down)loaded. (Default: 1000)
;    lst:       set a named variable to return the URL list of data files
;
; :EXAMPLES:
;   timespan, '2005-08-03/03:00', 1, /hour
;   iug_load_smart
;
; :Author:
; 	Tomo Hori (E-mail: horit@stelab.nagoya-u.ac.jp)
;
; :HISTORY:
; 	2012/05/08: Created
;
;-
PRO iug_load_smart, datatype=datatype, filter=filter, $
    maxfile=maxfile, $
    lst=lst
    
  ;;;;;;;;;;;;;; Datatype and Filter ;;;;;;;;;;;;;;
    
  datatype_all = strsplit('halpha', /extract)
  filter_all   = strsplit('m05 m08 p00 p05 p08', /extract)
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  ;;;;;;;;;;;;;; Data directory ;;;;;;;;;;;;;;
  
  local_data_dir_tmpl = $
    root_data_dir() + 'iugonet/smart/YYYY/MM/DD/T1/fits/'
    
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
    
    
  ;Initialize the TDAS environment
  thm_init
  
  ;Initialize the data load structure
  source = file_retrieve( /struct )
  
  
  ;Check the arguments
  IF ~KEYWORD_SET(maxfile) THEN maxfile = 1000L 
  maxfile = LONG( maxfile )
  IF ~KEYWORD_SET(datatype) THEN datatype='halpha'
  IF ~KEYWORD_SET(filter) THEN filter='m05'
  datatype_arr = STRLOWCASE( thm_check_valid_name( datatype, datatype_all, $
    /ignore_case, /include_all ) )
  filter_arr = STRLOWCASE( thm_check_valid_name( filter, filter_all, $
    /ignore_case, /include_all ) )
  IF datatype_arr[0] EQ '' OR filter_arr[0] EQ '' THEN RETURN ;No valid datatype/filter
  
  ;Get the time range
  get_timespan, tr
  tr_str = time_string(tr)
  STRPUT, tr_str, "T", 10 ;yyyy-mm-dd/hh:mm:ss --> yyyy-mm-ddThh:mm:ss
  
  ;;;; Loop for loading data for each datatype
  FOR i_dtype=0L, N_ELEMENTS(datatype_arr)-1 DO BEGIN
    datatype = datatype_arr[i_dtype]
    
    ;;; Loop for loading data for each filter
    FOR i_filter=0L, N_ELEMENTS(filter_arr)-1 DO BEGIN
      filter = filter_arr[i_filter]
      
      
      ;Set keywords for MDDB query
      mddb_base_url = 'http://search.iugonet.org/iugonet/open-search/'
      keyword='smart+AND+'+datatype+'+AND+'+filter+'+AND+fits'
      startdate = tr_str[0]
      enddate = tr_str[1]
      rpp_param = 'rpp=' + strtrim(string(maxfile),2) + '&'
      query = 'request?query='+keyword+'&ts='+startdate+'&te='+enddate+'&Granule=granule&'
      url_in = mddb_base_url+query+rpp_param
      
      ;Obtain the list of URLs of data files by throwing a query to MDDB
      lst = get_source_url_list( url_in )
      IF N_ELEMENTS(lst) EQ 0 AND STRLEN(lst[0]) EQ 0 THEN BEGIN
        PRINT, 'iug_load_smart: No data file was hit by MDDB query!'
        CONTINUE
      ENDIF
      lst = lst[ SORT(lst) ] ;Sort the URL list
      
      ;;;;;;Download  data files
      loaded_flist = ''
      FOR i_lst=0L, N_ELEMENTS(lst)-1 DO BEGIN
      
        url = lst[i_lst]
        url_str = strsplit( url, '/', /extract )
        nstr = N_ELEMENTS(url_str)
        fname = url_str[nstr-1]
        syyyy = url_str[4] & smm = url_str[5] & sdd = url_str[6]
        local_data_dir = local_data_dir_tmpl
        pos = STRPOS( local_data_dir, 'YYYY' ) & STRPUT, local_data_dir, syyyy, pos
        pos = STRPOS( local_data_dir, 'MM' ) & STRPUT, local_data_dir, smm, pos
        pos = STRPOS( local_data_dir, 'DD' ) & STRPUT, local_data_dir, sdd, pos
        remote_data_dir = 'http://' + STRJOIN( url_str[1:(nstr-2)], '/' ) + '/'
        
        ;Download if no data file in local dir or the remote data file is newer than the local one
        ;;;;print, local_data_dir  ;for debugging
        ;;;;print, remote_data_dir ;for debugging
        source.local_data_dir = local_data_dir
        source.remote_data_dir = remote_data_dir
        fpath = ''
        fpath = file_retrieve( fname, _extra=source, /last_version )
        
        IF fpath NE '' THEN append_array, loaded_flist, fpath
        
      ENDFOR
      
      ;;;;;;;;;;;;;;;; Read data files to generate tplot variables ;;;;;;;;;;;;;
      
      
      PRINT, loaded_flist ; for debugging
      
      
      
      
      
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      
    ENDFOR ;Loop for filter_arr
    
  ENDFOR ;Loop for datatype_arr
  
  
  
  RETURN
END

