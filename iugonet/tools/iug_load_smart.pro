;+
; PROCEDURE iug_load_smart
;
; :Description:
;		Load the solar image data in the FITS format obtained by
;		the SMART telescope at the Hida Observatory, Kyoto Univ.
;               Under some environment, you should set the following environment:
;               > setenv ROOT_DATA_DIR ~/data/
;               before you start the IDL.
;
; :Keywords:
;    datatype:  basically set the telescope (e.g., halpha)
;    filter:    filter name(s) (e.g., m05 )
;    lst:       set a named variable to return the URL list of data files
;
; :EXAMPLES:
;   timespan, '2005-08-03/03:00', 2, /hour
;   iug_load_smart,filter='p00'
;   tplot_names
;   get_data,'smart_t1_p00',data=d
;   window,0,xs=512,ys=512
;   tvscl,d.y[*,*,0]
;   movie,bytscl(d.y),order=0
;
; :Author:
; 	Tomo Hori (E-mail: horit@stelab.nagoya-u.ac.jp)
;       Satoru UeNo (E-mail: ueno@kwasan.kyoto-u.ac.jp)
;
; :HISTORY:
; 	2012/05/08: Created by TH
;       2012/05/17: Addition of FITS_READ and STORE_DATA by SU
;
;-
PRO iug_load_smart, datatype=datatype, filter=filter, $
    lst=lst
    
  ;;;;;;;;;;;;;; Datatype and Filter ;;;;;;;;;;;;;;
    
  datatype_all = strsplit('halpha', /extract)
  filter_all   = strsplit('m12 m08 m05 p00 p05 p08 p12', /extract)
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;;;;;;;;;;;;;; acknowledgement ;;;;;;;;;;;;;
  acknowledgstring = 'If you have any questions or requests on the SMART-T1 data, please contact to smart@kwasan.kyoto-u.ac.jp .'
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  ;;;;;;;;;;;;;; Data directory ;;;;;;;;;;;;;;
  
  local_data_dir_tmpl = $
    root_data_dir() + 'iugonet/KwasanHidaObs/smart_t1/YYYY/MM/DD/fits/'
    
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
    
    
  ;Initialize the TDAS environment
  thm_init
  
  ;Initialize the data load structure
  source = file_retrieve( /struct )
  
  
  ;Check the arguments
  IF ~KEYWORD_SET(datatype) THEN datatype='halpha'
  IF ~KEYWORD_SET(filter) THEN filter='p00'
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
      query = 'request?query='+keyword+'&ts='+startdate+'&te='+enddate+'&Granule=granule&'
      url_in = mddb_base_url+query
      
      ;Obtain the list of URLs of data files by throwing a query to MDDB
      lst = get_source_url_list( url_in )
      IF N_ELEMENTS(lst) EQ 1 AND STRLEN(lst[0]) EQ 0 THEN BEGIN
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
        ;;append_array, fname_arr, fname
        syyyy = url_str[4] & smm = url_str[5] & sdd = url_str[6]
        local_data_dir = local_data_dir_tmpl
        pos = STRPOS( local_data_dir, 'YYYY' ) & STRPUT, local_data_dir, syyyy, pos
        pos = STRPOS( local_data_dir, 'MM' ) & STRPUT, local_data_dir, smm, pos
        pos = STRPOS( local_data_dir, 'DD' ) & STRPUT, local_data_dir, sdd, pos
        remote_data_dir = 'http://' + STRJOIN( url_str[1:(nstr-2)], '/' ) + '/'
        
        ;Generate the data dir arrays
        ;;append_array, local_data_dir_arr, local_data_dir
        ;;append_array, remote_data_dir_arr, remote_data_dir
        
        ;Download data files
        source.local_data_dir = local_data_dir
        source.remote_data_dir = remote_data_dir
        fpath = ''
        fpath = file_retrieve( fname, _extra=source, /last_version )
        
        IF fpath NE '' THEN append_array, loaded_flist, fpath
        
      ENDFOR
      
      ;;;;;;;;;;;;;;;; Read data files to generate tplot variables ;;;;;;;;;;;;;
      
      ;PRINT, 'The following FITS files will be read: '
      ;PRINT, loaded_flist ; for debugging
      
    databuf=intarr(n_elements(loaded_flist),512,512)

    for j=0,n_elements(loaded_flist)-1 do begin
     file=loaded_flist[j]

      if file_test(/regular,file) then begin
        dprint,'Loading SMART_T1 H-alpha Full-disk Sun Image: ', file
        fexist = 1
      endif else begin
        dprint,'Loading SMART_T1 H-alpha Full-disk Sun Image ',file,' not found. Skipping'
        continue
      endelse
     
     fits_read,file,img,hd
     
     timearr = time_double(sxpar(hd,'DATE-OBS'))      
     redimg = congrid(img,512,512)

;     append_array,databuf,redimg
     databuf[j,*,*] = redimg
     append_array,timebuf,timearr
      
    endfor ; (j)



     tplot_name = 'smart_t1_'+filter

     dlimit=create_struct('data_att',create_struct('acknowledgment', acknowledgstring, 'PI_NAME', 'SMART Developing Team'),'SPEC',1)

     store_data, tplot_name, data={x:timebuf, y:databuf}, dlimit=dlimit


   databuf = 0
   timebuf = 0

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      
    ENDFOR ;Loop for filter_arr
    
  ENDFOR ;Loop for datatype_arr
  
  print,'*********************************************************************'
  print,acknowledgstring  
  print,'*********************************************************************'
  
  RETURN
END

