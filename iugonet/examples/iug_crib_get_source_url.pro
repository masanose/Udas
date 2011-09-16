;+
;PROCEDURE: IUG_CRIB_GET_SOURCE_URL
;    A sample crib sheet that explains how to use the "get_source_url_list" 
;    procedure. You can run this crib sheet by copying & pasting each 
;    command below (except for stop and end) into the IDL command line. 
;    Or alternatively compile and run using the command:
;        .run iug_crib_get_source_url
;
;Written by: Y.-M. Tanaka,  Sep. 15, 2011
;Last Updated: Y.-M. Tanaka,  Sep. 16, 2011
;-

;----- keyword -----;
keywords='nipr_1sec_fmag_syo_20100101'

;----- Parameters for get_source_url_list -----;
xmlfile='tmp.xml'
xmldir='./'
; xmlfile=root_data_dir()+'/iugonet/metadata/sample.xml'

mddb_base_url = 'http://search.iugonet.org/iugonet/open-search/'
query = 'request?query='+keywords+'&Granule=granule'
;query = 'request?query='+keywords+'&ts=2010-01-01&te=2010-01-05&Granule=granule'
url_in = mddb_base_url+query

;----- Get URL list -----;
url_out=get_source_url_list(url_in, xmldir, xmlfile)
print, 'url_out = ', url_out

;----- Get trunk of URL -----; 
url1=url_out(0)
splitter='fmag'
divpos=strpos(url1, splitter)
remote_data_dir=strmid(url1, 0, divpos)
print, 'remote_data_dir = ', remote_data_dir


end

