;+
; FUNCTION: GET_SOURCE_URL_LIST
;   url_out=get_source_url_list(url_in, xmldir, xmlfile)
;
; PURPOSE:
;   Gets URL for downloading data from the metadata database.
;
; KEYWORDS:
;   url_in = 
;
;   url_out = 
;
;   xmldir =  
;
; EXAMPLE:
;   url_in='http://search.iugonet.org/iugonet/open-search/request?'+$
;          'query=nipr_1sec_fmag_syo_20100101_v02.cdf'
;   url_out=get_source_url_list(url_in, './', 'tmp.xml')
;
; Written by Y.-M. Tanaka, Sep. 15, 2011 (ytanaka at nipr.ac.jp)
;-


function get_source_url_list, url_in, xmldir, xmlfile

;----- keyword check -----;
if ~keyword_set(xmldir) then xmldir=root_data_dir()
if ~keyword_set(xmlfile) then xmlfile='tmp.xml'

url_out=''

;----- download xmlfile -----; 
file_http_copy, xmlfile, serverdir=url_in, localdir=xmldir

;----- parse XML and get URL -----;
oDoc = OBJ_NEW('IDLffXMLDOMDocument')  ; Create IDLffXMLLOM objects
oDoc->Load, filename=xmldir+xmlfile   ; Load XML
; oPlugin = oDoc->GetFirstChild()
; oNodeList = oPlugin->GetElementsByTagname('dc:identifier')
oNodeList = oDoc->GetElementsByTagname('dc:identifier')
n = oNodeList->GetLength()

if n gt 0 then begin
  url_out=strarr(n)
  for i=0, n-1 do begin
    oName = oNodeList->Item(i)

    if OBJ_VALID(oName) then begin
      oNameText = oName->GetFirstChild()
      url_out(i)=oNameText->GetNodeValue()
    endif
  endfor
endif

OBJ_DESTROY, oDoc

return, url_out

end


