
;+
;PRO:
;  open_themis_template
;
;PURPOSE:
;
; opens a themis template
;
;Inputs:
;  filename:name of the file
;  
;Outputs:
;  statuscode: negative value indicates failure, 0 indicates success
;  statusmsg: a message to be returned in the event of an error
;  template: the template object
;
;
;$LastChangedBy: pcruce $
;$LastChangedDate: 2009-09-14 15:08:45 -0700 (Mon, 14 Sep 2009) $
;$LastChangedRevision: 6723 $
;$URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/thmsoc/tags/tdas_5_21/idl/themis/thm_ui_new/utilities/open_themis_template.pro $
;-

pro open_themis_template,filename=filename,template=template,statuscode=statuscode,statusmsg=statusmsg

catch,Error_status

if (Error_status NE 0) then begin
   statusmsg = !ERROR_STATE.MSG
   statuscode = -3
   catch,/cancel
   return
endif

file=filename

thm_ui_validate_file,file=file,statusmsg=statusmsg,statuscode=statuscode

if statuscode lt 0 then return

widget_control, /hourglass

; Load XML document from the filename
xmldoc=obj_new('IDLffXMLDOMDocument')
xmldoc->Load,file=file

; Create thm_ui_document object to receive XML data, using the
; no-arguments constructor

template=obj_new('thm_ui_template')

; Drill down into the DOM tree to get the first non-whitespace child
; of <body> element

body=xmldoc->GetFirstChild() ; should be 'body'
if (body->GetNodeName() NE 'body') then begin
  message,'Expected body node, got '+body->GetNodeName()
endif

sib=body->GetFirstChild()
; Skip any extraneous text elements (newlines, etc)
while (sib->GetNodeName() EQ '#text')  do begin
   sib=sib->GetNextSibling()
endwhile

; sib should be a THM_UI_TEMPLATE node
if (sib->GetNodeName() NE 'THM_UI_TEMPLATE') then begin
  message,'Expected THM_UI_DOCUMENT node, got '+sib->GetNodeName()
endif

template->BuildFromDOMElement,sib

; We're done with the XML DOM tree

obj_destroy,xmldoc

statuscode=0
statusmsg=STRING(file,format='("THEMIS template successfully read from ",A)')
return
end
