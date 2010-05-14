
;+
;PRO:
;  open_themis_document
;
;PURPOSE:
;
; opens a themis document
;
;Inputs:
;  info: info struct from main gui event handler
;  filename:name of the file
;  nodelete:indicates that preexisting data should not be deleted during read
;  
;Outputs:
;  statuscode: negative value indicates failure, 0 indicates success
;  statusmsg: a message to be returned in the event of an error
;
;
;$LastChangedBy: pcruce $
;$LastChangedDate: 2010-02-16 13:33:25 -0800 (Tue, 16 Feb 2010) $
;$LastChangedRevision: 7282 $
;$URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/thmsoc/tags/tdas_5_21/idl/themis/thm_ui_new/utilities/open_themis_document.pro $
;-

pro open_themis_document,info=info,filename=filename,statuscode=statuscode,statusmsg=statusmsg,nodelete=nodelete

catch,Error_status

if (Error_status NE 0) then begin
   statusmsg = !ERROR_STATE.MSG
   statuscode = -3
   catch,/cancel
   return
endif

historywin=info.historywin
statustext=info.statusbar
guiId = info.master

thm_ui_validate_file,filename=filename,statusmsg=statusmsg,statuscode=statuscode

if statuscode lt 0 then return

statustext->Update,'Opening THEMIS document '+filename
widget_control, /hourglass

; Load XML document from the filename
xmldoc=obj_new('IDLffXMLDOMDocument')
xmldoc->Load,filename=filename

; Create thm_ui_document object to receive XML data, using the
; no-arguments constructor

doc_to_load=obj_new('thm_ui_document')

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

; sib should be a THM_UI_DOCUMENT node
if (sib->GetNodeName() NE 'THM_UI_DOCUMENT') then begin
  message,'Expected THM_UI_DOCUMENT node, got '+sib->GetNodeName()
endif

doc_to_load->BuildFromDOMElement,sib

; We're done with the XML DOM tree

obj_destroy,xmldoc

; Invoke the onLoad method, passing in the windowstorage, loadedData,
; and windowMenus objects.
; This sets the loadedData element of the newly created
; callSequence object, replays the calls, and adds all the window objects
; to windowStorage while keeping windowMenus updated.

; Reset loadedData object (there may be other objects that hold references
; to the loadedData object, so obj_destroy/obj_new is too drastic).

if ~keyword_set(nodelete) then begin
  info.loadedData->reset
endif

doc_to_load->onLoad,windowStorage=info.windowStorage,windowMenus=info.windowMenus,loadedData=info.loadedData,historywin=historywin,statustext=statustext,guiId=guiId

; Update the draw window
thm_ui_orientation_update,info.drawObject,info.windowStorage
info.drawObject->update,info.windowStorage, info.loadedData
info.drawObject->draw

statuscode=0
statusmsg=STRING(filename,format='("THEMIS document successfully read from ",A)')
return
end
