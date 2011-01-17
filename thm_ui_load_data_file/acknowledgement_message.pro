;+
; NAME:
;    acknowledgement_message
;
; PURPOSE:
;
;    The purpose of this function is to show the acknowledgement
;    message for each IUGONET data set when you have downloaded the observation data
;    using the graphical user interface (GUI). The acknowledgement message is reported
;    to the user by using dialog_message if widgets are supported and MESSAGE otherwise.
;    
; CALLING SEQUENCE:
;
;    ok = acknowledgement(the_acknowledgement_message)
;
; INPUTS:
;
;    the_acknowledgement_message: This is a string argument containing 
;                                 the acknowledgement message you want reported. 
;
; KEYWORDS:
;
;    acknowledgement: Set this keyword to cause dialog_message to use the acknowledgement
;                     reporting dialog.
;
;    informatinal: Set this keyword to cause dialog_message to use the
;                  information dialog instead of the warning dialog. 
;
;    title: Set this keyword to the title of the dialog_message window.
;
;     
;
; OUTPUTS:
;
;    Currently the only output from the function is the string "OK".
;    
;Code:
;  A. Shinbori, 16/01/2011.
;  
;Modifications:
;
;  
;Acknowledgment:
; $LastChangedBy:  $
; $LastChangedDate:  $
; $LastChangedRevision:  $
; $URL $
;-

function acknowledgement_message, theMessage, Informational=information, NoName=noname, Title=title, _Extra=extra

; Check for presence and type of message.
s = size(theMessage)
messagetype = s[s[0]+1]
if messagetype ne 7 then begin
   message, "The message parameter must be a string.", _Extra=extra
endif

if keyword_Set(noname) then begin
   answer = dialog_message(theMessage, _Extra=extra, Title=title, Information=information)
endif

print, answer
return, answer

end