;+  
;NAME:
;  thm_ui_newline
;
;PURPOSE:
;  Returns a cross-platform newline character.
;  Specifically, used in the dialog_message boxes, which tend to
;  print junk characters if character 13 is used on non-windows platforms
;
;CALLING SEQUENCE:
;  newline = thm_ui_newline()
;  string = line1 + thm_ui_newline() + line2
;
;INPUT:
; none
; 
;OUTPUT:
; Newline character
;
;HISTORY:
;
;$LastChangedBy: pcruce $
;$LastChangedDate: 2009-08-04 14:59:24 -0700 (Tue, 04 Aug 2009) $
;$LastChangedRevision: 6526 $
;$URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/thmsoc/tags/tdas_5_21/idl/themis/thm_ui_new/utilities/thm_ui_newline.pro $
;-----------------------------------------------------------------------------------

function thm_ui_newline

  if strlowcase(!version.os_family eq 'windows') then begin
    return,string(13B) + string(10B)
  endif else begin
    return,string(10B)
  endelse

end