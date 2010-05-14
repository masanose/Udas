;+
;NAME:
; thm_ui_marker_list__define
;
;PURPOSE:
; Container of markers, inheriting generalized read/write routines from 
; THM_UI_READWRITE
;
;CALLING SEQUENCE:
; marker_list = Obj_New("THM_UI_MARKER_LIST")
;
;INPUT:
; none
;
;KEYWORDS:
; none
;
;OUTPUT:
; none
;
;METHODS:
; Add
; GetAll
;
;HISTORY:
;
;$LastChangedBy: jwl $
;$LastChangedDate: 2008-12-11 11:34:06 -0800 (Thu, 11 Dec 2008) $
;$LastChangedRevision: 4149 $
;$URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/thmsoc/tags/tdas_5_21/idl/themis/thm_ui_new/objects/thm_ui_marker_list__define.pro $ ;

function thm_ui_marker_list::init
  self.markers = obj_new('IDL_CONTAINER')
  return, 1
end

pro thm_ui_marker_list::add,objs
   self.markers->add,objs
end

function thm_ui_marker_list::GetAll
   return,self.markers->Get(/all)
end

pro thm_ui_marker_list__define
  struct = { THM_UI_MARKER_LIST, $
             markers:obj_new('IDL_CONTAINER'), $ ; Container of markers
             INHERITS THM_UI_READWRITE $ ; Generalized read/write methods
           }
end
