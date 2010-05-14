;+
;
;NAME: getctpath
;
;PURPOSE:
;  gets the path of the color table on the file system 
;
;CALLING SEQUENCE:
;   getctpath,color_table_path
;
;OUTPUT:
;   color_table_path:  the path to the color table
;
;
;$LastChangedBy: pcruce $
;$LastChangedDate: 2009-01-13 17:58:38 -0800 (Tue, 13 Jan 2009) $
;$LastChangedRevision: 4457 $
;$URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/thmsoc/tags/tdas_5_21/idl/themis/thm_ui_new/utilities/getctpath.pro $
;----------

pro getctpath,ctpathname

  ;get path of routine
 rt_info = routine_info('getctpath',/source)
 path = file_dirname(rt_info.path) + '/../Resources/'
 ctpathname = path + 'thm_gui_colors.tbl'
 
end