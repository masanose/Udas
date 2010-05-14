;+
;Procedure: save_calc_tables
;
;Purpose:  This simple routine calls the proper procedures to generate the files needed to run the mini language
;          You should run this routine if you've made a change to the mini_language descriptions and you want that change to
;          be reflected in the runtime behavior of calc.pro  This routine generates two files: grammar.sav and parse_tables.sav
;
;
; $LastChangedBy: pcruce $
; $LastChangedDate: 2008-09-12 16:21:16 -0700 (Fri, 12 Sep 2008) $
; $LastChangedRevision: 3487 $
; $URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/ssl_general/tags/tdas_5_21/mini/save_calc_tables.pro $
;-

pro save_calc_tables

  rt_info = routine_info('calc',/source)
  path = file_dirname(rt_info.path) + '/'
  
  grammar = productions()
  slr,grammar,parse_tables=parse_tables
  
  save,grammar,filename=path+'grammar.sav'
  save,parse_tables,filename=path+'parse_tables.sav'

end