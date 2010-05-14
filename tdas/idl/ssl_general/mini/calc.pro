;+
; Procedure: calc
;
; Purpose:  This routine takes a string as input and interprets the string as a mini-language.
;    This language can manipulate normal idl variables and tplot variables.  The idl variables
;    that can be modified are non-system variables in the scope in which the procedure is called.
;    
; Inputs: s : A string that will be interpreted as the mini language
;                    
; Keywords: error:
;              If an error occurs during processing this keyword will return a struct 
;              that contains information about the error.  If error is set at time of input
;              this routine will set it to undefined, before it interprets 's' to prevent internal errors.
;           
;           function_list: return an array of strings that lists of the names/syntax of
;                          available functions in the mini_language.  Return without processing 's'
;                          if an argument in the returned name is in brackets it is optional
;                          for example:  min(x[,dim]) 
;           operator_list:  return an array of strings that lists of the names of available
;                           operators in the mini_language. Return without processing 's'
;                           
;           verbose:  set this keyword if you want the routine to print errors to screen rather 
;                     than only return them via error
;                         
;           gui_data_obj: If 'calc' is being used inside the gui, then the loaded_data object will
;                         be passed in through this keyword.  NOTE: end users should not even set
;                         this argument. 
;              
; Outputs: none, but it will modify various variables in your environment
;              
; Examples:
;    calc,'a = 5'   
;    calc,'"pos_re" = "tha_state_pos"/6374'  
;    calc,'a += 7'  
;    calc,'"tvar" = "tvar" + var' 
;    calc,'"tvar" = ln("tvar")'
;    calc,'"tvar" = total("tvar"+3,2)'
;    calc,'"tvar" = -a + 5.43e-7 ^ ("thb_fgs_dsl_x" / total("thb_fgs_dsl_x"))
;    calc,operator_list=o,function_list=f
;
; Notes:
;    1. The language generally uses a fairly straightforward computational syntax.  The main
;       difference from idl is that quoted strings are treated as tplot variables in this language
;    2. A full specification of language syntax in backus-naur form can be found
;       in the file bnf_formal.txt, the programmatic specification of this syntax
;       can be found in productions.pro
;    3. The language is parsed using an slr parser.  The tables required to do this parsing
;       are generated and stored ahead of time in the file grammar.sav and parse_tables.sav
;    4. The routines that describe the evaluation rules for the language can be found in the file
;       mini_routines.pro
;    5. If you want to modify the way the language works you'll probably need to modify productions.pro,
;       regenerate the slr parse tables using save_calc_tables and modify/add routines to mini_routines.pro
;    6. Arrays must have the same dimensions to be combines, and tplot variables must also have the same times.
;    7. Procedures: min,max,mean,median,count,total  all take a second argument that allow you to select the
;       dimension over which the operation is performed
;       
; See Also:
;   All routines in the ssl_general/mini directory
;   The techniques used for this interpreter are based on two books:
;  
;   1. Compilers:Principles,Techniques,and Tools by Aho,Sethi,& Ullman 1988 (esp. Ch3)
;  
;   2. Structure & Interpretation of Computer Programs by Abelson & Sussman 1996 (esp. Ch4)
;   
;   If you want to understand/modify this program it may help to use these books as
;   a reference.
;   
;   Also see:  thm_crib_calc.pro for examples of usage
;      
; ToDo: 1. Implement Constants
;       2. Implement 0 argument functions
;       3. Implement keywords for functions
;       4. Implement procedures
;       5. Implement control statements
;       6. Optional auto-interpolation
;
; $LastChangedBy: pcruce $
; $LastChangedDate: 2010-03-23 12:05:32 -0700 (Tue, 23 Mar 2010) $
; $LastChangedRevision: 7429 $
; $URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/ssl_general/tags/tdas_5_21/mini/calc.pro $
;-

pro calc,s,error=error,function_list=function_list,operator_list=operator_list,verbose=verbose,gui_data_obj=gui_data_obj

  compile_opt idl2

  ;clear error if set at time of input
  if keyword_set(error) then begin
    t = temporary(error)
  endif
  
  ;if requested return list of functions
  if arg_present(function_list) then begin
    mini_routines
    list = (function_list()).name
    
    ;apply syntax description...this should probably be done in the function_list routine
    list[*] += '(x'
    list[[0,2]] += '[,base]'
    list[23:*] += '[,dim]'
    list[*] += ')'
    
    function_list = list
    
  endif  
  
  if arg_present(operator_list) then begin
    mini_routines

    list = (operator_list()).name
    
    ;replace unary/binary minus codes with normal minus
    ;...this should probably be done in the operator_list routine
    list = [list[0:2],'-',list[5:*]]
    operator_list = [list[0:3],'+',list[6:*]]
    
  endif
  
  if arg_present(operator_list) || arg_present(function_list) then begin
    return
  endif

  ;check that arg is set
  if ~keyword_set(s) then begin
    error = {type:'error',name:'Input string not set',value:'s'}
    return
  endif

  ;get path of routine
  rt_info = routine_info('calc',/source)
  path = file_dirname(rt_info.path) + '/'
 
 ;read grammar and parse table information from file
  restore,path+'grammar.sav'
  restore,path+'parse_tables.sav'
  
  if ~keyword_set(gui_data_obj) || ~obj_valid(gui_data_obj) then begin
    gui_data_obj = obj_new()
  endif
  
  ;set global variables
  mini_globals = {scope_level:scope_level()-1,gui_data_obj:gui_data_obj}
  
  defsysv,'!mini_globals',mini_globals

  ;split the input string into a list of tokens
  lex,s,token_list=token_list,error=error  
  
  if keyword_set(error) then begin
  
    if keyword_set(verbose) then begin
      if in_set('VALUE',tag_names(error)) then begin
        for i = 0,n_elements(error.value)-1 do begin
          print,error.value[i]
        endfor
      endif else begin
        dprint,error
      endelse
    
    endif
  
    return
  endif

  if is_endline_type(token_list[0]) then return

  ;evaluate the list of tokens using the parse table and grammar provided
  evaluate,token_list,grammar,parse_tables,error=error
  
  if keyword_set(error) then begin
  
    if keyword_set(verbose) then begin
    
      if in_set('VALUE',tag_names(error)) then begin
        for i = 0,n_elements(error.value)-1 do begin
          print,error.value[i]
        endfor
      endif else begin
        dprint,error
      endelse
     
    endif
  
    return
  endif
  
  return

end
