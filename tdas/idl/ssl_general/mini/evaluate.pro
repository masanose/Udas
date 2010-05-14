
;+
; Procedure: evaluate
;
; Purpose:  This routine performs the actual evaluation of an expression in the mini_language
;           It basically combines an slr shift/reduce parser with an evaluator
;           
; Inputs:
;    tk_list:  a list of token structures from the lex routine
;    grammar:  a grammar description structure
;    parse_tables: a parse table structure
;    
; Keywords: error: On error this routine returns a structure that describes the error that occurred
;
; $LastChangedBy: pcruce $
; $LastChangedDate: 2008-09-12 16:21:16 -0700 (Fri, 12 Sep 2008) $
; $LastChangedRevision: 3487 $
; $URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/ssl_general/tags/tdas_5_21/mini/evaluate.pro $
;-

pro evaluate,tk_list,grammar,parse_tables,error=error

  compile_opt idl2
  
  evaluator_routines
  
  stk = csstack(parse_tables.initial_state)
  
  i = 0
  
  while 1 do begin
  
    s = csstack(stk,/peek)
    
    if i ge n_elements(tk_list) then begin
      message,'parse error'
    endif
    
    if i eq 0 then begin
      previous = 0
    endif else begin
      previous = tk_list[i-1]
    endelse  
    
    tk = eval_token(tk_list[i],previous,grammar)
    
    if is_shift_code(parse_tables.action_table[s,tk.index]) then begin
    
      s = get_shift_num(parse_tables.action_table[s,tk.index])
    
      if s eq -1 then begin
        message,'shift code error'
      endif
    
      stk = csstack(tk,stk,/push)
      
      stk = csstack(s,stk,/push)
      
      i++
          
    endif else if is_reduce_code(parse_tables.action_table[s,tk.index]) then begin
    
      red = get_reduce_num(parse_tables.action_table[s,tk.index])
      
      if red[0] eq -1 then begin
        message,'reduce code error'
      endif
    
      reduction = grammar.production_list[red]
      
      ;do reduction here
      
      for j = reduction.length,1,-1 do begin
        stk = csstack(stk,/pop)
        (scope_varfetch('input'+strtrim(string(j),2),level=0)) = csstack(stk,/peek)
        stk = csstack(stk,/pop)
      endfor
        
      catch,err
        
      if err eq 0 then begin
        
        if reduction.length eq 1 then begin
          output = call_function(reduction.fun,input1)
        endif else if reduction.length eq 2 then begin
          output = call_function(reduction.fun,input1,input2)
        endif else if reduction.length eq 3 then begin
          output = call_function(reduction.fun,input1,input2,input3)
        endif else if reduction.length eq 4 then begin
          output = call_function(reduction.fun,input1,input2,input3,input4)
        endif else begin
          message,'unhandled reduction length'
        endelse
        
      endif else begin
        
        help, /Last_Message, Output=theErrorMessage
        catch,/cancel
        error = {type:'error',name:'reduction error',value:theErrorMessage,reduction:red}
        return
        
      endelse
     
      catch,/cancel
     
      s = csstack(stk,/peek)
      stk = csstack(output,stk,/push)
      idx = where(reduction.left eq grammar.nonterminals)
      s2 = long(parse_tables.goto_table[s,idx])
      stk = csstack(s2,stk,/push)
      
    endif else if parse_tables.action_table[s,tk.index] eq 'acc' then begin
    
      ;parsing completed! we're done
      return
      
    endif else if parse_tables.action_table[s,tk.index] eq 'err' then begin
    
      ;unspecified error
      error = {type:'error',name:'User statement syntax error',state:s,token:tk}
      return
      
    endif else begin
      message,'Unhandled parse table entry'
    endelse
  
  endwhile
  
end