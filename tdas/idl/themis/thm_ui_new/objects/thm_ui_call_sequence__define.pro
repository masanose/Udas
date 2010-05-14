;+
;NAME:
;  thm_ui_call_sequence__define
;
;PURPOSE:
;  Stores the sequence of procedure calls that was used to load data.
;
;
;CALLING SEQUENCE:
; obj = obj_new('thm_ui_call_sequence',loadeddata)
; 
;Methods:
;  addloadcall: This method should be called in load themis data, every time 
;               'thm_ui_new_load_data2obj' is called
;  getCalls:  This method returns the list of stored function calls as an
;             array of pointers to structs
;  setCalls: This method stores an array of pointers to structs that represent
;             function/procedure calls
;  reCall: This method will re-execute the sequence of stored function calls
;  
;  merge: This method will combine two call sequence objects.  It attempts to guarantee that
;          the correct data quantities are generated, while preventing duplication of effort. 
;
;  getDomElement: serializes this class into an XML Dom object
;  BuildFromDomElement:  deserializes this class from an XML Dom Object
;
;NOTE:  1.  This object is designed in such a way that it should be straightforward
;       to ingest calls from other types of routines. For example: calls to delete data
;       or calls to load data from non-themis sources.  The public interface
;       is set up in such a way that it should ease these types of extensions 
;       in the future.
;       
;       2.  Right now, it cannot guarantee that *all* the data that was saved will
;       be present when reCalled, because it does not keep track of custom tplot variables,
;       or data processing calls. 
; 
;HISTORY:
;$LastChangedBy: pcruce $
;$LastChangedDate: 2010-03-03 13:23:25 -0800 (Wed, 03 Mar 2010) $
;$LastChangedRevision: 7395 $
;$URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/thmsoc/tags/tdas_5_21/idl/themis/thm_ui_new/objects/thm_ui_call_sequence__define.pro $
;
;--------------------------------------------------------------------------------


;this routine adds load routine calls to the list
pro thm_ui_call_sequence::addloadcall,st_time0, $
                                      en_time0, $
                                      dtype, $
                                      observ, $
                                      outcoord
   
   compile_opt idl2
  
  in_st = {type:'loadthemisdata', $
           st_time:st_time0,$
           en_time:en_time0,$
           dtype:dtype,$
           observ:observ,$
           outcoord:outcoord}
           
  self->addSt,in_st 
                     
                     
end

;this routine adds goes load routine calls to the list 
pro thm_ui_call_sequence::addloadgoes,$
                            probes,$
                            datatype,$
                            timeRange
                            
  compile_opt idl2                       
                            
  in_st = {type:'loadgoesdata', $
           probes:probes,$
           datatype:datatype,$
           timeRange:timeRange}
          
           
  self->addSt,in_st       

end

;this routine adds ace load routine calls to the list 
pro thm_ui_call_sequence::addloadace,$
                            instrument,$
                            datatype,$
                            parameters,$
                            timeRange
                            
  compile_opt idl2                       
                            
  in_st = {type:'loadacedata', $
           instrument:instrument,$
           datatype:datatype,$
           parameters:parameters,$
           timeRange:timeRange}
          
           
  self->addSt,in_st       

end

;this routine adds ace load routine calls to the list 
pro thm_ui_call_sequence::addloadwind,$
                            instrument,$
                            datatype,$
                            parameters,$
                            timeRange
                            
  compile_opt idl2                       
                            
  in_st = {type:'loadwinddata', $
           instrument:instrument,$
           datatype:datatype,$
           parameters:parameters,$
           timeRange:timeRange}
          
           
  self->addSt,in_st       

end

pro thm_ui_call_sequence::addLoadOver,$
                           probes,$
                           date,$
                           duration,$
                           oplot_num
  in_st = {type:'loadoverdata',$
           probes:probes,$
           date:date,$
           duration:duration, $
           oplot_num:oplot_num $
           }
           
 self->addSt,in_st
           
end

pro thm_ui_call_sequence::addDprocOp,$
                          task,$
                          vars,$
                          params=params
                          
  if n_elements(params) eq 0 then begin
    params = 0
  endif
  
  in_st = {type:'dprocop',$
           task:task,$
           vars:vars,$
           params:params}
           
  self->addSt,in_st

end

pro thm_ui_call_sequence::addInterpOp,$
                          result,$
                          vars
  
  in_st = {type:'interpop',$
           result:result,$
           vars:vars}

  self->addSt,in_st
  
end       

pro thm_ui_call_sequence::addPwrSpecOp,$
                          popt,$
                          vars
                          
  in_st = {type:'pwrspecop',$
           popt:popt,$
           vars:vars}
           
  self->addSt,in_st
  
end         

pro thm_ui_call_sequence::addCalcOp,$
                           programText
                           
  in_st = {type:'calcop',$
           programText:programText}
           
  self->addSt,in_st       

end

pro thm_ui_call_sequence::addNudgeOp,$
                                   tn,$
                                   nshift,$
                                   shift_unit,$
                                   shift_scale,$
                                   use_records,$
                                   isspec
                                   
  in_st = {type:'nudgeop',$    
           tn:tn,$
           nshift:nshift,$
           shift_unit:shift_unit,$
           shift_scale:shift_scale,$
           use_records:use_records,$
           isspec:isspec}
           
  self->addSt,in_st
  
end

pro thm_ui_call_sequence::addDataInfoOp,$
                           name,$
                           mission,$
                           observatory,$
                           instrument,$
                           unit,$
                           coord,$
                           newname=newname
  
  if ~keyword_set(newname) then begin
    newname = ''
  endif
                        
  in_st = {type:'datainfoop',$
           name:name,$
           mission:mission,$
           observatory:observatory,$
           instrument:instrument,$
           unit:unit,$
           coord:coord,$
           newname:newname}
  
  self->addSt,in_st
  
end
  
pro thm_ui_call_sequence::addCotransOp,$
                          value,$
                          active
                          
  in_st = {type:'cotransop',$
           value:value,$
           active:active}
  
  self->addSt,in_st
                          
                          
end

;add thm_part_getspec replay
pro thm_ui_call_sequence::addGetSpecOp,$
                                 probe,$
                                 dtype,$
                                 trange,$
                                 start_angle,$
                                 suffix,$
                                 angle,$
                                 phi,$
                                 theta,$
                                 pitch,$
                                 gyro,$
                                 erange,$
                                 energy,$
                                 regrid,$
                                 other_dim,$
                                 normalize,$
                                 datagap,$
                                 mask_remove,$
                                 method_sunpulse_clean,$
                                 limit_sunpulse_clean,$
                                 fillin_method
                                 
                                 
                                 
                                 
 in_st = {type:'getspecop',$
           probe:probe,$
           dtype:dtype,$
           trange:trange,$
           start_angle:start_angle,$
           suffix:suffix,$
           angle:angle,$
           phi:phi,$
           theta:theta,$
           pitch:pitch,$
           gyro:gyro,$
           erange:erange,$
           energy:energy,$
           regrid:regrid,$
           other_dim:other_dim,$
           normalize:normalize,$
           datagap:datagap,$
           mask_remove:mask_remove,$
           method_sunpulse_clean:method_sunpulse_clean,$
           limit_sunpulse_clean:limit_sunpulse_clean,$
           fillin_method:fillin_method}
  
  self->addSt,in_st                             
                                 
                                 
                                 
end     
                                 
                                 
;returns the list of call structs
;This list will be an array of pointers to structs.
;The reason that it is not an array of structs, is
;because we will probably want to extend this to support
;heterogenous sequences of calls(multiple types of functions mixed together)
;returns 0 if no calls are stored
function thm_ui_call_sequence::getCalls

  compile_opt idl2

  if ~ptr_valid(self.call_list) then return,0
  
  return,*self.call_list

end

;sets the entire sequence of calls(for use with load)
pro thm_ui_call_sequence::setCalls,callarr

  compile_opt idl2

  if ptr_valid(self.call_list) then ptr_free,self.call_list
  
  self.call_list = ptr_new(callarr)

end

pro thm_ui_call_sequence::clearCalls

  if ptr_valid(self.call_list) then ptr_free,self.call_list
  
  self.call_list = ptr_new()

end

;this method will re-execute the sequence of stored 
;function calls
pro thm_ui_call_sequence::reCall,historywin=historywin,statustext=statustext,guiID=guiID

  compile_opt idl2
  
  calls = self->getCalls()

  if is_num(calls) then return
    
  for i=0,n_elements(calls)-1 do begin

    st = *calls[i]
    
    if st.type eq 'loadthemisdata' then begin
    
      thm_ui_new_load_data2obj,st.st_time,$
                               st.en_time,$
                               dtype=st.dtype,$
                               outcoord=st.outcoord[0],$
                               observ=st.observ,$
                               loadedData=self.loadedData,$
                               historywin=historywin,$
                               statustext=statustext,$
                               state_gui_id=guiID
                               
    
    endif else if st.type eq 'loadgoesdata' then begin
      thm_ui_load_goes_data_load_pro,$
                         st.probes,$
                         st.datatype,$
                         st.timeRange,$
                         self.loadedData,$
                         historyWin,$
                         statustext
   
   endif else if st.type eq 'loadwinddata' then begin
      thm_ui_load_wind_data_load_pro,$
                         st.instrument[0],$
                         st.datatype[0],$
                         st.parameters,$
                         st.timeRange,$
                         self.loadedData,$
                         historyWin,$
                         statustext
   endif else if st.type eq 'loadacedata' then begin
      thm_ui_load_ace_data_load_pro,$
                         st.instrument[0],$
                         st.datatype[0],$
                         st.parameters,$
                         st.timeRange,$
                         self.loadedData,$
                         historyWin,$
                         statustext
   endif else if st.type eq 'loadoverdata' then begin
      thm_ui_overplot, obj_new(),self.loadedData,obj_new(),$
                       probes=st.probes, date=st.date, dur=st.duration,  $
                       oplot_calls=ptr_new(st.oplot_num),/no_draw
   
   endif else if st.type eq 'dprocop' then begin
     
    if is_struct(st.params) then begin
      otp = self.loadedData->dproc(st.task, st.params, in_vars=st.vars, $
                      hwin = historywin, sbar = statustext)
    endif else begin
      otp = self.loadedData->dproc(st.task, in_vars=st.vars, $
                      hwin = historywin, sbar = statustext)
    endelse
   
   endif else if st.type eq 'interpop' then begin
     thm_ui_interpolate,st.result,st.vars,self.loadedData,historyWin,statustext,guiid=guiid
   endif else if st.type eq 'pwrspecop' then begin
     thm_ui_new_pwrspc, st.popt,st.vars, self.loadedData, historywin, statustext
   endif else if st.type eq 'calcop' then begin
     thm_ui_run_calc,st.programText,self.loadedData,historywin,statustext
   endif else if st.type eq 'nudgeop' then begin
     thm_ui_do_nudge,st.tn[0],st.nshift[0],st.shift_unit[0],st.shift_scale[0],st.use_records[0],st.isspec[0],self.loadedData,historyWin,statustext            
   endif else if st.type eq 'datainfoop' then begin
     self.loadedData->setDataInfo,$
       st.name[0],$
       mission=st.mission[0],$
       observatory=st.observatory[0],$
       instrument=st.instrument[0],$
       units=st.unit[0],$
       coordinate_system=st.coord[0],$
       newname=st.newname[0]
   endif else if st.type eq 'cotransop' then begin
     thm_ui_cotrans_new,guiId,st.value[0],st.active,self.loadedData,statustext,historywin,/silent
   endif else if st.type eq 'getspecop' then begin
     thm_ui_part_getspec_replay, st.probe, st.dtype, st.trange, $
              st.start_angle[0], st.suffix[0], st.angle[0], $
              st.phi, st.theta, st.pitch, st.gyro, $
              st.erange, st.energy[0], st.regrid, $
              st.other_dim[0], st.normalize[0], st.datagap[0], $
              st.mask_remove[0], st.method_sunpulse_clean[0], $
              st.limit_sunpulse_clean[0], st.fillin_method[0], $
              statustext, historyWin,self.loadedData
              
   endif else begin
      ok = error_message('Unrecognized function call',/traceback)
      return
    endelse
  
  endfor

end

;  This method will combine two call sequence objects.  It attempts to guarantee that
;    the correct data quantities are generated, while preventing duplication of effort.
;    
;  Right now this amounts to performing a set union, that #1 preserves order, #2 eliminates
;  the elements with the lowest index, in the event of duplication. #3 Equality is strict,
;  a weaker possibility entails considering calls equal if dtype,observe,& outcoord are equal
;  but not the times.(As times get overwritten)
;  
;  This is not the most stable or reliable method, but more effective and accurate
;  solutions, would require associating produced quantities with particular loads,
;  and accounting for calls to other data operations.  Implementing such features would
;  be time-consuming, and would probably require modifications of pre-existing tdas
;  routines.  
;
pro thm_ui_call_sequence::merge,callSequenceObj

  compile_opt idl2

  myCalls = self->getCalls()  
  yourCalls = callSequenceObj->getCalls()
  
  if is_num(myCalls[0]) && is_num(yourCalls[0]) then begin
    return
  endif else if is_num(yourCalls[0]) then begin
    return
  endif else if is_num(myCalls[0]) then begin
    self->setCalls,yourCalls
    return
  endif
  
  allCalls = [myCalls,yourCalls]
  outCalls = ptrarr(n_elements(allCalls))
  n = 0 
  
  for i = 0,n_elements(allCalls)-2 do begin
  
    equalp = 0
  
    for j = i+1,n_elements(allCalls)-1 do begin
   
      if self->callEqual(*allCalls[i],*allCalls[j]) then begin
      
        equalp = 1
        break
      
      endif       
    
    endfor
    
    if ~equalp then begin
      outCalls[n] = allCalls[i]
      n++
    endif else begin
      ptr_free,allCalls[i]
    endelse
   
  endfor
  
  outCalls[n] = allCalls[i]
  outCalls = outCalls[0:n]
  self->setCalls,outCalls

end

;Method for internal use.  Determines if two calls are equal
function thm_ui_call_sequence::callEqual,a,b

  compile_opt idl2

  at = [a]
  bt = [b]

  if ~is_struct(at) || ~is_struct(bt) then begin
    return,0
  endif

  a_tags = tag_names(at)
  b_tags = tag_names(bt)
  
  if ~array_equal(a_tags,b_tags) then begin
    return,0
  endif
  
  for i = 0,n_elements(a_tags)-1L do begin
  
    if ~array_equal(size(at.(i)),size(bt.(i))) then begin
      return,0
    endif
  
    if ~is_equal(at.(i),bt.(i)) then begin
      return,0
    endif
  endfor
  
  return,1
     
end
   
;Adds a struct to the call list
;NOTE: For internal use only.
pro thm_ui_call_sequence::addSt,st

  compile_opt idl2

  if ~is_struct(st) then return
  
  if ~ptr_valid(self.call_list) then begin
  
    arr = ptrarr(1)
  
    arr[0] = ptr_new(st)
  
    self.call_list = ptr_new(arr)
  endif else begin
    
    arr = *self.call_list
    arr_new = ptrarr(n_elements(arr)+1)
    
    arr_new[0:n_elements(arr)-1] = arr
    arr_new[n_elements(arr_new)-1] = ptr_new(st)
    
    ptr_free,self.call_list
    self.call_list = ptr_new(arr_new)
  endelse

end

;This method will write a copy of the call sequence to the
;file with the specified name
function thm_ui_call_sequence::GetDomElement,parent

  compile_opt idl2

  calls = self->getCalls()

  document = parent->getOwnerDocument()

  domfxlist = document->createElement('THM_UI_CALL_SEQUENCE')

  if ~is_num(calls) then begin

    self->appendXMLNewline,domfxlist

    for i = 0,n_elements(calls)-1 do begin
    
      str = *calls[i]
      
      self->addXMLDomItem,str,domfxlist,index_override=i
    
    endfor
  endif

  return,domfxlist

end

;This method will read a copy of a call sequence
;from the specified file.  This routine
;could easily be leveraged into a general purpose
;xml serialization routine
pro thm_ui_call_sequence::BuildFromDomElement,element

  compile_opt idl2
 
  if element->gettagname() ne 'THM_UI_CALL_SEQUENCE' then begin
    message,'Incorrect tag name: expected THM_UI_CALL_SEQUENCE, got '+element->gettagname()
    return
  endif
  
  fxcalls = element->getchildnodes()
 
  fxarray = self->getNodeArray(fxcalls)
    
  if ~is_num(fxarray) then begin
  
    call_list = ptrarr(n_elements(fxarray))
  
    for i = 0,n_elements(fxarray)-1 do begin
    
      fxitem = fxarray[i]
    
      if ~obj_isa(fxitem,'IDLFFXMLDOMELEMENT') then begin
        message,'DOMElement expected'
        return
      endif
      
      undefine,str
      self->readdomitem,fxitem,out=str,fail=fail,element=element
      
      if fail then begin
        ptr_free,call_list
        return
      endif

      call_list[element] = ptr_new(str)
      
    endfor
 
    self->setCalls,call_list
  
  endif

end

;This routine is used to report a DOM error when deserializing
pro idlXmlDomMsg,filename,line,col,message

  compile_opt idl2

  s = 'error occurred in:' +filename
  s += 'At Line: ' + strtrim(string(line),2) + ' Col: ' + strtrim(string(col),2)
  s += 'With message: ' + message
  
  message,s

end


;This add any item to the parent xml dom class
pro thm_ui_call_sequence::addXMLDomItem,item,parent,_extra=_extra

  if is_struct(item) then begin
    self->addXMLDomStruct,item,parent,_extra=_extra
  endif else if is_num(item,/integer) then begin
    self->addXMLDomInt,item,parent
  endif else if is_num(item,/float) then begin
    self->addXMLDomFloat,item,parent
  endif else if is_string(item,/blank) then begin
    self->addXMLDomString,item,parent
  endif else begin
    message,'Unsupported type for XML serialize'
    return
  endelse


end

;This add structs to the parent xml dom class
pro thm_ui_call_sequence::addXMLDomStruct,item,parent,index_override=index_override

  compile_opt idl2

  if ~is_struct(item) then begin
    message,'Illegal type passed to add xml dom struct'
    return
  endif

  document = parent->getOwnerDocument()

  for i = 0,n_elements(item)-1 do begin

    element = document->createElement('struct')

    if (n_elements(index_override) NE 0) then begin
      index=index_override
    endif else begin
      index=i
    endelse
    element->setattribute,'element',strtrim(string(index),2)
      
    str = item[i]
      
    tags = tag_names(str)
      
    self->appendXMLNewline,element
      
    for j = 0,n_elements(tags)-1 do begin
          
      attribute = document->createElement('attribute')
      attribute->setAttribute,'name',tags[j]
        
      self->addXMLDomItem,str.(j),attribute
    
      tmp = element->appendChild(attribute)
      
      self->appendXMLNewline,element
    
    endfor
    
    tmp = parent->appendChild(element)
 
    self->appendXMLNewline,parent
 
  endfor

end

;This add ints to the parent xml dom class
pro thm_ui_call_sequence::addXMLDomInt,item,parent

  compile_opt idl2
  
  if ~is_num(item,/integer) then begin
    message,'Illegal type passed to add xml dom int'
    return
  endif
  
  self->appendXMLNewline,parent
  
  for i = 0,n_elements(item)-1 do begin
  
    self->appendXMLElement,'int',strtrim(string(item[i],format='(I)'),2),parent,attribute='element',value=strtrim(string(i),2)
    self->appendXMLNewline,parent
  
  endfor

end

;This add floats to the parent xml dom class
pro thm_ui_call_sequence::addXMLDomFloat,item,parent

  compile_opt idl2
  
  if ~is_num(item,/float) then begin
    message,'Illegal type passed to add xml dom float'
    return
  endif
  
  self->appendXMLNewline,parent
  
  for i = 0,n_elements(item)-1 do begin
  
    self->appendXMLElement,'float',strtrim(string(item[i],format='(E21.13)'),2),parent,attribute='element',value=strtrim(string(i),2)
    self->appendXMLNewline,parent
  
  endfor

end

;Will add the string to the parent xml dom class
pro thm_ui_call_sequence::addXMLDomString,item,parent

  compile_opt idl2
  
  if ~is_string(item,/blank) then begin
    message,'Illegal type passed to add xml dom string'
    return
  endif
  
  self->appendXMLNewline,parent

  for i = 0,n_elements(item)-1 do begin

    self->appendXMLElement,'string',string(item[i],format='(A)'),parent,attribute='element',value=strtrim(string(i),2)
    self->appendXMLNewline,parent

  endfor

end

;Helper function creates a common element grouping
pro thm_ui_call_sequence::appendXMLElement,tagname,text,parent,attribute=attribute,value=value
  
  document = parent->getOwnerDocument()
  element = document->createElement(tagname)  
  
  if keyword_set(attribute) && keyword_set(value) then begin
    element->setattribute,attribute,value
  endif 
  
  str = document->createTextNode(text)
  tmp = element->appendChild(str)
  tmp = parent->appendChild(element)

end

;helper function to add line feeds/carriage returns to document
pro thm_ui_call_sequence::appendXMLNewline,parent

  document = parent->getOwnerDocument()
  element = document->createTextNode(string(13B)+string(10B))
  tmp = parent->appendChild(element)

end

;This method will read an item of some type from the xml doc tree
pro thm_ui_call_sequence::readDomItem,doctree,out=out,fail=fail,element=element

  compile_opt idl2

  fail = 1

  name = doctree->gettagname()
  
  if name eq 'struct' then begin
    self->readDomStruct,doctree,out=out,fail=fail,element=element
  endif else if name eq 'string' then begin
    self->readDomString,doctree,out=out,fail=fail,element=element
  endif else if name eq 'int' then begin
    self->readDomInt,doctree,out=out,fail=fail,element=element
  endif else if name eq 'float' then begin
    self->readDomFloat,doctree,out=out,fail=fail,element=element
  endif else begin
    out = 0
    message,'Unrecognized type'
    return
  endelse
  
  return
  
end

;This method will read a string from the xml document tree
pro thm_ui_call_sequence::readDomString,doctree,out=out,fail=fail,element=element

  compile_opt idl2

  fail = 1
  
  out = ''
  
  element = doctree->getattribute('element')

  if doctree->hasChildNodes() then begin

    child = doctree->getChildNodes()
  
    item = child->item(0)

    out = item->getdata()
  endif else begin
    out = ''
  endelse
  
  fail = 0

  return

end

;This method will read an int from the xml document tree
pro thm_ui_call_sequence::readDomInt,doctree,out=out,fail=fail,element=element

  compile_opt idl2

  fail = 1
  
  out = 0
   
  element = doctree->getattribute('element')
  
  child = doctree->getChildNodes()
  
  item = child->item(0)
 
  out = long(item->getdata())
  
  fail = 0

  return
  
end

;This method will read a float from the xml document tree
pro thm_ui_call_sequence::readDomFloat,doctree,out=out,fail=fail,element=element

  compile_opt idl2

  fail = 1
  
  out = 0D
  
  element = doctree->getattribute('element')
  
  child = doctree->getChildNodes()
  
  item = child->item(0)
 
  out = double(item->getdata())

  fail = 0

  return
  
end

;This method will read a struct from the xml document tree
pro thm_ui_call_sequence::readDomStruct,doctree,out=out,fail=fail,element=element

  compile_opt idl2
  
  fail = 1
  
  element = doctree->getattribute('element')
  
  attributes = doctree->getchildnodes()
  
  atrarr = self->getNodeArray(attributes)
  
  if is_num(atrarr) then begin
    message,'Struct must have some attributes'
    return
  endif
  
  for i = 0,n_elements(atrarr)-1 do begin
  
    atr = atrarr[i]
    
    if strlowcase(atr->gettagname()) ne 'attribute' then begin
      message,'Structs can contain only attributes'
      return
    endif
    
    name = atr->getattribute('name')
    
    children = atr->getchildnodes()
    
    childarr = self->getNodeArray(children)
    
    if is_num(childarr) then begin
      message,'attribute must have some value'
      return
    endif
    
    currentatt = 0
  
    for j = 0,n_elements(childarr)-1 do begin
  
      self->readDomItem,childarr[j],out=val,fail=failitem,element=child_element
  
      if failitem then begin
        return
      endif
  
      if ~keyword_set(currentatt) then begin
        currentatt = replicate(val,n_elements(childarr))
        
        ;currentatt[child_element] = val
      endif else begin
      
        if size(val,/type) ne size(currentatt[0],/type) then begin
          message,'array types do not match'
          return
        endif
        
        currentatt[child_element] = val
        
      endelse 
  
    endfor
    
    if ~keyword_set(out) then begin
      out = create_struct(name,currentatt)
    endif else begin
      str_element,out,name,currentatt,/add
    endelse
  
  endfor
  
  fail = 0
  
end

;This method takes a node list object and returns
;an array of the nodes in the list, but with extraneous
;text objects removed
function thm_ui_call_sequence::getNodeArray,nodelist

  compile_opt idl2

  length = nodelist->getlength()
  
  for i = 0,length-1 do begin
  
    item = nodelist->item(i)
  
    if ~obj_isa(item,'IDLFFXMLDOMTEXT') then begin
    
      if ~keyword_set(out) then begin
        out = [item]
      endif else begin
        out = [out,item]
      endelse
    
    endif
  
  endfor
  
  if ~keyword_set(out) then begin
    return,0
  endif else begin
    return,out
  endelse

end

; Set loadedData object
; This method exists so that obj_new('thm_ui_call_sequence') can
; return a valid object even if no loadedData object is supplied
; to the constructor, as will happen when opening a THEMIS document.

pro thm_ui_call_sequence::SetLoadedData,loadedData
  self.loadedData = loadedData
end

;init, requires loaded data
function thm_ui_call_sequence::init,loadedData

  compile_opt idl2

  self.call_list = ptr_new()

  if (n_elements(loadedData) GT 0) then begin
     self.loadedData = loadedData
  endif else begin
     self.loadedData = obj_new()
  endelse

  return,1

end

pro thm_ui_call_sequence__define

  compile_opt idl2

  struct = { thm_ui_call_sequence,     $
             call_list:ptr_new(), $  ;an array of ptrs to structs
             loadedData:obj_new() }  ; the loaded data object
      
end
