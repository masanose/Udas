;+ 
;NAME: 
; thm_ui_data__define
;
;PURPOSE:
; This is a data object that contains names ids (self, group, and parent)
;
;CALLING SEQUENCE:
; To Create:    myDataObj = Obj_New("THM_UI_DATA")
; To Use:       data = myDataObj->GetData() 
;
;OUTPUT:
; reference to a data object
;
;ATTRIBUTES:
; name           name of data
; timeName       name of the time component of this data
; dataPtr        ptr to the data component of this data
; yaxisName      name of the yaxis component of this data
; indepName      name of variable to use on indep axis instead of time
; limitPtr       ptr to the limit struct from the tplot variable that
;                   it originated 
; dlimitPtr      ptr to the dlimit struct from the tplot variable that
;                   it originated
; id             unique identifier for this data object
; groupName      group name
; fileName       data file name
; isTime         this boolean indicates whether a data object represents time
; isSpect        flag set when data is spectrographic
; mission        the mission the data is for
; observatory    spacecraft or groundstation of the data
; coordSys       coordinate system of the data, if applicable
; instrument     instrument of the data
; units          the units for this data qunatity
; yaxisunits     the units of the yaxis for this quantity(not important internally, but when we are passing around metadata during a tplot operation, it becomes more important)
; suffix         the suffix of the quantity(if applicable)
; settings       object representing the default data settings
;
;OUTPUT:
; data object
;
;METHODS:
; getName
; getDataPtr
; getRange
; Copy
; SetProperty 
; GetProperty
; GetAll
;
;NOTES:
;  Methods: GetProperty,SetProperty,GetAll,SetAll are now managed automatically using the parent class
;  thm_ui_getset.  You can still call these methods when using objects of type thm_ui_data, and
;  call them in the same way as before
;
;$LastChangedBy: pcruce $
;$LastChangedDate: 2009-09-10 15:50:56 -0700 (Thu, 10 Sep 2009) $
;$LastChangedRevision: 6711 $
;$URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/thmsoc/tags/tdas_5_20/idl/themis/thm_ui_new/objects/thm_ui_data__define.pro $
;-----------------------------------------------------------------------------------

function thm_ui_data::getName

  return,self.name

end

function thm_ui_data::getDataPtr

  return,self.dataPtr
  
end

function thm_ui_data::getRange

  compile_opt idl2

  if ~ptr_valid(self.dataPtr) then return,0
  
  return,[(*self.dataPtr)[0],(*self.dataPtr)[n_elements(*self.dataPtr)-1]]

end

function thm_ui_data::getGroupName

  return,self.groupname

end
;takes a data object and updates its dlimits structure.
pro thm_ui_data::updateDlimits

  compile_opt idl2  
  
  if ptr_valid(self.dlimitptr) then BEGIN
    dlimit = *self.dlimitptr
    if self.isSpect then begin
      str_element,dlimit,'spec',1,/add_replace
    endif else begin
      str_element,dlimit,'spec',0,/add_replace
    endelse
  endif else begin
    dlimit = {spec:self.isSpect}
  endelse
  
  if in_set('data_att', strlowcase(tag_names(dlimit))) && is_struct(dlimit.data_att) then begin
    data_att = dlimit.data_att
    str_element,data_att,'project',self.mission,/add_replace
    str_element,data_att,'observatory',self.observatory,/add_replace
    str_element,data_att,'instrument',self.instrument,/add_replace
    str_element,data_att,'filename',self.filename,/add_replace
    str_element,data_att,'coord_sys',self.coordSys,/add_replace
    str_element,data_att,'units',self.units,/add_replace
  endif else begin
    data_att = {project:self.mission,$
                observatory:self.observatory,$
                instrument:self.instrument,$
                filename:self.filename,$
                coord_sys:self.coordSys,$
                units:self.units}
  endelse
                              
  str_element,dlimit,'data_att',data_att,/add_replace
  
  if ptr_valid(self.dlimitptr) then begin
    *self.dlimitptr = dlimit
  endif else begin
    self.dlimitptr = ptr_new(dlimit)
  endelse
  
end



FUNCTION THM_UI_DATA::Copy
   out = Obj_New("THM_UI_DATA", '', 0)
   selfClass = Obj_Class(self)
   outClass = Obj_Class(out)
   IF selfClass NE outClass THEN BEGIN
       Print, 'Object classes not identical'
       RETURN, -1
   END
   Struct_Assign, self, out
   newTime=Obj_New("THM_UI_TIME_RANGE")
   IF Obj_Valid(self.timeRange) THEN newTime=self.timeRange->Copy() ELSE $
      newTime=Obj_New()
   out->SetProperty, TimeRange=newTime
   
   if obj_valid(self.settings) then begin
     newSettings = self.settings->Copy()
   endif else begin
     newSettings = obj_new('thm_ui_data_settings',self.name,0)
   endelse
   out->setProperty,settings=newSettings
   
   RETURN, out
END ;--------------------------------------------------------------------------------


;handles special case for units keyword and calls general purpose getset getProperty method
PRO THM_UI_DATA::GetProperty,units=units,$
                             _ref_extra=ex
  
   
   self->thm_ui_getset::getProperty,_extra=ex
              
   if arg_present(units) then begin
    
     if keyword_set(self.units) then begin
       units = self.units
     endif else if ptr_valid(self.dlimitPtr) && is_struct(*self.dlimitPtr) then begin
       dlimits = *self.dlimitptr  
       if in_set('data_att',strlowcase(tag_names(dlimits))) && $
          in_set('units',strlowcase(tag_names(dlimits.data_att)))  then begin
         units=dlimits.data_att.units
       endif else if in_set('ysubtitle',strlowcase(tag_names(dlimits))) then begin
         units=dlimits.ysubtitle
       endif else begin
         units=''
       endelse
       self.units=units
     endif else begin
       units=''
     endelse
   endif
   
END ;--------------------------------------------------------------------------------



;PRO THM_UI_DATA::Cleanup
;Obj_Destroy, self.timeRange
;RETURN
;END ;--------------------------------------------------------------------------------

 
FUNCTION THM_UI_DATA::Init,             $ ; The INIT method of the data object.
              name,                     $ ; var name of data, required
              id,                       $ ; unique identifier for this data object
              timeName=timeName,        $ ; name of the time element for this object
              dataPtr=dataPtr,          $ ; ptr to data element for this object
              yaxisName=yaxisName,      $ ; name of the yaxis element for this object(may be null)
              indepName=indepName,      $ ; name of variable to use on indep axis instead of time
              limitPtr=limitPtr,        $ ; ptr to limit element from the tplot var that it came from
              dlimitPtr=dlimitPtr,      $ ; ptr to dlimit element from the tplot var that it came from
              GroupName=groupname,      $ ; name of data file 
              FileName=filename,        $ ; name of data file 
              isTime=isTime,            $ ; indicates whether data object represents a time
              isYaxis=isYaxis,          $ ; indicates whether data object represents the yaxis of a multi-Dim data quantity
              isSpect=isSpect,          $ ; flag set if data is spectrographic
              mission = mission,        $ ; the mission of the data
              observatory = observatory,$ ; the spacecraft or groundstation of the data
              coordSys = coordSys,      $ ; the coordinate system of the data
              instrument = instrument,  $ ; the instrument from which the data was collected    
              timerange=timerange,      $ ; The time range object of the data object
              units=units,              $ ; the units of this variable
              yaxisunits=yaxisunits,    $ ; the units of the yaxis of this variable
              suffix=suffix,            $ ; suffix of this variable,if applicable
              settings=settings           ; data settings object

   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /Cancel
      ok = Error_Message(Traceback=1)
      RETURN, 0
   ENDIF
 
   ; Check that all parameters have values

   IF N_Elements(name) EQ 0 THEN name = ''

   if n_elements(timeName) eq 0 then timeName = ''
   if n_elements(dataPtr) eq 0 then dataPtr = ptr_new()
   if n_elements(yaxisName) eq 0 then yaxisName = ''
   if n_elements(indepName) eq 0 then indepName = ''
   if n_elements(limitptr) eq 0 then limitPtr = ptr_new()
   if n_elements(dlimitptr) eq 0 then dlimitptr = ptr_new()
   if n_elements(groupname) eq 0 then groupname = ''
   IF N_Elements(id) EQ 0 THEN id = 0
   IF N_Elements(filename) EQ 0 THEN filename = '' ELSE $
      IF Is_Numeric(filename) THEN RETURN, 0
    
   if n_elements(mission) eq 0 then mission = ''
   if n_elements(observatory) eq 0 then observatory = ''
   if n_elements(coordSys) eq 0 then coordSys = ''
   if n_elements(instrument) eq 0 then instrument = ''
   if n_elements(timerange) eq 0 then timerange = obj_new()
   if n_elements(units) eq 0 then units = ''
   if n_elements(yaxisunits) eq 0 then yaxisunits = ''
   if n_elements(suffix) eq 0 then suffix = ''
   if n_elements(settings) eq 0 then settings = obj_new()
   
   ; Set all parameters
   
   self.name = name
   self.timeName = timeName
   self.dataPtr = dataPtr
   self.yaxisName = yaxisName
   self.indepName = indepName
   self.limitPtr = limitPtr
   self.dlimitPtr = dlimitPtr
   self.id = id
   self.groupName = groupname
   self.fileName = filename
   self.isTime = keyword_set(isTime)
   self.isSpect = keyword_set(isSpect)
   self.isYaxis = keyword_set(isYaxis)
   self.mission = mission
   self.observatory = observatory
   self.coordSys = coordSys
   self.instrument = instrument
   self.timerange = timerange
   self.units = units
   self.yaxisunits = yaxisunits
   self.suffix = suffix
   self.settings = settings
   
   RETURN, 1
END ;--------------------------------------------------------------------------------



PRO THM_UI_DATA__DEFINE

   struct = { THM_UI_DATA,              $ 
   
              name: '',                 $  ; var name of data
              timeName:'',              $  ; name of the time component for this object
              yaxisName:'',             $  ; name of the yaxis component for this object(may be null)
              indepName:'',             $ ; name of variable to use on indep axis instead of time
              dataPtr:ptr_new(),        $  ; ptr to the data component for this object
              limitPtr:ptr_new(),       $  ; ptr to the limit struct
              dlimitPtr:ptr_new(),      $  ; ptr to the dlimit struct  
              id: 0,                    $ ; id for this object (tplot id )
              groupName: '',            $ ; url or file name  
              fileName: '',             $ ; url or file name  
              isTime:0,                 $ ; set to 1 if an object is a time data object
              isYaxis:0,                $ ; set flag to 1 if an object is a yaxis data object
              isSpect:0,                $ ; flag set if data is spectrographic
              mission: '' ,             $ ; The mission of the data.
              observatory:'',           $ ; the spacecraft or groundstation of the data
              coordSys:'',              $ ; the coordinate of the data
              instrument: '',           $ ; the instrument from which the data was collected
              timeRange:Obj_new(),      $ ; the time range object of the data object
              units:'',                 $ ; units for data product
              yaxisunits:'',            $ ; units for yaxis of this variable if applicable
              suffix:'',                $ ; suffix of this variable, if applicable
              settings:obj_new(),       $ ; data settings object for this data object
              inherits thm_ui_getset    $ ; generalized setProperty/getProperty/getAll/setAll methods   
                                
               
}

END