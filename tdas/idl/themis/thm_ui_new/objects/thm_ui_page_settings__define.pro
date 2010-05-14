;+ 
;NAME: 
; thm_ui_page_settings__define
;
;PURPOSE:  
; Page Settings object - holds the settings for page text, layout and data
;
;CALLING SEQUENCE:
; page = Obj_New("THM_UI_PAGE_SETTINGS")
;
;INPUT:
; none
;
;OUTPUT:
; page setting object reference
;
;ATTRIBUTES:
; title                 text object for title
; labels                text object for labels
; variables             text object for variables
; footer                text object for footer
; marker                text object for markers  
; token                 droplist index for title token
; ifootertoken           droplist index for footer token
; defMarkerTitle        default value for marker titles
; maintainRead          flag to maintain readability
; ShowTraceNames        flag to display trace names
; xPanelSpacing         horizontal spacing between panel(pts)
; yPanelSpacing         vertical spacing between panel(pts)
; heightProp            flag to maintain prop. heights 
; gutterWidth           width size for gutter
; displayOnScreen       flag to display on screen
; altTopBottom          flag to alternate top and bottom
; offsetFirstPage       flag to offset first page
; orientation           0=portrait, 1=landscape
; backgroundColor       name of background color
; leftPrintMargin       size of left print margin (in.)
; rightPrintMargin      size of right print margin (in.)
; topPrintMargin        size of top print margin (in.)
; bottomPrintMargin     size of bottom print margin (in.)
; canvasSize            size of the page in inches
; overlapMajorTicks     flag to overlap major ticks
; showValues            set this flag to show values
; closerThanValue       show data if closer than this
; closerThanUnits       closer units (hr,min,sec,day)
; useSameYRange         flag to use the same y range 
; numMajorTicks         number of major ticks to use
; numMinorTicks         number of minor ticks to use
; skipBlanks            flag to skip blanks
;
;METHODS:
; SetProperty   procedure to set keywords 
; GetProperty   procedure to get keywords 
; GetAll        returns the entire structure
; GetTokenNames returns all the token name options
; GetTokenName  return a token name given an index
; GetUnitNames  returns all closer than unit options
; GetUnitName   returns a unit name given an index
; SetTokenValue places the string value of the token into the text object
; 
;HISTORY:
;
;NOTES:
;
;  Methods: GetProperty,SetProperty,GetAll,SetAll are now managed automatically using the parent class
;  thm_ui_getset.  You can still call these methods when using objects of type thm_ui_page_settings, and
;  call them in the same way as before
;
;$LastChangedBy:pcruce $
;$LastChangedDate:2009-09-10 09:15:19 -0700 (Thu, 10 Sep 2009) $
;$LastChangedRevision:6707 $
;$URL:svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/thmsoc/trunk/idl/themis/thm_ui_new/objects/thm_ui_page_settings__define.pro $
;-----------------------------------------------------------------------------------



FUNCTION THM_UI_PAGE_SETTINGS::Copy
   out = Obj_New("THM_UI_PAGE_SETTINGS",/nosave)
   Struct_Assign, self, out
   ; copy title
   newTitle=Obj_New("THM_UI_TEXT")
   IF Obj_Valid(self.title) THEN newTitle=self.title->Copy() ELSE $
      newTitle=Obj_New()
   out->SetProperty, Title=newTitle
   ; copy labels
   newLabel=Obj_New("THM_UI_TEXT")
   IF Obj_Valid(self.labels) THEN newLabel=self.labels->Copy() ELSE $
      newLabel=Obj_New()
   out->SetProperty, Labels=newLabel
   ; copy Variables
   newVariables=Obj_New("THM_UI_TEXT")
   IF Obj_Valid(self.variables) THEN newVariables=self.variables->Copy() ELSE $
      newVariables=Obj_New()
   out->SetProperty, Variables=newVariables
   ; copy footer
   newFooter=Obj_New("THM_UI_TEXT")
   IF Obj_Valid(self.footer) THEN newFooter=self.footer->Copy() ELSE $
      newFooter=Obj_New()
   out->SetProperty, Footer=newFooter
   ; copy marker
   newMarker=Obj_New("THM_UI_TEXT")
   IF Obj_Valid(self.Marker) THEN newMarker=self.Marker->Copy() ELSE $
      newMarker=Obj_New()
   out->SetProperty, Marker=newMarker
   RETURN, out
END ;--------------------------------------------------------------------------------


;*********************
;Define Token Methods:
;*********************
FUNCTION THM_UI_PAGE_SETTINGS::GetTokenNames
 tokenNames = ['Time', 'Date', 'Year', 'Day of Year']
RETURN, tokenNames 
END ;--------------------------------------------------------------------------------
FUNCTION THM_UI_PAGE_SETTINGS::GetTokenName, index
IF index LT 0 OR index GT 7 THEN RETURN, -1 ELSE tokenNames=self->GetTokenNames()
RETURN, tokenNames[index]
END ;--------------------------------------------------------------------------------
FUNCTION THM_UI_PAGE_SETTINGS::GetTokenCommands
 tokenCommands= ['%time', '%date', '%year', '%doy']
RETURN, tokenCommands
END ;--------------------------------------------------------------------------------
FUNCTION THM_UI_PAGE_SETTINGS::GetTokenCommand, index
IF index LT 0 OR index GT 7 THEN RETURN, -1 ELSE tokenCommands=self->GetTokenCommands()
RETURN, tokenCommands[index]
END ;--------------------------------------------------------------------------------


function thm_ui_page_settings::EvaluateToken ,$
  value, $
  result, $
  inittoken, $
  token

compile_opt idl2, hidden

stringLength = StrLen(value)
tokenlength = strlen(token)
inittokenlength = strlen(inittoken)
case 1 of
  inittokenlength eq stringlength: title = token
  result eq 0: title = token + strmid(value, inittokenlength, stringlength - inittokenlength)
  result gt 0 && result lt stringlength-inittokenlength: $
    title = strmid(value, 0, result) + token + strmid(value, result+inittokenlength, stringlength-inittokenlength-result)
  result eq stringlength-inittokenlength: title = strmid(value, 0, stringlength-inittokenlength) + token
endcase
return,title
end
 

;returns a text object with the correct string
;rather than just a string
FUNCTION THM_UI_PAGE_SETTINGS::GetTitleString

  compile_opt idl2

  self->GetProperty, title=title
  title->GetProperty, Value=value

  value = self->interpolateTitle(value) 

  out = title->copy()
  out->setProperty,value=value

  RETURN,out

END  ;----------------------------------------------------------------------------------

 

;returns a text object with the correct string
;rather than just a string
FUNCTION THM_UI_PAGE_SETTINGS::GetFooterString

  compile_opt idl2

  self->GetProperty, footer=footer
  footer->GetProperty, Value=value

  value = self->interpolateTitle(value) 

  out = footer->copy()
  out->setProperty,value=value

  RETURN,out

END  ;----------------------------------------------------------------------------------

;this routine prevents duplicated code and promotes code reuse
;Original implementation of getFooterString used duplicated code
function thm_ui_page_settings::interpolateTitle,value

  compile_opt idl2
  
    WHILE stregex(value, '%',/boolean) DO BEGIN
      
    ; check for the time command
    IF stregex(value, '%time',/boolean) THEN BEGIN
      result = stregex(value, '%time')
      token = self->GetTokenValue(0)
      text = self->EvaluateToken(value, result, '%time', token)
      value = text
    ENDIF ELSE IF stregex(value, '%date',/boolean) then begin
      result = stregex(value, '%date')
      token = self->GetTokenValue(1)
      text = self->EvaluateToken(value, result, '%date', token)
      value = StrCompress(text)
    ENDIF ELSE IF stregex(value, '%year',/boolean) then begin
      result = stregex(value, '%year')
      token = self->GetTokenValue(2)
      text = self->EvaluateToken(value, result, '%year', token)
      value = text
    ENDIF else if stregex(value, '%doy',/boolean) then begin
      result = stregex(value, '%doy')
      token = self->GetTokenValue(3)
      text = self->EvaluateToken(value, result, '%doy', token)
      value = StrCompress(text)
    ENDIF ELSE BEGIN
      ;prevent infinite loop due to nonexistent format code
      break
    endelse
    
  ENDWHILE 
  
  return,value

end

FUNCTION THM_UI_PAGE_SETTINGS::GetTokenValue, index
   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /Cancel
      ok = Error_Message(Traceback=1)
      RETURN, -1
   ENDIF
 
   timeObj = Obj_New("THM_UI_TIME")
   timeObj->GetProperty, TString=timeString
   timeStruc = timeObj->GetStructure()
   
   CASE index OF
      0: titleValue = StrMid(timeString, 11, 8)   
      1: titleValue = StrMid(timeString, 0, 10)   
      2: titleValue = StrTrim(String(timeStruc.year),2)
      3: titleValue = StrTrim(String(timeStruc.doy),2)  
      ELSE:  
   ENDCASE
   
   RETURN, titleValue
  
END ;--------------------------------------------------------------------------------


;**********************
;Define Format Methods:
;**********************
FUNCTION THM_UI_PAGE_SETTINGS::GetFormatNames
  textObj = Obj_New("THM_UI_TEXT")
  formatNames = textObj->GetFormats()
  Obj_Destroy, textObj
RETURN, formatNames    
END ;--------------------------------------------------------------------------------



FUNCTION THM_UI_PAGE_SETTINGS::GetFormatName, index
FormatNames=self->GetFormatNames()
IF index LT 0 OR index GE n_elements(FormatNames) THEN RETURN, -1 ELSE RETURN, FormatNames[index]
END ;--------------------------------------------------------------------------------



FUNCTION THM_UI_PAGE_SETTINGS::GetFormatCommands
 formatCommands= ['\B', '\I', '\U', '\D', '\L']
; formatCommands= ['\P', '\\', '\B', '\I', '\N', '\U', '\D', '\L', '\H8', '\H24', '\A', '\G', '\S', '\O', '\R']
RETURN, formatCommands
END ;--------------------------------------------------------------------------------



FUNCTION THM_UI_PAGE_SETTINGS::GetFormatCommand, index
formatCommands=self->GetFormatCommands()
IF index LT 0 OR index GE n_elements(formatCommands) THEN RETURN, -1 ELSE RETURN, formatCommands[index]
END ;--------------------------------------------------------------------------------


FUNCTION THM_UI_PAGE_SETTINGS::GetUnitNames
RETURN, ['hours', 'minutes', 'seconds', 'days', '<none>']
END ;--------------------------------------------------------------------------------



FUNCTION THM_UI_PAGE_SETTINGS::GetUnitName, index
IF index LT 0 OR index GT 4 THEN RETURN, -1 ELSE unitNames=self->GetUnitNames()
RETURN, unitNames(index)
END ;--------------------------------------------------------------------------------


FUNCTION THM_UI_PAGE_SETTINGS::GetFontNames
  textObj = Obj_New("THM_UI_TEXT")
  fontNames = textObj->GetFonts()
  Obj_Destroy, textObj
RETURN, fontNames 
END ;--------------------------------------------------------------------------------



FUNCTION THM_UI_PAGE_SETTINGS::GetFontName, index
IF index LT 0 OR index GT 7 THEN RETURN, -1 ELSE fontNames=self->GetFontNames()
RETURN, fontNames(index)
END ;--------------------------------------------------------------------------------


PRO THM_UI_PAGE_SETTINGS::Save             

   obj = self->copy()
   if ptr_valid(self.origSettings) then ptr_free,self.origSettings
   
   self.origSettings = ptr_new(obj->getall())
      
RETURN
END ;--------------------------------------------------------------------------------



PRO THM_UI_PAGE_SETTINGS::Reset

   if ptr_valid(self.origSettings) then begin
      self->setall,*self.origSettings
      self->save
   endif
   
RETURN
END ;--------------------------------------------------------------------------------


FUNCTION THM_UI_PAGE_SETTINGS::Init,       $
      Title=title,                         $ ; text object for title
      Labels=labels,                       $ ; text object for labels
      Variables=variables,                 $ ; text object for variables
      Footer=footer,                       $ ; text object for footer
      Marker=marker,                       $ ; text object for markers  
      Token=token,                         $ ; droplist index for title options
      ifootertoken=ifootertoken,             $ ; droplist index for footer options
      DefMarkerTitle=defmarkertitle,       $ ; default value for marker titles
      maintainRead=maintainread,           $ ; flag to maintain readability
      ShowTraceNames=showtracenames,       $ ; flag to display trace names
      xPanelSpacing=xpanelspacing,         $ ; horizontal spacing between panel(pts)
      yPanelSpacing=ypanelspacing,         $ ; vertical spacing between panel(pts)
      HeightProp=heightprop,               $ ; flag to maintain prop. heights 
      GutterWidth=gutterwidth,             $ ; width size for gutter
      DisplayOnScreen=displayonscreen,     $ ; flag to display on screen
      AltTopBottom=alttopbottom,           $ ; flag to alternate top and bottom
      OffsetFirstPage=offsetfirstpage,     $ ; flag to offset first page
      Orientation=orientation,             $ ; 0=portrait, 1=landscape
      BackgroundColor=backgroundcolor,     $ ; name of background color
      LeftPrintMargin=leftprintmargin,     $ ; size of left print margin (in.)
      RightPrintMargin=rightprintmargin,   $ ; size of right print margin (in.)
      TopPrintMargin=topprintmargin,       $ ; size of top print margin (in.)
      BottomPrintMargin=bottomprintmargin, $ ; size of bottom print margin (in.)
      CanvasSize=canvassize,               $ ; x and y size of the draw area 
      OverlapMajorTicks=overlapmajorticks, $ ; flag to overlap major ticks
      ShowValues=showvalues,               $ ; set this flag to show values
      CloserThanValue=closerthanvalue,     $ ; show data if closer than this
      CloserThanUnits=closerthanunits,     $ ; closer units (hr,min,sec,day)
      UseSameYRange=usesameyrange,         $ ; flag to use the same y range 
      NumMajorTicks=nummajorticks,         $ ; number of major ticks to use
      NumMinorTicks=numminorticks,         $ ; number of minor ticks to use
      SkipBlanks=skipblanks,               $ ; flag to skip blanks
      Debug=debug,                         $ ; flag to debug
      nosave=nosave                          ; don't save on start-up

   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /Cancel
      ok = Error_Message(Traceback=Keyword_Set(debug))
      RETURN, 0
   ENDIF

   
      ; Check that all parameters have values
    
   IF NOT Obj_Valid(title) THEN title = Obj_New('THM_UI_TEXT')
   IF NOT Obj_Valid(labels) THEN labels = Obj_New('THM_UI_TEXT')
   IF NOT Obj_Valid(variables) THEN variables = Obj_New('THM_UI_TEXT',size=8)
   IF NOT Obj_Valid(footer) THEN footer = Obj_New('THM_UI_TEXT')
   IF NOT Obj_Valid(marker) THEN marker = Obj_New('THM_UI_TEXT')
   IF N_Elements(token) EQ 0 THEN token = 0
   IF N_Elements(ifootertoken) EQ 0 THEN ifootertoken = 0
   IF N_Elements(defmarkertitle) EQ 0 THEN defmarkertitle = ''
   IF N_Elements(xpanelspacing) EQ 0 THEN xpanelspacing = 60
   IF N_Elements(ypanelspacing) EQ 0 THEN ypanelspacing = 60
   IF N_Elements(gutterwidth) EQ 0 THEN gutterwidth = 50
   IF N_Elements(orientation) EQ 0 THEN orientation = 0
   IF N_Elements(backgroundcolor) EQ 0 THEN backgroundcolor = [255,255,255]
   IF N_Elements(leftprintmargin) EQ 0 THEN leftprintmargin = 1.25
   IF N_Elements(rightprintmargin) EQ 0 THEN rightprintmargin = 1.25 
   IF N_Elements(topprintmargin) EQ 0 THEN topprintmargin = 1.25
   IF N_Elements(bottomprintmargin) EQ 0 THEN bottomprintmargin = 1.25 
   IF N_Elements(canvassize) EQ 0 THEN canvassize = [8.5, 11.0] 
   IF N_Elements(overlapmajorticks) EQ 0 THEN overlapmajorticks = 0
   IF N_Elements(closerthanvalue) EQ 0 THEN closerthanvalue = 0 
   IF N_Elements(closerthanunits) EQ 0 THEN closerthanunits = 0
   IF N_Elements(nummajorticks) EQ 0 THEN  nummajorticks = 5
   IF N_Elements(numminorticks) EQ 0 THEN  numminorticks = 12
   IF N_Elements(maintainread) EQ 0 THEN  maintainread = 1
   IF N_Elements(showtracenames) EQ 0 THEN  showtracenames = 0
   IF N_Elements(heightprop) EQ 0 THEN  heightprop = 1
   IF N_Elements(displayonscreen) EQ 0 THEN  displayonscreen = 1
   IF N_Elements(alttopbottom) EQ 0 THEN  alttopbottom = 0
   IF N_Elements(offsetfirstpage) EQ 0 THEN  offsetfirstpage = 0
   IF N_Elements(showvalues) EQ 0 THEN  showvalues = 0
   IF N_Elements(usesameyrange) EQ 0 THEN  usesameyrange = 1
   IF N_Elements(skipblanks) EQ 0 THEN  skipblanks = 0

      ; Set all parameters

   self.title = title
   self.labels = labels
   self.variables = variables
   self.footer = footer
   self.marker = marker
   self.token = token
   self.ifootertoken = ifootertoken
   self.defMarkerTitle = defmarkertitle 
   self.maintainRead = maintainread 
   self.showTraceNames = showtracenames
   self.xpanelSpacing = xpanelspacing 
   self.ypanelSpacing = ypanelspacing 
   self.heightProp = heightprop
   self.gutterWidth = gutterwidth 
   self.displayOnScreen = displayonscreen
   self.altTopBottom = alttopbottom
   self.offsetFirstPage = offsetfirstpage 
   self.orientation = orientation
   self.backgroundColor = backgroundcolor 
   self.leftPrintMargin = leftprintmargin
   self.rightPrintMargin = rightprintmargin  
   self.topPrintMargin = topprintmargin 
   self.bottomPrintMargin = bottomprintmargin 
   self.canvasSize = canvassize 
   self.overlapMajorTicks = overlapmajorticks 
   self.showValues = showvalues
   self.closerThanValue = closerthanvalue  
   self.closerThanUnits = closerthanunits 
   self.useSameYRange =  usesameyrange 
   self.numMajorTicks =  nummajorticks
   self.numMinorTicks =  numminorticks 
   self.skipBlanks = skipblanks  

   if ~keyword_set(nosave) then self->save
   
RETURN, 1
END ;--------------------------------------------------------------------------------

PRO THM_UI_PAGE_SETTINGS__DEFINE

   struct = { THM_UI_PAGE_SETTINGS, $

    ; text settings

      title : Obj_New(),            $ ; text object for title
      labels : Obj_New(),           $ ; text object for labels
      variables : Obj_New(),        $ ; text object for variables
      footer : Obj_New(),           $ ; text object for footer
      marker : Obj_New(),           $ ; text object for markers  
      token : 0,                    $ ; droplist index for title token
      ifootertoken : 0,              $ ; droplist index for footer token
      defMarkerTitle : '',          $ ; default value for marker titles
      maintainRead: 0,              $ ; set this flag to maintain readability
      showTraceNames : 0,           $ ; set this flag to display trace names

        ; layout settings
    
      xpanelSpacing : 0,             $ ;horizontal spacing between panels (default=125)
      ypanelSpacing : 0,             $ ;vertical spacing between panels (default=125)
      heightProp : 0,               $ ; flag to maintain proportional heights 
      gutterWidth : 0,              $ ; width size for gutter
      displayOnScreen : 0,          $ ; flag to display on screen
      altTopBottom : 0,             $ ; flag to alternate top and bottom
      offsetFirstPage : 0,          $ ; flag to offset first page
      orientation : 0,              $ ; 0=portrait, 1=landscape
      backgroundColor : [255,255,255],    $ ; name of background color
      leftPrintMargin : 0.,          $ ; size of left print margin (inches)
      rightPrintMargin : 0.,         $ ; size of right print margin (inches)
      topPrintMargin : 0.,           $ ; size of top print margin (inches)
      bottomPrintMargin : 0.,        $ ; size of bottom print margin (inches)
      canvasSize : [0.D, 0.D],         $ ; x and y size of the draw area canvas
      
        ; data settings
    
      overlapMajorTicks : 0,        $ ; number major ticks to overlap
      showValues : 0,               $ ; set this flag to show values
      closerThanValue : 0,          $ ; show data if closer than this
      closerThanUnits : 0,          $ ; closer than units (hr, min, sec, day)
      useSameYRange : 0,            $ ; flag to use the same y range on panels    
      numMajorTicks : 0,            $ ; number of major ticks to use
      numMinorTicks : 0,            $ ; number of minor ticks to use
      skipBlanks : 0,               $ ; flag to skip blanks

      origSettings : Ptr_New(),     $ ; new settings object for reset/save operations
      INHERITS THM_UI_READWRITE,    $ ; generalized read/write methods
      INHERITS thm_ui_getset        $ ; generalized setProperty,getProperty,getAll,setAll
}

END