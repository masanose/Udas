;+
;
;Purpose:
;  helper function, just colocates some replicated code to modify the
;  canvas size if there is variation when windows or modes are switched.
; 
;
;$LastChangedBy: pcruce $
;$LastChangedDate: 2009-08-03 11:29:44 -0700 (Mon, 03 Aug 2009) $
;$LastChangedRevision: 6519 $
;$URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/thmsoc/tags/tdas_5_21/idl/themis/thm_ui_new/display/thm_ui_orientation_update.pro $
;-

pro thm_ui_orientation_update,drawObject,windowStorage

  cwindow = windowstorage->getactive()
  cwindow->getproperty, settings=cwsettings
  cwsettings->getproperty,canvasSize=canvasSize
  drawObject->getProperty,pageSize=pageSize,destination=dest
  
  if (canvasSize[0] ne pageSize[0] || $
      canvasSize[1] ne canvasSize[1]) && $
      obj_valid(dest) then begin
      
      cz = drawObject->getZoom()
      ;note this is done using !D values for consistency with thm_gui_new
      ;to get reliable behavior, both sections should probably be switched over
      ;to use the resolution property of the window object
      size_px = [canvasSize[0]*2.54*!D.X_PX_CM,canvasSize[1]*2.54*!D.Y_PX_CM]
      ;dest->getProperty,resolution=res
     ; size_px = size_cm/res  
      drawObject->setZoom,1
      dest->setProperty,virtual_dimensions=size_px
      drawObject->setZoom,cz
  endif

end