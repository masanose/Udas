;+
;procedure: gse2sse
;
;Purpose: Coordinate transformation between GSE & SSE coordinates(and the inverse)
;
;     SSE is defined as:
;        X: Moon->Sun Line
;        Y: Ecliptic North cross X
;        Z: X cross Y
;         
;     GSE is defined as:
;        X: Earth Sun Line(naturally in the ecliptic plane)
;        Y: Z x X
;        Z: Ecliptic North
;inputs:
;
;  name_in: 
;    Name of input tplot variable to be transformed
;  name_sun_pos:
;    Name of the solar position tplot variable in GEI coordinates
;  name_lun_pos:
;    Name of the lunar position tplot variable in GEI coordinates
;  name_out:
;    Name that the rotated variable should take.
;    
;keywords:
;
;   /SSE2GSE inverse transformation
;
;   /IGNORE_DLIMITS: Dlimits normally used to determine if coordinate
;   system is correct, to decide if position needs offset, or to 
;   stop incorrect transforms.  This option will stop this behavior. 
;
;Examples:
;      gse2sse,'tha_state_pos','slp_sun_pos_gse','slp_lun_pos_gse','tha_state_pos_sse'
;      gse2sse,'tha_state_pos_sse','slp_sun_pos_gse','slp_lun_pos_gse','tha_state_pos_gse',/sse2gse,/ignore_dlimits
;
;Notes:
;   #1 SSE coordinate Z-axis is generally not exactly parallel to ecliptic north,
;      as the moon will not always be in the ecliptic plane, and thus the moon->sun line
;      will not always lie in the ecliptic plane.
;   #2 If dlimit-labeled position passed in without /ignore_dlimits,
;      input will be offset to account for relative position of frames of reference.
;   #3 If dlimit-labeled velocity passed in without /ignore_dlimits,
;      input will be offset to account for relative motion of frames of reference
;   #4 If dlimit-labeled acceleration passed in without /ignore_dlimits,
;      warning will be raise, but offset will not be applied automatically
;   #5 Uses tvector_rotate, and sse_matrix_make to perform the rotation.
;      tvector_rotate will also interpolate the rotation matrix onto the time-grid of the input.
;      Interpolation done using quaterions and the spherical linear interpolation algorithm (SLERP)
;      
;Written by Jenni Kissinger and Patrick Cruce
;
; $LastChangedBy: pcruce $
; $LastChangedDate: 2009-08-18 14:45:25 -0700 (Tue, 18 Aug 2009) $
; $LastChangedRevision: 6602 $
; $URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/ssl_general/tags/tdas_5_21/cotrans/gse2sse.pro $
;-



pro gse2sse,name_in,name_sun_pos,name_lun_pos,name_out,sse2gse=sse2gse,ignore_dlimits=ignore_dlimits

  compile_opt idl2
  
  if n_params() ne 4 then begin
    message,'Aborted: Missing 1 or more required arguments: name_in,name_out,name_sun_pos,name_lun_pos
  end
  
  sun_name = tnames(name_sun_pos)
  lun_name = tnames(name_lun_pos)
  
  if n_elements(sun_name) ne 1 || n_elements(lun_name) ne 1 || sun_name[0] eq '' || lun_name[0] eq '' then begin
    message,'Aborted: Must load Sun/Moon position to perform this transformation (Load Routine:"thm_load_slp")'
  endif
  
  get_data,name_in,dlimit=dl,data=in_d
  get_data,sun_name,data=sun_d,dlimit=sun_dl
  get_data,lun_name,data=lun_d,dlimit=lun_dl
  
  if ~is_struct(sun_d) || ~is_struct(lun_d) then begin
    message,'Aborted: Must load Sun/Moon position to perform this transformation (Load Routine:"thm_load_slp")'
  endif
  
  if min(sun_d.x,/nan)-min(in_d.x,/nan) gt 60*60 || max(in_d.x,/nan) - max(sun_d.x,/nan) gt 60*60 then begin
    dprint,'NON-FATAL-ERROR: ' + sun_name + ' and ' + name_in + ' fail to overlap for time greater than 1 hour. Data may have significant interpolation errors.' 
  endif
  
  if min(lun_d.x,/nan)-min(in_d.x,/nan) gt 60*60 || max(in_d.x,/nan) - max(lun_d.x,/nan) gt 60*60 then begin
    dprint,'NON-FATAL-ERROR: ' + lun_name + ' and ' + name_in + ' fail to overlap for time greater than 1 hour. Data may have significant interpolation errors.' 
  endif
  
  sun_pos_coord = 'none' ;setting default, if data_att.coordsys not present
  str_element,sun_dl,'data_att.coord_sys',sun_pos_coord
  
  ;assumes sun pos in gei coordinates if ignore dlimits set
  if keyword_set(ignore_dlimits) || sun_pos_coord eq 'gei' then begin
    cotrans,sun_name,sun_name+'_gse_cotrans',/gei2gse,ignore_dlimits=ignore_dlimits
    sun_name = sun_name+'_gse_cotrans'
  endif else if sun_pos_coord ne 'gse' then begin
    message,'ERROR: ' + sun_name + ' needs to be in GSE or GEI coordinates'
  endif
 
  lun_pos_coord = 'none' ;setting default, if data_att.coordsys not present
  str_element,lun_dl,'data_att.coord_sys',lun_pos_coord
  
  ;assumes sun pos in gei coordinates if ignore dlimits set
  if keyword_set(ignore_dlimits) || lun_pos_coord eq 'gei' then begin
    cotrans,lun_name,lun_name+'_gse_cotrans',/gei2gse,ignore_dlimits=ignore_dlimits
    lun_name = lun_name+'_gse_cotrans'
  endif else if lun_pos_coord ne 'gse' then begin
    message,'ERROR: ' + lun_name + ' needs to be in GSE or GEI coordinates'
  endif else begin
    copy_data,lun_name,lun_name+'_gse_cotrans' ;working copy of data
    lun_name = lun_name+'_gse_cotrans'
  endelse
 
  sse_matrix_make,sun_name,lun_name,newname='sse_mat_cotrans',fail=fail,ignore_dlimits=ignore_dlimits
  
  if fail then begin
    message,'Failed to create SSE rotation matrix
  endif

  st_type = 'none'
  str_element,dl,'data_att.st_type',st_type

  name_one = name_in
  name_two = name_out

  ;perform affine offsets for forward transform
  if ~keyword_set(sse2gse) then begin
  
    dprint,'GSE-->SSE'
  
    if st_type eq 'pos' then begin
      if keyword_set(ignore_dlimits) then begin
        dprint,'WARNING: dlimits indicates data is a position.  Full transformation between SSE coords requires offset that will not be performed because IGNORE_DLIMITS is set'
      endif else begin
        tinterpol_mxn,lun_name,name_one,error=err,/overwrite
        if err eq 0 then begin
          message,'Error performing position offset during SSE transformation(interpolation operation)'
        endif
        
        calc,'"'+name_two+'" = "' + name_one + '"-"'+lun_name+'"',error=err
        if keyword_set(err) then begin
          message,'Error performing position offset during SSE transformation(subtraction operation)'
        endif
        
        name_one = name_two
      endelse
    endif else if st_type eq 'vel' then begin
      if keyword_set(ignore_dlimits) then begin
        dprint,'WARNING: dlimits indicates data is a velocity.  Full transformation between SSE coords requires offset that will not be performed because IGNORE_DLIMITS is set'
      endif else begin
        
        deriv_data,lun_name,/replace
        
        tinterpol_mxn,lun_name,name_one,/overwrite,error=err
        if err eq 0 then begin
          message,'Error performing velocity offset during SSE transformation(interpolation operation)'
        endif
        
        calc,'"'+name_two+'" = "' + name_one + '"-"' + lun_name + '"',error=err
        if keyword_set(err) then begin
          message,'Error performing velocity offset during SSE transformation(subtraction operation)'
        endif
        
        name_one = name_two
        
      endelse
    endif else if st_type eq 'acc' then begin
      dprint,'WARNING dlimits indicates data is an acceleration.  Full transformation between SSE coords requires offset that must be performed manually.'
    endif
  endif
  
  tvector_rotate,'sse_mat_cotrans',name_one,newname=name_two,error=err,invert=keyword_set(sse2gse),/vector_skip_nonmonotonic,/matrix_skip_nonmonotonic
  
  if err eq 0 then begin
  
    message,'Error performing rotation during SSE transformation
  
  endif
  
  ;performing affine offsets for inverse transform
  if keyword_set(sse2gse) then begin
  
    dprint,'SSE-->GSE'
  
    if st_type eq 'pos' then begin
      if keyword_set(ignore_dlimits) then begin
        dprint,'WARNING: dlimits indicates data is a position.  Full transformation between SSE coords requires offset that will not be performed because IGNORE_DLIMITS is set'
      endif else begin
        tinterpol_mxn,lun_name+'_gse_cotrans',name_one,/overwrite,error=err
        if err eq 0 then begin
          message,'Error performing position offset during SSE transformation(interpolation operation)'
        endif
        
        calc,'"'+name_two+'" = "' + name_one + '"+"'+lun_name+'"',error=err
        if keyword_set(err) then begin
          message,'Error performing position offset during SSE transformation(subtraction operation)'
        endif
        
        name_one = name_two
      endelse
    endif else if st_type eq 'vel' then begin
      if keyword_set(ignore_dlimits) then begin
        dprint,'WARNING: dlimits indicates data is a velocity.  Full transformation between SSE coords requires offset that will not be performed because IGNORE_DLIMITS is set'
      endif else begin
        
        deriv_data,lun_name,/replace
                
        tinterpol_mxn,lun_name,name_one,/overwrite,error=err
        
        if err eq 0 then begin
          message,'Error performing velocity offset during SSE transformation(interpolation operation)'
        endif
        
        calc,'"'+name_two+'" = "' + name_one + '"+"'+lun_name+'"',error=err
        if keyword_set(err) then begin
          message,'Error performing velocity offset during SSE transformation(subtraction operation)'
        endif
        
        name_one = name_two
        
      endelse
    endif else if st_type eq 'acc' then begin
      dprint,'WARNING dlimits indicates data is an acceleration.  Full transformation between SSE coords requires offset that must be performed manually.'
    endif
  endif
  
  
end

