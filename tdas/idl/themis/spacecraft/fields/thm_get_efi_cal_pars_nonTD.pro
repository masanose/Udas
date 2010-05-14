;+
;	Procedure:
;		thm_get_efi_cal_pars
;
;	Purpose:
;		Given the particular EFI waveform data type, and begin and end
;               of the time interval, return the waveform RAW->PHYS
;               transformation parameters.
;
;	Calling Sequence:
;	thm_get_efi_cal_pars, tbeg, tend, name, probes, cal_pars=cal_pars
;
;	Arguements:
;		tbeg, tend	DOUBLE, time in seconds since THEMIS epoch.
;		name	STRING, waveform data type indicator.
;		cal_pars	STRUCT, see Notes below for elements.
;
;   Keywords:
;       TEST:
;         Disables selected /CONTINUE to MESSAGE.  For QA testing only.
;
;Modifications:
;  Added FILE_RETRIEVE() mechanism so that this routine can read the
;    template and calibration files from the THEMIS local data directory.
;    Also, added the "probes" parameter for the above purpose,
;    W.M.Feuerstein, 2/27/2008.
;  Changed "History" to "Modifications" in header, commented out all local
;    calibration parameters, redifined ASCII template, now retrieving all
;    calibration parameters from calibration files (these were expanded last
;    Friday).  Updated doc'n, WMF, 3/17/08.
;  Implemented EAC/EDC gain conditional based on 1st element of TH?_EF?_HED_AC
;    tplot variable, updated doc'n, WMF, 3/18/08.
;  Inserted modern compile options, made ADC offsets integers in calibration
;    files, updated read templates, WMF, 3/20/08.
;  Fixed potential logical vs. bitwise conditional bug, updated calibration
;    fields, WMF, 3/21/2008.
;  Preparing for time-dependent calibration params. and AC/DC coupling gain
;    conditional -- code still commented, WMF, 4/4/2008 (F).
;  Replaced PRINT w/ MESSAGE for EAC/DC gain conditional warning. Added TEST kw
;    (default = 0) to disable CONTINUE kw to selected MESSAGE commands.  If a
;    non-EFI datatype is passed then routine will stop, WMF, 4/7/2008 (M).
;  Fixed typo "Defaulting to EAC gain"=>"...EDC...", rephrased, WMF,
;    4/9/2008 (W).
;  Reformatted, WMF, 4/10/2008 (Th).
;  Corrected an error to assigning the field gain to the output structure --
;    would only affect AC coupled data, WMF, 4/11/2008.
;  Implemented time-dependent EAC/EDC gain conditional, WMF, 4/22/2008 (Tu).
;  Removed "[0]" in some of the CP structure fields (moving to T-D calibration
;    parameters), clipping CAL_PAR_TIME to TIMES, WMF, 4/23/2008 (W).
;  Added 'efs' datatype to switch statement, WMF, 5/12/2008 (M).
;  Renamed from "thm_get_efi_cal_pars.pro" to "thm_get_efi_cal_pars_nonTD.pro", WMF, 9/9/2008.
;
;	Notes:
;	-- use of TBEG and TEND for time-dependent calibration parameters is
;          not currently implemented!
;	-- E-field gains and units are for voltages, not fields, since we have
;          not deployed yet!
;	-- Difference in gain between DC and AC coupled E-field data not
;          implemented yet!
;	-- Elements of cal_pars are as follows:
;		gain,	FLOAT[ 3 or 6], gain of source channel at DC in (phys
;               unit)/ADC.
;		offset, FLOAT[ 3 or 6], offset of channel in ADC.
;
; $LastChangedBy: mfeuerstein $
; $LastChangedDate: 2008-09-09 17:30:11 -0700 (Tue, 09 Sep 2008) $
; $LastChangedRevision: 3471 $
; $URL $
;-
pro thm_get_efi_cal_pars, times, name, probes, cal_pars=cal_pars, $
  test=test
;pro thm_get_efi_cal_pars, tbeg, tend, name, probes, cal_pars=cal_pars, $
;  test=test


compile_opt idl2, strictarrsubs              ;Bring this routine up to date.

if ~keyword_set(test) then test = 0   ;Certain MESSAGE commands /continue
                                      ;by default.

; nominal EFI waveform calibration parameters;
;	JWB, UCBSSL, 7 Feb 2007.
;cal_par_time = '2002-01-01/00:00:00' ;This now obtained from cal. file.

;These are now gotten from cal. file:
;====================================
;units_edc = 'V'	; <--- NOTE that E-field units are Volts!!!
;units_eac = 'V'	; <--- NOTE that E-field units are Volts!!!
;units_v = 'V'


;Smallest ADC step:
;===================
;adc_factor = 1.0/float( 2L^16 - 1L)  ;Now folded into cal. file.

;2 booms X max. voltage scale X smallest ADC step:
;=================================================
;gain_v = 2.0*105.2*adc_factor  ;Transferred to cal. file.
;gain_edc = 2.0*15.0*adc_factor ;Transferred to cal. file.
;gain_eac = 2.0*2.54*adc_factor ;Transferred to cal. file.


;Read in boom shorting factors, spin-independent, and
;spin-dependent offsets:
;=======================
;thx = 'tha'
;To be propagated to all satellites like this:
;=============================================
thx = 'th' + probes[0]
;
;cal_relpathname = thx+'/l1/eff/0000/'+'thm_efi_calib_params.txt'
;cal_templ_relpathname = thx+'/l1/eff/0000/'+'thm_efi_calib_templ.sav'
;To be propagated to all satellites like this:
;=============================================
cal_relpathname = thx+'/l1/eff/0000/'+thx+'_efi_calib_params.txt'
cal_templ_relpathname = thx+'/l1/eff/0000/'+thx+'_efi_calib_templ.sav'
;
cal_file = file_retrieve(cal_relpathname, _extra=!themis)
cal_templ_file = file_retrieve(cal_templ_relpathname, _extra=!themis)
restore,cal_templ_file
str=read_ascii(cal_file,template=templ)


;;Make the first dimension the time dimension for all fields:
;;===========================================================
;tag_names=tag_names(str)
;n_tag_names=n_elements(tag_names)
;n_tag_names_1=n_tag_names-1
;for i=0,n_tag_names_1 do begin
;  if size(tag_names[i],/n_dimensions) eq 2 then $
;    str=create_struct(str.(0:i-1),transpose(str.(i)),str.(i+1:n_tag_names_1))
;  endif
;endfor



;===============================================
;Clip calibration parameters to ranges of TIMES:
;===============================================
cptd = time_double(str.cal_par_time)     ;"Calibration Parameter Time Double."
w0=where(cptd lt times[0])
w1=where(cptd ge times[0] and cptd lt times[n_elements(times)-1])
case 1 of
  w0[0] ne -1 and w1[0] ne -1: w=[w0[n_elements(w0)-1],w1]
  w0[0] ne -1 and w1[0] eq -1: w=w0[n_elements(w0)-1]
  w0[0] eq -1 and w1[0] ne -1: w=w1
endcase
;
cptd=cptd[w]
tag_names=tag_names(str)


;Some fields are common to all datatypes:
;========================================
cal_par_time=str.cal_par_time
boom_length=str.boom_length
boom_shorting_factor=str.boom_shorting_factor


switch strlowcase( name) of
  'vaf':
  'vbf':
  'vap':
  'vbp':
  'vaw':
  'vbw': begin
    cal_pars = { $
      cal_par_time:cal_par_time, $
      gain:str.v_gain, $
      offset:str.v_offset, $
      units:str.v_unit, $
      boom_length:boom_length, $
      boom_shorting_factor:boom_shorting_factor $
      }

    break
  end
  'eff':
  'efp':
  'efw': begin
    ;===================================================
    ;Test for header_ac data.  If not found, default to EAC
    ;gain and print warning.  If found, set accordingly:
    ;===================================================
    tpname_hed_ac = 'th'+probes[0]+'_'+name+'_hed_ac'
    tplot_names,tpname_hed_ac,names=tpnames
    if ~(~size(tpnames,/type)) && tpnames[0] ne '' then begin
      get_data,tpnames[0],data=data_hed_ac
      ;
      ;Index AC vs. DC-coupled points in HED_AC:
      ;=========================================
      n_hed_ac = n_elements(data_hed_ac.y)
      n_hed_ac_2=n_hed_ac-2
      wac = where(data_hed_ac.y,n_wac,complement = wdc)
      ;
      ;
      ;GAIN will be set to EAC_GAIN (if all points are 1), EDC_GAIN
      ;(if all points are 0), or interpolated to TIMES otherwise:
      ;==========================================================
      aci=bytarr(n_elements(times))  ;"AC Interpolated".
      acx = data_hed_ac.x            ;Do not make structure ref. inside loop.
      acy = data_hed_ac.y            ;Do not make structure ref. inside loop.
      ;
      case 1 of
        n_wac eq n_hed_ac: gain = str.eac_gain
        n_wac eq 0: gain = str.edc_gain
        else: begin
          j=0
          ;
          ;Interpolation of HED_AC data:
          ;=============================
          for i=0,n_elements(times)-1 do begin
            if times[i]-acx[j] ge acx[j+1]-times[i] then j = $
              j ne n_hed_ac_2 ? ++j : j
          ;  if times[i]-acx[j] ge acx[j+1]-times[i] then ++j
            aci[i] = acy[j]
          endfor
          ;
          ;Multiply and merge:
          ;===================
          gain = dblarr(n_elements(times),3)
          gain[*,0] = aci*str.eac_gain[0] + (~aci)*str.edc_gain[0]
          gain[*,1] = aci*str.eac_gain[1] + (~aci)*str.edc_gain[1]
          gain[*,2] = aci*str.eac_gain[2] + (~aci)*str.edc_gain[2]
        end
      endcase
    endif else begin
      message,'*** WARNING!: TPLOT variable "'+ $
	tpname_hed_ac+ $
	'" not found.  Defaulting to EDC gain.', $
	continue=~test
      gain=str.edc_gain
    endelse
    ;
    ;
    cal_pars = { $
      cal_par_time:cal_par_time, $
      gain:gain, $
      offset:str.edc_offset, $
      units:str.e_unit, $
      boom_length:boom_length, $
      boom_shorting_factor:boom_shorting_factor, $
      dsc_offset:str.dsc_offset $
      }

    break
  end
  'efs': begin
    ;================================
    ;'efs' data is always DC coupled.
    ;================================
    gain=str.edc_gain
    ;
    cal_pars = { $
      cal_par_time:cal_par_time, $
      gain:gain, $
      offset:str.edc_offset, $
      units:str.e_unit, $
      boom_length:boom_length, $
      boom_shorting_factor:boom_shorting_factor, $
      dsc_offset:str.dsc_offset $
      }

    break
  end
  else:	begin
	  message,'NOT AN EFI DATATYPE!'
  end
endswitch

;return
end
