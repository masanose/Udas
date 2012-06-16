;+
; NAME: 
;   gui_acknowledgement, instrument=instrument, $
;                        datatype=datatype, $
;                        site_or_param=site_or_param, $
;                        par_names=par_names
;
; PURPOSE:
;   Show data policy for IUGONET data on GUI
;
; Written by: Y.-M. Tanaka, May 11, 2012 (ytanaka at nipr.ac.jp)
;-

function gui_acknowledgement, instrument=instrument, datatype=datatype, $
              site_or_param=site_or_param, par_names=par_names

if par_names[0] eq '' then return, 'Cancel'

;----- Get !iugonet.data_policy -----;
case instrument of 
  'Iitate_Planetary_Radio_Telescope': iug_var = !iugonet.data_policy.iprt
  'geomagnetic_field_index': begin
      case datatype of
        'ASY_index': iug_var = !iugonet.data_policy.gmag_wdc_ae_asy
        'AE_index' : iug_var = !iugonet.data_policy.gmag_wdc_ae_asy
        'Dst_index': iug_var = !iugonet.data_policy.gmag_wdc_dst
      endcase
    end
  'geomagnetic_field_fluxgate': begin
      case datatype of
        'magdas'   : iug_var = !iugonet.data_policy.gmag_magdas
        '210mm#'   : iug_var = !iugonet.data_policy.gmag_mm210
        'WDC_kyoto': iug_var = !iugonet.data_policy.gmag_wdc
        'NIPR_mag#': begin
            case site_or_param of
                'syo': iug_var = !iugonet.data_policy.gmag_nipr_syo
                'aed': iug_var = !iugonet.data_policy.gmag_nipr_ice
                'hus': iug_var = !iugonet.data_policy.gmag_nipr_ice
                'isa': iug_var = !iugonet.data_policy.gmag_nipr_ice
                'tjo': iug_var = !iugonet.data_policy.gmag_nipr_ice
            endcase
          end
      endcase
    end
  'SuperDARN#': begin
      case site_or_param of
        'hok': iug_var = !iugonet.data_policy.sdfit_hok
        'ksr': iug_var = !iugonet.data_policy.sdfit_ksr
        'sye': iug_var = !iugonet.data_policy.sdfit_syo
        'sys': iug_var = !iugonet.data_policy.sdfit_syo
      endcase
    end
  'Equatorial_Atomosphere_Radar'  : iug_var = !iugonet.data_policy.ear
  'Medium_Frequency_radar'        : iug_var = !iugonet.data_policy.mf_rish
  'Meteor_Wind_radar'             : iug_var = !iugonet.data_policy.meteor_rish
  'Middle_Upper_atomosphere_radar': iug_var = !iugonet.data_policy.mu
  'Boundary_Layer_Radar'          : iug_var = !iugonet.data_policy.blr_rish
  'Lower_Troposphere_Radar'       : iug_var = !iugonet.data_policy.ltr_rish
  'EISCAT_radar'                  : iug_var = !iugonet.data_policy.eiscat
  'Wind_Profiler_Radar_(LQ-7)'    : iug_var = !iugonet.data_policy.wpr_rish
endcase

;----- If iug_var is 0, show data policy. -----;
if iug_var eq 1 then begin
  Answer = 'OK'
endif else begin
  Answer = show_acknowledgement(instrument = instrument, datatype = datatype, $
	par_names = par_names)
  if Answer eq 'OK' then begin 
    iug_var = 1

    ;----- Put !iugonet.data_policy -----;
    case instrument of
      'Iitate_Planetary_Radio_Telescope': !iugonet.data_policy.iprt = iug_var
      'geomagnetic_field_index': begin
          case datatype of
            'ASY_index': !iugonet.data_policy.gmag_wdc_ae_asy = iug_var
            'AE_index' : !iugonet.data_policy.gmag_wdc_ae_asy = iug_var
            'Dst_index': !iugonet.data_policy.gmag_wdc_dst = iug_var
          endcase
        end
      'geomagnetic_field_fluxgate': begin
          case datatype of
            'magdas'   : !iugonet.data_policy.gmag_magdas = iug_var
            '210mm#'   : !iugonet.data_policy.gmag_mm210 = iug_var
            'WDC_kyoto': !iugonet.data_policy.gmag_wdc = iug_var
            'NIPR_mag#': begin
                case site_or_param of
                    'syo': !iugonet.data_policy.gmag_nipr_syo = iug_var
                    'aed': !iugonet.data_policy.gmag_nipr_ice = iug_var
                    'hus': !iugonet.data_policy.gmag_nipr_ice = iug_var
                    'isa': !iugonet.data_policy.gmag_nipr_ice = iug_var
                    'tjo': !iugonet.data_policy.gmag_nipr_ice = iug_var
                endcase
              end
          endcase
        end
      'SuperDARN#': begin
          case site_or_param of
            'hok': !iugonet.data_policy.sdfit_hok = iug_var
            'ksr': !iugonet.data_policy.sdfit_ksr = iug_var
            'sye': !iugonet.data_policy.sdfit_syo = iug_var
            'sys': !iugonet.data_policy.sdfit_syo = iug_var
          endcase
        end
      'Equatorial_Atomosphere_Radar'  : !iugonet.data_policy.ear = iug_var
      'Medium_Frequency_radar'        : !iugonet.data_policy.mf_rish = iug_var
      'Meteor_Wind_radar'             : !iugonet.data_policy.meteor_rish = iug_var
      'Middle_Upper_atomosphere_radar': !iugonet.data_policy.mu = iug_var
      'Boundary_Layer_Radar'          : !iugonet.data_policy.blr_rish = iug_var
      'Lower_Troposphere_Radar'       : !iugonet.data_policy.ltr_rish = iug_var
      'EISCAT_radar'                  : !iugonet.data_policy.eiscat = iug_var
      'Wind_Profiler_Radar_(LQ-7)'    : !iugonet.data_policy.wpr_rish = iug_var
    endcase
  endif
endelse

return, Answer

end
