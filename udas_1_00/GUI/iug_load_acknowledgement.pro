;+
;
;Name:
; iug_load_acknowledgement
;
;Purpose:
; Output of the data acknowledgement message for each observation data.
;
;Syntax:
; iug_load_acknowledgement, instrument= instrument, datatype = datatype
;
;Keywords:
;  instrument = Observation instrument type. For example, iug_load_acknowledgement, instrument = 'Boundary_Layer_Radar'.
;               The default is 'Middle_Upper_atomosphere_radar'.
;  datatype = Observation data type. For example, iug_load_acknowledgement, datatype = 'AE_index'.
;             The default is 'troposphere'. 
;
;Code:
;  A. Shinbori, 13/01/2011.
;  
;Modifications:
; 
;  
;Acknowledgment:
; $LastChangedBy:  $
; $LastChangedDate:  $
; $LastChangedRevision:  $
; $URL $
;-

pro iug_load_acknowledgement, instrument=instrument, datatype=datatype

;**************
;keyword check:
;**************
  if (not keyword_set(instrument)) then instrument='Middle_Upper_atomosphere_radar'
 
;****************************************
;Load 'toroposphere' data by default:
;****************************************
  if (not keyword_set(datatype)) then datatype='troposphere'
 
;Output of the data acknowledgement window for each instrument:
  if instrument eq 'Boundary_Layer_Radar' then begin   
     OK=acknowledgement_message('If you acquire boundary layer radar (BLR) data, we ask that you acknowledge us '+ $
                             'in your use of the data. This may be done by including text '+ $
                             'such as the BLR data provided by Research Institute for Sustainable '+ $
                             'Humanosphere of Kyoto University. We would also appreciate receiving '+ $
                             'a copy of the relevant publications.',$
                              /noname, /center, title='Acknowledgement') 
  endif
  
  if instrument eq 'Equatorial_Atomosphere_Radar' then begin   
     OK=acknowledgement_message('If you acquire equatorial atmosphere radar (EAR) data, we ask that you acknowledge us '+ $
                             'in your use of the data. This may be done by including text '+ $
                             'such as the EAR data provided by Research Institute for Sustainable '+ $
                             'Humanosphere of Kyoto University. We would also appreciate receiving '+ $
                             'a copy of the relevant publications.',$
                              /noname, /center, title='Acknowledgement') 
  endif

  if instrument eq 'geomagnetic_field_index' then begin
    if datatype eq 'Dst_index' then begin
       acknowledg_str = $
       'The DST data are provided by the World Data Center for Geomagnetism, Kyoto, and'+ $
       ' are not for redistribution (http://wdc.kugi.kyoto-u.ac.jp/). Furthermore, we thank'+ $
       ' the geomagnetic observatories (Kakioka [JMA], Honolulu and San Juan [USGS], Hermanus'+ $
       ' [RSA], Alibag [IIG]), NiCT, INTERMAGNET, and many others for their cooperation to'+ $
       ' make the Dst index available.'
    endif else begin
       acknowledg_str = $
       'The rules for the data use and exchange are defined'+ $
       ' by the Guide on the World Data Center System '+ $
       ' (ICSU Panel on World Data Centers, 1996).'+$
       ' Note that information on the appropriate institution(s)'+$
       ' is also supplied with the WDC data sets.'+$
       ' If the data are used in publications and presentations,'+$
       ' the data suppliers and the WDC for Geomagnetism, Kyoto'+$
       ' must properly be acknowledged.'+$
       ' Commercial use and re-distribution of WDC data are, in general, not allowed.'+$
       ' Please ask for the information of each observatory to the WDC.'
    endelse
    OK=acknowledgement_message(acknowledg_str,/noname, /center, title='Rules of the Road for WDC Data Use:')
  endif
  
  if instrument eq 'geomagnetic_field_fluxgate' then begin
     if datatype eq 'magdas' then begin
        OK=acknowledgement_message('Scientists who want to engage in collaboration with SERC should contact the project leader of MAGDAS/CPMN observations, Prof. Dr. K. Yumoto, Kyushu Univ., who will organize such collaborations.'+ $
                                   ''+ $
                                   'There is a possibility that the PI of MAGDAS will arrange offers so that there is less overlapping of themes between MAGDAS research groups.'+ $
                                   ''+ $
                                   'Before you use MAGDAS/CPMN data for your papers, you must agree to the following points;'+ $
                                   ''+ $
                                   '  1. Before you submit your paper, you must contact the PI (Prof. K. Yumoto: yumoto@serc.kyushu-u.ac.jp) and discuss authorship.'+ $
                                   '  2. When you submit your paper after doing the above item 1, you must mention the source of the data in the acknowledgment section of your paper.'+ $
                                   '  3. In general, you must use the following references:'+ $
                                   '    1. Yumoto, K., and the 210MM Magnetic Observation Group, The STEP 210 magnetic meridian network project, J. Geomag. Geoelectr., 48, 1297-1310., 1996.'+ $
                                   '    2. Yumoto, K. and the CPMN Group, Characteristics of Pi 2 magnetic pulsations observed at the CPMN stations: A review of the STEP results, Earth Planets Space, 53, 981-992, 2001.'+ $
                                   "    3. Yumoto K. and the MAGDAS Group, MAGDAS project and its application for space weather, Solar Influence on the Heliosphere and Earth's Environment: Recent Progress and Prospects, Edited by N. Gopalswamy and A. Bhattacharyya, ISBN-81-87099-40-2, pp. 309-405, 2006." + $
                                   '    4. Yumoto K. and the MAGDAS Group, Space weather activities at SERC for IHY: MAGDAS, Bull. Astr. Soc. India, 35, pp. 511-522, 2007.'+ $
                                   '  4. In all circumstances, if anything is published you must send a hardcopy to the following address:'+ $
                                   ''+ $
                                   '    Prof. Dr. Kiyohumi Yumoto PI of MAGDAS/CPMN Project Director of Space Environment Research Center, Kyushu University 53 6-10-1 Hakozaki, Higashi-ku Fukuoka 812-8581, JAPAN TEL/FAX:+81-92-642-4403, e-mail: yumoto@serc.kyushu-u.ac.jp'+ $
                                   '',$
                                   /noname, /center, title='Rules of the Road for MAGDAS/CPMN Data Use:') 
     endif
     if datatype eq 'WDC_kyoto' then begin
        acknowledg_str = $
        'The rules for the data use and exchange are defined'+ $
        ' by the Guide on the World Data Center System '+ $
        ' (ICSU Panel on World Data Centers, 1996).'+$
        ' Note that information on the appropriate institution(s)'+$
        ' is also supplied with the WDC data sets.'+$
        ' If the data are used in publications and presentations,'+$
        ' the data suppliers and the WDC for Geomagnetism, Kyoto'+$
        ' must properly be acknowledged.'+$
        ' Commercial use and re-distribution of WDC data are, in general, not allowed.'+$
        ' Please ask for the information of each observatory to the WDC.'     
     OK=acknowledgement_message(acknowledg_str,/noname, /center, title='Rules of the Road for WDC Data Use:')
     endif
  endif  
  
  if instrument eq 'Iitate_Planetary_Radio_Telescope' then begin   
     OK=acknowledgement_message('Please contact Dr. Hiroaki Misawa.',$
                              /noname, /center, title='Acknowledgement') 
  endif

  if instrument eq 'Medium_Frequency_radar' then begin
     OK=acknowledgement_message('If you acquire medium frequency (MF) radar data, we ask that you acknowledge us '+ $
                             'in your use of the data. This may be done by including text '+ $
                             'such as the MF data provided by Research Institute for Sustainable '+ $
                             'Humanosphere of Kyoto University. We would also appreciate receiving '+ $
                             'a copy of the relevant publications.',$
                              /noname, /center, title='Acknowledgement') 
  endif
  
  if instrument eq 'Meteor_Wind_radar' then begin
     OK=acknowledgement_message('If you acquire meteor wind (MW) radar data, we ask that you acknowledge us '+ $
                             'in your use of the data. This may be done by including text '+ $
                             'such as the MW data provided by Research Institute for Sustainable '+ $
                             'Humanosphere of Kyoto University. We would also appreciate receiving '+ $
                             'a copy of the relevant publications.',$
                              /noname, /center, title='Acknowledgement') 
  endif
  
  if instrument eq 'Middle_Upper_atomosphere_radar' then begin
     OK=acknowledgement_message('If you acquire middle and upper atmosphere (MU) radar data, we ask that you acknowledge us '+ $
                             'in your use of the data. This may be done by including text '+ $
                             'such as the MU data provided by Research Institute for Sustainable '+ $
                             'Humanosphere of Kyoto University. We would also appreciate receiving '+ $
                             'a copy of the relevant publications.',$
                              /noname, /center, title='Acknowledgement') 
  endif

  if instrument eq 'Radio_sonde' then begin
     OK=acknowledgement_message('If you acquire radio sonde radar data, we ask that you acknowledge us '+ $
                             'in your use of the data. This may be done by including text '+ $
                             'such as the radio sonde data provided by Research Institute for Sustainable '+ $
                             'Humanosphere of Kyoto University. We would also appreciate receiving '+ $
                             'a copy of the relevant publications.',$
                              /noname, /center, title='Acknowledgement') 
  endif
end