==================================================================
; UDAS plug-in software package for tdas_7_01
;                                                   April 16, 2013
==================================================================

  UDAS is a plug-in software package for TDAS (Themis Data Analysis Software 
suite; http://themis.ssl.berkeley.edu/software.shtml). UDAS consists of IDL 
procedures to download, visualize, and analyze observational data distributed 
from the IUGONET institutions(*). For detailed information on IUGONET, please 
see the project website at http://www.iugonet.org/en/.
  Our software has been developed in collaboration with ERG Science Center 
(ERG-SC website: http://gemsissc.stelab.nagoya-u.ac.jp/erg/).

(*) IUGONET institutions:
- Planetary Plasma and Atmospheric Research Center, Tohoku University
- National Institute of Polar Research
- Solar-Terrestrial Environment Laboratory, Nagoya University
- Research Institute for Sustainable Humanosphere, Kyoto University
- World Data Center for Geomagnetism, Kyoto University
- Kwasan and Hida Observatories, Kyoto University
- International Center for Space Weather Science and Education, Kyushu University
  (the former Space Environment Research Center, Kyushu University)

++++++++++++++++++++++++++++++++
+     System Requirements      +
++++++++++++++++++++++++++++++++
  The system requirements are the same as required for TDAS.
Please see TDAS Users' Guide.
(http://themis.ssl.berkeley.edu/software.shtml)

++++++++++++++++++++++++++++++++
+    Contents on the package   +
++++++++++++++++++++++++++++++++
  The following files (directories) are included in this package:

<iugonet>
------------------------------------------------------------
<load>
Load procedures for IUGONET data:
- iug_load_aws_rish     ; Automatic weather station data from RISH, Kyoto Univ
- iug_load_blr_rish     ; Boundary layer radar data from RISH
- iug_load_ear          ; Equatorial Atmospheric Radar (EAR) data from RISH
- iug_load_eiscat       ; EISCAT radar data
- iug_load_gmag_serc    ; MAGDAS magnetometer data from ICSWSE, Kyushu Univ
- iug_load_gmag_wdc     ; WDC geomagnetic indices and the magnetometer data
- iug_load_gmag_nipr    ; Fluxgate magnetometer data from NIPR
- iug_load_gmag_nipr_induction ; Induction magnetometer data from NIPR
- iug_load_hf_tohokuu   ; Jupiter's/solar wide band spectral data in HF-band
- iug_load_ionosonde_rish ; Ionogram data taken by the ionosonde at Shigaraki
- iug_load_iprt	        ; Iitate Planetary Ratio Telescope data from Tohoku Univ
- iug_load_irio_nipr    ; Imaging Riometer data from NIPR
- iug_load_lfrto        ; Low Frequency Radio Transmitter data from Tohoku Univ
- iug_load_ltr_rish     ; Lower troposphere radar from RISH
- iug_load_meteor_rish  ; Meteor wind radar data from RISH
- iug_load_mf_rish      ; MF radar data from RISH
- iug_load_mu           ; Middle Upper (MU) atmosphere radar data from RISH
- iug_load_radiosonde_rish ; Radiosonde data from RISH
- iug_load_smart        : SMART solar images from the Hida Observatory, Kyoto.
- iug_load_wpr_rish     ; Wind profiler radar (LQ7) from RISH
- iug_load_gmag_mm210   ; Alias for "erg_load_gmag_mm210"
- iug_load_gmag_stel_induction ; Alias for "erg_load_gmag_stel_induction"
- iug_load_sdfit        ; Alias for "erg_load_sdfit"

<examples>
Example crib sheets for IUGONET data

<gui>
Procudures in this directory are used to customize TDAS-GUI for 
IUGONET/ERG data
------------------------------------------------------------

++++++++++++++++++++++++++++++++
+           Examples           +
++++++++++++++++++++++++++++++++
CUI:
------------------------------------------------------------------------
  timespan,'1994-05-03' & iug_load_aws_rish, site='sgk'
  timespan,'2006-12-01' & iug_load_blr_rish, site='ktb'
  timespan,'2003-03-25' & iug_load_ear
  timespan,'2011-02-03' & iug_load_eiscat
  timespan,'2006-12-01' & iug_load_gmag_mm210, site='tik'
  timespan,'2006-12-01' & iug_load_gmag_nipr, site='syo'
  timespan,'2008-03-20' & iug_load_gmag_serc, site='anc'
  timespan,'2006-12-01',31 & iug_load_gmag_wdc, site='sym'
  timespan,'2006-04-17' & iug_load_gmag_nipr_induction, site='syo'
  timespan,'2008-02-28',1,/hour & iug_load_gmag_stel_induction, site='ath'
  timespan,'2004-01-09/22:00',1,/hour & iug_load_hf_tohokuu
  timespan,'2002-07-01',1,/hour & iug_load_ionosonde_rish
  timespan,'2010-11-01',5,/hour & iug_load_iprt
  timespan,'2004-04-07' & iug_load_irio_nipr, site='syo'
  timespan,'2010-05-29' & iug_load_lfrto,site='nal'
  timespan,'2004-10-01' & iug_load_ltr_rish, site='sgk'
  timespan,'2005-03-20' & iug_load_meteor_rish, site = 'ktb'
  timespan,'2005-03-20' & iug_load_mf_rish, site = 'pam'
  timespan,'2003-03-10' & iug_load_mu
  timespan,'2001-10-15' & iug_load_radiosonde_dawex_nc, site='drw'
  timespan,'2007-06-21' & iug_load_sdfit, site='hok'
  timespan,'2005-08-03/05:00',3,/minute & iug_load_smart,filter='p00'
  timespan,'2006-04-01' & iug_load_wpr_rish, site='sgk'
------------------------------------------------------------------------

GUI:
--------------------------------------------------------------------------
  Instrument Type       datatype        param1          param2  Date
--------------------------------------------------------------------------
- Auto._Weather_Station troposphere     sgk             *       1994-05-03
- Boundary_Layer_Radar  troposphere     ktb             *       2006-12-01
- EISCAT_radar          altitude_prof   esr_32m         *       2011-02-03
- Eq._Atom._Radar       troposphere     *(all)          *       2003-03-25
                        e_region        eb1p2a          *       2009-04-22
                        ef_region       efb1p16         *       2001-07-30
                        v_region        150p8c8b2a      *       2008-03-05
                        f_region        fb1p16a         *       2001-08-05
- geomag._fluxgate      magdas          anc             *       2008-03-20
                        210mm           tik             *       2006-12-01
                        WDC_kyoto       kak             *       2006-12-01
                        NIPR_mag        syo             *       2003-03-25
- geomag._induction     NIPR_mag        syo             *       2006-04-17
                        STEL            ath             *       2008-02-28
- geomag._index         Dst_index       WDC_kyoto       *       2006-12-01
                        AE_index        WDC_kyoto       *       2006-12-01
                        ASY_index       WDC_kyoto       *       2006-12-01
- HF_radio_spectrometer Sun_or_Jupiter  iit             *       2004-01-09
- IPRT                  Sun             iit             *       2010-11-01
- Imaging_Riometer      30MHz           syo             *       2004-04-07
- Ionosonde             ionosphere      sgk             *       2002-07-01
- Lower_Tropos._Radar   troposphere     sgk             *       2004-10-01
- LF_radio_transmitter  nal             *(all)          *       2010-05-29
- Medium_Freq._radar    thermosphere    pam             *       2004-03-25
- Meteor_Wind_radar     thermosphere    ktb             *       2002-12-01
- Mid._Upper_atm._radar troposphere     *(all)          *       2003-03-10
                        mesosphere      *(all)          *       2003-03-10
                        ionosphere      *(all)          *       1989-03-06
                        mesosphere      *(all)          *       1990-05-18
- Radiosonde            DAWEX           drw             *       2001-10-15
- SuperDARN             ionosphere      hok             *       2006-12-15
- Wind_Prof._Radar      troposphere     sgk             *       2006-04-01
--------------------------------------------------------------------------

++++++++++++++++++++++++++++++++
+        !!! NOTICE !!!        +
++++++++++++++++++++++++++++++++
1. SuperDARN radar data in CDF are distributed by ERG project science
center (ERG-SC) at Solar-Terrestrial Environment Laboratory, Nagoya
University, in collaboration with SuperDARN PI groups. For use of the
SuperDARN data, it is highly recommended to install the latest ERG-SC
plug-in libraries which are available from
http://gemsissc.stelab.nagoya-u.ac.jp/erg_socware/bleeding_edge/. 
Asfor questions about the data and the plug-in package, please feel 
freeto contact the ERG-SC office (E-mail: erg-sc-core at
st4a.stelab.nagoya-u.ac.jp).

2. Load procedure of MAGDAS data (iug_load_gmag_serc) is currently applicable 
only to MAGDAS 1-minute averaged data observed during recent WHI campaign 
(from March 20 to April 16, 2008). You can see more details about this data:
http://magdas.serc.kyushu-u.ac.jp/whi/index.php
Future data release is a work in progress.

3. Procedures "iug_load_iprt" and "iug_load_smart" need the additional 
FITS library, such as fits_read, sxpar, fits_open, fits_close, gettok, 
sxdelpar, sxaddpar, valid_num, and readfits. For the use of these procedures, 
get FITS I/O procedures from the IDL Astronomy Library 
(http://idlastro.gsfc.nasa.gov/fitsio.html).
You can download these procedures by running "get_fitslib".

4. You can get useful information of our data (ex., data availability, 
contact person, access URL, etc...) at the IUGONET metadata database:
http://search.iugonet.org/iugonet/.

5. For some kinds of data, it may be difficult to load more than one 
week by using GUI due to the memory problem.

++++++++++++++++++++++++++++++++
+       Acknowledgements       +
++++++++++++++++++++++++++++++++
  We acknowledge the cooperation and generosity of the THEMIS Science Support 
Team in allowing us to use TDAS for our analysis software. 

++++++++++++++++++++++++++++++++
+            Contact           +
++++++++++++++++++++++++++++++++
  For questions, comments, and requests about UDAS, please contact 
the UDAS development team (iugonet2009 at gmail.com).

