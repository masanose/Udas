==================================================================
; UDAS plug-in software package for tdas_6_00
; 					May 16, 2011
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
- Space Environment Research Center, Kyushu University


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
<erg_ground>
Load procedures for ERG-related data:
- erg_load_sdfit 	; SuperDARN radar data

<load>
Load procedures for IUGONET data:
- iug_load_blr_rish_txt	; Boundary layer radar data from RISH, Kyoto Univ
- iug_load_ear		; Equatorial Atmospheric Radar (EAR) data from RISH
- iug_load_gmag_serc	; MAGDAS magnetometer data from SERC, Kyushu Univ
- iug_load_gmag_wdc	; WDC geomagnetic indices and the magnetometer data
- iug_load_iprt		; Iitate Planetary Ratio Telescope data from Tohoku Univ
- iug_load_ltr_rish_txt	; Lower troposphere radar from RISH
- iug_load_meteor_rish	; Meteor wind radar data from RISH
- iug_load_mf_rish	; MF radar data from RISH
- iug_load_mu		; Middle Upper (MU) atmosphere radar data from RISH
- iug_load_gmag_mm210	; Alias for "erg_load_gmag_mm210"
- iug_load_gmag_nipr	; Alias for "erg_load_gmag_nipr"
- iug_load_sdfit	; Alias for "erg_load_sdfit"

<examples>
Example crib sheets for IUGONET/ERG data

<gui>
Procudures in this directory are used to customize TDAS-GUI for 
IUGONET/ERG data
------------------------------------------------------------


++++++++++++++++++++++++++++++++
+           Examples           +
++++++++++++++++++++++++++++++++

CUI:
------------------------------------------------------------------------
  timespan,'2007-06-21' & erg_load_sdfit, site='hok'
  timespan,'2006-12-01' & iug_load_blr_rish_txt, site='ktb'
  timespan,'2003-03-25' & iug_load_ear
  timespan,'2006-12-01' & iug_load_gmag_mm210, site='tik'
  timespan,'2006-12-01' & iug_load_gmag_nipr, site='syo'
  timespan,'2008-03-20' & iug_load_gmag_serc, site='anc'
  timespan,'2006-12-01',31 & iug_load_gmag_wdc, site='sym'
  timespan,'2010-11-01',/hours,5 & iug_load_iprt
  timespan,'2004-10-01' & iug_load_ltr_rish_txt, site='sgk'
  timespan,'2005-03-20' & iug_load_meteor_rish, site = 'ktb'
  timespan,'2005-03-20' & iug_load_mf_rish, site = 'pam'
  timespan,'2003-03-10' & iug_load_mu
------------------------------------------------------------------------

GUI:
--------------------------------------------------------------------------
  Instrument Type	datatype	param1		param2	Date
--------------------------------------------------------------------------
- Boundary_Layer_Radar	troposphere	ktb		*	2006-12-01
- Eq._Atom._Radar	troposphere	ear_std 	*	2003-03-25
			e_region	eb1p2a		*	2009-04-22
			ef_region	efb1p16		*	2001-07-30
			v_region	150p8c8b2a	*	2008-03-05
			f_region	fb1p16a		*	2001-08-01
- geomag._fluxgate	magdas		anc		*	2008-03-20
			210mm		tik		*	2006-12-01
			WDC_kyoto	kak		*	2006-12-01
			NIPR_mag	syo		*	2003-03-25
- geomag._index		Dst_index	WDC_kyoto	*	2006-12-01
			AE_index	WDC_kyoto	*	2006-12-01
			ASY_index	WDC_kyoto	*	2006-12-01
- IPRT 			Sun		iit		*	2010-11-01
- Lower_Tropos._Radar	troposphere	sgk		*	2004-10-01
- Medium_Freq._radar	thermosphere	pam		*       2004-03-25
- Meteor_wind_radar	thermosphere	ktb		*	2002-12-01
- MUpper_atom._radar 	troposphere	mur_standard	*	2003-03-10
- SuperDARN 		ionosphere	hok		*	2006-12-15
--------------------------------------------------------------------------


++++++++++++++++++++++++++++++++
+        !!! NOTICE !!!        +
++++++++++++++++++++++++++++++++
1. Japanese SuperDARN radar data (HOK,KSR,SYE,SYS) in CDF are distributed
by Energization and Radiation in Geospace Science Center (ERG-SC) at
Solar-Terrestrial Environment Laboratory, Nagoya University, in
collaboration with Japanese SuperDARN PI groups.
  Access to these data are currently restricted to only users in Japan.
The data will be open to foreign researchers in future upon
confirmation by the SuperDARN PI committee.
  As for questions and request for the data, please feel free to contact
the ERG-SC office (E-mail: erg-sc-core at st4a.stelab.nagoya-u.ac.jp).

2. Load procedure of MAGDAS data (iug_load_gmag_serc) is currently applicable 
only to MAGDAS 1-minute averaged data observed during recent WHI campaign 
(from March 20 to April 16, 2008). You can see more details about this data:
http://magdas.serc.kyushu-u.ac.jp/whi/index.php
Future data release is a work in progress.

3. Procedure "iug_load_iprt" calls fits_read, sxpar, fits_open, 
fits_close, gettok, sxdelpar. For the use of this procedure,
get FITS I/O procedures from the IDL Astronomy Library   
(http://idlastro.gsfc.nasa.gov/fitsio.html).

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
  SuperDARN KSR radar data was provided by the National Institute of 
Information and Communications Technology (NICT) through ERG-SC. The load 
program for this data was developed in collaboration with NICT (NICT website: 
http://www.nict.go.jp/index-e.html).


++++++++++++++++++++++++++++++++
+            Contact           +
++++++++++++++++++++++++++++++++
  For questions, comments, and requests about UDAS, please contact 
the UDAS development team (udas at iugonet.org).
