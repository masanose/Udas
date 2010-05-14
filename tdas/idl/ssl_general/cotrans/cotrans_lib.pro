


;+
;pro sub_GSE2GSM
;
;Purpose: transforms data from GSE to GSM
;
;
;keywords:
;   /GSM2GSE : inverse transformation
;Example:
;      sub_GSE2GSM,tha_fglc_gse,tha_fglc_gsm
;
;      sub_GSE2GSM,tha_fglc_gsm,tha_fglc_gse,/GSM2GSE
;
;
;Notes: under construction!!  will run faster in the near future!!
;
;Written by Hannes Schwarzl
; $LastChangedBy: pcruce $
; $LastChangedDate: 2007-11-19 13:25:56 -0800 (Mon, 19 Nov 2007) $
; $LastChangedRevision: 2056 $
; $URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/ssl_general/tags/tdas_5_21/cotrans/cotrans_lib.pro $
;-


pro sub_GSE2GSM,data_in,data_out,GSM2GSE=GSM2GSE




data_out=data_in


;convert the time
timeS=time_struct(data_in.X)

;get direction
if keyword_set(GSM2GSE) then begin
	message,/continue,'GSM-->GSE'
	isGSM2GSE=1
endif else begin
	message,/continue,'GSE-->GSM'
	isGSM2GSE=0
endelse


if isGSM2GSE eq 0 then begin
	subGSE2GSM,TIMES,data_in.Y,DATA_outARR
endif else begin
    subGSM2GSE,TIMES,data_in.Y,DATA_outARR
endelse


data_out.Y=DATA_outARR

MESSAGE,/CONTINUE,'done'

;RETURN,data_out
end


;#################################################




;+
;pro: sub_GEI2GSE
;
;Purpose: transforms THEMIS fluxgate magnetometer data from GEI to GSE
;
;
;keywords:
;   /GSE2GEI : inverse transformation
;Example:
;      sub_GEI2GSE,tha_fglc_gei,tha_fglc_gse
;
;      sub_GEI2GSE,tha_fglc_gse,tha_fglc_gei,/GSE2GEI
;
;
;Notes: under construction!!  will run faster in the near future!!
;
; $LastChangedBy: pcruce $
; $LastChangedDate: 2007-11-19 13:25:56 -0800 (Mon, 19 Nov 2007) $
; $LastChangedRevision: 2056 $
; $URL $
;-


pro sub_GEI2GSE,data_in,data_out,GSE2GEI=GSE2GEI



data_out=data_in



;convert the time
timeS=time_struct(data_in.X)

;get direction
if keyword_set(GSE2GEI) then begin
	MESSAGE,/CONTINUE,'GSE-->GEI'
	isGSE2GEI=1
endif else begin
	MESSAGE,/CONTINUE,'GEI-->GSE'
	isGSE2GEI=0
endelse




if isGSE2GEI eq 0 then begin
	subGEI2GSE,TIMES,data_in.Y,DATA_outARR
endif else begin
    subGSE2GEI,TIMES,data_in.Y,DATA_outARR
endelse

data_out.Y=DATA_outARR

MESSAGE,/CONTINUE,'done'

;RETURN,data_out
end



;+
;pro sub_GSM2SM
;
;Purpose: transforms data from GSM to SM
;
;
;keywords:
;   /SM2GSM : inverse transformation
;Example:
;      sub_GSM2SM,tha_fglc_gsm,tha_fglc_sm
;
;      sub_GSM2SM,tha_fglc_sm,tha_fglc_gsm,/SM2GSM
;
;
;Notes: under construction!!  will run faster in the near future!!
;
;Written by Hannes Schwarzl
; $LastChangedBy: pcruce $
; $LastChangedDate: 2007-11-19 13:25:56 -0800 (Mon, 19 Nov 2007) $
; $LastChangedRevision: 2056 $
; $URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/ssl_general/tags/tdas_5_21/cotrans/cotrans_lib.pro $
;-


pro sub_GSM2SM,data_in,data_out,SM2GSM=SM2GSM




data_out=data_in


;convert the time
timeS=time_struct(data_in.X)

;get direction
if keyword_set(SM2GSM) then begin
	MESSAGE,/CONTINUE,'SM-->GSM'
	isSM2GSM=1
endif else begin
	MESSAGE,/CONTINUE,'GSM-->SM'
	isSM2GSM=0
endelse


if isSM2GSM eq 0 then begin
	subGSM2SM,TIMES,data_in.Y,DATA_outARR
endif else begin
    subSM2GSM,TIMES,data_in.Y,DATA_outARR
endelse


data_out.Y=DATA_outARR

MESSAGE,/CONTINUE,'done'

;RETURN,data_out
end


;+
;pro sub_GEI2GEO
;
;Purpose: transforms data from GEI to GEO
;
;
;keywords:
;   /GEO2GEI : inverse transformation
;Example:
;      sub_GEI2GEO,tha_fglc_gei,tha_fglc_geo
;
;      sub_GEI2GEO,tha_fglc_geo,tha_fglc_gei,/GEO2GEI
;
;
;Notes:
;
;Written by Patrick Cruce(pcruce@igpp.ucla.edu)
; $LastChangedBy: pcruce $
; $LastChangedDate: 2007-11-19 13:25:56 -0800 (Mon, 19 Nov 2007) $
; $LastChangedRevision: 2056 $
; $URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/ssl_general/tags/tdas_5_21/cotrans/cotrans_lib.pro $
;-


pro sub_GEI2GEO,data_in,data_out,GEO2GEI=GEO2GEI




data_out=data_in


;convert the time
timeS=time_struct(data_in.X)

;get direction
if keyword_set(GEO2GEI) then begin
	message,/continue,'GEO-->GEI'
  subGEO2GEI,TIMES,data_in.Y,DATA_outARR
endif else begin
	message,/continue,'GEI-->GEO'
  subGEI2GEO,TIMES,data_in.Y,DATA_outARR
endelse

data_out.Y=DATA_outARR

message,/continue,'done'

;RETURN,data_out
end


;#################################################





;#################################################
;#################################################
;################## sub functions ################
;#################################################
;#################################################


;+
;proceddure: subGEI2GSE
;
;Purpose: transforms data from GEI to GSE
;
;INPUTS: TIMES as time_struct, DATA_in as nx3 array
;
;
;keywords:
;
;Example:
;
;
;Notes: under construction!!  will run faster in the near future!!
;
; $LastChangedBy: pcruce $
; $LastChangedDate: 2007-11-19 13:25:56 -0800 (Mon, 19 Nov 2007) $
; $LastChangedRevision: 2056 $
; $URL $
;-


pro subGEI2GSE,TIMES,DATA_in,DATA_out



; get array sizes
count=SIZE(DATA_in[*,0],/N_ELEMENTS)
MESSAGE,/CONTINUE,'number of records: ' + string(count)

DATA_out=dblarr(count,3)

tgeigse_vect,TIMES[*].year,TIMES[*].doy,TIMES[*].hour,TIMES[*].min,double(TIMES[*].sec)+TIMES[*].fsec,DATA_in[*,0],DATA_in[*,1],DATA_in[*,2],xgse,ygse,zgse

;for i=0L,count-1L do begin
		;ctimpar,iyear,imonth,iday,ih,im,is
		;This has to be changed to be faster!!!!!!!!!!!!!!!
;		ctimpar,TIMES[i].year,TIMES[i].month,TIMES[i].date,TIMES[i].hour,TIMES[i].min,double(TIMES[i].sec)+TIMES[i].fsec
;		tgeigse,DATA_in[i,0],DATA_in[i,1],DATA_in[i,2],xgse,ygse,zgse
		DATA_out[*,0]=xgse
		DATA_out[*,1]=ygse
		DATA_out[*,2]=zgse
;endfor


;return,DATA_out
end

;#################################################

;+
;procedure: subGSE2GEI
;
;Purpose: transforms data from GSE to GEI
;
;INPUTS: TIMES as time_struct, DATA_in as nx3 array
;
;
;keywords:
;
;Example:
;
;
;Notes: under construction!!  will run faster in the near future!!
;
; $LastChangedBy: pcruce $
; $LastChangedDate: 2007-11-19 13:25:56 -0800 (Mon, 19 Nov 2007) $
; $LastChangedRevision: 2056 $
; $URL $
;-

pro subGSE2GEI,TIMES,DATA_in,DATA_out



; get array sizes
count=SIZE(DATA_in[*,0],/N_ELEMENTS)
MESSAGE,/CONTINUE,'number of records: ' + string(count)

DATA_out=dblarr(count,3)


tgsegei_vect,TIMES[*].year,TIMES[*].doy,TIMES[*].hour,TIMES[*].min,double(TIMES[*].sec)+TIMES[*].fsec,DATA_in[*,0],DATA_in[*,1],DATA_in[*,2],xgei,ygei,zgei

;for i=0L,count-1L do begin
;		;ctimpar,iyear,imonth,iday,ih,im,is
;		;This has to be changed to be faster!!!!!!!!!!!!!!!
;		ctimpar,TIMES[i].year,TIMES[i].month,TIMES[i].date,TIMES[i].hour,TIMES[i].min,double(TIMES[i].sec)+TIMES[i].fsec
;		tgsegei,DATA_in[i,0],DATA_in[i,1],DATA_in[i,2],xgei,ygei,zgei
		DATA_out[*,0]=xgei
		DATA_out[*,1]=ygei
		DATA_out[*,2]=zgei
;endfor


;return,DATA_out
end





;#################################################

;+
;procedure: subGSE2GSM
;
;Purpose: transforms data from GSE to GSM
;
;INPUTS: TIMES as time_struct, DATA_in as nx3 array
;
;
;keywords:
;
;Example:
;
;
;Notes: under construction!!  will run faster in the near future!!
;
; $LastChangedBy: pcruce $
; $LastChangedDate: 2007-11-19 13:25:56 -0800 (Mon, 19 Nov 2007) $
; $LastChangedRevision: 2056 $
; $URL $
;-

pro subGSE2GSM,TIMES,DATA_in,DATA_out



; get array sizes
count=SIZE(DATA_in[*,0],/N_ELEMENTS)
MESSAGE,/CONTINUE,'number of records: '+string(count)

DATA_out=dblarr(count,3)

tgsegsm_vect,TIMES[*].year,TIMES[*].doy,TIMES[*].hour,TIMES[*].min,double(TIMES[*].sec)+TIMES[*].fsec,DATA_in[*,0],DATA_in[*,1],DATA_in[*,2],xgsm,ygsm,zgsm

;for i=0L,count-1L do begin
;		;ctimpar,iyear,imonth,iday,ih,im,is
;		;This has to be changed to be faster!!!!!!!!!!!!!!!
;		ctimpar,TIMES[i].year,TIMES[i].month,TIMES[i].date,TIMES[i].hour,TIMES[i].min,double(TIMES[i].sec)+TIMES[i].fsec
;		tgsegsm,DATA_in[i,0],DATA_in[i,1],DATA_in[i,2],xgsm,ygsm,zgsm
		DATA_out[*,0]=xgsm
		DATA_out[*,1]=ygsm
		DATA_out[*,2]=zgsm
;endfor


;return,DATA_out
end


;#################################################

;+
;procedure: subGSM2GSE
;
;Purpose: transforms data from GSM to GSE
;
;INPUTS: TIMES as time_struct, DATA_in as nx3 array
;
;
;keywords:
;
;Example:
;
;
;Notes: under construction!!  will run faster in the near future!!
;
; $LastChangedBy: pcruce $
; $LastChangedDate: 2007-11-19 13:25:56 -0800 (Mon, 19 Nov 2007) $
; $LastChangedRevision: 2056 $
; $URL $
;-

pro subGSM2GSE,TIMES,DATA_in,DATA_out



; get array sizes
count=SIZE(DATA_in[*,0],/N_ELEMENTS)
MESSAGE,/CONTINUE,'number of records: ' + string(count)

DATA_out=dblarr(count,3)

tgsmgse_vect,TIMES[*].year,TIMES[*].doy,TIMES[*].hour,TIMES[*].min,double(TIMES[*].sec)+TIMES[*].fsec,DATA_in[*,0],DATA_in[*,1],DATA_in[*,2],xgse,ygse,zgse

;for i=0L,count-1L do begin
;		;ctimpar,iyear,imonth,iday,ih,im,is
;		ctimpar,TIMES[i].year,TIMES[i].month,TIMES[i].date,TIMES[i].hour,TIMES[i].min,double(TIMES[i].sec)+TIMES[i].fsec
;		tgsmgse,DATA_in[i,0],DATA_in[i,1],DATA_in[i,2],xgse,ygse,zgse
		DATA_out[*,0]=xgse
		DATA_out[*,1]=ygse
		DATA_out[*,2]=zgse
;endfor


;return,DATA_out
end




;#################################################

;+
;procedure: subGSM2SM
;
;Purpose: transforms data from GSM to SM
;
;INPUTS: TIMES as time_struct, DATA_in as nx3 array
;
;
;keywords:
;
;Example:
;
;
;Notes: under construction!!
;
; $LastChangedBy: pcruce $
; $LastChangedDate: 2007-11-19 13:25:56 -0800 (Mon, 19 Nov 2007) $
; $LastChangedRevision: 2056 $
; $URL $
;-

pro subGSM2SM,TIMES,DATA_in,DATA_out



; get array sizes
count=SIZE(DATA_in[*,0],/N_ELEMENTS)
MESSAGE,/CONTINUE,'number of records: ' + string(count)

DATA_out=dblarr(count,3)

tgsmsm_vect,TIMES[*].year,TIMES[*].doy,TIMES[*].hour,TIMES[*].min,double(TIMES[*].sec)+TIMES[*].fsec,DATA_in[*,0],DATA_in[*,1],DATA_in[*,2],xsm,ysm,zsm

;for i=0L,count-1L do begin
;		;ctimpar,iyear,imonth,iday,ih,im,is
;		ctimpar,TIMES[i].year,TIMES[i].month,TIMES[i].date,TIMES[i].hour,TIMES[i].min,double(TIMES[i].sec)+TIMES[i].fsec
;		tgsmgse,DATA_in[i,0],DATA_in[i,1],DATA_in[i,2],xgse,ygse,zgse
		DATA_out[*,0]=xsm
		DATA_out[*,1]=ysm
		DATA_out[*,2]=zsm
;endfor


;return,DATA_out
end




;#################################################

;+
;procedure: subSM2GSM
;
;Purpose: transforms data from SM to GSM
;
;INPUTS: TIMES as time_struct, DATA_in as nx3 array
;
;
;keywords:
;
;Example:
;
;
;Notes: under construction!!
;
; $LastChangedBy: pcruce $
; $LastChangedDate: 2007-11-19 13:25:56 -0800 (Mon, 19 Nov 2007) $
; $LastChangedRevision: 2056 $
; $URL $
;-

pro subSM2GSM,TIMES,DATA_in,DATA_out



; get array sizes
count=SIZE(DATA_in[*,0],/N_ELEMENTS)
MESSAGE,/CONTINUE,'number of records: '+string(count)

DATA_out=dblarr(count,3)

tsmgsm_vect,TIMES[*].year,TIMES[*].doy,TIMES[*].hour,TIMES[*].min,double(TIMES[*].sec)+TIMES[*].fsec,DATA_in[*,0],DATA_in[*,1],DATA_in[*,2],xgsm,ygsm,zgsm

;for i=0L,count-1L do begin
;		;ctimpar,iyear,imonth,iday,ih,im,is
;		ctimpar,TIMES[i].year,TIMES[i].month,TIMES[i].date,TIMES[i].hour,TIMES[i].min,double(TIMES[i].sec)+TIMES[i].fsec
;		tgsmgse,DATA_in[i,0],DATA_in[i,1],DATA_in[i,2],xgse,ygse,zgse
		DATA_out[*,0]=xgsm
		DATA_out[*,1]=ygsm
		DATA_out[*,2]=zgsm
;endfor


;return,DATA_out
end

 
;#################################################

;+
;procedure: subGEI2GEO
;
;Purpose: transforms data from GEI to GEO
;
;INPUTS: TIMES as time_struct, DATA_in as nx3 array
;
;
;keywords:
;
;Example:
;
;
;Notes: under construction!!
;
; $LastChangedBy: pcruce $
; $LastChangedDate: 2007-11-19 13:25:56 -0800 (Mon, 19 Nov 2007) $
; $LastChangedRevision: 2056 $
; $URL $
;-
pro subGEI2GEO,TIMES,DATA_in,DATA_out


      csundir_vect,TIMES.year,TIMES.doy,TIMES.hour,TIMES.min,TIMES.sec,gst,slong,srasn,sdecl,obliq

      sgst=sin(gst)
      cgst=cos(gst)				

      x_out = cgst*DATA_IN[*,0] + sgst*DATA_IN[*,1]

      y_out = -sgst*DATA_IN[*,0] + cgst*DATA_IN[*,1]

      z_out = DATA_IN[*,2]

      DATA_out = [[x_out],[y_out],[z_out]]


end


;#################################################

;+
;procedure: subGEO2GEI
;
;Purpose: transforms data from GEO to GEI
;
;INPUTS: TIMES as time_struct, DATA_in as nx3 array
;
;
;keywords:
;
;Example:
;
;
;Notes: under construction!!
;
; $LastChangedBy: pcruce $
; $LastChangedDate: 2007-11-19 13:25:56 -0800 (Mon, 19 Nov 2007) $
; $LastChangedRevision: 2056 $
; $URL $
;-
pro subGEO2GEI,TIMES,DATA_in,DATA_out

  csundir_vect,TIMES.year,TIMES.doy,TIMES.hour,TIMES.min,TIMES.sec,gst,slong,srasn,sdecl,obliq

      sgst=sin(gst)
      cgst=cos(gst)				

      x_out = cgst*DATA_IN[*,0] - sgst*DATA_IN[*,1]

      y_out = sgst*DATA_IN[*,0] + cgst*DATA_IN[*,1]

      z_out = DATA_IN[*,2]

      DATA_out = [[x_out],[y_out],[z_out]]


end



;#################################################

;+
;procedure: csundir_vect
;
;Purpose: calculates the direction of the sun
;         (vectorized version of csundir from ROCOTLIB by
;          Patrick Robert)
;
;INPUTS: integer time
;
;
;output :      gst      greenwich mean sideral time (radians)
;              slong    longitude along ecliptic (radians)
;              sra      right ascension (radians)
;              sdec     declination of the sun (radians)
;              obliq    inclination of Earth's axis (radians)
;
;
;
;Notes: under construction!!
;
; $LastChangedBy: pcruce $
; $LastChangedDate: 2007-11-19 13:25:56 -0800 (Mon, 19 Nov 2007) $
; $LastChangedRevision: 2056 $
; $URL $
;-
pro csundir_vect,iyear,idoy,ih,im,is,gst,slong,sra,sdec,obliq

;----------------------------------------------------------------------
; *   Class  : basic compute modules of Rocotlib Software
; *   Object : compute_sun_direction in GEI system
; *   Author : C.T. Russel, 1971, rev. P.Robert,1992,2001,2002
; *   IDL Ver: C. Guerin, CETP, 2004
;
; *   Comment: calculates four quantities in gei system necessary for
;              coordinate transformations dependent on sun position
;              (and, hence, on universal time and season)
;              Initial code from C.T. Russel, cosmic electro-dynamics,
;              v.2, 184-196, 1971.
;              Adaptation P.Robert, November 1992.
;              Revised and F90 compatibility, P. Robert June 2001.
;              Optimisation of DBLE computations and comments,
;              P. Robert, December 2002
;
; *   input  : iyear : year (1901-2099)
;              idoy : day of the year (1 for january 1)
;              ih,im,is : hours, minutes, seconds U.T.
;
; *   output : gst      greenwich mean sideral time (radians)
;              slong    longitude along ecliptic (radians)
;              sra      right ascension (radians)
;              sdec     declination of the sun (radians)
;              obliq    inclination of Earth's axis (radians)
;----------------------------------------------------------------------

;     double precision dj,fday

;      if(iyear lt 1901 or iyear gt 2099) then begin
;        message,/continue ,'*** Rocotlib/csundir: year = ',iyear
;        message,/continue ,'*** Rocotlib/csundir: year must be >1901 and <2099'
;        stop
;        endif

      pi= acos(-1.)
      pisd= pi/180.

; *** Julian day and greenwich mean sideral time

      fday=double(ih*3600.+im*60.+is)/86400.d
      jj=365L*long(iyear-1900)+fix((iyear-1901)/4)+idoy
      dj=double(jj) -0.5d + fday
      gst=float((279.690983d +0.9856473354d*dj +360.d*fday +180d) $
         mod 360d )*pisd

; *** longitude along ecliptic

      vl= float( (279.696678d +0.9856473354d*dj) mod 360d )
      t=float(dj/36525d)
      g=float( (358.475845d +0.985600267d*dj) mod  360d )*pisd
      slong=(vl+(1.91946 -0.004789*t)*sin(g) +0.020094*sin(2.*g))*pisd

; *** inclination of Earth's axis

      obliq=(23.45229 -0.0130125*t)*pisd
      sob=sin(obliq)
      cob=cos(obliq)

;     precession of declination (about 0.0056 deg.)

      pre= (0.0055686 - 0.025e-4*t)*pisd

; *** declination of the sun

      slp=slong -pre
      sind=sob*sin(slp)
      cosd=sqrt(1. -sind^2 )
      sc=sind/cosd
      sdec=atan(sc)

; *** right ascension of the sun

;     sra=pi -atan2((cob/sob)*sc, -cos(slp)/cosd)
      sra=pi -atan ((cob/sob)*sc, -cos(slp)/cosd)

      return
      end




;+
;procedure: tgeigse_vect
;
;Purpose: GEI to GSE transformation
;         (vectorized version of tgeigse from ROCOTLIB by
;          Patrick Robert)
;
;
;
;Notes: under construction!!
;
; $LastChangedBy: pcruce $
; $LastChangedDate: 2007-11-19 13:25:56 -0800 (Mon, 19 Nov 2007) $
; $LastChangedRevision: 2056 $
; $URL $
;-
      pro tgeigse_vect,iyear,idoy,ih,im,is,xgei,ygei,zgei,xgse,ygse,zgse

;----------------------------------------------------------------------
; *   Class  : transform modules of Rocotlib Software
; *   Object : transforms_gei_to_gse: GEI -> GSE  system
; *   Author : P. Robert, CRPE, 1992
; *   IDL Ver: C. Guerin, CETP, 2004
; *   Comment: terms of transformation matrix are given in common
;
; *   input  : xgei,ygei,zgei cartesian gei coordinates
; *   output : xgse,ygse,zgse cartesian gse coordinates
;----------------------------------------------------------------------


      csundir_vect,iyear,idoy,ih,im,is,gst,slong,srasn,sdecl,obliq

	  gs1=cos(srasn)*cos(sdecl)  ;
      gs2=sin(srasn)*cos(sdecl)  ;
      gs3=sin(sdecl)             ;

      ge1=  0.                   ;
      ge2= -sin(obliq)           ;
      ge3=  cos(obliq)           ;

      gegs1= ge2*gs3 - ge3*gs2    ;
      gegs2= ge3*gs1 - ge1*gs3    ;
      gegs3= ge1*gs2 - ge2*gs1    ;

      xgse=   gs1*xgei +   gs2*ygei +   gs3*zgei
      ygse= gegs1*xgei + gegs2*ygei + gegs3*zgei
      zgse=   ge1*xgei +   ge2*ygei +   ge3*zgei

      return
      end

;     XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX


;+
;procedure: tgsegei_vect
;
;Purpose: GSE to GEI transformation
;         (vectorized version of tgsegei from ROCOTLIB by
;          Patrick Robert)
;
;
;
;Notes: under construction!!
;
; $LastChangedBy: pcruce $
; $LastChangedDate: 2007-11-19 13:25:56 -0800 (Mon, 19 Nov 2007) $
; $LastChangedRevision: 2056 $
; $URL $
;-
      pro tgsegei_vect,iyear,idoy,ih,im,is,xgse,ygse,zgse,xgei,ygei,zgei

;----------------------------------------------------------------------
; *   Class  : transform modules of Rocotlib Software
; *   Object : transforms_gei_to_gse: GEI -> GSE  system
; *   Author : P. Robert, CRPE, 1992
; *   IDL Ver: C. Guerin, CETP, 2004
; *   Comment: terms of transformation matrix are given in common
;
; *   input  : xgei,ygei,zgei cartesian gei coordinates
; *   output : xgse,ygse,zgse cartesian gse coordinates
;----------------------------------------------------------------------


      csundir_vect,iyear,idoy,ih,im,is,gst,slong,srasn,sdecl,obliq

	  gs1=cos(srasn)*cos(sdecl)  ;
      gs2=sin(srasn)*cos(sdecl)  ;
      gs3=sin(sdecl)             ;

      ge1=  0.                   ;
      ge2= -sin(obliq)           ;
      ge3=  cos(obliq)           ;

      gegs1= ge2*gs3 - ge3*gs2    ;
      gegs2= ge3*gs1 - ge1*gs3    ;
      gegs3= ge1*gs2 - ge2*gs1    ;

      xgei= gs1*xgse + gegs1*ygse + ge1*zgse
      ygei= gs2*xgse + gegs2*ygse + ge2*zgse
      zgei= gs3*xgse + gegs3*ygse + ge3*zgse

      return
      end

;     XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX



;+
;procedure: tgsegsm_vect
;
;Purpose: GSE to GSM transformation
;         (vectorized version of tgsegsm from ROCOTLIB by
;          Patrick Robert)
;
;
;
;Notes: under construction!!
;
; $LastChangedBy: pcruce $
; $LastChangedDate: 2007-11-19 13:25:56 -0800 (Mon, 19 Nov 2007) $
; $LastChangedRevision: 2056 $
; $URL $
;-

	  pro tgsegsm_vect,iyear,idoy,ih,im,is,xgse,ygse,zgse,xgsm,ygsm,zgsm



	  cdipdir_vect,iyear,idoy,gd1,gd2,gd3

      ; ok from here on


      csundir_vect,iyear,idoy,ih,im,is,gst,slong,srasn,sdecl,obliq

      gs1=cos(srasn)*cos(sdecl)  ;tttttt
      gs2=sin(srasn)*cos(sdecl)  ;tttttt
      gs3=sin(sdecl)             ;tttttt

; *** sin and cos of GMST

      sgst=sin(gst)				;*
      cgst=cos(gst)				;*

; *** ecliptic pole in GEI system

      ge1=  0.                   ;*
      ge2= -sin(obliq)           ;*
      ge3=  cos(obliq)           ;*



; *** dipole direction in GEI system

      gm1= gd1*cgst - gd2*sgst   ;* gd1 ->from internal field
      gm2= gd1*sgst + gd2*cgst   ;* gd2 ->from internal field
      gm3= gd3                   ;* gd3 ->from internal field


	  ; *** cross product MxS in GEI system

      gmgs1= gm2*gs3 - gm3*gs2    ;*
      gmgs2= gm3*gs1 - gm1*gs3    ;*
      gmgs3= gm1*gs2 - gm2*gs1    ;*

      rgmgs=sqrt(gmgs1^2 + gmgs2^2 + gmgs3^2)    ;*


	  cdze= (ge1*gm1   + ge2*gm2   + ge3*gm3)/rgmgs
      sdze= (ge1*gmgs1 + ge2*gmgs2 + ge3*gmgs3)/rgmgs
      epsi=1.e-6
;      if(abs(sdze^2 +cdze^2-1.) gt epsi) then begin
;         message,/continue, '*** Rogralib error 3'
;         stop
;      endif


	  xgsm= xgse
      ygsm=  cdze*ygse + sdze*zgse
      zgsm= -sdze*ygse + cdze*zgse


      END




;+
;procedure: tgsmgse_vect
;
;Purpose: GSM to GSE transformation
;         (vectorized version of tgsmgse from ROCOTLIB by
;          Patrick Robert)
;
;
;
;Notes: under construction!!
;
; $LastChangedBy: pcruce $
; $LastChangedDate: 2007-11-19 13:25:56 -0800 (Mon, 19 Nov 2007) $
; $LastChangedRevision: 2056 $
; $URL $
;-
pro tgsmgse_vect,iyear,idoy,ih,im,is,xgsm,ygsm,zgsm,xgse,ygse,zgse



	  cdipdir_vect,iyear,idoy,gd1,gd2,gd3

      ; ok from here on

      csundir_vect,iyear,idoy,ih,im,is,gst,slong,srasn,sdecl,obliq

      gs1=cos(srasn)*cos(sdecl)  ;tttttt
      gs2=sin(srasn)*cos(sdecl)  ;tttttt
      gs3=sin(sdecl)             ;tttttt

; *** sin and cos of GMST

      sgst=sin(gst)				;*
      cgst=cos(gst)				;*

; *** ecliptic pole in GEI system

      ge1=  0.                   ;*
      ge2= -sin(obliq)           ;*
      ge3=  cos(obliq)           ;*



; *** dipole direction in GEI system

      gm1= gd1*cgst - gd2*sgst   ;* gd1 ->from internal field
      gm2= gd1*sgst + gd2*cgst   ;* gd2 ->from internal field
      gm3= gd3                   ;* gd3 ->from internal field


	  ; *** cross product MxS in GEI system

      gmgs1= gm2*gs3 - gm3*gs2    ;*
      gmgs2= gm3*gs1 - gm1*gs3    ;*
      gmgs3= gm1*gs2 - gm2*gs1    ;*

      rgmgs=sqrt(gmgs1^2 + gmgs2^2 + gmgs3^2)    ;*


	  cdze= (ge1*gm1   + ge2*gm2   + ge3*gm3)/rgmgs
      sdze= (ge1*gmgs1 + ge2*gmgs2 + ge3*gmgs3)/rgmgs
      epsi=1.e-6
;      if(abs(sdze^2 +cdze^2-1.) gt epsi) then begin
;         message,/continue, '*** Rogralib error 3'
;         stop
;      endif


      xgse= xgsm
      ygse= cdze*ygsm - sdze*zgsm
      zgse= sdze*ygsm + cdze*zgsm


      END




;+
;procedure: tgsmsm_vect
;
;Purpose: GSM to SM transformation
;         (vectorized version of tgsmsma from ROCOTLIB by
;          Patrick Robert)
;
;
;
;Notes: under construction!!
;
; $LastChangedBy: pcruce $
; $LastChangedDate: 2007-11-19 13:25:56 -0800 (Mon, 19 Nov 2007) $
; $LastChangedRevision: 2056 $
; $URL $
;-
pro tgsmsm_vect,iyear,idoy,ih,im,is,xgsm,ygsm,zgsm,xsm,ysm,zsm



	  cdipdir_vect,iyear,idoy,gd1,gd2,gd3

      ; ok from here on

      csundir_vect,iyear,idoy,ih,im,is,gst,slong,srasn,sdecl,obliq

      gs1=cos(srasn)*cos(sdecl)  ;tttttt
      gs2=sin(srasn)*cos(sdecl)  ;tttttt
      gs3=sin(sdecl)             ;tttttt

; *** sin and cos of GMST

      sgst=sin(gst)
      cgst=cos(gst)

; *** direction of the sun in GEO system

      ps1=  gs1*cgst + gs2*sgst
      ps2= -gs1*sgst + gs2*cgst
      ps3=  gs3

; *** computation of mu angle

      smu= ps1*gd1 + ps2*gd2 + ps3*gd3
      cmu= sqrt(1.-smu*smu)


; do the transformation
	  xsm= cmu*xgsm - smu*zgsm
      ysm= ygsm
      zsm= smu*xgsm + cmu*zgsm


      END





;+
;procedure: tsmgsm_vect
;
;Purpose: SM to GSM transformation
;         (vectorized version of tsmagsm from ROCOTLIB by
;          Patrick Robert)
;
;
;
;Notes: under construction!!
;
; $LastChangedBy: pcruce $
; $LastChangedDate: 2007-11-19 13:25:56 -0800 (Mon, 19 Nov 2007) $
; $LastChangedRevision: 2056 $
; $URL $
;-
pro tsmgsm_vect,iyear,idoy,ih,im,is,xsm,ysm,zsm,xgsm,ygsm,zgsm



	  cdipdir_vect,iyear,idoy,gd1,gd2,gd3

      ; ok from here on

      csundir_vect,iyear,idoy,ih,im,is,gst,slong,srasn,sdecl,obliq

      gs1=cos(srasn)*cos(sdecl)  ;tttttt
      gs2=sin(srasn)*cos(sdecl)  ;tttttt
      gs3=sin(sdecl)             ;tttttt

; *** sin and cos of GMST

      sgst=sin(gst)
      cgst=cos(gst)

; *** direction of the sun in GEO system

      ps1=  gs1*cgst + gs2*sgst
      ps2= -gs1*sgst + gs2*cgst
      ps3=  gs3

; *** computation of mu angle

      smu= ps1*gd1 + ps2*gd2 + ps3*gd3
      cmu= sqrt(1.-smu*smu)


; do the transformation
	  xgsm=  cmu*xsm + smu*zsm
      ygsm=  ysm
      zgsm= -smu*xsm + cmu*zsm


      END






;+
;procedure: cdipdir_vect
;
;Purpose: calls cdipdir from ROCOTLIB in a vectorized environment
;
;
;
;Notes: under construction!!
;
; $LastChangedBy: pcruce $
; $LastChangedDate: 2007-11-19 13:25:56 -0800 (Mon, 19 Nov 2007) $
; $LastChangedRevision: 2056 $
;
; faster algorithm (for loop across all points avoided) Hannes 05/25/2007
;
; $URL $
;-
PRO cdipdir_vect,iyear,idoy,gd1,gd2,gd3


		n=n_elements(iyear)

		IF (n eq 1) THEN BEGIN
			cdipdir,iyear,idoy,gd1,gd2,gd3
			RETURN
		ENDIF

		gd1=fltarr(n)
		gd2=fltarr(n)
		gd3=fltarr(n)

;		t1 = SYSTIME(1)
;       faster coding!!

		;get the date changes
		iDiff    =abs(TS_DIFF(idoy, 1))+abs(TS_DIFF(iyear, 1))
		iDiff(n-1L) =1 ;always calculate at last element
		noZeros  =WHERE( iDiff NE 0)

		nn=n_elements(noZeros)

		;loop only through the date changes (usually only once for day-files)
		indexStart=0L
		for ii = 0L,nn-1L do begin
			i=noZeros(ii)
			cdipdir,iyear(i),idoy(i),gd1i,gd2i,gd3i
			gd1(indexStart:i)=gd1i
			gd2(indexStart:i)=gd2i
			gd3(indexStart:i)=gd3i
			indexStart=i+1L
		ENDFOR


;		t2 = SYSTIME(1)

;		t3 = SYSTIME(1)
;		cdipdir,iyear(0),idoy(0),gd1i,gd2i,gd3i


		;still a bit of a bottle neck
;		iyearPrev=iyear(0)
;		idoyPrev = idoy(0)
;		indexStart=0L
;
;		for i=1L,n-1L do begin
;
;
;			IF ( (idoy(i) ne idoyPrev) || (iyear(i) ne iyearPrev)) then begin
;				gd1(indexStart:i-1L)=gd1i
;				gd2(indexStart:i-1L)=gd2i
;				gd3(indexStart:i-1L)=gd3i
;				cdipdir,iyear(i),idoy(i),gd1i,gd2i,gd3i
;				iyearPrev=iyear(i)
;				idoyPrev = idoy(i)
;				indexStart=i
;			ENDIF
;
;			IF (i eq n-1L) THEN BEGIN
;				gd1(indexStart:i)=gd1i
;				gd2(indexStart:i)=gd2i
;				gd3(indexStart:i)=gd3i
;				break
;			ENDIF
;
;		ENDFOR
;		t4 = SYSTIME(1)
;
;
;		MESSAGE,/CONTINUE,'Time compare: New:'
;		MESSAGE,/CONTINUE,t2-t1
;		MESSAGE,/CONTINUE,'Time compare: Old:'
;		MESSAGE,/CONTINUE,t4-t3
;
	  END


;+
;procedure: cdipdir
;
;Purpose: cdipdir from ROCOTLIB. direction of Earth's magnetic axis in GEO
;
;
;
;Notes: under construction!!
;
; $LastChangedBy: pcruce $
; $LastChangedDate: 2007-11-19 13:25:56 -0800 (Mon, 19 Nov 2007) $
; $LastChangedRevision: 2056 $
; $URL $
;-
      pro cdipdir,iyear,idoy,d1,d2,d3
;----------------------------------------------------------------------
;
; *   Class  : basic compute modules of Rocotlib Software
; *   Object : compute_dipole_direction in GEO system
; *   Author : P. Robert, CRPE, 1992 +Tsyganenko 87 model
; *   IDL Ver: C. Guerin, CETP, 200sion v2.0 P. Robert, nov. 2006e
;
; *   Comment: Compute geodipole axis direction from International
;              Geomagnetic Reference Field (IGRF) models for time
;              interval 1965 to 2005. For time out of interval,
;              computation is made for nearest boundary.
;              Code extracted from geopack, N.A. Tsyganenko, Jan. 5 2001
;              Revised P.R., November 23 2006, full compatible with last
;              revision of geopacklib of May 3 2005.
;              (see http://www.ngdc.noaa.gov/IAGA/vmod/igrf.html)
;
; *   input  :  iyear (1965 - 2005), idoy= day of year (1/1=1)
; *   output :  d1,d2,d3  cartesian dipole components in GEO system
;
; ----------------------------------------------------------------------


;     Coefficients of the igrf field model, calculated for a given year
;     and day from their standard epoch values.

;     dimension g(105),h(105)
      g=FLTARR(105)
      h=FLTARR(105)
      nloop= 104
;

;     dimension g65(105),h65(105),g70(105),h70(105),g75(105),h75(105), $
;      g80(105),h80(105),g85(105),h85(105),g90(105),h90(105),g95(105), $
;      h95(105),g00(105),h00(105),g05(105),h05(105),dg05(45),dh05(45)

      g65=FLTARR(105)
      h65=FLTARR(105)
      g70=FLTARR(105)
      h70=FLTARR(105)
      g75=FLTARR(105)
      h75=FLTARR(105)

      g80=FLTARR(105)
      h80=FLTARR(105)
      g85=FLTARR(105)
      h85=FLTARR(105)
      g90=FLTARR(105)
      h90=FLTARR(105)
      g95=FLTARR(105)

      h95=FLTARR(105)
      g00=FLTARR(105)
      h00=FLTARR(105)
      g05=FLTARR(105)
      h05=FLTARR(105)
      dg05=FLTARR(45)
      dh05=FLTARR(45)

      g65= [0.,-30334.,-2119.,-1662.,2997.,1594.,1297.,-2038.,1292., $
      856.,957.,804.,479.,-390.,252.,-219.,358.,254.,-31.,-157.,-62.,  $
      45.,61.,8.,-228.,4.,1.,-111.,75.,-57.,4.,13.,-26.,-6.,13.,1.,13., $
      5.,-4.,-14.,0.,8.,-1.,11.,4.,8.,10.,2.,-13.,10.,-1.,-1.,5.,1.,-2., $
      -2.,-3.,2.,-5.,-2.,4.,4.,0.,2.,2.,0., $
      0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0., $
      0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.]

      h65= [0.,0.,5776.,0.,-2016.,114.,0.,-404.,240.,-165.,0.,148., $
      -269.,13.,-269.,0.,19.,128.,-126.,-97.,81.,0.,-11.,100.,68.,-32., $
      -8.,-7.,0.,-61.,-27.,-2.,6.,26.,-23.,-12.,0.,7.,-12.,9.,-16.,4., $
      24.,-3.,-17.,0.,-22.,15.,7.,-4.,-5.,10.,10.,-4.,1.,0.,2.,1.,2., $
      6.,-4.,0.,-2.,3.,0.,-6.,  $
      0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0., $
      0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.]

      g70= [0.,-30220.,-2068.,-1781.,3000.,1611.,1287.,-2091.,1278., $
      838.,952.,800.,461.,-395.,234.,-216.,359.,262.,-42.,-160.,-56., $
      43.,64.,15.,-212.,2.,3.,-112.,72.,-57.,1.,14.,-22.,-2.,13.,-2., $
      14.,6.,-2.,-13.,-3.,5.,0.,11.,3.,8.,10.,2.,-12.,10.,-1.,0.,3., $
      1.,-1.,-3.,-3.,2.,-5.,-1.,6.,4.,1.,0.,3.,-1.,  $
      0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0., $
      0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.]

      h70= [0.,0.,5737.,0.,-2047.,25.,0.,-366.,251.,-196.,0.,167., $
      -266.,26.,-279.,0.,26.,139.,-139.,-91.,83.,0.,-12.,100.,72.,-37., $
      -6.,1.,0.,-70.,-27.,-4.,8.,23.,-23.,-11.,0.,7.,-15.,6.,-17.,6., $
      21.,-6.,-16.,0.,-21.,16.,6.,-4.,-5.,10.,11.,-2.,1.,0.,1.,1.,3., $
      4.,-4.,0.,-1.,3.,1.,-4.,   $
      0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0., $
      0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.]

      g75= [0.,-30100.,-2013.,-1902.,3010.,1632.,1276.,-2144.,1260., $
      830.,946.,791.,438.,-405.,216.,-218.,356.,264.,-59.,-159.,-49., $
      45.,66.,28.,-198.,1.,6.,-111.,71.,-56.,1.,16.,-14.,0.,12.,-5., $
      14.,6.,-1.,-12.,-8.,4.,0.,10.,1.,7.,10.,2.,-12.,10.,-1.,-1.,4., $
      1.,-2.,-3.,-3.,2.,-5.,-2.,5.,4.,1.,0.,3.,-1.,  $
      0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0., $
      0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.]

      h75= [0.,0.,5675.,0.,-2067.,-68.,0.,-333.,262.,-223.,0.,191., $
      -265.,39.,-288.,0.,31.,148.,-152.,-83.,88.,0.,-13.,99.,75.,-41., $
      -4.,11.,0.,-77.,-26.,-5.,10.,22.,-23.,-12.,0.,6.,-16.,4.,-19.,6., $
      18.,-10.,-17.,0.,-21.,16.,7.,-4.,-5.,10.,11.,-3.,1.,0.,1.,1.,3., $
      4.,-4.,-1.,-1.,3.,1.,-5.,  $
      0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0., $
      0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.]

      g80= [0.,-29992.,-1956.,-1997.,3027.,1663.,1281.,-2180.,1251., $
      833.,938.,782.,398.,-419.,199.,-218.,357.,261.,-74.,-162.,-48., $
      48.,66.,42.,-192.,4.,14.,-108.,72.,-59.,2.,21.,-12.,1.,11.,-2., $
      18.,6.,0.,-11.,-7.,4.,3.,6.,-1.,5.,10.,1.,-12.,9.,-3.,-1.,7.,2., $
      -5.,-4.,-4.,2.,-5.,-2.,5.,3.,1.,2.,3.,0.,  $
      0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0., $
      0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.]

      h80= [0.,0.,5604.,0.,-2129.,-200.,0.,-336.,271.,-252.,0.,212., $
      -257.,53.,-297.,0.,46.,150.,-151.,-78.,92.,0.,-15.,93.,71.,-43., $
      -2.,17.,0.,-82.,-27.,-5.,16.,18.,-23.,-10.,0.,7.,-18.,4.,-22.,9., $
      16.,-13.,-15.,0.,-21.,16.,9.,-5.,-6.,9.,10.,-6.,2.,0.,1.,0.,3., $
      6.,-4.,0.,-1.,4.,0.,-6.,  $
      0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0., $
      0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.]

      h75= [0.,0.,5675.,0.,-2067.,-68.,0.,-333.,262.,-223.,0.,191., $
      -265.,39.,-288.,0.,31.,148.,-152.,-83.,88.,0.,-13.,99.,75.,-41., $
      -4.,11.,0.,-77.,-26.,-5.,10.,22.,-23.,-12.,0.,6.,-16.,4.,-19.,6., $
      18.,-10.,-17.,0.,-21.,16.,7.,-4.,-5.,10.,11.,-3.,1.,0.,1.,1.,3., $
      4.,-4.,-1.,-1.,3.,1.,-5.,  $
      0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0., $
      0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.]

      g85= [0.,-29873.,-1905.,-2072.,3044.,1687.,1296.,-2208.,1247., $
      829.,936.,  780.,361.,-424.,170.,-214.,355.,253.,-93.,-164.,-46., $
      53.,65.,51.,-185.,4.,16.,-102.,74.,-62.,3.,24.,-6.,4.,10.,0.,21., $
      6.,0.,-11.,-9.,4.,4.,4.,-4.,5.,10.,1.,-12.,9.,-3.,-1.,7.,1.,-5., $
      -4.,-4.,3.,-5.,-2.,5.,3.,1.,2.,3.,0.,  $
      0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0., $
      0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.]

      h75= [0.,0.,5675.,0.,-2067.,-68.,0.,-333.,262.,-223.,0.,191., $
      -265.,39.,-288.,0.,31.,148.,-152.,-83.,88.,0.,-13.,99.,75.,-41., $
      -4.,11.,0.,-77.,-26.,-5.,10.,22.,-23.,-12.,0.,6.,-16.,4.,-19.,6., $
      18.,-10.,-17.,0.,-21.,16.,7.,-4.,-5.,10.,11.,-3.,1.,0.,1.,1.,3., $
      4.,-4.,-1.,-1.,3.,1.,-5.,  $
      0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0., $
      0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.]

      h85= [0.,0.,5500.,0.,-2197.,-306.,0.,-310.,284.,-297.,0.,232., $
      -249.,69.,-297.,0.,47.,150.,-154.,-75.,95.,0.,-16.,88.,69.,-48., $
      -1.,21.,0.,-83.,-27.,-2.,20.,17.,-23.,-7.,0.,8.,-19.,5.,-23.,11., $
      14.,-15.,-11.,0.,-21.,15.,9.,-6.,-6.,9.,9.,-7.,2.,0.,1.,0.,3., $
      6.,-4.,0.,-1.,4.,0.,-6.,  $
      0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0., $
      0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.]

      h75= [0.,0.,5675.,0.,-2067.,-68.,0.,-333.,262.,-223.,0.,191., $
      -265.,39.,-288.,0.,31.,148.,-152.,-83.,88.,0.,-13.,99.,75.,-41., $
      -4.,11.,0.,-77.,-26.,-5.,10.,22.,-23.,-12.,0.,6.,-16.,4.,-19.,6., $
      18.,-10.,-17.,0.,-21.,16.,7.,-4.,-5.,10.,11.,-3.,1.,0.,1.,1.,3., $
      4.,-4.,-1.,-1.,3.,1.,-5.,  $
      0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0., $
      0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.]

      g90= [  0., -29775.,  -1848.,  -2131.,   3059.,   1686.,   1314., $
          -2239.,   1248.,    802.,    939.,    780.,    325.,   -423., $
            141.,   -214.,    353.,    245.,   -109.,   -165.,    -36., $
             61.,     65.,     59.,   -178.,      3.,     18.,    -96., $
             77.,    -64.,      2.,     26.,     -1.,      5.,      9., $
              0.,     23.,      5.,     -1.,    -10.,    -12.,      3., $
              4.,      2.,     -6.,      4.,      9.,      1.,    -12., $
              9.,     -4.,     -2.,      7.,      1.,     -6.,     -3., $
             -4.,      2.,     -5.,     -2.,      4.,      3.,      1., $
              3.,      3.,      0.,  $
      0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0., $
      0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.]

      h90= [  0.,      0.,   5406.,      0.,  -2279.,   -373.,      0., $
           -284.,    293.,   -352.,      0.,    247.,   -240.,     84., $
           -299.,      0.,     46.,    154.,   -153.,    -69.,     97., $
              0.,    -16.,     82.,     69.,    -52.,      1.,     24., $
              0.,    -80.,    -26.,      0.,     21.,     17.,    -23., $
             -4.,      0.,     10.,    -19.,      6.,    -22.,     12., $
             12.,    -16.,    -10.,      0.,    -20.,     15.,     11., $
             -7.,     -7.,      9.,      8.,     -7.,      2.,      0., $
              2.,      1.,      3.,      6.,     -4.,      0.,     -2., $
              3.,     -1.,     -6.,   $
      0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0., $
      0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.]

      g95= [  0., -29692.,  -1784.,  -2200.,   3070.,   1681.,   1335., $
          -2267.,   1249.,    759.,    940.,    780.,    290.,   -418., $
            122.,   -214.,    352.,    235.,   -118.,   -166.,    -17., $
             68.,     67.,     68.,   -170.,     -1.,     19.,    -93., $
             77.,    -72.,      1.,     28.,      5.,      4.,      8., $
             -2.,     25.,      6.,     -6.,     -9.,    -14.,      9., $
              6.,     -5.,     -7.,      4.,      9.,      3.,    -10., $
              8.,     -8.,     -1.,     10.,     -2.,     -8.,     -3., $
             -6.,      2.,     -4.,     -1.,      4.,      2.,      2., $
              5.,      1.,      0.,      $
      0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0., $
      0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.]

      h95= [  0.,      0.,   5306.,      0.,  -2366.,   -413.,      0., $
           -262.,    302.,   -427.,      0.,    262.,   -236.,     97., $
           -306.,      0.,     46.,    165.,   -143.,    -55.,    107., $
              0.,    -17.,     72.,     67.,    -58.,      1.,     36., $
              0.,    -69.,    -25.,      4.,     24.,     17.,    -24., $
             -6.,      0.,     11.,    -21.,      8.,    -23.,     15., $
             11.,    -16.,     -4.,      0.,    -20.,     15.,     12., $
             -6.,     -8.,      8.,      5.,     -8.,      3.,      0., $
              1.,      0.,      4.,      5.,     -5.,     -1.,     -2., $
              1.,     -2.,     -7.,    $
      0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0., $
      0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.]

      g00= [  0.,-29619.4, -1728.2, -2267.7,  3068.4,  1670.9,  1339.6, $
          -2288.,  1252.1,   714.5,   932.3,   786.8,    250.,   -403., $
           111.3,  -218.8,   351.4,   222.3,  -130.4,  -168.6,   -12.9, $
            72.3,    68.2,    74.2,  -160.9,    -5.9,    16.9,   -90.4, $
            79.0,   -74.0,      0.,    33.3,     9.1,     6.9,     7.3, $
            -1.2,    24.4,     6.6,    -9.2,    -7.9,   -16.6,     9.1, $
             7.0,    -7.9,     -7.,      5.,     9.4,      3.,   - 8.4, $
             6.3,    -8.9,    -1.5,     9.3,    -4.3,    -8.2,    -2.6, $
             -6.,     1.7,    -3.1,    -0.5,     3.7,      1.,      2., $
             4.2,     0.3,    -1.1,     2.7,    -1.7,    -1.9,     1.5, $
            -0.1,     0.1,    -0.7,     0.7,     1.7,     0.1,     1.2, $
             4.0,    -2.2,    -0.3,     0.2,     0.9,    -0.2,     0.9, $
            -0.5,     0.3,    -0.3,    -0.4,    -0.1,    -0.2,    -0.4, $
            -0.2,    -0.9,     0.3,     0.1,    -0.4,     1.3,    -0.4, $
             0.7,    -0.4,     0.3,    -0.1,     0.4,      0.,     0.1]


      h00= [  0.,      0.,  5186.1,      0., -2481.6,  -458.0,      0., $
          -227.6,   293.4,  -491.1,      0.,   272.6,  -231.9,   119.8, $
          -303.8,      0.,    43.8,   171.9,  -133.1,   -39.3,   106.3, $
              0.,   -17.4,    63.7,    65.1,   -61.2,     0.7,    43.8, $
              0.,   -64.6,   -24.2,     6.2,     24.,    14.8,   -25.4, $
            -5.8,     0.0,    11.9,   -21.5,     8.5,   -21.5,    15.5, $
             8.9,   -14.9,    -2.1,     0.0,   -19.7,    13.4,    12.5, $
            -6.2,    -8.4,     8.4,     3.8,    -8.2,     4.8,     0.0, $
             1.7,     0.0,     4.0,     4.9,    -5.9,    -1.2,    -2.9, $
             0.2,    -2.2,    -7.4,     0.0,     0.1,     1.3,    -0.9, $
            -2.6,     0.9,    -0.7,    -2.8,    -0.9,    -1.2,    -1.9, $
            -0.9,     0.0,    -0.4,     0.3,     2.5,    -2.6,     0.7, $
             0.3,     0.0,     0.0,     0.3,    -0.9,    -0.4,     0.8, $
             0.0,    -0.9,     0.2,     1.8,    -0.4,    -1.0,    -0.1, $
             0.7,     0.3,     0.6,     0.3,    -0.2,    -0.5,    -0.9]


      g05= [  0.,-29556.8, -1671.8, -2340.5,   3047.,  1656.9,  1335.7, $
         -2305.3,  1246.8,   674.4,   919.8,   798.2,   211.5,  -379.5, $
           100.2,  -227.6,   354.4,   208.8,  -136.6,  -168.3,   -14.1, $
            72.9,    69.6,    76.6,  -151.1,   -15.0,    14.7,   -86.4, $
            79.8,   -74.4,    -1.4,    38.6,    12.3,     9.4,     5.5, $
             2.0,    24.8,     7.7,   -11.4,    -6.8,   -18.0,    10.0, $
             9.4,   -11.4,    -5.0,     5.6,     9.8,     3.6,    -7.0, $
             5.0,   -10.8,    -1.3,     8.7,    -6.7,    -9.2,    -2.2, $
            -6.3,     1.6,    -2.5,    -0.1,     3.0,     0.3,     2.1, $
             3.9,    -0.1,    -2.2,     2.9,    -1.6,    -1.7,     1.5, $
            -0.2,     0.2,    -0.7,     0.5,     1.8,     0.1,     1.0, $
             4.1,    -2.2,    -0.3,     0.3,     0.9,    -0.4,     1.0, $
            -0.4,     0.5,    -0.3,    -0.4,     0.0,    -0.4,     0.0, $
            -0.2,    -0.9,     0.3,     0.3,    -0.4,     1.2,    -0.4, $
             0.7,    -0.3,     0.4,    -0.1,     0.4,    -0.1,    -0.3]

      h05= [  0.,     0.0,  5080.0,     0.0, -2594.9,  -516.7,     0.0, $
          -200.4,   269.3,  -524.5,     0.0,   281.4,  -225.8,   145.7, $
          -304.7,     0.0,    42.7,   179.8,  -123.0,   -19.5,   103.6, $
             0.0,   -20.2,    54.7,    63.7,   -63.4,     0.0,    50.3, $
             0.0,   -61.4,   -22.5,     6.9,    25.4,    10.9,   -26.4, $
            -4.8,     0.0,    11.2,   -21.0,     9.7,   -19.8,    16.1, $
             7.7,   -12.8,    -0.1,     0.0,   -20.1,    12.9,    12.7, $
            -6.7,    -8.1,     8.1,     2.9,    -7.9,     5.9,     0.0, $
             2.4,     0.2,     4.4,     4.7,    -6.5,    -1.0,    -3.4, $
            -0.9,    -2.3,    -8.0,     0.0,     0.3,     1.4,    -0.7, $
            -2.4,     0.9,    -0.6,    -2.7,    -1.0,    -1.5,    -2.0, $
            -1.4,     0.0,    -0.5,     0.3,     2.3,    -2.7,     0.6, $
             0.4,     0.0,     0.0,     0.3,    -0.8,    -0.4,     1.0, $
             0.0,    -0.7,     0.3,     1.7,    -0.5,    -1.0,     0.0, $
             0.7,     0.2,     0.6,     0.4,    -0.2,    -0.5,    -1.0]

      dg05= [  0.0,   8.8,    10.8,   -15.0,    -6.9,    -1.0,    -0.3, $
              -3.1,  -0.9,    -6.8,    -2.5,     2.8,    -7.1,     5.9, $
              -3.2,  -2.6,     0.4,    -3.0,    -1.2,     0.2,    -0.6, $
              -0.8,   0.2,    -0.2,     2.1,    -2.1,    -0.4,     1.3, $
              -0.4,   0.0,    -0.2,     1.1,     0.6,     0.4,    -0.5, $
               0.9,  -0.2,     0.2,    -0.2,     0.2,    -0.2,     0.2, $
               0.5,  -0.7,     0.5]

      dh05= [  0.0,   0.0,   -21.3,     0.0,   -23.3,   -14.0,     0.0, $
               5.4,  -6.5,    -2.0,     0.0,     2.0,     1.8,     5.6, $
               0.0,   0.0,     0.1,     1.8,     2.0,     4.5,    -1.0, $
               0.0,  -0.4,    -1.9,    -0.4,    -0.4,    -0.2,     0.9, $
               0.0,   0.8,     0.4,     0.1,     0.2,    -0.9,    -0.3, $
               0.3,   0.0,    -0.2,     0.2,     0.2,     0.4,     0.2, $
              -0.3,   0.5,     0.4]


;     save iy,id,ipr

      if N_ELEMENTS(iy)  EQ 0 THEN iy=-1
      if N_ELEMENTS(id)  EQ 0 THEN id=-1
      if N_ELEMENTS(ipr) EQ 0 THEN ipr=-1

; ----------------------------------------------------------------------

     f10="(' * ROCOTLIB/cdipdir: Warning! year=',i4.4," + $
        "'   dipole direction can be computed between 1965-2010.',"+ $
        "'   It will be computed for year ',i4.4)"

; *** Computation are not done if date is the same as previous call

      if(iyear EQ iy AND idoy EQ id) then return

      iy=iyear
      id=idoy
      iday=idoy

; *** Check date interval of validity

;     we are restricted by the interval 1965-2010, for which the igrf
;     coefficients are known;
;     if iyear is outside this interval, then the subroutine uses the
;     nearest limiting value and prints a warning:

      if(iy LT 1965) then BEGIN
                    iy=1965
                    if(ipr NE 1) then print, format=f10, iyear, iy
                    ipr=1
                    endif

      if(iy GT 2010) then BEGIN
                    iy=2010
                    if(ipr NE 1) then print, format=f10, iyear, iy
                    ipr=1
                    endif

; *** Starting computations

      if (iy LT 1970) then goto, G50      ;interpolate between 1965 - 1970
      if (iy LT 1975) then goto, G60      ;interpolate between 1970 - 1975
      if (iy LT 1980) then goto, G70      ;interpolate between 1975 - 1980
      if (iy LT 1985) then goto, G80      ;interpolate between 1980 - 1985
      if (iy LT 1990) then goto, G90      ;interpolate between 1985 - 1990
      if (iy LT 1995) then goto, G100     ;interpolate between 1990 - 1995
      if (iy LT 2000) then goto, G110     ;interpolate between 1995 - 2000
      if (iy LT 2005) then goto, G120     ;interpolate between 2000 - 2005

;     extrapolate beyond 2005:


      dt=float(iy)+float(iday-1)/365.25 -2005.
      for n=0, nloop DO BEGIN
        g(n)=g05(n)
        h(n)=h05(n)
;        if (n.gt.45) goto 40
        if (n GT 44) then goto, G40
        g(n)=g(n)+dg05(n)*dt
        h(n)=h(n)+dh05(n)*dt
 G40:
      endfor
      goto, G300

;     interpolate betweeen 1965 - 1970:

 G50: f2=(float(iy)+float(iday-1)/365.25 -1965)/5.
      f1=1.-f2
      for n=0, nloop DO BEGIN
        g(n)=g65(n)*f1+g70(n)*f2
        h(n)=h65(n)*f1+h70(n)*f2
      endfor
      goto, G300

;     interpolate between 1970 - 1975:

 G60: f2=(float(iy)+float(iday-1)/365.25 -1970)/5.
      f1=1.-f2
      for n=0, nloop DO BEGIN
        g(n)=g70(n)*f1+g75(n)*f2
        h(n)=h70(n)*f1+h75(n)*f2
      endfor
      goto, G300

;     interpolate between 1975 - 1980:

 G70: f2=(float(iy)+float(iday-1)/365.25 -1975)/5.
      f1=1.-f2
      for n=0, nloop DO BEGIN
        g(n)=g75(n)*f1+g80(n)*f2
        h(n)=h75(n)*f1+h80(n)*f2
      endfor
      goto, G300

;     interpolate between 1980 - 1985:

 G80: f2=(float(iy)+float(iday-1)/365.25 -1980)/5.
      f1=1.-f2
      for n=0, nloop DO BEGIN
        g(n)=g80(n)*f1+g85(n)*f2
        h(n)=h80(n)*f1+h85(n)*f2
      endfor
      goto, G300

;     interpolate between 1985 - 1990:

 G90: f2=(float(iy)+float(iday-1)/365.25 -1985)/5.
      f1=1.-f2
      for n=0, nloop DO BEGIN
        g(n)=g85(n)*f1+g90(n)*f2
        h(n)=h85(n)*f1+h90(n)*f2
      endfor
      goto, G300

;     interpolate between 1990 - 1995:

G100: f2=(float(iy)+float(iday-1)/365.25 -1990)/5.
      f1=1.-f2
      for n=0, nloop DO BEGIN
        g(n)=g90(n)*f1+g95(n)*f2
        h(n)=h90(n)*f1+h95(n)*f2
      endfor
      goto, G300

;     interpolate between 1995 - 2000:

G110: f2=(float(iy)+float(iday-1)/365.25 -1995)/5.
      f1=1.-f2
      for n=0, nloop DO BEGIN
;     the 2000 coefficients (g00) go through the order 13, not 10
        g(n)=g95(n)*f1+g00(n)*f2
        h(n)=h95(n)*f1+h00(n)*f2
      endfor
      goto, G300

;     interpolate between 2000 - 2005:

G120: f2=(float(iy)+float(iday-1)/365.25 -2000)/5.
      f1=1.-f2
      for n=0, nloop DO BEGIN
        g(n)=g00(n)*f1+g05(n)*f2
        h(n)=h00(n)*f1+h05(n)*f2
      endfor
      goto, G300

;   coefficients for a given year have been calculated; now multiply
;   them by schmidt normalization factors:

G300: s=1.

;     do n=2,14
      for n=2,14 DO BEGIN
        mn=n*(n-1)/2 +1
        s=s*float(2*n-3)/float(n-1)
;        g(mn)=g(mn)*s
;        h(mn)=h(mn)*s
        g(mn-1)=g(mn-1)*s
        h(mn-1)=h(mn-1)*s
        p=s

        for m=2,n DO BEGIN
           aa=1.
           if (m EQ 2) then aa=2.
           p=p*sqrt(aa*float(n-m+1)/float(n+m-2))
           mnn=mn+m-1
;           g(mnn)=g(mnn)*p
;           h(mnn)=h(mnn)*p
           g(mnn-1)=g(mnn-1)*p
           h(mnn-1)=h(mnn-1)*p
        endfor
      endfor

;          g10=-g(2)
;          g11= g(3)
;          h11= h(3)

          g10=-g(1)
          g11= g(2)
          h11= h(2)

;     now calculate the components of the unit vector ezmag in geo
;     coord.system:
;     sin(teta0)*cos(lambda0), sin(teta0)*sin(lambda0), and cos(teta0)
;           st0 * cl0                st0 * sl0                ct0

      sq=g11^2 +h11^2
      sqq=sqrt(sq)
      sqr=sqrt(g10^2 +sq)
      sl0=-h11/sqq
      cl0=-g11/sqq
      st0=sqq/sqr
      ct0=g10/sqr

      stcl=st0*cl0
      stsl=st0*sl0

; *** direction of dipole axis in GEO system:

      d1=stcl
      d2=stsl
      d3=ct0

      return
      end

;     XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX



pro cotrans_lib
; does nothing.
; call cotrans_lib at the beginning of any routine
; that needs to use any cotrans_lib routines, to ensure
; that they are compiled.
end
