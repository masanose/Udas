;+
; NAME:
;     GSM2LMN
;
; PURPOSE:
;	Routine transforms vector field from GSM to LMN (boundary-normal)
;	coordinate system for magnetopause. Shue et al., 1998 magnetopause model
;	is used.
;
; CATEGORY:
;	Coordinate Transformation
;
; CALLING SEQUENCE:
;	gsm2lmn,txyz,Bxyz,Blmn,swdat
;
; INPUTS:
;	txyz: | t | x  | y  | z | - time and GSM position of the input vector (Bxyz).
;           - 2D array (nvectors,4)
;	Bxyz: | Bx | By | Bz | - vector field to transform (in GSM).
;           - 2D array (nvectors,3)
; OPTIONAL INPUT:
;	swdat: | t | Dp | Bz | of IMF at the bow-shock nose covering time
;		interval of interest. 2D array (ntimepoints,3). The time points
;		may be different from those of the vector field. However, they
;		should use the same time units.
;		If this input is not provided, the SPDF standard static SW data
;		are generated.
;
; KEYWORDS: none
;
; PARAMETERS: none
;
; OUTPUTS:
;	Blmn: | Bl | Bm | Bn | - vector in LMN at the same space-time points.
;		- 2D array (nvectors,3)
;
; DEPENDENCIES: None - can be used alone. Lowest-level part of LMN transform package.
;
; MODIFICATION HISTORY:
;	Written by:	Vladimir Kondratovich 2007/12/28 on the base of
;	the code xyz2lmnshue by Liu Jiang (09/21/2007)
;-
;
; THE CODE BEGINS:

PRO gsm2lmn,txyz,Bxyz,Blmn,swdat

;Some input check for vector field
sizebxyz=size(bxyz)
sizetxyz=size(txyz)
if sizebxyz(0) ne 2 then begin
   print,'gsm2lmn: Bxyz must be vector (array).'
   return
endif
if sizetxyz(0) ne 2 then begin
   print,'gsm2lmn: You must provide space-time coordinates of input vectors as 2D array.'
   return
endif
if sizebxyz(1) ne sizetxyz(1) then begin
   print,'gsm2lmn: first dimensions of coordinate and vector arrays must be equal.'
   return
endif

;Check SW input.
sizesw=size(swdat)
if sizesw(0) eq 0 then begin
   ;Generate static "standard" SW data.
   print,'gsm2lmn: No SW input detected. Static MP considered with Bz=0 and Dp=2.088 nPa.'
   swdata=dblarr(sizebxyz(1),3)
   swdata(*,0)=txyz(*,0)
   swdata(*,1)=2.088
   swdata(*,2)=0.D
endif else begin
   swdata=dblarr(sizebxyz(1),3)
   ;Replace bad values with static data and interpolate Bz and Dp onto the timegrid of Bxyz.
   timeb=reform(txyz(*,0))
   swdata(*,0)=timeb
   timesw=reform(swdat(*,0))
   dparr=reform(swdat(*,1))
   ind=where(finite(dparr),n,complement=indbad,ncomplement=nbad)
   if nbad gt 0 then dparr(indbad)=2.088; nPa - a 'standard' value
   dparra=interpol(dparr,timesw,timeb)
   swdata(*,1)=dparra
   bzarr=reform(swdat(*,2))
   ind=where(finite(bzarr),n,complement=indbad,ncomplement=nbad)
   if nbad gt 0 then bzarr(indbad)=0.; nT - a 'standard' value
   bzarra=interpol(bzarr,timesw,timeb)
   swdata(*,2)=bzarra
endelse

blmn = dblarr(sizebxyz(1),sizebxyz(2))
npoints=sizebxyz(1)

;Transformation starts
for i = 0, npoints-1 do begin
   bz=swdata(i,2)
   dp=swdata(i,1)>0.01
   x = txyz(i,1)
   y = txyz(i,2)
   z = txyz(i,3)
   alpha = (0.58-0.007*bz)*(1+0.024*alog(dp))

   theta=acos(x/sqrt(x^2+y^2+z^2))
   rho=sqrt(y^2+z^2)
   if rho gt 0. then begin
      tang1 = [0.,z,-y]
      tang2 = [x,y,z]*alpha*sin(theta)/(1+cos(theta))+[-rho^2,x*y,x*z]/rho
      tang1 = tang1/sqrt(total(tang1^2))
      tang2 = tang2/sqrt(total(tang2^2))
      dN = crossp(tang1,tang2)
      dM = crossp(dN, [0,0,1])
      dM = dM/sqrt(total(dM^2))
      dL = crossp(dM, dN)
   endif else begin
      dN=[1.,0.,0.]
      dM=[0.,-1.,0.]
      dL=[0.,0.,1.]
   endelse 
   transm = [[dL],[dM],[dN]]

   blmn(i,*) = bxyz(i,*) # transm
endfor

print, 'gsm2lmn finished.'

END
