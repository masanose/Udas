;+
;FUNCTION:	p_3d_new(dat,ENERGY=en,ERANGE=er,EBINS=ebins,ANGLE=an,ARANGE=ar,BINS=bins)
;INPUT:	
;	dat:	structure,	2d data structure filled by get_eesa_surv, get_eesa_burst, etc.
;KEYWORDS
;	ENERGY:	fltarr(2),	optional, min,max energy range for integration
;	ERANGE:	fltarr(2),	optional, min,max energy bin numbers for integration
;	EBINS:	bytarr(na),	optional, energy bins array for integration
;					0,1=exclude,include,  
;					na = dat.nenergy
;	ANGLE:	fltarr(2,2),	optional, angle range for integration
;				theta min,max (0,0),(1,0) -90<theta<90 
;				phi   min,max (0,1),(1,1)   0<phi<360 
;	ARANGE:	fltarr(2),	optional, min,max angle bin numbers for integration
;	BINS:	bytarr(nb),	optional, angle bins array for integration
;					0,1=exclude,include,  
;					nb = dat.ntheta
;	BINS:	bytarr(na,nb),	optional, energy/angle bins array for integration
;					0,1=exclude,include
;PURPOSE:
;	Returns the pressure tensor, [Pxx,Pyy,Pzz,Pxy,Pxz,Pyz], eV/cm^3 
;NOTES:	
;	Function normally called by "get_3dt" or "get_2dt" to
;	generate time series data for "tplot.pro".
;
;CREATED BY:
;	J.McFadden	00-2-24	
;LAST MODIFICATION:
;	J.McFadden	05-2-8		Fixed diagonalization
;	J.McFadden	06-2-23		changed the s/c pot calculation to the same as n_2d_new.pro
;-
function p_3d_new,dat2,ENERGY=en,ERANGE=er,EBINS=ebins,ANGLE=an,ARANGE=ar,BINS=bins

p3dxx = 0.
p3dyy = 0.
p3dzz = 0.
p3dxy = 0.
p3dxz = 0.
p3dyz = 0.

if dat2.valid eq 0 then begin
  print,'Invalid Data'
  return, [p3dxx,p3dyy,p3dzz,p3dxy,p3dxz,p3dyz]
endif

dat = conv_units(dat2,"df")		; Use Energy Flux
na = dat.nenergy
nb = dat.nbins
	if dat.data_name eq 'Pesa High' and dat.nbins eq 97 then dat.data(*,96)=0.
	
ebins2=replicate(1b,na)
if keyword_set(en) then begin
	ebins2(*)=0
	er2=[energy_to_ebin(dat,en)]
	if er2(0) gt er2(1) then er2=reverse(er2)
	ebins2(er2(0):er2(1))=1
endif
if keyword_set(er) then begin
	ebins2(*)=0
	er2=er
	if er2(0) gt er2(1) then er2=reverse(er2)
	ebins2(er2(0):er2(1))=1
endif
if keyword_set(ebins) then ebins2=ebins

bins2=replicate(1b,nb)
if keyword_set(an) then begin
	if ndimen(an) ne 2 then begin
		print,'Error - angle keyword must be (2,2)'
	endif else begin
		bins2=angle_to_bins(dat,an)
	endelse
endif
if keyword_set(ar) then begin
	bins2(*)=0
	if ar(0) gt ar(1) then begin
		bins2(ar(0):nb-1)=1
		bins2(0:ar(1))=1
	endif else begin
		bins2(ar(0):ar(1))=1
	endelse
endif
if keyword_set(bins) then bins2=bins

if ndimen(bins2) ne 2 then bins2=ebins2#bins2

data = dat.data*bins2
energy = dat.energy
denergy = dat.denergy
theta = dat.theta/!radeg
phi = dat.phi/!radeg
dtheta = dat.dtheta/!radeg
dphi = dat.dphi/!radeg
;domega = dat.domega
;	if ndimen(domega) eq 1 then domega=replicate(1.,dat.nenergy)#domega
;mass = dat.mass * 1.6e-22
mass = dat.mass 
;Const = (mass/(2.*1.6e-12))^(-.5)

charge=1.
value=0 & str_element,dat,'charge',value
if value ne 0 then charge=dat.charge		
if ((value eq 0) and (dat.mass lt 0.00010438871)) then charge=-1.		; this line works for Wind which does not have dat.charge
value=0 & str_element,dat,'sc_pot',value
if value ne 0 then energy=energy+(charge*dat.sc_pot/abs(charge))>0.		; energy/charge analyzer

; solid_angle_corr=4.*!pi/total(domega(0,*))	; this should be correct in the structure
; if (solid_angle_corr lt .99 or solid_angle_corr gt 1.01) and max(theta) gt 1.2 then print,'Error in dat.domega'   
; solid_angle_corr=1.

th1=theta-dtheta/2.
th2=theta+dtheta/2.
ph1=phi-dphi/2.
ph2=phi+dphi/2.
cth1 = cos(th1)
cth2 = cos(th2)
sth1 = sin(th1)
sth2 = sin(th2)
cph1 = cos(ph1)
cph2 = cos(ph2)
sph1 = sin(ph1)
sph2 = sin(ph2)
s_2ph1 = sin(2.*ph1)
s_2ph2 = sin(2.*ph2)
s2_ph1 = sph1^2
s2_ph2 = sph2^2
s3_th1 = sth1^3
s3_th2 = sth2^3 
c3_th1 = cth1^3
c3_th2 = cth2^3 

Const = (1.d*mass)^(-2.5)*(2.)^1.5
p3dxx = Const*total(denergy*(energy^(1.5))*data*((ph2-ph1)/2.+(s_2ph2-s_2ph1)/4.)*(sth2-sth1-(s3_th2-s3_th1)/3.))
p3dyy = Const*total(denergy*(energy^(1.5))*data*((ph2-ph1)/2.-(s_2ph2-s_2ph1)/4.)*(sth2-sth1-(s3_th2-s3_th1)/3.))
p3dzz = Const*total(denergy*(energy^(1.5))*data*dphi*(s3_th2-s3_th1)/3.)
p3dxy = Const*total(denergy*(energy^(1.5))*data*((s2_ph2-s2_ph1)/2.)*(sth2-sth1-(s3_th2-s3_th1)/3.))
p3dxz = Const*total(denergy*(energy^(1.5))*data*(sph2-sph1)*((c3_th1-c3_th2)/3.))
p3dyz = Const*total(denergy*(energy^(1.5))*data*(cph1-cph2)*((c3_th1-c3_th2)/3.))

;print,p3dxx,p3dyy,p3dzz,p3dxy,p3dxz,p3dyz
;print,total(((ph2-ph1)/2.+(s_2ph2-s_2ph1)/4.)*(sth2-sth1-(s3_th2-s3_th1)/3.))
;print,total(((ph2-ph1)/2.-(s_2ph2-s_2ph1)/4.)*(sth2-sth1-(s3_th2-s3_th1)/3.))
;print,total(dphi*(s3_th2-s3_th1)/3.)
;print,total(((s2_ph2-s2_ph1)/2.)*(sth2-sth1-s3_th2+s3_th1)/3.)
;print,total((sph2-sph1)*((c3_th1-c3_th2)/3.))
;print,total((cph1-cph2)*((c3_th1-c3_th2)/3.))

flux=j_3d_new(dat2,ENERGY=en,ERANGE=er,EBINS=ebins,ANGLE=an,ARANGE=ar,BINS=bins)
density=n_3d_new(dat2,ENERGY=en,ERANGE=er,EBINS=ebins,ANGLE=an,ARANGE=ar,BINS=bins)
vel = flux/density

p3dxx = mass*(p3dxx-vel(0)*flux(0)/1.e10)
p3dyy = mass*(p3dyy-vel(1)*flux(1)/1.e10)
p3dzz = mass*(p3dzz-vel(2)*flux(2)/1.e10)
p3dxy = mass*(p3dxy-vel(0)*flux(1)/1.e10)
p3dxz = mass*(p3dxz-vel(0)*flux(2)/1.e10)
p3dyz = mass*(p3dyz-vel(1)*flux(2)/1.e10)

; Rotate the tensor about the magnetic field to diagonalize
; This should give a result that diagonalizes the pressure tensor about dat2.magf
; where magf is in s/c coordinates -- ie same as the dat2.theta and dat2.phi coordinates.

; First form the pressure tensor
	p = [[p3dxx,p3dxy,p3dxz],[p3dxy,p3dyy,p3dyz],[p3dxz,p3dyz,p3dzz]]
;	print,p
; Rotate p about Z-axis by the angle between X and the projection of B on the XY plane
;	print,dat2.magf
if finite(total(dat2.magf)) && (total(dat2.magf*dat2.magf) gt 0.) then begin ;jmm, 10-feb-2009
;if finite(dat2.magf) then begin ; this crashes because dat2.magf is 3d
	bx=dat2.magf(0)
	by=dat2.magf(1)
	bz=dat2.magf(2)
	ph=atan(by,bx)
;	print,ph
	rot_ph=([[cos(ph),-sin(ph),0],[sin(ph),cos(ph),0],[0,0,1]])
	p = rot_ph#p#transpose(rot_ph)
;	print,p
; Then rotate p about Y-axis by the angle between Bz and B 
	th=!pi/2.-atan(bz,(bx^2+by^2)^.5)
;	print,th
	rot_th=[[cos(th),0,sin(th)],[0,1,0],[-sin(th),0,cos(th)]]
	p = rot_th#p#transpose(rot_th)
endif 

;	Pressure is in units of eV/cm**3

;return, [p3dxx,p3dyy,p3dzz,p3dxy,p3dxz,p3dyz]
return, [p(0,0),p(1,1),p(2,2),p(0,1),p(0,2),p(1,2)]

end

