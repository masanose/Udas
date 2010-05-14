;+
;FUNCTION:	j_3d_new(dat,ENERGY=en,ERANGE=er,EBINS=ebins,ANGLE=an,ARANGE=ar,BINS=bins)
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
;	Returns the flux, [Jx,Jy,Jz], 1/(cm^2-s) 
;NOTES:	
;	Function normally called by "get_3dt" or "get_2dt" to
;	generate time series data for "tplot.pro".
;
;CREATED BY:
;	J.McFadden	00-2-24
;LAST MODIFICATION:
;-
function j_3d_new,dat2,ENERGY=en,ERANGE=er,EBINS=ebins,ANGLE=an,ARANGE=ar,BINS=bins

flux3dx = 0.
flux3dy = 0.
flux3dz = 0.

if dat2.valid eq 0 then begin
  print,'Invalid Data'
  return, [flux3dx,flux3dy,flux3dz]
endif

dat = conv_units(dat2,"df")		; Use distribution function
na = dat.nenergy
nb = dat.nbins
	
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
domega = dat.domega
	if ndimen(domega) eq 1 then domega=replicate(1.,dat.nenergy)#domega
mass = dat.mass
Const = 2./mass/mass*1e5
charge=1.
value=0 & str_element,dat,'charge',value
if value ne 0 then charge=dat.charge		
if ((value eq 0) and (dat.mass lt 0.00010438871)) then charge=-1.		; this line works for Wind which does not have dat.charge
value=0 & str_element,dat,'sc_pot',value
if value gt 0 or value lt 0 then energy=energy+(charge*dat.sc_pot/abs(charge))>0.		; energy/charge analyzer

solid_angle_corr=4.*!pi/total(domega(0,*))	; this should be correct in the structure
if (solid_angle_corr lt .99 or solid_angle_corr gt 1.01) and max(theta) gt 1.2 then print,'Error in dat.domega'   
solid_angle_corr=1.

flux3dx = Const*total(denergy*(energy)*data*(dtheta/2.+cos(2*theta)*sin(dtheta)/2.)*(2.*sin(dphi/2.)*cos(phi)))
flux3dy = Const*total(denergy*(energy)*data*(dtheta/2.+cos(2*theta)*sin(dtheta)/2.)*(2.*sin(dphi/2.)*sin(phi)))
flux3dz = Const*total(denergy*(energy)*data*(2.*sin(theta)*cos(theta)*sin(dtheta/2.)*cos(dtheta/2.))*dphi)

; units are 1/cm^2-s

return, [flux3dx,flux3dy,flux3dz]
end
