;+
;PROCEDURE:	thm_read_esa_sweep_full_mode
;PURPOSE:	
;	Returns data structure with energy sweep tables
;INPUT:		
;	fm	int		flight model of esa sensor for energy calibration
;
;KEYWORDS:
;
;CREATED BY:	J. McFadden
;VERSION:	1
;LAST MODIFICATION:  07/03/22
;MOD HISTORY:
;
;NOTES:	  
;	Used by thm_load_esa_pkt.pro
;-
function thm_read_esa_sweep_full_mode,fm
		  
; default modes 1,2,3,4,5

; Ion Energy modes

i_energy=fltarr(32,5)
i_denergy=fltarr(32,5)
i_nenergy=intarr(5)
;i_cal=1.6
; old cal before top cap fix i_cal=3827./2512.
;i_cal=3770./2512.		; determined from fm4
i_cal=[1.539,1.501, 1.497,1.501,1.519,1.5]

; Ion mode 0 - ~1/2 sweep, no linear, I&T mode
	dac=thm_esa_energy_steps(xstart=8181,xslope=16,cstart=0,cslope=0,number=128,retrace=3,dblsweep=0)	
; The following is for LEO only
	dac=thm_esa_energy_steps(xstart=16383,xslope=17,cstart=0,cslope=0,number=128,retrace=3,dblsweep=0)	
	i_energy(*,0)=i_cal(fm-1)*(1.*dac(0,*)+dac(1,*)+dac(2,*)+dac(3,*))/4.
;	i_energy(*,0)=i_cal(fm-1)*dac(1,*)
;	i_denergy(1:31,0)=i_cal(fm-1)*(dac(3,0:30)-dac(0,1:31))
	i_denergy(1:30,0)=i_cal(fm-1)*(1.*dac(3,0:29)+dac(0,1:30)-dac(3,1:30)-dac(0,2:31))/2.
	i_denergy(31,0)=i_cal(fm-1)*(1.*dac(3,30)+dac(0,31)-2.*dac(3,31))/2.
	i_nenergy(0)=32

; Ion mode 1 - Full sweep, no linear, Msph mode
	dac=thm_esa_energy_steps(xstart=16383,xslope=17,cstart=0,cslope=0,number=128,retrace=3,dblsweep=0)	
	i_energy(*,1)=i_cal(fm-1)*(1.*dac(0,*)+dac(1,*)+dac(2,*)+dac(3,*))/4.
;	i_energy(*,1)=i_cal(fm-1)*dac(1,*)
;	i_denergy(1:31,1)=i_cal(fm-1)*(dac(3,0:30)-dac(0,1:31))
	i_denergy(1:30,1)=i_cal(fm-1)*(1.*dac(3,0:29)+dac(0,1:30)-dac(3,1:30)-dac(0,2:31))/2.
	i_denergy(31,1)=i_cal(fm-1)*(1.*dac(3,30)+dac(0,31)-2.*dac(3,31))/2.
	i_nenergy(1)=32
	
; Ion mode 2 - ~1/4 sweep, no linear, solar wind mode
	dac=thm_esa_energy_steps(xstart=4146,xslope=17,cstart=0,cslope=0,number=128,retrace=3,dblsweep=1)	
	i_energy(*,2)=i_cal(fm-1)*(1.*dac(0,*)+dac(1,*)+dac(2,*)+dac(3,*))/4.
	i_energy(16:31,2)=i_energy(0:15,2)
;	i_energy(*,2)=i_cal(fm-1)*dac(1,*)
;	i_denergy(1:31,2)=i_cal(fm-1)*(dac(3,0:30)-dac(0,1:31))
	i_denergy(1:15,2)=i_cal(fm-1)*(1.*dac(3,0:14)+dac(0,1:15)-dac(3,1:15)-dac(0,2:16))/2.
	i_denergy(16:31,2)=i_denergy(0:15,2)
	i_nenergy(2)=16

; Ion mode 3 - Full sweep, no linear, shock mode - no low energies
	dac=thm_esa_energy_steps(xstart=16383,xslope=11,cstart=0,cslope=0,number=128,retrace=3,dblsweep=0)	
	i_energy(*,3)=i_cal(fm-1)*(1.*dac(0,*)+dac(1,*)+dac(2,*)+dac(3,*))/4.
;	i_energy(*,3)=i_cal(fm-1)*dac(1,*)
;	i_denergy(1:31,3)=i_cal(fm-1)*(dac(3,0:30)-dac(0,1:31))
	i_denergy(1:30,3)=i_cal(fm-1)*(1.*dac(3,0:29)+dac(0,1:30)-dac(3,1:30)-dac(0,2:31))/2.
	i_denergy(31,3)=i_cal(fm-1)*(1.*dac(3,30)+dac(0,31)-2.*dac(3,31))/2.
	i_nenergy(3)=32

; Electron Energy modes

e_energy=fltarr(32,4)
e_denergy=fltarr(32,4)
e_nenergy=intarr(4)
;e_cal=2.
;e_cal=795.6/419.
; old calibration e_cal=7856./4095.
;e_cal=7810./4095.	; based on fm4
e_cal=[1.939,1.905,1.907,1.905,1.927,1.9]

; Electron mode 0 - 1/2 sweep, no linear, 32E I&T mode
	dac=thm_esa_energy_steps(xstart=8191,xslope=16,cstart=0,cslope=0,number=128,retrace=3,dblsweep=0)	
; The following is for LEO only
	dac=thm_esa_energy_steps(xstart=16383,xslope=17,cstart=0,cslope=0,number=128,retrace=3,dblsweep=0)	
	e_energy(*,0)=e_cal(fm-1)*(1.*dac(0,*)+dac(1,*)+dac(2,*)+dac(3,*))/4.
	e_denergy(1:30,0)=e_cal(fm-1)*(1.*dac(3,0:29)+dac(0,1:30)-dac(3,1:30)-dac(0,2:31))/2.
	e_denergy(31,0)=e_cal(fm-1)*(1.*dac(3,30)+dac(0,31)-2.*dac(3,31))/2.
	e_nenergy(0)=32

; Electron mode 1 - Full sweep, no linear, 32E Msph mode
	dac=thm_esa_energy_steps(xstart=16383,xslope=17,cstart=0,cslope=0,number=128,retrace=3,dblsweep=0)	
	e_energy(*,1)=e_cal(fm-1)*(1.*dac(0,*)+dac(1,*)+dac(2,*)+dac(3,*))/4.
	e_denergy(1:30,1)=e_cal(fm-1)*(1.*dac(3,0:29)+dac(0,1:30)-dac(3,1:30)-dac(0,2:31))/2.
	e_denergy(31,1)=e_cal(fm-1)*(1.*dac(3,30)+dac(0,31)-2.*dac(3,31))/2.
	e_nenergy(1)=32
	
; Electron mode 2 - 1/4 sweep, no linear, 32E solar wind mode
	dac=thm_esa_energy_steps(xstart=4095,xslope=15,cstart=0,cslope=0,number=128,retrace=3,dblsweep=0)	
	e_energy(*,2)=e_cal(fm-1)*(1.*dac(0,*)+dac(1,*)+dac(2,*)+dac(3,*))/4.
	e_denergy(1:30,2)=e_cal(fm-1)*(1.*dac(3,0:29)+dac(0,1:30)-dac(3,1:30)-dac(0,2:31))/2.
	e_denergy(31,2)=e_cal(fm-1)*(1.*dac(3,30)+dac(0,31)-2.*dac(3,31))/2.
	e_nenergy(2)=32

; Electron mode 3 - Full sweep, no linear, 15E Msph mode
	dac=thm_esa_energy_steps(xstart=16383,xslope=17,cstart=0,cslope=0,number=128,retrace=3,dblsweep=0)	
	tmp=reform(e_cal(fm-1)*(1.*dac(0,*)+dac(1,*)+dac(2,*)+dac(3,*))/4.)
	e_energy(0:14,3)=(tmp(2*findgen(15)+1)+tmp(2*findgen(15)+2))/2.
	e_denergy(0:14,3)=e_cal(fm-1)*(1.*dac(3,2*findgen(15))+dac(0,2*findgen(15)+1)-dac(3,2*findgen(15)+2)-dac(0,2*findgen(15)+3))/2.
	e_nenergy(3)=15

mode={i_energy:i_energy,i_denergy:i_denergy,i_nenergy:i_nenergy,$
	e_energy:e_energy,e_denergy:e_denergy,e_nenergy:e_nenergy}


return,mode

end
