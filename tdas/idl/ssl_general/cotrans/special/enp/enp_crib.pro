;+
;Procedure: enp_crib.pro
;
;Purpose:  A crib showing how to transform data into the ENP and PEN coordinate systems. 
;
;   E: sat to earth (in-plane)
;   N: east (in-plane)
;   P: north (perpendicular to plane).
;
;  Defined relative to another coordinate system:
;   P_sat = spacecraft position in geocentric interial coordinate system
;   V_sat = deriv(P_sat)   (spacecraft velocity in the same coordinate system.)
;
;   P_enp = P_sat cross V_sat
;   E_enp = -P_sat
;   N_enp = P_enp cross P_sat
;
;Notes:
;
; $LastChangedBy: pcruce $
; $LastChangedDate: 2009-09-18 17:02:29 -0700 (Fri, 18 Sep 2009) $
; $LastChangedRevision: 6752 $
; $URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/ssl_general/tags/tdas_5_21/cotrans/special/enp/enp_crib.pro $
;-



  ;set the time
  timespan,'2007-12-13'

  thm_load_goesmag,probe='g10'

  ;----------------
  ;ENP system
  ;----------------
  
  ;input position MUST be earth-centered and inertial
  ;velocity/orbital plane determined using derivative of position.
  ;If there are not enough points, the result will be inaccurate.  Also, may have small error at the tails.
  enp_matrix_make,'g10_pos_gei'  ;accepts newname,suffix keywords, can use globbing on inputs

  ;--------------------
  ;GEI->ENP
  ;--------------------

  ;this routine interpolate rotation matrices automatically, no need to match cadence ahead of time
  tvector_rotate,'g10_pos_gei_enp_mat','g10_b_gei'   ;accepts newname,suffix keywords, can use globbing on inputs

  options,'g10_b_gei_rot',labels=['E','N','P'],ytitle='g10_b_enp'
  
  options,'g10_b_*',yrange=[-100,150]

  tplot,['g10_b_gei_rot','g10_b_enp'] ;should match
  
  stop
  
  ;--------------------
  ;ENP->GEI
  ;-------------------- 
 
  tvector_rotate,'g10_pos_gei_enp_mat','g10_b_enp',/invert  ;invert keyword runs transformation backwards

  options,'g10_b_enp_rot',labels=['Bx','By','Bz'],ytitle='g10_b_gei'

  tplot,['g10_b_enp_rot','g10_b_gei'] ;should match
  
  stop
  
  ;-----------------------
  ;User Provided Velocity, Rather than derivative of position velocity
  ;-----------------------
  
  tinterpol_mxn,'g10_pos_gei','g10_b_enp'
  
  enp_matrix_make,'g10_pos_gei',velocity_tvar='g10_vel_gei'
  
  tvector_rotate,'g10_pos_gei_enp_mat','g10_b_gei'   ;accepts newname,suffix keywords, can use globbing on inputs

  options,'g10_b_gei_rot',labels=['E','N','P'],ytitle='g10_b_enp'

  tplot,['g10_b_gei_rot','g10_b_enp'] ;should match
  
  stop
  
  ;------------------------------
  ;User provided Single Orbital Element, doesn't use velocity at all
  ;------------------------------
  
  ;GOES 10 orbital elements for 2007-12-17
  orbital_time = time_double('2007-12-17/01:37:56')
  orbital_ras =   81.7391D ;right ascension of ascending node
  orbital_inclination = 2.2461 ;orbital inclination
  
  orbital_elements = [orbital_time,orbital_ras,orbital_inclination]
  
  enp_matrix_make,'g10_pos_gei',orbital_elements=orbital_elements
   
  tvector_rotate,'g10_pos_gei_enp_mat','g10_b_gei'   ;accepts newname,suffix keywords, can use globbing on inputs
  
  options,'g10_b_gei_rot',labels=['E','N','P'],ytitle='g10_b_enp'

  tplot,['g10_b_gei_rot','g10_b_enp'] ;Should be small error due to accumultated time-series error orbital elements.
                                      ;Occurs at the GEI-POS-Z 0-crossing.
                                      ;Should be fixed by interpolated orbital elements. 

  stop
  
  ;------------------------------
  ;User provided Interpolated Orbital Element, doesn't use velocity at all
  ;-------------------------
  
  orbital_time1 = time_double('2007-12-10/06:52:04')
  orbital_ras1 = 081.8024D
  orbital_inclination1 = 2.2298D
  
  orbital_time2 = time_double('2007-12-17/01:37:56')
  orbital_ras2 =   81.7391D ;right ascension of ascending node
  orbital_inclination2 = 2.2461D ;orbital inclination
  
  ;Can have as many orbital elements as you want, not limited to providing elements at only 2 times 
  orbital_elements = [[orbital_time1,orbital_ras1,orbital_inclination1],[orbital_time2,orbital_ras2,orbital_inclination2]]
  
  enp_matrix_make,'g10_pos_gei',orbital_elements=orbital_elements
   
  tvector_rotate,'g10_pos_gei_enp_mat','g10_b_gei'   ;accepts newname,suffix keywords, can use globbing on inputs
  
  options,'g10_b_gei_rot',labels=['E','N','P'],ytitle='g10_b_enp'

  tplot,['g10_b_gei_rot','g10_b_enp']
  
end