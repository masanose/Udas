;+
;NAME:
; thm_efi_boom_deployment_time
;PURPOSE:
; Provides the boom_deployment time(s) for EFI on THEMIS
; probes. Before this time, EFI and all ESA, SST, MOM, moments are
; problematic.
;CALLING SEQUENCE:
; boom_time = thm_efi_boom_deployment_time(probe = probe)
;INPUT:
; All via keyword
;KEYWORDS:
; probe = 'a', 'b', 'c', 'd', 'e', the probes for which you want the
;         time
;OUTPUT:
; boom_time = the time of EFI boom_deployment for input probes:
; Dates for each Probe when boom deploy was completed:
;
;Probe                Boom Deploy Complete
;THA (P5)        14 Jan 2008, 1700 UT.
;THB (P1)        17 Nov 2007, 0600 UT.
;THC (P2)        16 May 2007, 2100 UT.
;THD (P3)        7 Jun 2007, 1700 UT.
;THE (P4)        7 Jun 2007, 1900 UT.
;
;Notes:
;-- UT of "Deploy Complete" is first whole hour after final AXB deploy
;operation.
;-- Dates do not include final three-axis Sensor Diagnostic Test (SDT);
;see EFI Engineering Intervals table for details (forthcoming).
;HISTORY:
; 14-may-2008, jmm, jimm@ssl.berkeley.edu
; $LastChangedBy: jimm $
; $LastChangedDate: 2008-05-16 11:23:39 -0700 (Fri, 16 May 2008) $
; $LastChangedRevision: 3111 $
; $URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/thmsoc/tags/tdas_5_21/idl/themis/spacecraft/fields/thm_efi_boom_deployment_time.pro $
;-
Function thm_efi_boom_deployment_time, probe = probe, _extra = _extra

;check for valid probe
  If(is_string(probe)) Then Begin
    sc = strlowcase(probe)     ;this is probably overkill, but what if
    nsc = n_elements(sc)       ;somebody inputs ['A', 'B c']?
    sc1 = ''
    For j = 0, nsc-1 Do Begin
      pj = strcompress(/remove_all, strsplit(sc[j], /extract))
      sc1 = [sc1, pj]
    Endfor
    If(n_elements(sc1) Gt 1) Then sc1 = sc1[1:*] $
    Else sc1 = ['a', 'b', 'c', 'd', 'e']
    sc = temporary(sc1)
  Endif Else sc = ['a', 'b', 'c', 'd', 'e']
  print, 'SC=', sc
;Now check for probes
  nsc = n_elements(sc)
  otp = dblarr(nsc)
  For j = 0, nsc-1 Do Begin
    Case sc[j] Of
      'a':otp[j] = time_double('2008-01-14/17:00:00')
      'b':otp[j] = time_double('2007-11-17/06:00:00')
      'c':otp[j] = time_double('2007-05-16/21:00:00')
      'd':otp[j] = time_double('2007-06-07/17:00:00')
      'e':otp[j] = time_double('2007-06-07/19:00:00')
      Else:message, /info, 'Invalid Probe: '+sc[j]
    Endcase
  Endfor
  If(n_elements(otp) Eq 1) Then otp = otp[0]
  Return, otp
End

    
  
