;+
;pro thm_sst_crib
; This is an example crib sheet that will load Solid State Telescope data.
; Open this file in a text editor and then use copy and paste to copy
; selected lines into an idl window. Or alternatively compile and run
; using the command:
; .RUN THM_SST_CRIB
;Author: Davin Larson
;
;Modified: 2007-07-19 by Matt Davis.  This a simplified version of Davin's
;		original crib and is intended to demonstrate the very basics of
;		loading in the SST energy spectrograms.
;		The more comprehensive crib can be found with the SST routines in:
;		idl/themis/spacecraft/particles/SST/thm_sst_crib.pro
;
; $Id: thm_crib_sst.pro 1586 2007-09-18 12:42:50Z davin-win $
;-

;set the date and duration (in days)
timespan,'2007-06-05',1

;set the spacecraft
prb = ['a','b','c','d','e']
prb = 'c'
thx = 'th'+prb

;load the data
thm_load_sst,probe=prb       ;bpif keyword_set(dbg)
inst = ['psif','psef']     ; SST
thm_part_moments,probe=prb,instr=inst

;view the loaded data names
tplot_names

;plot the ion and electron spectrograms
tplot,[thx+'_psif_en_eflux',thx+'_psef_en_eflux']



end
