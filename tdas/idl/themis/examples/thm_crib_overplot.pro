;+
; NAME: thm_crib_overplot
;
; PURPOSE: this crib describes how to generate overview plots
;          if there are any arguments or features for these 
;          procedures you would like to request, please feel 
;          free to ask.
;
; $LastChangedBy: pcruce $
; $LastChangedDate: 2008-01-30 12:39:05 -0800 (Wed, 30 Jan 2008) $
; $LastChangedRevision: 2328 $
; $URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/thmsoc/tags/tdas_5_21/idl/themis/examples/thm_crib_overplot.pro $
;-


;To get the single-spacecraft overview plot

thm_gen_overplot, probe='b', date='2007-12-11'

;set the /makepng keyword  to get output files, this will give the full
;day file, and the six-hour ones

;to get FGM tohban plots
thm_fgm_overviews, '2007-12-11', /nopng 

;for the tohban plots, png files are done as a default, /nopng turns this 
;off, also for these thedate is nota keyword

;ESA, SST are the same
thm_esa_overviews, '2007-12-11', /nopng
thm_sst_overviews, '2007-12-11', /nopng

;for the esa you get burst, reduced and full plots, for sst you get
;full and reduced plots.

;for the new memory plots, the date is a keyword input. (these really 
;should be consistent..
thm_memory_plots, date = '2007-12-11', /nopng

;here is an example of how to create a moment overview plot
;this plot uses on-board moments whenever possible
thm_fitmom_overviews,'2007-11-15','b'

;to make png's
;thm_fitmom_overviews,'2007-11-15','b',/makepng

;here is an example of how to create another moment overview plot
;this plot always uses ground processed moments
thm_fitgmom_overviews,'2007-11-15','b'

;to make png's
;thm_fitmom_overviews,'2007-11-15','b',/makepng



End
