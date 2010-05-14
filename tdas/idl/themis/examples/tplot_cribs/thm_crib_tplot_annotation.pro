;+
; NAME: thm_crib_tplot_annotation
; 
; PURPOSE:  Crib to demonstrate tplot annotation commands  
;           You can run this crib by typing:
;           IDL>.compile thm_crib_tplot_annotation
;           IDL>.go
;           
;           When you reach a stop, press
;           IDL>.c
;           to continue
;           
;           Or you can copy and paste commands directly onto the command line
;
; SEE ALSO: thm_crib_tplot.pro  (basic tplot commands)
;           thm_crib_tplot_layout.pro  (how to arrange plots within a window, and data within a plot)
;           thm_crib_tplot_range.pro   (how to control the range and scaling of plots)
;           thm_crib_tplot_export_print.pro (how to export images of plots into pngs and postscripts)
;
;
; NOTES:
;  1.  As a rule of thumb, "tplot_options" controls settings that are global to any tplot
;   "options" controls settings that are specific to a tplot variable
;   
;  2.  If you see any useful commands missing from these cribs, please let us know.
;   these cribs can help double as documentation for tplot.
;
; $LastChangedBy: jimm $
; $LastChangedDate: 2010-04-07 14:48:41 -0700 (Wed, 07 Apr 2010) $
; $LastChangedRevision: 7469 $
; $URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/thmsoc/tags/tdas_5_21/idl/themis/examples/tplot_cribs/thm_crib_tplot_annotation.pro $
;-



;this line deletes data so we start the crib fresh
store_data,'*',/delete

;first we set a time and load some data.
timespan,'2008-03-23'

;loading FGM line data in GSM coordinates
thm_load_fgm,probe='a',coord='gsm', level = 'l2'

;loading state data in GSM coordinates
thm_load_state,probe='a',coord='gsm'

;loading ESA for spectral data
thm_load_esa,probe='a'

;increasing the xmargin so it is easier to see the labels
tplot_options,'xmargin',[15,15] ;15 characters on each side

;basic plot for comparision
tplot,['tha_peif_en_eflux','tha_fgs_gsm']

print,'  This first plot is the default, for reference. 
print,'Type ".c" to begin crib examples.'
stop

;use "title" to set the plot window title
tplot_options,title='HELLO WORLD'
tplot ;this just replots the previous plot with the new options
print,'  Set the plot title with "title"'
print,'Type ".c" to begin'
stop

;use x/y/ztitle & x/y/zsubtitle, to set individual axis titles
;by default, the variable name is used
options,'tha_peif_en_eflux',ytitle="I'm a ytitle",ztitle="I'm a ztitle"

;use options to set the ysubtitle
options,'tha_fgs_gsm',ysubtitle="I'm a ysubtitle",xtitle="I'm an xtitle"

;replot
tplot

print,'  Use x/y/ztitle & x/y/zsubtitle, to set individual axis titles'
print,'Type ".c" to begin'
stop

;increase the whole plot charsize using "charsize"

tplot_options,'charsize',1.2  ;this value is a multiple of the default character size

tplot

print,'  Set the global character size using "charsize"'
print,'Type ".c" to begin'
stop

;change the charsize for each axis individually using xcharsize,ycharsize

options,'tha_fgs_gsm',xcharsize=.6,ycharsize=1.4
options,'tha_peif_en_eflux',ycharsize=1.0
tplot

print,'  Change the charsize for each axis individually using "x/ycharsize"'
print,'Type ".c" to begin'
stop

;resetting sizes to more managable values
options,'tha_fgs_gsm',xcharsize=1.0,ycharsize=1.0,xtitle=''

;use "labels" to set labels for a line plot

options,'tha_fgs_gsm',labels=['Xcomp','Ycomp','Zcomp'] ;number of elements in labels should match number of components in line plot

tplot

print,'  Use "labels" to set labels for a line plot'
print,'Type ".c" to begin'
stop

;zooming in so feature is more visible
tlimit,'2008-03-23/12:00:00','2008-03-23/20:00:00'

;setting labflag will put axis labels where lines end, instead of evenly spacing labels along the axis
options,'tha_fgs_gsm',labflag=2

;labflag = 1 is the default for line plots
;labflag = 0 turns labels off

tplot
print,'  Use "labflag" to control positioning of labels'
print,'Type ".c" to begin'
stop

;resetting labflag
options,'tha_fgs_gsm',labflag=1

;"colors" option controls line/label color

options,'tha_fgs_gsm',colors=['b','m','c'] ;number of elements should match number of components
;valid values for colors include
;'x','m','b','c','g','y','r','w', 'd','z', and 0-255
;'x' or 0 is black
;'m' or 1 is magenta
;'b' or 2 is blue
;'c' or 3 is cyan
;'g' or 4 is green
;'y' or 5 is yellow
;'r' or 6 is red
;'w' or 255 is white
;'d' is foreground color(!p.color)
;'z' is background color(!p.background)
;10-255 are elements in a continuous color table. (The default is a basic rainbow table)

tplot

print,'  "colors" option controls line/label color'
print,'Type ".c" to begin'
stop
;reset colors
options,'tha_fgs_gsm',colors=[2,4,6]

;use !p.color and !p.background to change background & foreground colors
;can only use numerical color indexes (not letters)

!p.color = 1
!p.background=3

tplot

print,'  "!p.color" and "!p.background" can change foreground & background colors
print,'Type ".c" to begin'
stop

;resetting foreground & background
!p.color = 0
!p.background = 255

;label the x-axis using a single variable(distance in re)

calc,'"tha_pos_re" = "tha_state_pos"/6374.4',/verbose ;convert km into RE
calc,'"tha_dist_re" = sqrt(total("tha_pos_re",2))',/verbose  ;euclidean norm

options,'tha_dist_re',ytitle="Dist(RE)"  ;ytitle is used to label variables

tplot,var_label='tha_dist_re'

print,'  "var_label" can be used to label the x-axis 
print,'Type ".c" to begin'
stop

;label the x-axis using multiple single variables(state position gsm in re)

split_vec,'tha_pos_re' ;split tplot variable into individual components

;used to set label for var_label option
options,'tha_pos_re_x',ytitle='X Pos(RE)'
options,'tha_pos_re_y',ytitle='Y Pos(RE)'
options,'tha_pos_re_z',ytitle='Z Pos(RE)'

tplot,var_label=['tha_pos_re_x','tha_pos_re_y','tha_pos_re_z']

print,'  "var_label" can be used to label with multiple components
print,'Type ".c" to begin'
stop

;use x/y ticklen to control length of major ticks 

options,'tha_fgs_gsm',xticklen=.15,yticklen=.05 ;value is a proportion of panel occupied by ticks, ie 1 = 100%, so we create a grid with this call
options,'tha_peif_en_eflux',xticklen=.20,yticklen=.25 ;these ticks are 25% and 35% of the panel size, respectively

;var_label = '' turns off variable labels
tplot,var_label=''

print,'  "x/yticklen" can be used to control tick length'

stop

;use ticklen to create a grid
options,'tha_fgs_gsm',xticklen=1,yticklen=1 ;value is a proportion of panel occupied by ticks, ie 1 = 100%, so we create a grid with this call
options,'tha_peif_en_eflux',xticklen=1,yticklen=1;these ticks are 25% and 35% of the panel size, respectively

tplot
print,'You can set long ticks to create a grid on major ticks.'

stop

;resetting tick-len
options,'tha_fgs_gsm',xticklen=.1,yticklen=.05
options,'tha_peif_en_eflux',xticklen=.1,yticklen=.05

;Control thickness of borders & ticks with y/x-thick

options,'tha_fgs_gsm',xthick=2.0,ythick=2.0
options,'tha_peif_en_eflux',xthick=5.0,ythick=1.0

tplot
print,'You can control border/tick/grid thickness using x/y thick'

stop

;Control line thickness using 'thick'
options,'tha_fgs_gsm',thick=2.0

tplot
print,'You can control line thickness using thick'

print,"We're done!"

end
