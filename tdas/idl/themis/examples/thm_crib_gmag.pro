;+
;pro thm_crib_gmag
; This is an example crib sheet that will load ground mag data.
; Open this file in a text editor and then use copy and paste to copy
; selected lines into an idl window. Or alternatively compile and run
; using the command:
; .RUN THM_CRIB_GMAG
;-

!quiet=1                             ;  Turn off annoying screen messages.
wset,0
pos=ptrace(option=1)                 ;  Set program trace to display line numbers.
del_data,'*'                         ;  Delete all TPLOT variables.
erase                                ;  Clear the screen.
timespan,'6-10-2',2,/days            ;  Define the time span of interest.
thm_load_gmag,site='bmls ccnv fykn',/subtract_average    ;  Loads data from two sites
options,'thg_mag_????',labels=['Bx','By','Bz']
tplot_options, 'title', 'GMAG Examples'
print
print,'Defined TPLOT Variables;'
tplot_names ,/time                   ;  Display all stored variables
print
print,ptrace(),'             Deleted old data, (down)loaded GMAG Data, and Displayed tplot names'
print,ptrace(),'             Note that 3 sites were loaded each with 2 days of data.'
print,ptrace(),'             All files are downloaded automatically if not found.'
stop


wshow,0                                ; Raise window
tplot,"thg_mag_????"                   ; tplot accepts wildcard characters
print,ptrace(),'             Plotted all TPLOT variables that match "thg_mag_????" '
stop


split_vec,'thg_mag_ccnv'               ; Split one of the 3 vectors into components
options,'*_[xyz]' ,/ynozero
tplot,'thg_mag_ccnv*'
print,ptrace(),'             Split the 3 vector into its components.  Plot them'
stop


wshow,0
tr = ['2006-10-2/16:00','2006-10-3/05']  ; Define a time range
timebar,tr                               ; Display it
print,ptrace(),'             Define and Display a time range'
stop



tlimit,tr                               ; Zoom into the time range
print,ptrace(),'             Zoom into the defined time range'
stop



                                   ; Compute the wavelet transform of x component
print,ptrace(),'             Computing wavelet transform. Please wait.....'
wav_data,'thg_mag_ccnv_x',/kol  ,trange=tr   ,maxpoints=24l*3600*2
zlim,'*pow', .0001,.01,1           ; Set color limits (log scale)
wshow,0,icon=0
tplot,'*ccnv_x*',trange=tr         ; PLOT the wavelet transform
print,ptrace(),'             Computed wavelet transform of one component and plot it.'
stop



tr2 = ['2006-10-03/02:13:30', '2006-10-03/03:46:00']
timebar,tr2                        ;  Display region with Pi2 waves
print,ptrace(),'             Identiy region with Pi2 waves'
stop



tlimit,tr2              ;
print,ptrace(),'             Zoom of region with Pi2 waves'
stop



tlimit,tr
tr1   =  ['2006-10-02/18:23:00', '2006-10-02/18:49:30']
timebar,tr1
print,ptrace(),'             Zoomed out again and displayed region with PC1(?) waves'
stop



tlimit,tr1
print,ptrace(),'             Zoom of region with Pc1 waves: [',strjoin(time_string(tr1,tformat='hhmm:ss'),' to '),']'
stop





tlimit,tr
print,ptrace(),'             Select you own time range of interest: (right click to end)'
wshow,0,icon=0
ctime,my_tr,/silent
tlimit,my_tr


end

