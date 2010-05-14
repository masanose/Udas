;+
; Name: thm_crib_plotxyz
;
; Purpose:crib to demonstrate capabilities of plotxyz
;
; Notes: run it by compiling in idl and then typing ".go"
;        or copy and paste.
;
; SEE ALSO: bin1d.pro,bin2d.pro,plotxyz.pro
;
; $LastChangedBy: pcruce $
; $LastChangedDate: 2008-02-06 13:43:58 -0800 (Wed, 06 Feb 2008) $
; $LastChangedRevision: 2352 $
; $URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/ssl_general/trunk/tplot/tplotxy.pro $
;-

;sets the colors correctly
thm_init

;generate some test data

x = dindgen(7)
y = dindgen(7)
z = dindgen(7,7)

;basic plot
plotxyz,x,y,z

stop

;demonstrate rectangular isotropic plot

x = dindgen(7)
y = dindgen(9)
z = dindgen(7,9)

plotxyz,x,y,z

stop

;demonstrate changing scaling axes to change plot

x *= 2

;this change to z makes it easier to see what goin on
z = z mod 2

plotxyz,x,y,z

stop

;irregular scaling

x[0] = -2
y[8] = 12
y[7] = 11.5

plotxyz,x,y,z

stop

;interpolation

x = dindgen(7)
y = dindgen(9)
z = dindgen(7,9)

plotxyz,x,y,z,/interp

stop

;no isotropic

plotxyz,x,y,z,/noiso

stop

;log scaling

x2 = 2^x
y2 = 2^y
z2 = 2^z

plotxyz,x2,y2,z2,/xlog,/ylog,/zlog

stop

;range manipulation

plotxyz,x,y,z,xrange=[2.,4.],yrange=[2.,5.],zrange=[10.,60.]

stop

;preparing and using real data

timespan,'2008-02-14'

thm_load_state,probe='a',coord='gsm'

thm_load_mom,probe='a'

tkm2re,'tha_state_pos',/replace

get_data,'tha_state_pos',data=d

;this code is just preparing the data for bin2d
pos = d.y

pos_tm = d.x

get_data,'tha_peim_density',data=d

den = interpol(d.y,d.x,pos_tm)

;Vassilis's super useful bin2d proc bins the data
;Check out the header to see all the useful stuff it
;it can do

;Look at documentation for grad.pro to see how to automatically
;generate the gradient, and how to overplot it on the output of
;plotxyz  

bin2d,pos[*,0],pos[*,1],den,binum=20,averages=averages,medians=medians,xcenters=x_centers,ycenters=y_centers,flagnodata=!values.d_nan

;plot it.  You can also use the tick keywords like xticks and yticks to manipulate the spacing on your grid.
plotxyz,x_centers,y_centers,averages,xtitle='X [RE GSM]',ytitle='Y [RE GSM]',title='THEMIS A Density V Pos GSM',background=255,/grid

stop

;you can also overplot arrows on xyz plots

x = dindgen(7)
y = dindgen(9)
z = dindgen(7,9)

plotxyz,x,y,z

x = dindgen(6*8) mod 6
y = double(indgen(6*8) / 6)

xy = [[x],[y]]


plotxyvec,xy,replicate(.7,6*8,2),/over

stop


;You can do multiple plots/panel and interleave with tplotxy/plotxy, plotxyvec

;multi uses columns first
plotxyz,x_centers,y_centers,averages,multi='2,2',/grid

x = dindgen(10)
y = alog(dindgen(10)+1)
u = x[1:9]-x[0:8]
v = y[1:9]-y[0:8]

plotxy,[[x],[y]],/add,/noiso,title="change arrows"

plotxyvec,[[x[1:9]],[y[1:9]]],[[u],[v]],/over

tplotxy,'tha_state_pos',/add,ymargin=[.2,.05]

x = double(indgen(19*19)mod 19)-9
y = double(indgen(19*19)/ 19)-9
u = -1 * (x/((abs(x)+abs(y))))
v = -1 * (y/((abs(x)+abs(y))))
u[19*19/2] = 0
v[19*19/2] = 0

plotxyvec,[[x],[y]],[[u],[v]],/grid,/add,xticks=8,yticks=8,title="inward arrows",uarrowdatasize=1.0

stop

;auto replot

window,xsize=750,ysize=750

plotxyz

stop

;charsize

window,xsize=800,ysize=400

plotxyz,x_centers,y_centers,averages,multi='2,1',title='Density Averages',xtitle='X RE GSM',ytitle='Y RE GSM',charsize = 1.5,/grid,/interp

plotxyz,x_centers,y_centers,medians,/add,title='Density Medians',xtitle='X RE GSM',ytitle='Y RE GSM',charsize = .75,/grid

stop

;margins and window title
;margins are measured using 2 numbers [0.:1.] that represents the
;proportion of the area alotted to that particular plot on that axis
;that is occupied by the margin on that size
;x margin is [left,right]
;y margin is [bottom,top] 
;
;can be changed for each plot independently


plotxyz,x_centers,y_centers,averages,multi='2,1',wtitle='POS VS DEN PLOTS',xmargin=[.25,.05],ymargin=[.25,.05],/grid

plotxyz,x_centers,y_centers,medians,/add,xmargin = [0,.2],ymargin= [.1,.1],/grid

stop

end


