;+
;Procedure: plot_part_slice2d
;
;Purpose: Set options for plotting 2D particle slices.
;
;Arguments:
; PART_SLICE: 2D array of values to plot 
;
;Keywords:
; XGRID: X location of data points.
; YGRID: Y location of data points.
; RANGE: Two-element array specifying range of flux (zrange) of velocities.
;
;Created by Bryan Kerr
;-

pro plot_part_slice2d, part_slice, xgrid=xgrid, ygrid=ygrid, $
                       range=range, olines=olines, nlines=nlines, $
                       fill=fill, $
                       _extra=ex

compile_opt idl2

maximum = max(part_slice)
minimum = min(part_slice[where(part_slice ne 0)])
thelevels = 10.^(indgen(nlines)/float(nlines)*(alog10(maximum) - alog10(minimum)) + alog10(minimum))
thecolors = round((indgen(nlines)+1)*(!d.table_size-9)/nlines)+7
fill = 1
contour, part_slice, xgrid, ygrid, /isotropic, $
    /closed,levels=thelevels,c_color = thecolors,fill=fill,$
    title = 'this would be time title', $
    ystyle = 1,$
    ticklen = -0.01,$
    xstyle = 1,$
    xrange = minmax(xgrid),$
    yrange = minmax(xgrid),$
    xtitle = 'x title test',$
    ytitle = 'y title test';,position = position
draw_color_scale,range=[minimum,maximum],log = 1,yticks=10



end