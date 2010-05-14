;+
;NAME:
;  bitplot
;
;INPUT: (optional if DATA keyword is set)
;    x: array of x values
;    y: array of y values
;
;PURPOSE:
;  Plots 'ON' bits for housekeeping type data.
;  Can be used by "tplot".
;  See "_tplot_example" and "_get_example_dat" for an example.
;
;KEYWORDS:
;    PSYMS: array of IDL plot psym values corresponding to each bit.
;          OVERPLOT: create plot without erasing previous plot.
;    DI: value to be given to first bit in plot.  Default is 0.
;    LIMITS: TPLOT limits structure corresponding to the variable plotted.
;    DATA: TPLOT data structure corresponding to the variable plotted.
;    NUMBITS: the number of bits that will be plot
;    SYMSIZE: set the size of the symbol
;
; $LastChangedBy: pcruce $
; $LastChangedDate: 2008-02-28 19:13:34 -0800 (Thu, 28 Feb 2008) $
; $LastChangedRevision: 2428 $
; $URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/ssl_general/tags/tdas_5_21/tplot/bitplot.pro $
;-
pro bitplot,x,y,psyms=psyms,overplot=overplot,di=di,limits=lim,data=data,numbits=nb,symsize=symsize
if keyword_set(data) then begin
  x = data.x
  y = data.y
  extract_tags,stuff,data,except=['x','y','dy']
endif
extract_tags,stuff,lim
extract_tags,plotstuff,stuff,/plot
extract_tags,oplotstuff,stuff,/oplot
str_element,lim,'numbits',nb
str_element,stuff,'labels',val=labels
str_element,lim,'psyms',psyms
str_element,lim,'symsize',symsize
labsize = 1.
str_element,stuff,'labsize',val=labsize
str_element,stuff,'colors',colors
chsize = !p.charsize
if not keyword_set(chsize) then chsize = 1.

if not keyword_set(nb) then begin
case size(/type,y) of
  1:  nb = 8
  2:  nb = 16
  3:  nb = 32
  12: nb = 16
  13: nb = 32
  14: nb = 64
  15: nb = 64
  else: nb = 0
endcase
endif
if n_elements(di) eq 0 then di = 0
if not keyword_set(psyms) then psyms = replicate(3,nb)
npsyms = n_elements(psyms)
if not keyword_set(overplot) then $
   plot,/nodata,x,y,yrange=[-1+di,nb+di],/ystyle,_extra=plotstuff

if not keyword_set(symsize) then begin
   symsize = 1.0
endif

if n_elements(colors) ne 0 then col = get_colors(colors)  $
else col = !p.color
ncol = n_elements(col)

bit = 1l
for i=0,nb-1 do begin
  ind = where(y and bit,c)
  if c ne 0 then $
     oplot,x(ind),replicate(i+di,n_elements(ind)),psym=psyms(i mod npsyms),col=col[i mod ncol],symsize=symsize
  bit = bit * 2
endfor

if keyword_set(labels) then begin
   charsize = !p.charsize
   if charsize eq 0 then charsize = 1.
   nlab = n_elements(labels)
   yp = indgen(nlab) + di
   xp = replicate(!x.crange(1),nlab)
   yw = !y.window
   xw = !x.window
   if not keyword_set(lbsize) then $
     lbsize = charsize < (yw(1)-yw(0))/(nlab+1) *!d.y_size/!d.y_ch_size $
   else lbsize = lbsize*charsize
   xyouts,xp,yp,"  "+labels,charsize=lbsize,color= col[indgen(nlab) mod ncol]
endif

end