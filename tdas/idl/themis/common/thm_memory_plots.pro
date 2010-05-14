;+
;NAME:
; thm_memeory_plots
;PURPOSE:
; the plots show the number of raw data packets in satellite memory
;CALLING SEQUENCE:
; thm_memory_plots,date=date,dur=dur
;KEYWORDS:
; date = the start date for the plots
; dur = duration for the plots
; nopng = if set, do not create a png file
; directory = if set, put the answer in this directory, otherwise put
;             it in the local working directory
; mode = 'survey' or 'burst', only can be used for /nopng, the default
;        is to plot both
;HISTORY:
; 19-dec-2007, from Andreas Kieling
; 9-jan-2008, jmm, Added directory keyword
;$LastChangedBy: jimm $
;$LastChangedDate: 2008-03-07 14:09:37 -0800 (Fri, 07 Mar 2008) $
;$LastChangedRevision: 2461 $
;$URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/thmsoc/tags/tdas_5_21/idl/themis/common/thm_memory_plots.pro $
;
Pro thm_memory_plots, date = date, dur = dur, nopng = nopng, $
                      directory = directory, mode = mode, _extra = _extra

  del_data, '*'

  If(keyword_set(directory)) Then Begin
    dir = directory             ;slash check
    ll = strmid(dir, strlen(dir)-1, 1)
    If(ll Ne '/' And ll Ne '\') Then dir = dir+'/'
  Endif Else dir = './'

  If(Not keyword_set(date)) Then date = time_string(systime(/sec), /date_only)
  If(not keyword_set(dur)) Then dur = 1 ; day

  timespan, date, dur, /day

  tplot_options, 'xmargin', [ 18, 10]
  tplot_options, 'ymargin', [ 5, 5]

  thm_load_hsk, probe = 'a'
  thm_load_hsk, probe = 'b'
  thm_load_hsk, probe = 'c'
  thm_load_hsk, probe = 'd'
  thm_load_hsk, probe = 'e'

  ylim, 'th?_hsk_issr_survey_raw', 0, 30000, 0
  ylim, 'th?_hsk_issr_burst_raw', 0, 25000, 0
  title = 'P5, P1, P2, P3, P4 (TH-A,B,C,D,E)'
  If(not keyword_set(nopng)) Then Begin
    dur_str = strcompress(/remove_all, string(dur))
    date_str = time_string(time_double(date), /date_only)
    tplot, 'th?_hsk_issr_survey_raw', title = title
    makepng, dir+date_str+'-'+dur_str+'days-memory-survey'
    tplot, 'th?_hsk_issr_burst_raw', title = title
    makepng, dir+date_str+'-'+dur_str+'days-memory-burst'
  Endif Else Begin
    If(keyword_set(mode)) Then Begin
      xmode = strcompress(strlowcase(mode), /remove_all)
      case xmode of
        'burst': tplot, 'th?_hsk_issr_burst_raw', title = title
        'survey':tplot, 'th?_hsk_issr_survey_raw', title = title
        else:tplot, 'th?_hsk_issr_survey_raw', title = title
      endcase
  Endif Else Begin
      window, 0, xs = 560, ys = 660
      tplot, 'th?_hsk_issr_survey_raw', title = title, window = 0
      window, 1, xs = 560, ys = 660
      tplot, 'th?_hsk_issr_burst_raw', title = title, window = 1
    Endelse
  Endelse

  Return

end
