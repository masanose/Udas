;+
;NAME:
; thm_ui_set_cursor
;
;PURPOSE:
;  Replaces the cursor on non-windows machines
;    
;CALLING SEQUENCE:
; thm_ui_set_cursor,win
; 
;INPUT:
; win: an IDLgrWindow
;
;OUTPUT:
; 
;HISTORY:
;$LastChangedBy: pcruce $
;$LastChangedDate: 2009-04-21 09:43:43 -0700 (Tue, 21 Apr 2009) $
;$LastChangedRevision: 5703 $
;$URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/thmsoc/tags/tdas_5_21/idl/themis/thm_ui_new/utilities/thm_ui_set_cursor.pro $
;
;------------------------------------------------------------------------------
pro thm_ui_set_cursor,win

  ;cursor_string = strarr(16,16)
  cursor_in = strarr(16)
  
  cursor_in = [ $  
        '     # # #      ', $  
        '   #   .   #    ', $  
        '       #        ', $  
        ' #     .     #  ', $  
        '       #        ', $  
        '#      .      # ', $  
        '      ###       ', $  
        '#.#.#.#.#.#.#.# ', $  
        '      ###       ', $  
        '#      .      # ', $  
        '       #        ', $  
        ' #     .     #  ', $  
        '       #        ', $  
        '  #    .    #   ', $  
        '     # # #      ', $  
        '                ']  
  
  
 ; cursor_string[*] = ' '
  ;cursor_string[*,6] = '.'
  ;cursor_string[*,8] = '.'
  ;cursor_string[6,*] = '.
  ;cursor_string[8,*] = '.'
  ;cursor_string[indgen(8)*2,7] = '#'
;  cursor_string[7,indgen(8)*2] = '#'
;  cursor_string[7,*] = '#'
;  cursor_string[*,7] = '#'
 ; cursor_string[6:8,6:8] = '#'
 ; cursor_string[7,7] = ' '
  
  ;for i = 0,15 do begin
  ;  cursor_in[i] = strjoin(cursor_string[*,i])
  ;endfor
  
  image = create_cursor(cursor_in,hotspot=hotspot,mask=mask)
 ; print,hotspot

 hotspot = [7,8]
  
  win->setCurrentCursor,image=image,hotspot=hotspot,mask=mask
  
end