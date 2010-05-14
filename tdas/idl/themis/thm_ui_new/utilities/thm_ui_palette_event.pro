PRO THM_UI_PALETTE_EVENT, tlb, winId, color
   Device, Decomposed=1
   !P.Color = !P.Color < (!D.Table_Size - 2)
   color = PickColor(!P.Color, Group_Leader=tlb, Cancel=cancelled)
   result=color24(reform(color))
   IF NOT cancelled THEN BEGIN
      TVLCT, color, !P.Color
      WSet, winId
      xBox = [1, 1, 1, 49, 49]
      yBox = [0.3, 0.3, 19, 19,0.3]
      POLYFILL, xBox, yBox, color=result, /Device
   ENDIF
   color = reform(color)
END  

