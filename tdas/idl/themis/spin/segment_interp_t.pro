pro segment_interp_t,segment,t,spincount,t_last,spinphase,spinper
  if (t LT segment.t1) then begin
    dt = segment.t1 - t
    spinper=360.0D/segment.b
    fracspins = dt/spinper
    intspins = ceil(fracspins)
    spinphase = (intspins-fracspins)*360.0D
    spincount = segment.c1 - intspins
    t_last = segment.t1 - intspins*spinper
  endif else if (t GT segment.t2) then begin
    dt = t-segment.t2
    bp = segment.b + 2.0D*segment.c*(segment.t2 - segment.t1)
    spinper = 360.0D/bp
    fracspins = dt/spinper
    intspins = floor(fracspins)
    spinphase = (fracspins-intspins)*360.0D
    spincount = segment.c2 + intspins
    t_last = segment.t2 + intspins*spinper
  endif else begin
    dt = t-segment.t1
    phi = (segment.b*dt + segment.c*dt*dt)
    bp = segment.b + 2.0D*segment.c*dt
    spinper = 360.0D/bp
    spinphase = phi mod 360.0D
    fracspins = phi/360.0D
    spincount = floor(fracspins)
    phi_lastpulse = spincount*360.0D
    if (abs(segment.c) LT 1.0D-12) then begin
       dt = phi_lastpulse/segment.b 
    endif else begin
       b = segment.b
       c = segment.c
       dt = (-b + sqrt(b*b - 4.0D*c*(-phi_lastpulse)))/(2.0D*c)
    endelse
    t_last = segment.t1 + dt;
    spincount = spincount+segment.c1 
  endelse
end
