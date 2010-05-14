;Written by Larry Kepko (larry.kepko@unh.edu)

function whatindex, data, time

direction = 1;
tolerance = 0;

if (direction gt 0) then begin
    indi1 = where(data le time);
    indi2 = where(data ge time);
endif else begin
    indi1 = where(data ge time);
    indi2 = where(data le time);
endelse

if (indi1(0) ge 0L) and (indi2(0) ge 0L) then begin

    ;   Find which index is closer to the time we want

    dt1 = data(indi1(n_elements(indi1)-1L)) - time;
    dt2 = data(indi2(1L)) - time;

    if (tolerance gt 0L) then begin
        indi = indi2(1L);
    endif else begin
        if (abs(dt1) gt abs(dt2)) then begin
            indi = indi2(1L);
        endif else begin
            indi = indi1(n_elements(indi1) - 1L);
        endelse
    endelse

endif else if (indi1(0) < 0L) then begin

    ;
    ;   No times that are less than time we are looking for
    ;   So return the first point
    ;

    indi = 1L;

endif else if (indi2(0) lt 0L) then begin

    ;
    ;   No times greater than time they are looking for
    ;   So return last point

    indi = n_elements(data) - 1L;

endif

return, indi

end
