;Written by Larry Kepko (larry.kepko@unh.edu)

function whatindices, data, times

nn = n_elements(times)

retindis = lonarr(nn, 1)


for i = 0L, nn - 1 do begin
	retindis[i] = whatindex(data, times[i])
endfor

return, retindis

end