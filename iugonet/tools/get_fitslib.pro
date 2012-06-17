;+
; NAME:
;   GET_FITSLIB
;
; PURPOSE:
;   To download FITS library needed for loading IPRT data.
;    
; Example:
;   .run get_fitslib
;
; Written by Y.-M. Tanaka, June 15, 2012 (ytanaka at nipr.ac.jp)
;-

thm_init

;----- Set URLs -----;
url_fits1 = 'http://idlastro.gsfc.nasa.gov/ftp/pro/fits/'
url_fits2 = 'http://idlastro.gsfc.nasa.gov/ftp/pro/misc/'
; dir_fits_def = './'

;----- Select directory to install tdas -----;
dir_fits=dialog_pickfile(/directory, $
             title='Select directory to download fits library.', $
             path=dir_fits_def)
print, 'dir_fits = ', dir_fits

if(dir_fits eq '') then begin
    print, 'Directory was not selected.'
endif else begin
    ;----- Download tdas -----;
    source = file_retrieve(/struct)

    source.local_data_dir = dir_fits + 'fitslib/fits/'
    source.remote_data_dir = url_fits1
    print,'Downloading fits library from ' + url_fits1
    files = file_retrieve('*.pro', _extra=source)

    source.local_data_dir = dir_fits + 'fitslib/misc/'
    source.remote_data_dir = url_fits2
    print,'Downloading fits library from ' + url_fits2
    files = file_retrieve('*.pro', _extra=source)
endelse

end
