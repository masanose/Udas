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
url_fits = ['http://idlastro.gsfc.nasa.gov/ftp/pro/fits/',$
            'http://idlastro.gsfc.nasa.gov/ftp/pro/fits/',$
            'http://idlastro.gsfc.nasa.gov/ftp/pro/fits/',$
            'http://idlastro.gsfc.nasa.gov/ftp/pro/fits/',$
            'http://idlastro.gsfc.nasa.gov/ftp/pro/fits/',$
            'http://idlastro.gsfc.nasa.gov/ftp/pro/fits/',$
            'http://idlastro.gsfc.nasa.gov/ftp/pro/misc/',$
            'http://idlastro.gsfc.nasa.gov/ftp/pro/misc/']
fname_fits = ['fits_close.pro', 'fits_open.pro', 'fits_read.pro', $
            'sxaddpar.pro', 'sxdelpar.pro', 'sxpar.pro', 'gettok.pro', $
            'valid_num.pro']

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
    local_data_dir = dir_fits + 'fitslib/'
    print,'Downloading fits library'
    files = file_retrieve(fname_fits, local_data_dir=local_data_dir, remote_data_dir=url_fits)
endelse

end
