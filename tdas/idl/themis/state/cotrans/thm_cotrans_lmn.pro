; +
; NAME: THM_COTRANS_LMN
; 
; 
; , name_in, name_out, time, IMF, probe=probein, GSM=GSM, GSE=GSE, _Extra=ex
; 
; PURPOSE:
; 
; INPUTS (same as COTRANS.PRO):
;   name_in: data in the input coordinate system (t-plot variable name, or
;            array)
;   name_out: variable name for output (t-plot variable name, or array)
;   time: (optional) array of times for input values, if provided then the
;         first parameter is an array, and the second parameter is a named
;         variable to contain the output array.
;         
; KEYWORDS:
;   probe = Probe name. The default is 'all', i.e., load all available probes.
;           This can be an array of strings, e.g., ['a', 'b'] or a string
;           delimited by spaces, e.g., 'a b'
;   /GSM: Set to indicate input data is in GSM coordinates.
;   /GSE: Set to indicate input data is in GSE coordinates.   
; 
; NOTES:
;   Syntax: just like the function COTRANS.PRO
;       method 1: use tplot names, example:
;               thm_cotrans_lmn, 'tha_fgs_gse', 'tha_fgs_lmn'
;               then we can get a tplot name 'tha_fgs_lmn' to tplot or do other things
;       method 2: use variables, example:
;               thm_cotrans_lmn, gsedata, lmndata, time, probe='a', /gse
;               note time matched to the input data (gsedata here), the spacecraft which gets this
;               data, and the coordinate of the input data (/gse here) should be specified. and
;               the output (lmndata) is also matched to the variable time.
;    Use _EXTRA keyword to pass keywords to SOLARWIND_LOAD.PRO
;        resol - SW resolution in seconds
;        hro - use SW HRO database with default 1 min resolution
;        /hro,/min5 - use SW HRO database with 5 min resolution
;        h1 - use OMNI-2 1 hour SW database
;        wind - use WIND SW data
;               
; by Liu Jiang
; original edition: 09/21/2007
; latest edition: 09/21/2007
; by Vladimir Kondratovich:
; 2007/12/28 modified calls to take into account low-level changes. New SW keywords:
; 	resol - SW resolution in seconds
; 	hro - use SW HRO database with default 1 min resolution
; 	/hro,/min5 - use SW HRO database with 5 min resolution
; 	h1 - use OMNI-2 1 hour SW database
; 	wind - use WIND SW data

pro thm_cotrans_lmn, name_in, name_out, time, probe=probein, GSM=GSM, GSE=GSE, _Extra=ex

    if n_params() eq 2 then begin
    ; get the data using t-plot name
        get_data, name_in, data=data_in, limit=l_in, dl=dl_in ; krb
        data_in_coord = cotrans_get_coord(dl_in) ; krb
        ;probe = probein ; to be commended
        probe = strmid(dl_in.cdf.gatt.source_name,2,1)
    endif else begin
        data_in = {x:time, y:name_in}
        probe = probein
        data_in_coord = 'unknown'
    endelse
    ; examine if data input is correct
    if strmatch(data_in_coord,'unknown') && ~ keyword_set(GSE) && ~ keyword_set(GSM) then begin
        print, 'Please specify the coordinate of input data.'
        return
    endif

    if keyword_set(GSE) then begin
        if ~ strmatch(data_in_coord, 'unknown') && ~ strmatch(data_in_coord,'gse') then begin
           print, 'coord of input '+name_in+': '+data_in_coord+'must be GSE'
           return
        end
        data_in_coord = 'gse'
    end
    if keyword_set(GSM) then begin
        if ~ strmatch(data_in_coord, 'unknown') && ~ strmatch(data_in_coord,'gsm') then begin
           print, 'coord of input '+name_in+': '+data_in_coord+'must be GSM'
           return
        end
        data_in_coord = 'gsm'
    end
    ; transform coordinate to gsm no matter what the input coordinate is
    case 1 of
        strcmp(data_in_coord, 'gse'): begin
            sub_GSE2GSM, data_in, data_med
            print, 'The input GSE data has been transformed to GSM'
        end
        strcmp(data_in_coord, 'gsm'): begin
            data_med = data_in
            print, 'The input data has already been in GSM coordinate'
        end
        else: begin
            print, 'The input coordinate is not supported yet'
            return
        end
    endcase
    ; transform data from gsm to lmn
    thm_gsm2lmn_wrap, data_med, data_conv, probe, _Extra=ex
    out_coord = 'lmn'
    ; store transformed data
    if n_params() eq 2 then begin
        dl_conv = dl_in
        cotrans_set_coord,  dl_conv, out_coord ;krb
    ;; clear ytitle, so that it won't contain wrong info.
        str_element, dl_conv, 'ytitle', /delete
        dl_conv.labels = ['Bl', 'Bm', 'Bn']
        l_conv=l_in
        str_element, l_conv, 'ytitle', /delete

        store_data,name_out,data=data_conv, limit=l_conv, dl=dl_conv ;krb
    endif else name_out = data_conv.y
end
