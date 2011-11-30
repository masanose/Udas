;+
;PROCEDURE: IUG_CRIB_GMAG_WDC
;    A sample crib sheet that explains how to use the "iug_cirb_gmag_wdc"
;    procedure. You can run this crib sheet by copying & pasting each
;    command below (except for stop and end) into the IDL command line.
;    Or alternatively compile and run using the command:
;        .run iug_crib_gmag_wdc
;
;Written by: Y. KOYAMA, Aug 9,2011
;Last Updated: Y. KOYAMA, Oct 6,2011
;-

;Initialize
thm_init

;Set the date and time for loading data
timespan, '2007-01-22',2

;Load geomagnetic observation data at Kakioka observatory.
iug_load_gmag_wdc, site='kak', resolution='min'

;List the loaded data names
tplot_names

;Set the title of the plot 
tplot_options, 'title', 'Sample plot of iug_crib_gmag_wdc'

;Plot data
tplot,'wdc_mag_kak_1min'

end
