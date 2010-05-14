;+
;Procedure: mva_crib
;
;Purpose:  A crib on showing how to transform into minimum variance
;analysis coordinates

;Notes:
;
; $LastChangedBy: jimm $
; $LastChangedDate: 2008-05-13 18:09:41 -0700 (Tue, 13 May 2008) $
; $LastChangedRevision: 3081 $
; $URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/ssl_general/tags/tdas_5_21/cotrans/special/minvar/mva_crib.pro $
;-

del_data,'*'

;timespan,'2007-07-10/07:48:00',16,/minute
timespan,'2007-07-10/08:10:00',22,/minute

thm_load_fgm,probe='c',coord='gse'

;default call just makes a single transformation matrix that covers
;the entire interval
minvar_matrix_make,'thc_fgs_gse',tstart='2007-07-10/07:54:00',tstop='2007-07-10/07:56:30'

tvector_rotate,'thc_fgs_gse_mva_mat','thc_fgs_gse',newname='mva_data_day'

options,'mva_data_day',labels=['maxvar','midvar','minvar']
options,'mva_data_day',labflag=1

tplot,'thc_fgs_gse mva_data_day'

;used for an old time
;tlimit,'2007-05-30/10:00:00','2007-05-30/14:00:00'

print,'Heres the fgm data translated into mva coordinates using a single transformation matrix'

stop

timespan,'2007-07-10/07:30:00',1,/hour

thm_load_fgm,probe='c',coord='gse'

minvar_matrix_make,'thc_fgs_gse',twindow=600,tslide=300

tvector_rotate,'thc_fgs_gse_mva_mat','thc_fgs_gse',newname='mva_data_hour'

options,'mva_data_hour',labels=['maxvar','midvar','minvar']
options,'mva_data_hour',labflag=1

tplot,'thc_fgs_gse mva_data_hour'

print,'Heres the fgm data translated into mva coordinates using a different transformation every hour'

stop

minvar_matrix_make,'thc_fgs_gse',twindow=300,tslide=150

tvector_rotate,'thc_fgs_gse_mva_mat','thc_fgs_gse',newname='mva_data_min'

options,'mva_data_min',labels=['maxvar','midvar','minvar']
options,'mva_data_min',labflag=1



tplot,'mva_data_min'

print,'Heres the fgm data translated into mva coordinates using a different transformation every 5 minutes'

stop

tplot,'mva_data_*'

print,'Heres all 3'

end

