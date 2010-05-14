;+
;Function: goes_config_filedir.pro
;Purpose: Get the applications user directory for THEMIS data analysis software
;
;$LastChangedBy: jwl $
;$LastChangedDate: 2009-04-10 09:49:50 -0700 (Fri, 10 Apr 2009) $
;$LastChangedRevision: 5595 $
;$URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/ssl_general/tags/tdas_5_21/missions/goesmag/goes_config_filedir.pro $
;-

Function goes_config_filedir, app_query = app_query, _extra = _extra

  readme_txt = ['Directory for configuration files for use by ', $
                'the THEMIS Data Analysis Software']

  If(keyword_set(app_query)) Then Begin
    tdir = app_user_dir_query('themis', 'goes_config', /restrict_os)
    If(n_elements(tdir) Eq 1) Then tdir = tdir[0] 
    Return, tdir
  Endif Else Begin
    Return, app_user_dir('themis', 'THEMIS Configuration Process', $
                         'goes_config', $
                         'THEMIS configuration Directory', $
                         readme_txt, 1, /restrict_os)
  Endelse

End