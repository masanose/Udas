pro gui_update

tdas_dir = 'D:\data\tdas\tdas_5_21\'
up_dir ='C:\user\kagi\iugonet\git\udas.git\thm_ui_load_data_file\'


file = 'thm_ui_calendar.pro'
file_copy,up_dir+file $
         ,tdas_dir+'idl\themis\thm_ui_new\utilities\'+file,/over

file = 'thm_ui_call_sequence__define.pro'
file_copy,up_dir+file $
         ,tdas_dir+'idl\themis\thm_ui_new\objects\'+file,/over

file = 'thm_ui_init_load_window.pro'
file_copy,up_dir+file $
         ,tdas_dir+'idl\themis\thm_ui_new\panels\thm_ui_load_data_file\'+file,/over

file = 'thm_ui_load_iugonet_data.pro'
file_copy,up_dir+file $
         ,tdas_dir+'idl\themis\thm_ui_new\panels\thm_ui_load_data_file\'+file,/over

file = 'thm_ui_load_iugonet_data_load_pro.pro'
file_copy,up_dir+file $
         ,tdas_dir+'idl\themis\thm_ui_new\panels\thm_ui_load_data_file\'+file,/over

print,'update complete'

end