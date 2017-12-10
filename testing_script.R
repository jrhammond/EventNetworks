#devtools::install_github('jrhammond/EventNetworks')
pacman::p_load(EventNetworks)
?eventNetworks

test <- EventNetworks::eventNetworks(
  start_date = 20140101
           , end_date = 20150101
           , level = 'pentaclass'
           , dv_key = '002231de-d465-401b-ac91-c2697b948694'
           , phoenix_loc = 'C:\\Users\\Jesse\\Box Sync\\DataSets\\phoenix'
           , icews_loc = 'C:\\Users\\Jesse\\Box Sync\\DataSets\\icews'
           , histphoenix_loc = 'C:\\Users\\Jesse\\Box Sync\\DataSets\\CCHPED_v2017_06_30'
           , dv_server = 'harvard.dataverse.edu'
           , update = F
           , actorset = 'states'
           , codeset = 'all'
           , time_window = 'month'
           , code_subset = 'all'
           , tie_type = 'count'
           , sources = 'all'
  )
