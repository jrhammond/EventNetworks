#'
#' Convert Phoenix event data to daily event-networks.
#'
#'  Take event-level data and convert it into
#'  networks of interaction by time period. Output is in
#'  the form of a nested list object where each element is
#'  an R network object. These networks can then be processed
#'  and analyzed.
#'
#'  @param start_date start date of time period as Ymd-format integer (ex:
#'          June 1, 2014 as 20140601).
#'  @param end_date end date of time period as Ymd-format integer (ex:
#'          June 1, 2014 as 20140601).
#'  @param level level of event granularity ('eventcode' or 'rootcode').
#'          'Eventcode' creates a network for each of the 226 sub-codes in
#'          CAMEO. 'Rootcode' creates a network for each of the 20 event
#'          root codes in CAMEO.
#'  @param phoenix_loc folder containing Phoenix data sets as daily .csv
#'          data tables. Automatically checks for new data sets each time
#'          the function is run, and downloads new daily data as it becomes
#'          available. Currently in 'one-and'done' format
#'          where it downloads the first time, and checks thereafter.
#'  @param icews_loc folder containing ICEWS data sets as daily .tab data
#'          tables. Because I don't know how to work a SWORD API, these will
#'          need to be manually downloaded and updated.
#'  @param datasource source of event data ('phoenix', 'icews', or 'both').
#'          Corresponds to the data source used to gather raw data.
#'          Currently defaults to 'both', as Phoenix archives  only go
#'          back to mid-2014. Currently not used.
#'
#'  @return master_networks a LIST object containing daily event-networks.
#'
#'  @keywords phoenix, event data
#'
#'  @import data.table
#'  @import countrycode
#'  @import reshape2
#'  @import statnet
#'  @import tsna
#'  @import plyr
#'  @import lubridate
#'
#'  @export


phoenix_net <- function(start_date, end_date, level, phoenix_loc, icews_loc, datasource = 'both'){

  ######
  #
  # Set up some initial values
  #
  ######

  ## Date objects
  if (class(start_date) %in% c('numeric', 'integer')
      | class(end_date) %in% c('numeric', 'integer')){
    start_date <- as.Date(lubridate::ymd(start_date))
    end_date <- as.Date(lubridate::ymd(end_date))
  }
  dates <- seq.Date(start_date, end_date, by = 'day')

  ## Actors (default to 255 ISO3C state codes)
  actors <- countrycode::countrycode_data$iso3c
  actors <- as.factor(sort(actors))
  n <- length(actors)

  ## Factor variables describing CAMEO categories
  if(level == 'rootcode'){
    codes <- factor(1:20)
    levels(codes) <- as.character(1:20)
  } else{
    codes <- factor(1:298)
    levels(codes) <- as.character(
      c(10:21, 211:214, 22:23, 231:234, 24, 241:244, 25, 251:256, 26:28, 30:31
        , 311:314, 32:33, 331:334, 34, 341:344, 35, 351:356, 36:46, 50:57
        , 60:64, 70:75, 80:81, 811:814, 82:83, 831:834, 84, 841:842, 85:86
        , 861:863, 87, 871:874, 90:94, 100:101, 1011:1014, 102:103, 1031:1034
        , 104, 1041:1044, 105, 1051:1056, 106:108, 110:112, 1121:1125, 113:116
        , 120:121, 1211:1214, 122, 1221:1224, 123, 1231:1234, 124, 1241:1246
        , 125:129, 130:131, 1311:1313, 132, 1321:1324, 133:138, 1381:1385
        , 139:141, 1411:1414, 142, 1421:1424, 143, 1431:1434, 144, 1441:1444
        , 145, 1451:1454, 150:155, 160:162, 1621:1623, 163:166, 1661:1663
        , 170:171, 1711:1712, 172, 1721:1724, 173:176, 180:182, 1821:1823, 183
        , 1831:1834, 184:186, 190:195, 1951:1952, 196, 200:204, 2041:2042)
      )
  }

  ######
  #
  # Set up some empty storage objects
  #
  ######

  # Storage for daily network objects
  master_networks <- vector('list', length(codes))
  names(master_networks) <- paste0('code', codes)

  # Storage for comparison of Phoenix and ICEWS reporting overlap
  filler <- rep(NA, length(dates))
  sources_overlap <- data.table(date = dates
                                , total_events = filler
                                , phoenix_only = filler
                                , icews_only = filler
                                , both_sources = filler)

  ######
  #
  # Download raw files from Phoenix data repo and ICEWS dataverse.
  #
  ######

  ## Download new Phoenix data tables. This will download the entire
  ##  archive the first time this function is run and fully populate
  ##  the destination folder.

  ## NOTE: This currently requires a clumsy step where it reinstalls phoxy
  ##      every time the code is run. This should be cleaned up, but I'm not
  ##      100% sure how to do so in a way that's both accurate and polite.
  message('Checking Phoenix data...')
#   devtools::install_github('jrhammond/phoxy')
  library(phoxy)
  phoxy::update_phoenix(destpath = phoenix_loc, phoenix_version = 'current')

  ## Check to see if ICEWS folder exists and that it has at least one 'valid'
  ##  ICEWS data table stored.
  message('Checking ICEWS data...')
  icews_checkfile <- 'events.2000.20150313082808.tab'
  icews_files <- list.files(icews_loc)
  if(!icews_checkfile %in% icews_files){
    stop('Please enter a valid path that contains the ICEWS yearly files.')
  } else {
    message('ICEWS file location is valid.')
  }

  ######
  #
  # Read and parse ICEWS data for merging.
  #
  ######

  ## Read and parse ICEWS data
  message('Ingesting ICEWS data...')
  icews_data <- phoxy::ingest_icews(icews_loc, start_date, end_date)

  ## Clean ICEWS data and format to Phoenix-style CAMEO codes
  ##  for actors and states
  message('Munging ICEWS data...')
  icews_data <- icews_cameo(icews_data)

  ## Subset ICEWS data to only keep key columns
  icews_data <- icews_data[, list(date, sourceactorentity
                                  , targetactorentity, rootcode
                                  , eventcode)]
  icews_data[, source := 'icews']

  ######
  #
  # Read and parse Phoenix data for merging.
  #
  ######

  ## Read and parse Phoenix data
  message('Ingesting Phoenix data...')
  phoenix_data <- ingest_phoenix(phoenix_loc = phoenix_loc, start_date = start_date, end_date = end_date)

  ## Subset Phoenix data to only keep key columns
  phoenix_data <- phoenix_data[, list(date, sourceactorentity
                                      , targetactorentity, rootcode
                                      , eventcode)]
  phoenix_data[, source := 'phoenix']

  ## Coerce Phoenix rootcode/eventcode columns to numeric - there are a few typos
  phoenix_data[, rootcode := as.integer(rootcode)]
  phoenix_data <- phoenix_data[!is.na(rootcode)]
  phoenix_data[, eventcode := as.numeric(eventcode)]
  phoenix_data <- phoenix_data[!is.na(eventcode)]

  ######
  #
  # Combine ICEWS and Phoenix data
  #
  ######

  master_data <- rbind(icews_data, phoenix_data)

  ######
  #
  # Pre-format data by de-duplicating, cleaning dates and actors,
  # and dropping unused columns
  #
  ######

  ## Subset events: keep only events within date range
  master_data <- master_data[date %in% dates]

  ## Subset events and columns: only events that:
  ##  1. involve specified actor set on both side (as ENTITIES)
  ##  2. involve TWO DIFFERENT actors (i.e. no self-interactions)
  master_data <- master_data[(sourceactorentity %in% actors
                              & targetactorentity %in% actors)
                          | (sourceactorentity %in% paste0(actors, 'GOV')
                             & targetactorentity %in% paste0(actors, 'GOV'))
                          | (sourceactorentity %in% paste0(actors, 'MIL')
                           & targetactorentity %in% paste0(actors, 'MIL'))]
  master_data <- master_data[substr(sourceactorentity, 1, 3) != substr(targetactorentity, 1, 3)]

  ## Subset columns: drop unused event column
  if(level == 'rootcode'){
    master_data[, eventcode := NULL]
  } else {
    master_data[, rootcode := NULL]
  }

  ## Set names to generic
  setnames(master_data, c('date', 'actora', 'actorb', 'code', 'source'))

  ## Set actor codes to state-level factors
  ## NOTE: this removes differentiation of actors within states (ex: gov vs mil)
  ##  and as such should probably be moved to an argument in the future.
  master_data[, actora := factor(substr(actora, 1, 3), levels = levels(actors))]
  master_data[, actorb := factor(substr(actorb, 1, 3), levels = levels(actors))]

  ## Set CAMEO coded event/root codes to factors
  master_data[, code := factor(code, levels = codes)]

  ## Subset events: drop duplicated events/days/actors
  master_data <- master_data[!duplicated(master_data)]

  ## Set keys
  setkeyv(master_data, c('date', 'actora', 'actorb', 'code', 'source'))

  ######
  #
  # Export : how much overlap between Phoenix and ICEWS reporting?
  #
  ######

  ## Create some temporary flag variables
  master_data[, dup_fromtop := duplicated(
    master_data[, list(date, actora, actorb, code)])]
  master_data[, dup_frombot := duplicated(
    master_data[, list(date, actora, actorb, code)], fromLast = T)]

  ## Export data on reporting overlap
  sources_overlap[, phoenix_only := master_data[
    , sum(dup_fromtop == F & source == 'phoenix'), by = date][,V1]]
  sources_overlap[, icews_only := master_data[
    , sum(dup_frombot == F & source == 'icews'), by = date][,V1]]
  sources_overlap[, both_sources := master_data[, sum(dup_fromtop == T), by = date][, V1]]
  sources_overlap[, total_events := sum(phoenix_only, icews_only, both_sources), by = date]

  ## Drop flags and source variable
  master_data[, dup_fromtop := NULL]
  master_data[, dup_frombot := NULL]
  master_data[, source := NULL]

  ## Drop duplicated variables
  master_data <- unique(master_data)

  ######
  #
  # For each time period in the specified range, subset the master data set,
  #  convert interactions to network ties, and turn the resulting edgelist
  #  into a network object. Save networks to a master list object.
  #
  ######

  dates_ref <- as.integer(format(dates, '%Y%m%d'))
  for(this_code in codes){

    ## Subset by root/event code
    event_data <- master_data[code %in% this_code]

    ## Create temporary storage list for code/day networks
    code_networks <- vector('list', length(dates))
    names(code_networks) <- paste0('date', dates_ref)

    for(today in dates){
      date_ref <- dates_ref[which(dates %in% today)]
      ## Pull today's network multiplex
      daily_data <- event_data[date %in% today]

      ## Initialize the network size and characteristics
      event_net <- network.initialize(n = n, directed = T, loops = F)
      network.vertex.names(event_net) <- levels(actors)

      ## Add edges based on dyadic ties
      add.edges(event_net, tail = daily_data$actora, head = daily_data$actorb)

      ## Store in network list
      code_networks[paste0('date', date_ref)] <- list(event_net)
    }

    ## Collapse to TSNA dynamic-network object
    temporal_codenet <- networkDynamic(network.list = code_networks
                                       , onsets = dates_ref
                                       , termini = dates_ref
                                       , verbose = F)

    ## Store list in master network list
    master_networks[paste0('code', this_code)] <- list(temporal_codenet)
  }

  return(master_networks)
}
