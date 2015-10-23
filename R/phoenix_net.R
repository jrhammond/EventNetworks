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
#'
#'  @return master_networks a LIST object containing daily event-networks.
#'
#'  @keywords phoenix, event data
#'

phoenix_net <- function(start_date, end_date, level){

  require(data.table)
  require(countrycode)
  require(reshape2)
  require(statnet)
  require(tsna)
  require(plyr)
  require(lubridate)


  ######
  #
  # Set up some initial values
  #
  ######

  ## Date objects
  if (class(start_date) %in% c('numeric', 'integer')
      | class(end_date) %in% c('numeric', 'integer')){
    start_date <- as.Date(ymd(start_date))
    end_date <- as.Date(ymd(end_date))
  }
  dates <- seq.Date(start_date, end_date, by = 'day')
  dates <- as.integer(format(dates, '%Y%m%d'))

  ## Actors (default to 255 ISO3C state codes)
  actors <- countrycode::countrycode_data$iso3c
  actors <- as.factor(sort(actors))
  n <- length(actors)

  ## Set up column headers (raw files are headless) and date range
  headers <- c('eventid', 'date', 'year', 'month', 'day'
               , 'sourceactorfull', 'sourceactorentity', 'sourceactorrole'
               , 'sourceactorattribute', 'targetactorfull', 'targetactorentity'
               , 'targetactorrole', 'targetactorattribute', 'eventcode'
               , 'eventrootcode', 'pentaclass', 'goldsteinscore', 'issues'
               , 'lat', 'long', 'locationname', 'statename', 'countrycode'
               , 'sentenceid', 'urls', 'newssources')

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

  # Storage for daily Phoenix files
  master_data <- vector('list', length(dates))
  names(master_data) <- as.character(dates)

  # Storage for daily network objects
  master_networks <- vector('list', length(codes))
  names(master_networks) <- paste0('code', codes)

  ######
  #
  # Download raw files from Phoenix data set and combine into one
  #  large data table. This avoids accidental double-counting of
  #  events that happen on day A but are reported on day B.
  #
  ######

  for(date in dates){
    temp <- tempfile()
    try(
      {
        download.file(paste0(
          'https://s3.amazonaws.com/openeventdata/current/events.full.'
          , date, '.txt.zip'),temp)
        data <- data.table(read.table(unz(temp, paste('events.full.', date
                                                      , '.txt', sep = ''))
                                      , header = F, fill = T, sep = '\t'
                                      , quote = "", na.string = ''
                                      , stringsAsFactor = F))
        setnames(data, headers)
        master_data[as.character(date)] <- list(data)
      }
    , silent = T
    )
    unlink(temp)
  }

  ## Convert list of data.tables to single large table
  master_data <- data.table(rbindlist(master_data))

  ######
  #
  # Pre-format data by de-duplicating, cleaning dates and actors,
  # and dropping unused columns
  #
  ######

  ## Coerce eventrootcode/eventcode columns to numeric - there are a few typos
  master_data[, eventrootcode := as.numeric(eventrootcode)]
  master_data <- master_data[!is.na(eventrootcode)]
  master_data[, eventcode := as.numeric(eventcode)]
  master_data <- master_data[!is.na(eventcode)]

  ## Subset events and columns: only events that:
  # 1. involve specified actor set on both side (as ENTITIES)
  # 2. involve TWO DIFFERENT actors (i.e. no self-interactions)
  # and only USEFUL columns
  master_data <- master_data[(sourceactorentity %in% actors
                              & targetactorentity %in% actors)
                          & (sourceactorrole %in% 'GOV'
                             | is.na(sourceactorrole))
                          & (targetactorrole %in% 'GOV'
                             | is.na(targetactorrole))
                          & sourceactorentity != targetactorentity
                          , list(date, sourceactorentity, targetactorentity
                                , eventrootcode, eventcode)]
  setkeyv(master_data, c('date', 'sourceactorentity'
                         , 'targetactorentity', 'eventrootcode'))

  ## Subset columns: drop unused event column
  if(level == 'rootcode'){
    master_data[, eventcode := NULL]
  } else {
    master_data[, eventrootcode := NULL]
  }

  ## Set names to generic
  setnames(master_data, c('date', 'actora', 'actorb', 'code'))

  ## Subset events: drop duplicated events/days/actors
  master_data <- master_data[!duplicated(master_data)]

  ## Subset events: keep only events within date range
  master_data <- master_data[date %in% dates]
  master_data <- data.table(master_data)

  ## Set CAMEO coded event/root codes to factors
  master_data[, code := factor(code, levels = codes)]

  ## Set actor codes to factors
  master_data[, actora := factor(actora, levels = levels(actors))]
  master_data[, actorb := factor(actorb, levels = levels(actors))]

  ######
  #
  # For each time period in the specified range, subset the master data set,
  #  convert interactions to network ties, and turn the resulting edgelist
  #  into a network object. Save networks to a master list object.
  #
  ######


  for(this_code in codes){

    ## Subset by code
    event_data <- master_data[code %in% this_code]

    ## Create temporary storage list for code/day networks
    code_networks <- vector('list', length(dates))
    names(code_networks) <- paste0('date', dates)

    for(today in dates){
      ## Pull today's network multiplex
      daily_data <- event_data[date %in% today]

      ## Initialize the network size and characteristics
      event_net <- network.initialize(n = n, directed = T, loops = F)
      network.vertex.names(event_net) <- levels(actors)

      ## Add edges based on dyadic ties
      add.edges(event_net, tail = daily_data$actora, head = daily_data$actorb)

      ## Store in network list
      code_networks[paste0('date', today)] <- list(event_net)
    }

    ## Collapse to TSNA dynamic-network object
    temporal_codenet <- networkDynamic(network.list = code_networks
                                       , onsets = dates, termini = dates
                                       , verbose = F)

    ## Store list in master network list
    master_networks[paste0('code', this_code)] <- list(temporal_codenet)
  }

  return(master_networks)
}

