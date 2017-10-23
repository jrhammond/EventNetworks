#' Convert Phoenix event data to daily event-networks.
#'
#'  Take event-level data and convert it into
#'  networks of interaction by time period. Output is in
#'  the form of a nested list object where each element is
#'  an R network object. These networks can then be processed
#'  and analyzed.
#'
#' @param start_date start date of time period as Ymd-format integer (ex:
#'          June 1, 2014 as 20140601).
#' @param end_date end date of time period as Ymd-format integer (ex:
#'          June 1, 2014 as 20140601).
#' @param dv_server Dataverse server address from which to download
#'          up-to-date ICEWS data. Defaults to Harvard Dataverse at
#'          harvard.dataverse.edu.
#' @param dv_key Unique user key to access SWORD API and automatically find and
#'           download up-to-date ICEWS data.
#' @param level level of event granularity ('eventcode', 'rootcode',
#'           'pentaclass', or 'goldstein'). 'Eventcode' creates a network for
#'           each of the 226 sub-codes in CAMEO. 'Rootcode' creates a network
#'           for each of the 20 event root codes in CAMEO. 'Pentaclass' creates
#'           a network for each of the 0-4 pentaclass codes in CAMEO.
#'           'Goldstein' creates one or two networks denoting mean Goldstein
#'           scores, either aggregated positive+negative or separated into
#'           two separate networks for positive and negative Goldstein scores.
#' @param phoenix_loc folder containing Phoenix data sets as daily .csv
#'          data tables. Automatically checks for new data sets each time
#'          the function is run, and downloads new daily data as it becomes
#'          available. Currently in 'one-and'done' format
#'          where it downloads the first time, and checks thereafter.
#' @param icews_loc folder containing ICEWS data sets as daily .tab data
#'          tables. Because I don't know how to work a SWORD API, these will
#'          need to be manually downloaded and updated.
#' @param update should phoenixNet attempt to download new data? This will attempt
#'          to download any Phoenix data files that 'should' be present in the
#'          Phoenix data directory (one data file per day, from 2014-06-20 through
#'          the present day) and denote whether or not any of these files
#'          come up missing in the process.
#' @param actorset set of actors for which to create event-networks. Defaults
#'          to the 255 ISO-coded states in the international system. Specifying
#'          a specific state or set of states (as 3-character ISO codes) will
#'          extract all the 'major' actors withint that state/states.
#' @param codeset subset of event codes as specified by 'level'. This is useful
#'          if you desire to extract only a portion of interactions recorded
#'          by CAMEO, but has to align with the code aggregation specified
#'          in the 'level' argument. For example, if you specify 'rootcode',
#'          the 'codeset' you specify has to be one or more root codes between
#'          1 and 20. Defaults to 'all'.
#' @param code_subset subset of EVENTCODES that can be aggregated up to higher
#'          order interactions. For example, you might want to only look at
#'          event codes below 100, but then aggregate those event codes to
#'          rootcode or pentaclass.
#' @param time_window temporal window to build event-networks. Valid
#'          entries are 'day', 'week', 'month', 'quarter', or 'year'.
#' @param tie_type type of ties to return. Default is binarized ties where
#'          a tie represents the presence of one OR MORE interactions in the
#'          time period specified. Valid entries are 'binary', 'count'
#'          (count of events), 'meangoldstein' (mean Goldstein score),
#'          'sepgoldstein' (mean positive/negative Goldstein scores separated).
#'          NOTE: choosing a Goldstein score as tie type negates the "level"
#'          argument.
#' @param sources use only Phoenix or ICEWS data in creating event networks.
#'          Valid entries are 'phoenix', 'icews', or 'both' (default).
#'
#' @return master_networks a LIST object containing temporally referenced event-networks.
#'
#' @rdname eventNetworks
#'
#' @author Jesse Hammond
#'
#' @note This function is still in early development and may contain significant errors.
#'        Don't trust it.
#'

#' @export
#'
#' @import data.table
#' @import countrycode
#' @import reshape2
#' @import statnet
#' @import tsna
#' @import plyr
#' @import lubridate
#' @import dataverse
eventNetworks <- function(start_date
                        , end_date
                        , level
                        , dv_key
                        , phoenix_loc
                        , icews_loc
                        , dv_server = 'harvard.dataverse.edu'
                        , update = TRUE
                        , actorset = 'states'
                        , codeset = 'all'
                        , time_window = 'day'
                        , code_subset = 'all'
                        , tie_type = 'binary'
                        , sources = 'both'
                        ){

  ######
  #
  # Set up some initial values: Time windows
  #
  ######

  ## Date objects
  if (class(start_date) %in% c('numeric', 'integer')
      | class(end_date) %in% c('numeric', 'integer')){
    start_date <- as.Date(lubridate::ymd(start_date))
    end_date <- as.Date(lubridate::ymd(end_date))
  }
  dates <- seq.Date(start_date, end_date, by = 'day')
  dates <- unique(lubridate::floor_date(dates, time_window))

  ######
  #
  # Set up some initial values: Actors
  #
  ######

  ## Paste-function that can handle NA entries
  ## (http://stackoverflow.com/questions/13673894/suppress-nas-in-paste)
  paste3 <- function(...,sep=", ") {
    L <- list(...)
    L <- lapply(L,function(x) {x[is.na(x)] <- ""; x})
    ret <-gsub(paste0("(^",sep,"|",sep,"$)"),"",
               gsub(paste0(sep,sep),sep,
                    do.call(paste,c(L,list(sep=sep)))))
    is.na(ret) <- ret==""
    ret
  }

  ## Default actors: 255 ISO-coded countries
  if('states' %in% actorset){
    # Set up set of primary actor codes
    statelist <- unique(countrycode::countrycode_data$iso3c)
    statelist <- statelist[!is.na(statelist)]
    statelist <- c(statelist, 'KSV', 'IGO')
    actors <- as.factor(sort(statelist))
    n <- length(actors)

  } else {
    ## Set up set of secondary actor codes
    secondary_actors <- c('GOV', 'MIL', 'REB', 'OPP', 'PTY', 'COP', 'JUD'
                          , 'SPY', 'MED', 'EDU', 'BUS', 'CRM', 'CVL')
    statelist <- countrycode::countrycode_data$iso3c
    statelist <- statelist[!is.na(statelist)]
    actors <- unique(statelist[statelist %in% actorset])
    actors <- actors[!is.na(actors)]
    actors <- unique(as.vector(outer(actors, secondary_actors, paste, sep = '')))
    actors <- as.factor(sort(actors))
    n <- length(actors)
  }

  ######
  #
  # Set up some initial values: Event codes
  #
  ######

  ## Factor variables describing CAMEO categories
  if(level == 'rootcode'){
    codes <- factor(1:20)
    levels(codes) <- as.character(1:20)
  } else if(level == 'eventcode'){
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
  } else if(level == 'pentaclass'){
    codes <- factor(0:4)
    levels(codes) <- as.character(0:4)
  } else if(level == 'goldstein'){
    if(tie_type == 'netgoldstein'){
      codes <- 1
    } else if(tie_type == 'sepgoldstein'){
      codes = c('mean_neg_goldstein', 'mean_pos_goldstein')
    }
  }

  ## Subset of event codes
  if(!any('all' %in% codeset)){
    if(sum(!codeset %in% codes) > 0){
      message('Warning: some event codes do not match specified event class. Proceeding with valid event codes.')
    }
    codes <- codes[codes %in% codeset]
    if(length(codes) == 0){
      stop('Please enter a valid set of event codes or pentaclass values.')
    }
  }

  ######
  #
  # Set up some empty storage objects
  #
  ######

  # Storage for daily network objects
  master_networks <- vector('list', length(codes))
  if(level != 'goldstein'){
    names(master_networks) <- paste0('code', codes)
  } else{
    names(master_networks) <- codes
  }


  # Storage for comparison of Phoenix and ICEWS reporting overlap
  filler <- rep(NA, length(dates))
  sources_overlap <- data.table(date = dates
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

  if(update == T){
    message('Checking Phoenix data...')
    EventNetworks::update_phoenix(destpath = phoenix_loc)
    message('Checking ICEWS data...')
    EventNetworks::update_icews(destpath = icews_loc)
  }

  ######
  #
  # Read and parse ICEWS data for merging.
  #
  ######

  if (sources %in% c('ICEWS', 'both')){
    ## Check to see if ICEWS folder exists and that it has at least one 'valid'
    ##  ICEWS data table stored.
    years <- format(unique(lubridate::floor_date(dates, 'year')), '%Y')
    message('Checking ICEWS data...')
    icews_files <- list.files(icews_loc)
    icews_years <- ldply(strsplit(icews_files, '\\.'))$V2
    access_years <- which(icews_years %in% years)

    if(length(access_years) == 0){
      message('No ICEWS files found in the specified timespan.')
      icews_data <- data.table(date = as.Date(character())
                               , sourceactorentity = character()
                               , targetactorentity = character()
                               , rootcode = numeric()
                               , eventcode = integer()
                               , goldstein = numeric()
                               , 'source' = character())
    } else{

      ## Read and parse ICEWS data
      message('Ingesting ICEWS data...')
      icews_data <- ingest_icews(icews_loc, start_date, end_date)

      ## Clean ICEWS data and format to Phoenix-style CAMEO codes
      ##  for actors and states
      message('Munging ICEWS data...')
      icews_data <- icews_cameo(icews_data)

      ## Subset ICEWS data to only keep key columns
      icews_data <- icews_data[, list(date, sourceactorentity
                                      , targetactorentity, rootcode
                                      , eventcode, goldstein)]
      icews_data[, source := 'icews']

    }
  } else {
    icews_data <- data.table(date = as.Date(character())
                             , sourceactorentity = character()
                             , targetactorentity = character()
                             , rootcode = numeric()
                             , eventcode = integer()
                             , goldstein = numeric()
                             , 'source' = character())
  }


  ######
  #
  # Read and parse Phoenix data for merging.
  #
  ######

  ## Only parse Phoenix data if desired

  if (sources == 'ICEWS'){
    # phoenix_data <- data.table(date = as.Date(character())
    #                            , sourceactorentity = character()
    #                            , targetactorentity = character()
    #                            , rootcode = numeric()
    #                            , eventcode = integer()
    #                            , goldstein = numeric()
    #                            , 'source' = character())
    message('Only using ICEWS data.')

    #master_data <- icews_data
    master_data <- copy(icews_data)
    setnames(master_data, c('sourceactorentity', 'targetactorentity')
             , c('actora', 'actorb'))

  } else {

    if(end_date < as.Date('2014-06-20')){
      ## Only parse Phoenix data if it exists in that date range
      phoenix_data <- data.table(date = as.Date(character())
                                 , sourceactorentity = character()
                                 , targetactorentity = character()
                                 , rootcode = numeric()
                                 , eventcode = integer()
                                 , goldstein = numeric()
                                 , 'source' = character())
      message('Specified timespan ends before Phoenix data coverage begins.')

      master_data <- icews_data
      setnames(master_data, c('sourceactorentity', 'targetactorentity')
               , c('actora', 'actorb'))

    }
    else {

      ## Read and parse Phoenix data
      message('Ingesting Phoenix data...')
      phoenix_data <- ingest_phoenix(phoenix_loc = phoenix_loc
                                     , start_date = start_date
                                     , end_date = end_date)

      ## Subset Phoenix data to only keep key columns
      phoenix_data <- phoenix_data[, list(date, paste3(sourceactorentity
                                                       , sourceactorrole, sep = '')
                                          , paste3(targetactorentity
                                                   , targetactorrole, sep = '')
                                          , rootcode, eventcode, goldstein)]
      setnames(phoenix_data, c('V2', 'V3')
               , c('sourceactorentity', 'targetactorentity'))
      phoenix_data[, source := 'phoenix']

      ## Drop any missing data
      phoenix_data <- phoenix_data[!is.na(rootcode)]
      phoenix_data <- phoenix_data[!is.na(eventcode)]
      phoenix_data <- phoenix_data[!is.na(goldstein)]


      ######
      #
      # Combine ICEWS and Phoenix data
      #
      ######

      master_data <- rbind(icews_data, phoenix_data)
      if(nrow(master_data) == 0){
        stop('No Phoenix or ICEWS data available for the specified timespan.')
      }
      setnames(master_data, c('sourceactorentity', 'targetactorentity')
               , c('actora', 'actorb'))

    }

  }

  ## Subset events: if a subset of EVENTCODES are specified, keep only that
  ##  set of events and aggregate up from there.
  if(!any('all' %in% code_subset)){
    master_data <- master_data[eventcode %in% code_subset]
  }

  ## Create new variable: Pentaclass (0-4)
  master_data[rootcode %in% c(1, 2), pentaclass := 0L]
  master_data[rootcode %in% c(3, 4, 5), pentaclass := 1L]
  master_data[rootcode %in% c(6, 7, 8), pentaclass := 2L]
  master_data[rootcode %in% c(9, 10, 11, 12, 13, 16), pentaclass := 3L]
  master_data[rootcode %in% c(14, 15, 17, 18, 19, 20), pentaclass := 4L]

  ######################################
  ## IMPORTANT ASSUMPTION HERE:
  ## I am *ASSUMING* that NULL/NA entries after a state code
  ##  implies that the actor is the GOVERNMENT. As such I am replacing
  ##  all such missing entries with 'GOV'.
  ######################################
  master_data[actora %in%  countrycode::countrycode_data$iso3c
              , actora := paste0(actora, 'GOV')]
  master_data[actorb %in%  countrycode::countrycode_data$iso3c
              , actorb := paste0(actorb, 'GOV')]

  ######
  ## Subset events and columns: only events that:
  ##  1. involve specified actor set on both side (as ENTITIES)
  ##  2. involve TWO DIFFERENT actors (i.e. no self-interactions
  ##      as specified by user)
  ######
  if(('states' %in% actorset)){
    master_data <- master_data[
      substr(actora, 1, 3) %in% actors & substr(actora, 4, 6) %in% c('GOV', 'MIL', '')
      & substr(actorb, 1, 3) %in% actors & substr(actorb, 4, 6) %in% c('GOV', 'MIL', '')
      ]
    master_data[substr(actora, 1, 6) %in% 'IGOEUR', actora := 'EUR']
    master_data[substr(actorb, 1, 6) %in% 'IGOEUR', actorb := 'EUR']
    master_data[substr(actora, 1, 6) %in% 'IGOUNO', actora := 'UNO']
    master_data[substr(actorb, 1, 6) %in% 'IGOUNO', actorb := 'UNO']
    master_data[, actora := substr(actora, 1, 3)]
    master_data[, actorb := substr(actorb, 1, 3)]
    master_data[, actora := factor(actora, levels = levels(actors))]
    master_data[, actorb := factor(actorb, levels = levels(actors))]
    master_data <- master_data[actora != actorb]

  } else{
    master_data[, actora := substr(actora, 1, 6)]
    master_data[, actorb := substr(actorb, 1, 6)]
    master_data <- master_data[(actora %in% actors
                              & actorb %in% actors
                              & actora != actorb)]
    master_data[, actora := factor(actora, levels = actors)]
    master_data[, actorb := factor(actorb, levels = actors)]
  }

  ## Subset columns: drop unused event column
  if(level == 'rootcode'){
    master_data[, eventcode := NULL]
    master_data[, goldstein := NULL]
    master_data[, pentaclass := NULL]
  } else if(level == 'eventcode') {
    master_data[, rootcode := NULL]
    master_data[, goldstein := NULL]
    master_data[, pentaclass := NULL]
  } else if(level == 'goldstein') {
    master_data[, eventcode := NULL]
    master_data[, rootcode := NULL]
    master_data[, pentaclass := NULL]
  } else if(level == 'pentaclass') {
    master_data[, eventcode := NULL]
    master_data[, rootcode := NULL]
    master_data[, goldstein := NULL]
    setcolorder(master_data, c(1,2,3,5,4))
  }


  ######
  #
  # Format data by de-duplicating, separating by date,
  # and dropping unused columns
  #
  ######

  ## Set names to generic
  setnames(master_data, c('date', 'actora', 'actorb', 'code', 'source'))

  ## Set CAMEO coded event/root/pentaclass codes to factors
  # if(!level == 'goldstein'){
  #   master_data[, code := factor(code, levels = codes)]
  # }

  ## Set keys
  setkeyv(master_data, c('date', 'actora', 'actorb', 'code', 'source'))

  ## Drop duplicated variables
  master_data <- unique(master_data)

  ## Aggregate dates to specified time window
  master_data[, date := lubridate::floor_date(date, time_window)]

  ## Subset events: keep only events within date range
  master_data <- master_data[date %in% dates]

  ## Subset events
  if(tie_type == 'binary'){
    ## Subset events: drop duplicated events/days/actors
    master_data <- master_data[!duplicated(master_data)]
  } else if(tie_type == 'count'){
    ## Subset events: drop duplicated events/days/actors
    master_data <- master_data[, .N, by = list(date, actora, actorb, code)]
  } else if(tie_type == 'meangoldstein'){
    master_data <- master_data[, mean_goldstein := mean(code), by = list(date, actora, actorb)]
  } else if(tie_type == 'sepgoldstein'){
    master_data[, pos_goldstein := NA_real_]
    master_data[code > 0, pos_goldstein := code]
    master_data[, neg_goldstein := NA_real_]
    master_data[code < 0, neg_goldstein := code]
    master_data[, mean_pos_goldstein := mean(pos_goldstein, na.rm = T), by = list(date, actora, actorb)]
    master_data[is.na(mean_pos_goldstein), mean_pos_goldstein := 0]
    master_data[, mean_neg_goldstein := mean(neg_goldstein, na.rm = T), by = list(date, actora, actorb)]
    master_data[is.na(mean_neg_goldstein), mean_neg_goldstein := 0]
    master_data <- master_data[mean_pos_goldstein != 0 | mean_neg_goldstein != 0, ]
    master_data <- unique(master_data, by = c('date', 'actora', 'actorb'))
    master_data[, code := as.integer(mean_pos_goldstein > 0) + 1]
  }

  ## Format for networkDynamic creation
  master_data[, date := as.integer(format(date, '%Y%m%d'))]
  master_data[, end_date := date]
  # setcolorder(master_data, c('date', 'end_date', 'actora', 'actorb', 'code'))

  ######
  #
  # For each time period in the specified range, subset the master data set,
  #  convert interactions to network ties, and turn the resulting edgelist
  #  into a network object. Save networks to a master list object.
  #
  ######


  if(time_window == 'day'){
    dates <- c(dates, dates[length(dates)])
    dates[length(dates)] <- lubridate::ymd(dates[length(dates)]) + lubridate::days(1)
  } else if(time_window == 'week'){
    dates <- c(dates, dates[length(dates)])
    dates[length(dates)] <- lubridate::ymd(dates[length(dates)]) + lubridate::weeks(1)
  } else if(time_window == 'month'){
    dates <- c(dates, dates[length(dates)])
    dates[length(dates)] <- lubridate::ymd(dates[length(dates)]) %m+% months(1)
  } else if(time_window == 'quarter'){
    dates <- c(dates, dates[length(dates)])
    dates[length(dates)] <- lubridate::ymd(dates[length(dates)]) %m+% months(1)
  } else if(time_window == 'year'){
    dates <- c(dates, dates[length(dates)])
    dates[length(dates)] <- lubridate::ymd(dates[length(dates)]) + lubridate::years(1)
  }

  # dates <- c(dates, (max(dates) + 1))
  dates <- as.integer(format(dates, '%Y%m%d'))
  for(this_code in codes){

    ## Subset by root/event code
    if(tie_type == 'binary'){
      event_data <- master_data[code %in% this_code, list(date, end_date, actora, actorb)]
    } else if(tie_type == 'count'){
      event_data <- master_data[code %in% this_code, list(date, end_date, actora, actorb, N)]
    } else if(tie_type == 'meangoldstein'){
      event_data <- master_data[code %in% this_code, list(date, end_date, actora, actorb, mean_goldstein)]
    } else if(tie_type == 'sepgoldstein'){
      event_data <- master_data[get(this_code) != 0, list(date, end_date, actora, actorb, get(this_code))]
    }

    if(nrow(event_data) > 0){
      # event_data[, date := as.integer(format(date, '%Y%m%d'))]
      # event_data[, end_date := as.integer(format(end_date, '%Y%m%d'))]
      event_data[, actora := as.integer(actora)]
      event_data[, actorb := as.integer(actorb)]

      ## Initialize the network size and characteristics
      event_net <- network::network.initialize(n = n, directed = T, loops = F)
      network.vertex.names(event_net) <- levels(actors)

      ## Generate networkDynamic object
      net_period <- list(observations = list(c(min(dates), max(dates)))
                         , mode = 'discrete', time.increment = 1
                         , time.unit = 'step')
      if(tie_type == 'binary'){
        temporal_codenet <- networkDynamic(base.net = event_net
                                           , edge.spells = event_data
                                           , net.obs.period = net_period
                                           , verbose = F)
      } else if(tie_type == 'count'){
        temporal_codenet <- networkDynamic(base.net = event_net
                                           , edge.spells = event_data
                                           , net.obs.period = net_period
                                           , create.TEAs = T
                                           , edge.TEA.names = 'N'
                                           , verbose = F)
      } else {
        temporal_codenet <- networkDynamic(base.net = event_net
                                           , edge.spells = event_data
                                           , net.obs.period = net_period
                                           , create.TEAs = T
                                           , edge.TEA.names = this_code
                                           , verbose = F)
    }
    ## Store list in master network list
    if(level != 'goldstein'){
      master_networks[paste0('code', this_code)] <- list(temporal_codenet)
      } else{
      master_networks[this_code] <- list(temporal_codenet)
      }
    }
  }
  return(master_networks)
}

