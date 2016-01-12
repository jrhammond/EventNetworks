#' Scrape, merge, and process Phoenix and ICEWS data into
#' a large data table for aggregation and subsetting.
#'
#'
#' @param phoenix_loc folder containing Phoenix data sets as daily .csv
#'          data tables. Automatically checks for new data sets each time
#'          the function is run, and downloads new daily data as it becomes
#'          available. Currently in 'one-and'done' format
#'          where it downloads the first time, and checks thereafter.
#' @param icews_loc folder containing ICEWS data sets as daily .tab data
#'          tables. Because I don't know how to work a SWORD API, these will
#'          need to be manually downloaded and updated.
#'
#' @return master_table a data.table object containing ALL merged/processed
#'          Phoenix and ICEWS data. One row per event-dyad-day.
#'
#' @rdname phoenix_tables
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
#' @import lubridate
#' @import dummies
phoenix_tables <- function(phoenix_loc, icews_loc, update = T){

  ######
  #
  # Set up some initial values: Time windows
  #
  ######

  ## Date objects
  start_date <- as.Date('1995-01-01')
  end_date <- Sys.Date()
  dates <- seq.Date(start_date, end_date, by = 'day')

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

  ## Set up set of secondary actor codes
  secondary_actors <- c('GOV', 'MIL', 'REB', 'OPP', 'PTY', 'COP', 'JUD'
                        , 'SPY', 'MED', 'EDU', 'BUS', 'CRM', 'CVL')
  statelist <- countrycode::countrycode_data$iso3c
  actors <- unique(statelist[statelist %in% states$isoc])
  actors <- actors[!is.na(actors)]
  actors <- c(actors, unique(as.vector(outer(actors, secondary_actors, paste, sep = ''))))
  actors <- as.factor(sort(actors))
  n <- length(actors)

  ######
  #
  # Set up some initial values: Event codes
  #
  ######

  ## Factor variables describing CAMEO categories
  rootcodes <- factor(1:20)
  levels(rootcodes) <- as.character(1:20)

  eventcodes <- factor(1:298)
  levels(eventcodes) <- as.character(
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

  pentaclasses <- factor(0:4)
  levels(pentaclasses) <- as.character(0:4)

  ######
  #
  # Set up some empty storage objects
  #
  ######

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
    update_phoenix(destpath = phoenix_loc, phoenix_version = 'current')
  }


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

  ## Modify more complex ICEWS actor codes
  icews_data[nchar(sourceactorentity) == 9, sourceactorentity :=
               paste0(substr(icews_data[nchar(sourceactorentity) == 9, sourceactorentity], 1, 3)
                      , substr(icews_data[nchar(sourceactorentity) == 9, sourceactorentity], 7, 9))
             ]

  icews_data[nchar(targetactorentity) == 9, targetactorentity :=
               paste0(substr(icews_data[nchar(targetactorentity) == 9, targetactorentity], 1, 3)
                      , substr(icews_data[nchar(targetactorentity) == 9, targetactorentity], 7, 9))
             ]

  ######
  #
  # Read and parse Phoenix data for merging.
  #
  ######

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

  ######
  #
  # Combine ICEWS and Phoenix data
  #
  ######

  try({
    master_data <- rbind(icews_data, phoenix_data)
  }, silent = T)
  if(class(master_data)[1] == 'try-error'){
    message('Specified range does not include Phoenix data.')
    master_data <- icews_data
  }
  setnames(master_data, c('sourceactorentity', 'targetactorentity')
           , c('actora', 'actorb'))

  ## Drop any missing data
  master_data <- master_data[complete.cases(master_data), ]

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
  #
  # Pre-format data by de-duplicating, cleaning dates and actors,
  # and dropping unused columns
  #
  ######

  ## De-duplicate
  master_data <- unique(master_data)

  ## Subset events and columns: only events that:
  ##  1. involve specified actor set on both side (as ENTITIES)
  ##  2. involve TWO DIFFERENT actors (i.e. no self-interactions
  ##      as specified by user)
  master_data <- master_data[(actora %in% actors
                              & actorb %in% actors)]
  master_data <- master_data[actora != actorb]
  master_data[, actora := factor(actora, levels = levels(actors))]
  master_data[, actorb := factor(actorb, levels = levels(actors))]

  ## Set CAMEO coded event/root codes to factors
  master_data[, rootcode := factor(rootcode, levels = rootcodes)]
  master_data$eventcode <- gsub('!', '', master_data$eventcode)
  master_data[, eventcode := factor(as.integer(eventcode), levels = eventcodes)]
  master_data[, pentaclass := factor(pentaclass, levels = pentaclasses)]

  ## Set keys
  setkeyv(master_data, c('date', 'actora', 'actorb', 'eventcode', 'source'))


  ######
  #
  # Export : how much overlap between Phoenix and ICEWS reporting?
  #
  ######

  ## Create some temporary flag variables
  master_data[, dup_fromtop := duplicated(
    master_data[, list(date, actora, actorb, rootcode, eventcode)])]
  master_data[, dup_frombot := duplicated(
    master_data[, list(date, actora, actorb, rootcode, eventcode)], fromLast = T)]

  ## Export data on reporting overlap
  # Phoenix reporting only
  dates_tab <- data.table(date = dates)
  phoenix_only <- master_data[, sum(dup_fromtop == F
                                    & source == 'phoenix'), by = date]
  phoenix_only <- merge(dates_tab, phoenix_only, by = 'date', all.x = T)
  phoenix_only[is.na(V1), V1 := 0]
  sources_overlap$phoenix_only <- phoenix_only$V1

  # ICEWS reporting only
  icews_only <- master_data[, sum(dup_frombot == F
                                  & source == 'icews'), by = date]
  icews_only <- merge(dates_tab, icews_only, by = 'date', all.x = T)
  icews_only[is.na(V1), V1 := 0]
  sources_overlap$icews_only <- icews_only$V1

  # Both sources report
  both_sources <- master_data[, sum(dup_fromtop == T), by = date]
  both_sources <- merge(dates_tab, both_sources, by = 'date', all.x = T)
  both_sources[is.na(V1), V1 := 0]
  sources_overlap$both_sources <- both_sources$V1

  ## Drop flags and source variable
  master_data[, dup_fromtop := NULL]
  master_data[, dup_frombot := NULL]
  master_data[, source := NULL]

  ## Drop duplicated variables
  master_data <- unique(master_data)

  ## Subset events: keep only events within date range
  master_data <- master_data[date %in% dates]

  ## Create list of all actors in data set for output
  main_actors <- actors[!actors %in% statelist]

  ## BIG DUMMY SECTION: dummy out all categorical event/root/pentaclass codes
  master_data <- data.table(dummy.data.frame(master_data, names = c('pentaclass', 'rootcode', 'eventcode')))

  return(list(diagnostics = sources_overlap, netdata = master_data, actorlist = main_actors))
}
