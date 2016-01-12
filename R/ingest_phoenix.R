#' Ingest the Phoenix Dataset
#'
#' Given a directory with individual Phoenix dataset files, quickly read
#' them all in, name them correctly, and combine them into one dataframe.
#'
#' @param phoenix_loc The path to the Phoenix folder.
#' @param start_date Start of date range as YYYYMMDD integer format.
#' @param end_date End of date range as YYYYMMDD integer format.
#'
#' @return A single dataframe with all the Phoenix events in the folder.
#' @author Andy Halterman, forked by Jesse Hammond
#' @note This function, like Phoenix, is still in development and may contain errors and change quickly.
#' @examples
#'
#' events <- ingest_phoenix("~/OEDA/phoxy_test/", 20140620, 20150101)
#'
#' @import data.table
#' @rdname ingest_phoenix
#' @export

ingest_phoenix <- function(phoenix_loc, start_date, end_date){
  # Handle messy file paths
  lastletter <- stringr::str_sub(phoenix_loc ,-1, -1)
  if (lastletter != "/"){
    phoenix_loc <- paste0(phoenix_loc, "/")
  }

  ## List files
  files <- list.files(phoenix_loc)

  ## Pull files that fall in the date range provided
  filesdates <- as.integer(
    do.call('rbind', (stringr::str_split(files, '\\.')))[, 3])
  filesdates <- as.Date(lubridate::ymd(filesdates))
  if(start_date < min(filesdates)){
    message('Note: specified range precedes the earliest Phoenix data.')
  }
  if(end_date > max(filesdates)){
    message('Note: specified range exceeds the latest Phoenix data. IT\'S NOT A CRYSTAL BALL PEOPLE')
  }
  files <- files[filesdates >= start_date & filesdates <= end_date]
  files <- paste0(phoenix_loc, files)

  ## Set column dtypes
  coltypes <- c('character', rep('integer', 4), rep('character', 10)
                , 'integer',  'numeric', 'character', 'numeric'
                , 'numeric', rep('character', 6))
  ## Set column name
  phoenix_names <- c('eventid', 'date', 'year', 'month', 'day'
                     , 'sourceactorfull', 'sourceactorentity', 'sourceactorrole'
                     , 'sourceactorattribute', 'targetactorfull', 'targetactorentity'
                     , 'targetactorrole', 'targetactorattribute', 'eventcode'
                     , 'rootcode', 'pentaclass', 'goldstein', 'issues'
                     , 'lat', 'long', 'locationname', 'statename', 'countrycode'
                     , 'sentenceid', 'urls', 'newssources')

  ## Quick and dirty: fread all files
  read_one <- function(file){
    t <- tryCatch(data.table::fread(file, stringsAsFactors = F, sep = '\t'
                        , colClasses = coltypes, na.strings = '')
                  , error = function(e) message(paste0('error reading ', file)))
    if(is.null(t) == F){
      return(t)
    } else {
      message('object is not a data.frame')
    }
  }

  message("Reading in files...")
  event_list  <- plyr::llply(files, read_one, .progress = plyr::progress_text(char = '='))

  ## Bind everything together
  events <- data.table::rbindlist(event_list)
  data.table::setnames(events, phoenix_names)
  
  ## Convert codes to INTEGER type
  suppressWarnings(events$eventcode <- as.integer(events$eventcode))
  suppressWarnings(events$rootcode <- as.integer(events$rootcode))

  if(nrow(events) > 0){
    ## Convert dates to DATE object
    events$date <- as.Date(lubridate::ymd(events$date))  # use lubridate, then de-POSIX the date.
    message("Process complete")
    return(events)

  } else{
    events <- data.table(date = structure(NA_real_, class="Date")
                         , sourceactorentity = NA_character_
                         , targetactorentity = NA_character_
                         , rootcode = NA_integer_
                         , eventcode = NA_integer_
                         , goldstein = NA_real_)
    message("Process complete")
    return(events)
  }
}


