#' Ingest the ICEWS Event Dataset
#'
#' Given a directory with individual ICEWS dataset files, quickly read
#' them all in, name them correctly, and combine them into one dataframe.
#'
#' @param dir The path to the ICEWS folder.
#' @param start_date Start of date range as YYYYMMDD integer format.
#' @param end_date End of date range as YYYYMMDD integer format.
#'
#' @return A single dataframe with all the ICEWS events in the folder.
#' @author Andy Halterman, forked by Jesse Hammond
#' @note This function is still in development and may contain errors and change quickly.
#' @examples
#'
#' events <- ingest_icews("~/ICEWS/study_28075/Data/", 20101201, 20140101)
#'
#' @rdname ingest_icews
#' @export

ingest_icews <- function(dir, start_date, end_date){
  # Handle messy file paths
  lastletter <- stringr::str_sub(dir ,-1, -1)
  if (lastletter != "/"){
    dir <- paste0(dir, "/")
  }

  ## List files
  files <- list.files(dir)

  ## Quick regex in case of zips still there
  files <- files[grep("\\.tab$", files)]

  ## Pull files that fall in the date range provided
  startyear <- as.integer(substr(start_date, 1, 4))
  endyear <- as.integer(substr(end_date, 1, 4))
  filesyears <- as.integer(
    do.call('rbind', (stringr::str_split(files, '\\.')))[, 2])
  if(endyear > max(filesyears)){
    message('Note: specified range exceeds the most recent ICEWS entries.')
  }
  files <- files[filesyears >= startyear & filesyears <= endyear]
  files <- paste0(dir, files)

  ## Set column dtypes
  coltypes <- c('integer', rep('character', 5), 'integer', 'numeric'
                , rep('character', 3), 'integer', 'integer'
                , rep('character', 5), 'numeric', 'numeric')

  ## Quick and dirty: fread all files
  read_one <- function(file){
    t <- tryCatch(fread(file, stringsAsFactors = F, sep = '\t'
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

  # Bind everything together
  events <- rbindlist(event_list)

  if(nrow(events) > 0){
    # Set names
    names(events) <- c("event_id", "date", "Source.Name", "Source.Sectors",
                       "Source.Country", "Event.Text", "eventcode", "goldstein", "Target.Name",
                       "Target.Sectors", "Target.Country", "Story.ID", "Sentence.Number",
                       "Publisher", "City", "District", "Province", "Country", "Latitude",
                       "Longitude")
    # Use lubridate, then de-POSIX the date.
    events$date <- as.Date(lubridate::ymd(events$date))
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


