#' Download the Phoenix Dataset
#'
#' Download and unzip all of the data files for the Phoenix dataset from the
#' Phoenix data website into a given directory.
#'
#' @param destpath The path to the directory where Phoenix should go.
#' @param phoenix_version. Download a specific version of Phoenix ("v0.1.0" or the current version by default).
#'
#' @return NULL
#' @author Original code credit: Andy Halterman
#' @note This function, like Phoenix, is still in development and may contain errors and change quickly.
#' @examples
#'
#' download_phoenix("~/OEDA/phoxy_test/", phoenix_version = "current")
#'
#' @rdname download_phoenix


## Function 1:
##    Process the start/end dates desired, and generate a list of
##    data links to try and download.
get_phoenixlinks <- function(
  start_date = as.Date('2014-06-20')
  , end_date = Sys.Date()
) {

  # Create a range of dates for which to download Phoenix data.
  dates <- seq.Date(
    start_date
    , end_date
    , by = 'day'
  )
  dates <- as.integer(format(dates, '%Y%m%d'))

  # Access the Phoenix raw data from Amazon repository.
  links <- paste0(
    'https://s3.amazonaws.com/oeda/data/current/events.full.'
    , dates
    , '.txt.zip'
  )

  return(links)
}

### Function 2:
##    Given a single link, try to download that specific Phoenix data file.
##    If that day's data is not available, notify the user with an error message.
dw_phoenixfile <- function(link, destpath) {
  # extract filename from link
  m <- regexpr('[^/]*(?=\\.zip$)', link, perl = T)
  filename <- regmatches(link, m)

  # download method
  if (.Platform$OS.type == 'windows') {
    download_method <- 'auto'
  } else{
    download_method <- 'curl'
  }

  # Attempt to download and unzip to destpath
  temp <- tempfile()
  download.file(link, temp, method = download_method, quiet = T)
  options(warn = 2)

  tryCatch(
    unzip(temp, exdir = destpath)
    , error = function(e){
      message(
        paste(
          'Unable to download file '
          , filename
          , '. It appears that Phoenix data for this date is missing.'
          , sep = ''
        )
      )
    }
  )

  options(warn = 1)
  unlink(temp)
}
