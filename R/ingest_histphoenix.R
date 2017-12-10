#' Ingest the historic Phoenix Dataset
#'
#' Given a directory with the historic Phoenix dataset files, quickly read
#' them all in, name them correctly, and combine them into one dataframe.
#'
#' @param histphoenix_loc The path to the Phoenix folder.
#' @param start_date
#' @param end_date
#'
#' @return A single dataframe with all the historic Phoenix events in the folder.
#' @note This function, like Phoenix, is still in development and may contain errors and change quickly.
#' @examples
#'
#' events <- ingest_histphoenix("~/histphoenix")
#'
#' @import data.table
#' @import countrycode
#' @import bit64
#' @rdname ingest_histphoenix
#' @export

ingest_histphoenix <- function(
  histphoenix_loc
  , .start_date = start_date
  , .end_date = end_date
  , .statelist = statelist
  ){

  ## Identify appropriate files in the historic Phoenix folder - don't want
  ##  to try and read in PDF docs or .csv metadata tables
  histphoenix_files <- list.files(histphoenix_loc)[
    intersect(
      grep('Phoenix', list.files(histphoenix_loc))
      , grep('csv', list.files(histphoenix_loc))
    )
  ]

  ## Read in Phoenix files from historic sources
  histphoenix_data <- data.table()
  for(filename in histphoenix_files){
    this_phoenix <- fread(paste(histphoenix_loc, filename, sep = '/'))
    this_phoenix$aid <- bit64::as.integer64(this_phoenix$aid)
    histphoenix_data <- rbind(
      histphoenix_data
      , this_phoenix
    )
  }

  ######
  #
  # Parse the historic Phoenix data
  #
  ######

  ######################################
  ## IMPORTANT ASSUMPTION HERE:
  ## I am *ASSUMING* that NULL/NA entries after a state code
  ##  implies that the actor is the GOVERNMENT. As such I am replacing
  ##  all such missing entries with 'GOV'.
  ######################################
  histphoenix_data[source_root %in% .statelist
                   & (source_agent == ''
                      | source_agent == 'GOV')
                   , source := paste0(source_root, 'GOV')]
  histphoenix_data[target_root %in% .statelist
                   & (target_agent == ''
                      | source_agent == 'GOV')
                   , target := paste0(target_root, 'GOV')]

  ## Drop any missing data
  histphoenix_data <- histphoenix_data[!is.na(code)]
  histphoenix_data <- histphoenix_data[!is.na(root_code)]
  histphoenix_data <- histphoenix_data[!is.na(goldstein)]
  histphoenix_data <- histphoenix_data[!is.na(quad_class)]

  ## Parse dates
  histphoenix_data$date <- as.Date(histphoenix_data$story_date, format = '%m/%d/%Y')

  return(histphoenix_data)
}



