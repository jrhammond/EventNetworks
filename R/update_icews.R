#' Update a local directory of ICEWS dataset files with new files from the server
#'
#' Checks the contents of a directory containing ICEWS event data files, checks whether the
#' server has new events, and downloads them to that directory. (It'll have some version handling ability,
#' too, either from the file names or by reading in the events.)
#'
#' @param destpath The path to download ICEWS into.
#'
#' @return NULL
#' @author Original concept and code for Phoenix: Andy Halterman
#' @note This function, like Phoenix, is still in development and may contain errors and change quickly.
#' @examples
#'

#' @import Rcurl
#' @export
#'
update_icews <- function(destpath){
  # pulls all the links from the ICEWS dataverse
  link_data <- phoenixNet::get_icewslinks()
  link_filelist <- link_data[, label]
  link_filelist <- sapply(link_filelist, 'substr', 1, 30)

  ## Identify whether local ICEWS data exists
  icews_files <- list.files(destpath)

  ## Determine what needs to be updated/downloaded
  icews_delete <- icews_files[!icews_files %in% link_filelist]
  icews_download <- link_filelist[!link_filelist %in% icews_files]

  if(length(icews_download) == 0){
    message('ICEWS data is current through the most recent month.')
  }
  else{
    message('Updating ICEWS with most recent data release...')

    ## Delete out-of-date ICEWS files
    if(length(icews_delete) > 0){
      file.remove(paste0('/Users/jesse/Dropbox/Minerva/icews/', icews_delete))
    }
    ids <- link_data[label %in% names(icews_download), id]

    message("Downloading and unzipping files.")
    plyr::l_ply(
      ids
      , phoenixNet:::dw_icewsfile
      , destpath = destpath
      , metadata = link_data
      , .progress = plyr::progress_text(char = '=')
      )
  }

}





