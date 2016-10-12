#' Update a local directory of Phoenix dataset files with new files from the server
#'
#' Checks the contents of a directory containing Phoenix event data files, checks whether the
#' server has new events, and downloads them to that directory. (It'll have some version handling ability,
#' too, either from the file names or by reading in the events.)
#'
#' @param destpath The path to download Phoenix into.
#'
#' @return NULL
#' @author Andy Halterman
#' @note This function, like Phoenix, is still in development and may contain errors and change quickly.
#' @examples
#'

#' @import Rcurl
#' @export
#'
update_phoenix <- function(destpath){
  # pulls all the links from the OEDA Phoenix page
  links <- phoenixNet::get_phoenixlinks()
  links_shortened <- as.data.frame(stringr::str_match(links, "events.full.(\\d+).txt"), stringsAsFactors=FALSE)
  filelist <- list.files(destpath)
  filelist_shortened <- as.data.frame(stringr::str_match(filelist, "events.full.(\\d+).txt"), stringsAsFactors=FALSE)
  # All rows in links_shortened that do not have a match in filelist_shortened.
  new_files <- dplyr::anti_join(links_shortened, filelist_shortened, by = "V2")
  if(nrow(new_files) == 0){
    message('Phoenix data is current through today.')
  }
  else{
    message("There are ", nrow(new_files), " undownloaded daily files. Downloading now...")
    ll <- paste0("https://s3.amazonaws.com/oeda/data/current/", new_files$V1, ".zip")

    message("Downloading and unzipping files.")
    plyr::l_ply(ll, phoenixNet:::dw_phoenixfile, destpath = destpath, .progress = plyr::progress_text(char = '='))
  }

}





