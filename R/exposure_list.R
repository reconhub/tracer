#' Extract individual exposures from an epicontacts outbreak
#'
#' @param outbreak the outbreak as an epicontacts object
#' @param date_onset_column the column name of for the date of onset
#'
#' @return a list with one entry per individual. Each entry is a vector
#'         of exposure dates
#' @export
exposure_list <- function(outbreak, date_onset_column) {
  linelist <- outbreak$linelist
  contacts <- outbreak$contacts
  stopifnot(is.data.frame(linelist))
  stopifnot(is.data.frame(contacts))
  stopifnot(length(date_onset_column) == 1)
  stopifnot(is.character(date_onset_column))
  linelist_cols <- colnames(linelist)
  stopifnot("id" %in% linelist_cols)
  if (!date_onset_column %in% linelist_cols) {
    stop(paste0("Column: ", date_onset_column, " is not in linelist"))
  }
  lapply(seq_len(nrow(linelist)), function(case_nr) {
    case_record <- linelist[case_nr, ]
    exposures <- contacts[contacts$to == case_record$id, ]$from
    sort(linelist[linelist$id == exposures, ][[date_onset_column]])
  })
}
