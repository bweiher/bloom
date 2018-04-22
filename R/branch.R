
#' Create a data.table of all input combinations
#'
#' @export
#' @param cols a data.table containing cols
#' @param retain_all hold back an `All` option for each col supplied in cols

branch <- function(cols, retain_all) {
  if (!is.data.table(cols)) stop("cols needs to be a data.table")


  if (isTRUE(retain_all)) { # if we want to do it, then...
    all_chosen <- setNames(data.table::data.table(matrix(nrow = 1, ncol = ncol(cols))), names(cols)) # create dt
    for (j in seq_along(names(cols))) data.table::set(all_chosen, j = j, value = "All") # fill in All for values
    cols <- data.table::rbindlist(list(all_chosen, cols)) # bind rows
  }

  # generate a dataframe of all possible input combinations
  unique(data.table::setDT(expand.grid(unique(cols), stringsAsFactors = FALSE)))
}
