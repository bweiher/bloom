#' Compute aggregate functions on subsets of a dataset
#'
#'
#' Creates subsets of a dataset and then applies aggregate functions over it, while retaining knowledge of which filtering conditions were applied.
#'
#' @export
#' @importFrom data.table ":="
#' @param datatable a data.table object
#' @param cols columns to group by and retain after aggregation
#' @param retain_all hold back an `All` option for each col supplied in cols
#' @param fn a function taking a datatable that returns

bloom <- function(datatable, cols, retain_all=TRUE, fn) {

  # if(missing(...)) stop("You need to supply arguments to aggregate by")

  dlist <- list() # allocate list
  data.table::setDT(datatable) # ensure is data.table
  grid <- branch(cols = cols, retain_all = retain_all) # generate a dt of all possible input combinations
  chr <- paste0(names(grid), " == ") # prepare filtering statements to apply

  # for each row in the grid ...
  for (z in seq_along(1:nrow(grid))) {
    chr2 <- character(length = length(chr)) # allocate character vector
    for (g in seq_along(chr2)) { # create filter conditions for this row in the grid
      if (grid[[g]][z] == "All" || is.na(grid[[g]][z])) {
        chr2[[g]] <- ""
      } else {
        chr2[[g]] <- paste0(chr[g], "'", grid[[g]][z], "'")
      }
    }

    # filter out if setting == `All`
    chr2 <- chr2[nchar(chr2) > 0]

    # filter the datatable, or don't if there isn't a condition
    filter_condition <- paste(chr2, collapse = " & ")
    if (filter_condition == "") {
      agg_dt <- fn(datatable[, ])
    } else {
      agg_dt <- fn(datatable[eval(parse(text = filter_condition)), ])
    }


    # for each column supplied, create a new one so that you know how the aggregation was done
    for (g in seq_along(1:length(chr))) {
      agg_dt[, eval(parse(text = "names(grid)[g] := grid[[g]][z]"))]
    }

    dlist[[z]] <- agg_dt
  }

  data.table::rbindlist(dlist) # collapse list to dt
}
