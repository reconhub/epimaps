#' Make an `epiflows` object
#' 
#' Description
#'
#' @export
make_epiflows <- function(linelist, flows) {
  linelist <- validateLinelist(linelist)
  flows <- validateFlows(flows, linelist)

  structure(
    list(flows = flows, linelist = linelist),
    class = "epiflows"
  )
}

## Terminates the workflow and throws an error
## when x is NULL, NA, or an empty object (e.g., character(0)).
stopIfInvalid <- function(x) {
  objectName <- as.character(substitute(x))

  if (is.null(x)) {
    stop(objectName, " is NULL")
  }
  if (length(x) == 0) {
    stop(objectName, " is empty")
  }
  if (all(is.na(x))) {
    stop(objectName, " is NA")
  }
}

## If linelist is valid, returns it as a data frame.
## If not, stops the workflow.
validateLinelist <- function(linelist) {
  stopIfInvalid(linelist)

  linelist <- as.data.frame(linelist, stringsAsFactors = FALSE)
  if (!"code" %in% colnames(linelist)) {
    stop("`code` column is mandatory in linelist")
  }
  linelist$code <- as.character(linelist$code)
  linelist
}

## If flows is valid (and complies with data from linelist),
## returns it as a data frame; if not, stops the workflow.
validateFlows <- function(flows, linelist) {
  stopIfInvalid(flows)
  flows <- as.data.frame(flows)

  if (!all(sapply(flows, is.numeric))) {
    stop("flows must contain numeric values only")
  }
  if (length(diffs <- setdiff(colnames(flows), rownames(flows))) > 0) {
    stop("Column and row names differ\nDifferences: ", diffs)
  }
  if (length(diffs <- setdiff(colnames(flows), linelist$code)) > 0) {
    stop("Codes should match those from linelist\nDifferences: ", diffs)
  }
  flows
}

