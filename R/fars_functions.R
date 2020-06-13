
#' @export
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @title fars_read
#'
#' @description Attempt to import data from a csv file
#' with the given file name. Raises exception if file does not exist
#'
#' @param filename string, full path to file or file name in project directory
#'
#' @return (if successful) dplyr::tbl_df format containing the data
#' extracted form the file
#'
#' @examples
#' fars_read("data-raw/accident_2013.csv.bz2")
#'
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#' @export
#'
#' @title make_filename
#'
#' @description Generate a string containing the accident data file name and
#' expected extension labeled by the given year
#'
#' @param year integer, or convertible to integer containing the year
#' label to be assigned to accident data file name
#'
#' @return string containing the file name
#'
#' @examples
#' make_filename(2013)
#'
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("data-raw/accident_%d.csv.bz2", year)
}


#' @export
#'
#' @importFrom dplyr mutate select
#' @importFrom magrittr "%>%"
#'
#' @title fars_read_years
#'
#' @description Iterate over a given collection of year labels,
#' and for each one try to extract tables of year and month labels
#' associated with any accident data that can be found. In case
#' of a missing file for a given year, a warning message is raised
#' and a null table is associated with that year
#'
#' @param years list containing a list of integers, or
#' labels convertible to integers, representing the year labels of the
#' accident data files
#'
#' @return returns a list of the same dimension as 'years', containing
#' dplyr::tbl_df objects with year and MONTH columns of the underlying data,
#' or NULL if no data are found
#'
#' @examples
#' fars_read_years(list(2013, 2014, 2015))
#'
fars_read_years <- function(years) {
  lapply(years, function(year) {
    if(year %in% names(accident)){
      dat <- accident[[as.character(year)]]
      dplyr::mutate(dat, year = year) %>%
        dplyr::select(MONTH, year)
    } else {
      warning("invalid year: ", year)
      return(NULL)
    }
  })
}


#' @export
#'
#' @importFrom dplyr bind_rows group_by summarize
#' @importFrom tidyr spread
#' @importFrom magrittr "%>%"
#'
#' @title fars_summarize_years
#'
#' @description Read in dataset over a given collection of year labels
#' and compute a count of the number of records found by year and MONTH
#'
#' @param years list containing a list of integers, or
#' labels convertible to integers, representing the year labels of the
#' accident data files. Warning messages will be generated if no files are found
#' for a given year
#'
#' @return a dplyr::tbl_df object containing the number of records found in
#' the files indicated by 'years' broken down by year (columns) and by month (rows)
#'
#' @examples
#' fars_summarize_years(list(2013, 2014, 2015))
#'
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}


#' @export
#'
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#'
#' @title fars_map_state
#'
#' @description Generate a plot of the geographical distribution of accidents
#' for a selected US state for a given year
#'
#' @param state.num an integer between 1 and 56 indicating the index of the US
#' state (or district) to be plotted. An error message is raised if an invalid
#' state index is specified.
#'
#' @param year integer, or convertible to integer containing the year for which
#' to plot the accident dataset. An error message is created if there are no
#' data available for that year
#'
#' @return Plot of a selected US state with the geographical location of every
#' accident for a given year superimposed
#'
#' @examples
#' fars_map_state(1, 2013)
#'
fars_map_state <- function(state.num, year) {
  if(!(year %in% names(accident)))
    stop("invalid year: ", year)
  data <- accident[[as.character(year)]]

  state.num <- as.integer(state.num)
  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}
