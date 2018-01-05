#' Reading a csv file into R
#'
#' The function fars_read gets as input the path of the file name to load into R.
#' It checks whether the file exists and if not, stops.
#' If the file exists, with correct format, it is loaded into R with the
#' read_csv function from the readr package and messages,which type each column
#' is, are suppressed.
#' At least, the function uses the tbl_df function from the dplyr package,
#' although there is no need to do that.
#'
#' Erros occur by files which exist but are not in (zipped) csv format, or files
#' which are in csv format but do not have the default delimiters.
#' Error occurs if more than one filename is passed.
#'
#' @param filename A string containing the path for the file
#' @importFrom readr read_csv Function to read csv files, identifying
#'             automatically the type of each column
#' @importFrom dplyr tbl_df Function for converting tables into tibbles (tbl,
#'             tbl_df)
#' @return A tibble dataframe containing the information of the csv file
#' @examples
#' filename <- "accident_2013.csv.bz2"
#' accident_2013 <- fars_read(filename)
#' filename <- "accident_2015.csv.bz2"
#' accident_2013 <- fars_read(filename)
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}


#' Creating a compromized file name
#'
#' The function make filename creates a pre-defined name of a bz2 zipped file,
#' which is accident_year.csv.bz2. The year is the input of the function.
#' A warning occurs, if the input parameter is a single value but cannot be
#' converted into a integer, because year is then NA.
#'
#' An error occurs, if the input cannot be coerced to type integer - e.g. for
#' dataframes.
#'
#' @param year A string or number indicating the year for naming the file
#' @return A character string, indicating the zipped file name like
#'         accident_year.csv.bz2
#' @examples
#' zipped_file <- make_filename(2013)
#' make_filename(2014)
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}


#' Reading the bz2 zipped files with corresponding year in their name and
#' selecting only months and years of this dataset.
#'
#' The function fars_read_years can load several files corresponding to their
#' year into R, since it gets a list/vector of years as input and returns
#' the month and years of that dataframes.
#' It is still possible to only load one dataset and manipulate it.
#' The lapply functionality allows to create for each element (year of years)
#' the zipped file name, check whether it exists with tryCatch and if it throws
#' an error write out a warning that the year is invalid and return NULL.
#' If the file exists, the inner of tryCatch is executed and the data is loaded
#' into R. Then a new column year is created, which is the current element of
#' years chosen in lapply and only Month and year of the dataset are selected.
#'
#' Errors occur in the same cases as for make_filename, but not for fars_read,
#' since this is part of the tryCatch.
#'
#' @importFrom fars_read
#' @importFrom make_filename
#' @importFrom dplyr mutate select
#' @param years A list/vector containing the years for which data should be
#'              loaded and month + year are extracted
#' @return A dataframe containing the month and the year of each observation
#'         of the zipped file - the year coming as input and has to be part of
#'         the file name.
#' @examples
#' fars_read_years(list(2013,2014,2015))
#' fars_read_years(2013)
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>%
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}


#' Reading bz2 zipped data files and creating a summary of the number of
#' observations per month per year (years correspond to datasets)
#'
#' The function fars_summarize_years uses the fars_read_years function to
#' load the datasets and creates a list of each dataset containing only the
#' month and year per observation.
#' All this data is combined, via binding all rows together.
#' Next the data is grouped by month and year that the number of observations
#' of each grouping can be measued.
#' In the last step year and the number of observations per month and year (=n)
#' are spread, meaning new columns are created for each element of the year
#' column and it's value is the number of n in this month.
#'
#' Error occur, in the same cases as for fars_read_years.
#' If there are no errors in this helper function, everything works well.
#'
#' @importFrom fars_read
#' @importFrom make_filename
#' @importFrom fars_read_years
#' @importFrom dplyr bind_rows group_by summarize
#' @importFrom tidyr spread
#' @inheritParams fars_read_years
#' @return A dataframe containing the number of each month and the number of
#'         observations for each month in each of the input years
#' @examples
#' fars_summarize_years(list(2013,2014,2015))
#' fars_summarize_years(2013)
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}


#' Plotting the locations of accidents for one state in one year with its bounds
#'
#' The function fars_map_state reads the bz2 zipped file into R via the functions
#' make_filename and fars_read. Converts the state.num to integers.
#' Checks whether the state number exists in the dataset - if not, it stops and
#' writes out the invalid state number. If the state number exists, the data is
#' filtered by this state.
#' If there are no observations for this state the message "no accidents to plot"
#' appears. Seems unnecessary, since there have to be observations due to the
#' previous check.
#' LONGITUD values which exceed 900 are corrected to NA.
#' The same logic is applied for LATITUDE values over 90.
#' Then, the locations of the accidents in that state are plotted.
#'
#' Errors occur if there are errors in make_filename or fars_read or the
#' state.num cannot be converted t integer.
#' Additionally errors occur if the observations are out of bounds of the state.
#'
#' @importFrom make_filename
#' @importFrom fars_read
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#' @param state.num A number, indicating the number of states for filtering
#' @inheritParams make_file_name
#' @return A plot, showing the locations (longitud and latitude) of accidents in
#'         the chosen state and year
#' @examples
#' fars_map_state(1,2014)
#' fars_map_state(9,2013)
#' \dontrun{
#' fars_map_state(15,2013) # leading to a mentioned error}
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
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