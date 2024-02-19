#' Get Row with Maximum Acreage
#'
#' This function takes a data frame and returns the row with the maximum acreage.
#' It assumes that the data frame has a column named 'Acreage'.
#'
#' @param df A data frame containing an 'Acreage' column.
#'
#' @return Returns the row from the data frame that has the maximum value in the 'Acreage' column.
#'
#' @examples
#' data <- data.frame(Acreage = c(100, 200, 150), Name = c("Farm1", "Farm2", "Farm3"))
#' get_max_row(data)
#'
#' @export
#'
get_max_row <- function(df) {
  # Get index of max acreage row
  max_idx <- which.max(df$Acreage)

  # Return full row
  return(df[max_idx, ])
}


#' Get Rows Meeting a Specified Cutoff
#'
#' This function sorts a data frame by the 'Acreage' column in descending order and
#' returns the top rows based on a cumulative percentage cutoff of the 'Acreage' column.
#' It assumes the data frame contains an 'Acreage' column.
#'
#' @param df A data frame containing an 'Acreage' column.
#' @param cutoff A numeric value (default is 0.90) specifying the cumulative percentage cutoff.
#'               Rows are selected such that the cumulative acreage is just above this cutoff.
#'
#' @return Returns a subset of the input data frame, including rows up to the point
#'         where the cumulative percentage of acreage first exceeds the specified cutoff.
#'
#' @examples
#' data <- data.frame(Acreage = c(100, 200, 150, 50), Name = c("Farm1", "Farm2", "Farm3", "Farm4"))
#' get_top_rows(data, 0.90)
#'
#' @export
#'
get_top_crops_acerage <- function(df, cutoff = NULL) {
  # Calculate total acres
  total_acres <- sum(df$Acreage)

  # Compute default cutoff value if not provided
  if (is.null(cutoff)) {
    # Example: Set cutoff to a value based on some characteristic of df
    # This is just an example - you'll need to replace it with your actual logic
    cutoff <- 0.90
  }
  # Sort crops by acreage
  sorted_df <- df[order(-df$Acreage),]

  # Cumulative acreage percentage
  sorted_df$cumper <- cumsum(sorted_df$Acreage) / total_acres

  # Find the first row where cumulative percentage exceeds the cutoff
  first_row_above_cutoff <- which(sorted_df$cumper > cutoff)[1]

  # Filter rows meeting cutoff
  top_rows <- sorted_df[1:first_row_above_cutoff,]

  return(top_rows)
}


#' Get Top Crops by Occurrence
#'
#' This function sorts a data frame by the occurrence of crops (represented by the column 'n')
#' in descending order and returns the top rows based on a cumulative percentage cutoff.
#' It calculates the cumulative percentage of the 'n' column and returns rows up to the
#' point where this cumulative percentage first exceeds the specified cutoff.
#'
#' @param df A data frame containing a column 'n' which represents the occurrence or count of crops.
#' @param cutoff A numeric value specifying the cumulative percentage cutoff (default is 0.90).
#'               Rows are selected such that the cumulative occurrence is just above this cutoff.
#'
#' @return A subset of the input data frame, including rows up to the point where the cumulative
#'         percentage of occurrence (n) first exceeds the specified cutoff.
#'
#' @examples
#' # Assuming 'data' is a data frame with a column 'n' representing the occurrence of crops
#' data <- data.frame(Crop = c("Wheat", "Corn", "Rice"), n = c(100, 200, 150))
#' fn_get_top_crops_occurrence(data, 0.90)
#'
#' @export
#'
fn_get_top_crops_occurrence <- function(df, cutoff = NULL) {
  # Calculate total occurrences
  total_acres <- sum(df$n)

  # Compute default cutoff value if not provided
  if (is.null(cutoff)) {

    cutoff <- 0.90
  }

  # Sort crops by occurrence
  sorted_df <- df[order(-df$n),]

  # Cumulative occurrence percentage
  sorted_df$cumper <- cumsum(sorted_df$n) / total_acres

  # Find the first row where cumulative percentage exceeds the cutoff
  first_row_above_cutoff <- which(sorted_df$cumper > cutoff)[1]

  # Filter rows meeting cutoff
  top_rows <- sorted_df[1:first_row_above_cutoff,]

  return(top_rows)
}


#' Get Unique Values from DataFrame
#'
#' This function takes a dataframe as input and returns a dataframe with
#' unique values from two columns: 'Name' and 'Crop'. The output dataframe
#' contains two columns, 'state' and 'crop', where each column lists the
#' unique values from the respective columns of the input dataframe.
#'
#' @param df A dataframe with at least two columns: 'Name' and 'Crop'.
#'           The 'Name' column represents states and the 'Crop' column
#'           represents crops.
#'
#' @return A dataframe with two columns: 'state' and 'crop'. Each column
#'         contains unique values extracted from the 'Name' and 'Crop'
#'         columns of the input dataframe, respectively.
#'
#' @examples
#' df <- data.frame(Name = c("State1", "State2", "State1"),
#'                  Crop = c("Crop1", "Crop2", "Crop1"))
#' unique_values <- fn_get_unique_values(df)
#' print(unique_values)
#'
#' @export
fn_get_unique_values <- function(df) {
  unique_states <- unique(df$Name)
  unique_crops <- unique(df$Crop)
  return(data.frame(state = unique_states, crop = unique_crops))
}
