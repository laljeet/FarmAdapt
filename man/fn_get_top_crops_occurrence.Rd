% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Functions.R
\name{fn_get_top_crops_occurrence}
\alias{fn_get_top_crops_occurrence}
\title{Get Top Crops by Occurrence}
\usage{
fn_get_top_crops_occurrence(df, cutoff = 0.9)
}
\arguments{
\item{df}{A data frame containing a column 'n' which represents the occurrence or count of crops.}

\item{cutoff}{A numeric value specifying the cumulative percentage cutoff (default is 0.90).
Rows are selected such that the cumulative occurrence is just above this cutoff.}
}
\value{
A subset of the input data frame, including rows up to the point where the cumulative
        percentage of occurrence (n) first exceeds the specified cutoff.
}
\description{
This function sorts a data frame by the occurrence of crops (represented by the column 'n')
in descending order and returns the top rows based on a cumulative percentage cutoff.
It calculates the cumulative percentage of the 'n' column and returns rows up to the
point where this cumulative percentage first exceeds the specified cutoff.
}
\examples{
# Assuming 'data' is a data frame with a column 'n' representing the occurrence of crops
data <- data.frame(Crop = c("Wheat", "Corn", "Rice"), n = c(100, 200, 150))
fn_get_top_crops_occurrence(data, 0.90)

}
