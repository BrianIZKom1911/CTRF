#' Add dummy vairables to an existing dataframe.
#'
#' @param data Dataframe. The main dataset to work on.
#' @param colname Character. Name of the column by which you create dummy variables.
#'
#' @return An Dataframe augmented with new columns.
#' @export
getendummies <- function(data, colname){
    values <- unique(data[[colname]])
    for (val in values) {
        data[[paste0(colname, "_", val)]] <- as.numeric(data[[colname]]==val)
    }
    return(data)
}

#' Add Fourier functional terms to an existing Dataframe
#' with small and fixed Degrees (3).
#' @importFrom dplyr mutate
#'
#' @param data Dataframe. The main dataset to work on.
#' @param colname Character. Name of the column.
#' @param a Numeric. Minimum of the column specified.
#' @param b Numeric. Maximum of the column specified.
#'
#' @return An Dataframe augmented with new columns.
#' @export
fff33 <- function(data, colname, a=NULL, b=NULL){
    a <- ifelse(is.null(a), min(data[[colname]]), a)
    b <- ifelse(is.null(b), max(data[[colname]]), b)
    data <- data |>
        dplyr::mutate(ts = (.data[[colname]]-a)/(b-a),
                      ts2 = ts^2, ts3 = ts^3,
                      sin1 = sin(2*pi*ts), cos1 = cos(2*pi*ts),
                      sin2 = sin(4*pi*ts), cos2 = cos(4*pi*ts),
                      sin3 = sin(6*pi*ts), cos3 = cos(6*pi*ts))
    return(data)
}

#' Add Fourier functional terms to an existing dataframe
#' with small and fixed Degrees (5).
#' Degree p or q>=3 is rarely used in the context of temperature--elec.demand.
#' @importFrom dplyr mutate
#'
#' @param data Dataframe. The main dataset to work on.
#' @param colname Character. Name of the column.
#' @param a Numeric. Minimum of the column specified.
#' @param b Numeric. Maximum of the column specified.
#'
#' @return An Dataframe augmented with new columns.
#' @export
fff55 <- function(data, colname, a=NULL, b=NULL){
    a <- ifelse(is.null(a), min(data[[colname]]), a)
    b <- ifelse(is.null(b), max(data[[colname]]), b)
    data <- data |>
        dplyr::mutate(ts = (.data[[colname]]-a)/(b-a),
                      ts2 = ts^2, ts3 = ts^3, ts4 = ts^4, ts5 = ts^5,
                      sin1 = sin(2*pi*ts), cos1 = cos(2*pi*ts),
                      sin2 = sin(4*pi*ts), cos2 = cos(4*pi*ts),
                      sin3 = sin(6*pi*ts), cos3 = cos(6*pi*ts),
                      sin4 = sin(8*pi*ts), cos4 = cos(8*pi*ts),
                      sin5 = sin(10*pi*ts), cos5 = cos(10*pi*ts))
    return(data)
}

#' Standardize an vector
#'
#' @param x Vector and likewise. Usually the columnized data.
#' @param mean Numeric. Mean of x, na removed.
#' @param median Boolean. Whether use median instead of mean.
#'
#' @return An Vector of standardized values.
#' @export
stdize <- function(x, mean=NULL, median=FALSE){
    mean <- ifelse(is.null(mean), mean(x, na.rm=TRUE), mean)
    ## default mean function or provided number
    m <- ifelse(median, median(x, na.rm=TRUE), mean)
    ## output of previous step (default) or median
    stdev <- sd(x, na.rm=TRUE)
    xstd <- (x - m)/stdev
    return(xstd)
}

#' Normalize an vector
#'
#' @param x Vector and likewise. Usually the columnized data.
#' @param a Numeric. Minimum of the column specified.
#' @param b Numeric. Maximum of the column specified.
#'
#' @return An Vector of normalized values.
#' @export
nmlize <- function(x, a=NULL, b=NULL){
    a <- ifelse(is.null(a), min(x), a)
    b <- ifelse(is.null(b), max(x), b)
    xnml <- (x - a)/(b - a)
    return(xnml)
}

#' Finalize the necessary modifications
#' @importFrom dplyr filter mutate
#'
#' @param df Dataframe. The main dataset to work on.
#' It must contain the complete list of columns.
#' @param hour Numeric. Hour.
#'
#' @return An Dataframe augmented with new columns.
#' @export
data_finalize <- function(df, hour) {
    df <- df |>
        dplyr::filter(Hour==hour) |>
        fff33(colname = "temperature") |> # add FFF terms
        dplyr::mutate(prcp = precipitation,
                      rhum = stdize(relative_humidity), # normalize or standardize variables
                      wsp = stdize(wind_speed, median=TRUE),
                      skc = stdize(skycover, median=TRUE),
                      ntd = nmlize(n_day))
    df[df$prcp > 1, "prcp"] <- 1 # winsorize precipitation
    df$prcp <- df$prcp*10
    return(df)
}
