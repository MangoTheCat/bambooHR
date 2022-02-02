#' @title Check Date Format
#'
#' @description Functions to check date and datetime formats
#' @name date_formats
NULL

#' @describeIn date_formats Check format is YYYY-MM-DD
#' @keywords internal
is_ymd <- function(date = NULL) {
  check <- grepl("^[1-9]{1}[0-9]{3}-[0-1]{1}[0-9]{1}-[0-3]{1}[0-9]{1}$",
                 trimws(date))
  return(check)
}

#' @describeIn date_formats Check format is YYYY-MM-DD HH:MM:SS
#' @keywords internal
is_ymd_hms <- function(date = NULL){
  check <- grepl("^[1-9]{1}[0-9]{3}-[0-1]{1}[0-9]{1}-[0-3]{1}[0-9]{1} [0-2]{1}[0-9]{1}:[0-5]{1}[0-9]{1}:[0-6]{1}[0-9]{1}$",
                 trimws(date))
  return(check)
}

#' @describeIn date_formats Check format is YYYY-MM-DD HH:MM:SS.mmm
#' @keywords internal
is_ymd_hmsms <- function(date = NULL){
  check <- grepl("^[1-9]{1}[0-9]{3}-[0-1]{1}[0-9]{1}-[0-3]{1}[0-9]{1} [0-2]{1}[0-9]{1}:[0-5]{1}[0-9]{1}:[0-6]{1}[0-9]{1}\\.[0-9]{3}$",
                 trimws(date))
  return(check)
}


#' Coerce to ISO 8601
#'
#' Coerce dates or datetimes to ISO 8601 format for Bamboo HR REST API.
#'
#' Input can be simply given as a character string. See \link{date_formats} for
#' allowed format types.
#'
#' @name ISO_8601
#'
#' @return Character string in ISO 8601 format.
NULL


#' @param datetime Character string. The datetime to coerce.
#'
#' @examples
#' as_ISO8601_character("2022-09-01")
#' as_ISO8601_character("2022-09-01 10:59:59.001")
#'
#' @describeIn ISO_8601 Coerce datetime into ISO8601 format. This will convert
#' represent the datetime relative to UTC using \code{+} notation. Milliseconds are
#' removed.
#'
#' @keywords internal
as_ISO8601_character <- function(datetime){
  if (! is.null(datetime)) {
    tm <- strftime(datetime, "%Y-%m-%dT%H:%M:%S")
    time_z <- strftime(datetime, "%z") # format +0200
    # Add : between hour and rest
    time_z_colon <- paste0(
      substring(time_z, 1, 3),
      ":",
      substring(time_z, 4, 5))

    paste0(tm, time_z_colon)
  } else {
    return(NULL)
  }
}

#' @param date Character string. The date to coerce.
#'
#' @describeIn ISO_8601 Coerce a date to ISO8601 format. This will convert the date
#' into UTC (applying \code{Z} notation) and remove any milliseconds.
#'
#' @examples
#' coerce_ISO8601("2022-09-01")
#' coerce_ISO8601("2022-09-01 10:59:59.001")
#'
#' @keywords internal
coerce_ISO8601 <- function(date){
  if(is_ymd(date)){
    date <- lubridate::ymd_hms(paste0(date, " 00:00:00"))
  } else if(is_ymd_hms(date)){
    date <- lubridate::ymd_hms(date)
  } else if(is_ymd_hmsms(date)){
    date <- lubridate::ymd_hms(date)
  } else {
    stop("Date/datetime not in format ymd, ymd_hms, or ymd_hmsms. See ?date_formats.")
  }
  iso_date <- paste0(lubridate::format_ISO8601(date),"Z")
  return(iso_date)
}
