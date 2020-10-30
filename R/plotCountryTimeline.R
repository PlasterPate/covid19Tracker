
#' plotCountryTimeline
#'
#' @param dateRange A date vector of length 2. indicates a time period between two days
#' @param country Name of the country to be plotted. Tt should start with capital letter.
#'
#' @export
#'
#' @examples
#' plotCountryTimeline(c("2020/2/05", "2020/08/27"), "US")
#'
#' plotCountryTimeline(c("2020-10-01", "2020-10-29"), "Germany")
#'
plotCountryTimeline <- function(dateRange, country){
  # Get updated data
  timeSeriesFull = fetchData()

  # Determine first day and last day included in the data
  firstDay = min(timeSeriesFull$Date)
  lastDay = max(timeSeriesFull$Date)

  # Get proper starting and ending day with exception handling
  properDateRange = .getDateRange(dateRange, firstDay, lastDay)
  fromDate = properDateRange[1]
  toDate = properDateRange[2]

  # Check if the country name exists in the list of countries
  if(!(country %in% unique(timeSeriesFull$Region))){
    stop("wrong country name! check your spelling or visit documentation to see possible country names")
  }

  # Filter data between the date range and in the selected country
  timeSeriesFull %>%
    dplyr::filter(Date >= dateRange[1] &
                    Date <= dateRange[2] &
                    Region == country) %>%
    tidyr::gather(key = "Case", value = "Count", Confirmed, Death)-> countryTimeline

  # Zero out incorrect count labels
  countryTimeline$Count[countryTimeline$Count < 0] = 0

  # Plot time series of corona cases in a country, in a selected period of time
  countryTimeline %>%
    ggplot2::ggplot(ggplot2::aes(x = Date, y = Count, color = Case)) +
    ggplot2::geom_line(size = 1.3) +
    ggplot2::scale_x_date(breaks = scales::pretty_breaks(10),
                          guide = ggplot2::guide_axis(angle = 45, check.overlap = T)) +
    ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(8)) +
    ggplot2::labs(x = "\nDate", y = "Number of Cases\n") +
    ggplot2::ggtitle(paste("Daily New Cases In", country), subtitle = paste("From", fromDate, "To", toDate)) +
    # ggplot2::theme(axis.text.x=ggplot2::element_text(angle=45, hjust=1)) +
    ggplot2::theme_bw()
}

# plotCountryTimeline(c("2020-6-1", "2020-8-13"), "Iran")
# timeSeriesFull %>%
#   dplyr::group_by(Region) %>%
#   dplyr::count() -> tmp

.getDateRange <- function(dateRange, firstDay, lastDay){
  # Throw exception if dateRange wasn't in correct format
  if (length(dateRange) != 2) {
    stop("dateRange argument should be a vector of length 2!")
  }

  # Determine starting day and ending day selected by input
  fromDate = lubridate::ymd(dateRange[1])
  toDate = lubridate::ymd(dateRange[2])

  # Throw exception if dateRange second date was before dateRange first date
  if(fromDate > toDate){
    stop("First date of dateRange shouldn't be ahead of its second date!")
  }

  if(fromDate > lastDay | toDate < firstDay){
    stop("dateRange was out of the acceptable range of data!")
  }

  # If one side of the dateRange was out of range, change it to the nearest possible date
  fromDate = max(fromDate, firstDay)
  toDate = min(toDate, lastDay)

  return(c(fromDate, toDate))
}

