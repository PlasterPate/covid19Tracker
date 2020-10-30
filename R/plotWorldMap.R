
#' plotWorldMap
#'
#' @description This function plots a choropleth map of the world showing the spread
#'  of coronavirus in a specific time period
#'
#' @param dateRange a date vector of length 1 or 2. if one element is passed, indicates a
#'  single day. if two elements are passed, indicates a sequence of days between two days.
#' @param type type of the coronavirus case. one of "Confirmed" or "Death"
#'
#' @export
#'
#' @examples
#' plotWorldMap(c("2020/03/26","2020/05/12"), type = "Confirmed")
#'
#' plotWorldMap(c("2020/06/21"), type = "Death")
#'
plotWorldMap <- function(dateRange, type){
  # Get updated data
  timeSeriesFull = fetchData()

  # Determine first day and last day included in the data
  firstDay = min(timeSeriesFull$Date)
  lastDay = max(timeSeriesFull$Date)

  # Get proper starting and ending day with exception handling
  properDateRange = .getDateRange(dateRange, firstDay, lastDay)
  fromDate = properDateRange[1]
  toDate = properDateRange[2]

  # Throw excepttion for incorrect 'type' argument
  if(type != "Confirmed" & type != "Death"){
    stop("incorrect value passed for 'type'! It should be one of \"Confirmed\" or \"Death\".")
  }

  # Filter data by dateRange, then calculate total number of confirmed and death cases
  timeSeriesFull %>%
    dplyr::filter(Date >= fromDate & Date <= toDate) %>%
    dplyr::group_by(Region, Lat, Long) %>%
    dplyr::summarise(Confirmed = sum(Confirmed),
                     Death = sum(Death))-> timeSeries

  # Zero out incorrect count labels
  timeSeries$Confirmed[timeSeries$Confirmed < 0] = 0
  timeSeries$Death[timeSeries$Death < 0] = 0

  # Get world spatial data for countries
  world <- rnaturalearth::ne_countries(scale = 110, returnclass = "sf")

  spatialPoints = sf::st_as_sf(timeSeries,
                               coords = c("Long","Lat"),
                               crs=4326)

  # Find the intersection of coordinates with map polygons
  geoIndex = sf::st_intersects(spatialPoints, world)
  geoIndex = lapply(geoIndex,
                    FUN = function(x){ifelse(is.null(x[1]),NA,x[1])}) %>% unlist()

  timeSeries[["Country"]] = world[["name"]][geoIndex]

  # Merge map spatial data with timeSeries dataset
  worldData = merge(world, timeSeries, by. = "name",by.y = "Country",all.x = T) %>%
    dplyr::filter(!is.na(Lat))

  # Plot choropleth map of the world based on type of casees
  log_n = as.integer(log(max(worldData[[type]]), base = 10)) + 1
  log_n = max(log_n, 6)
  print(log_n)
  ggplot2::ggplot() +
    ggplot2::geom_sf(data = worldData, ggplot2::aes(fill = !!ggplot2::sym(type))) +
    ggplot2::scale_fill_stepsn(colors = c("white", "yellow", "red"), show.limits = T,
                      na.value = "gray", oob = scales::squish_infinite,
                      values = scales::rescale(scales::log_breaks(n = log_n, base = 10)(c(1,max(worldData[[type]]))), to = c(0,1)),
                      breaks = scales::log_breaks(n = log_n, base = 10)(c(1,max(worldData[[type]])))
                      ) +
    ggplot2::labs(x = "Longitude", y = "Latitude") +
    ggplot2::ggtitle(paste(type, "Cases Around The World"), subtitle = paste("From", fromDate, "To", toDate)) +
    ggplot2::theme_minimal()
}

.getDateRange <- function(dateRange, firstDay, lastDay){
  # Throw exception if dateRange wasn't in correct format
  if (length(dateRange) != 1 & length(dateRange) != 2) {
    stop("dateRange argument should be a vector of length 1 or 2!")
  }

  # Determine starting day and ending day selected by input
  fromDate = lubridate::ymd(dateRange[1])
  toDate = lubridate::as_date(ifelse(length(dateRange) == 2, lubridate::ymd(dateRange[2]), fromDate))

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

# plotWorldMap(c("2020/03/26","2020/05/12"), type = "Confirmed")
# plotWorldMap(c("2020/10/27", "2020-10-27"), type = "Death")

options(scipen = 999)

