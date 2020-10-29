
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
#' \dontrun{plotWorldMap(c("2020/03/26","2020/05/12"), type = "Confirmed")}
#' \dontrun{plotWorldMap(c("2020/06/21"), type = "Death")}
#'
plotWorldMap <- function(dateRange, type){
  # Throw exception if dateRange wasn't in correct format
  if (length(dateRange) != 1 & length(dateRange) != 2) {
    stop("dateRange argument should be a vector of length 1 or 2!")
  }

  # Determine starting day and ending day selected by input
  from_date = lubridate::ymd(dateRange[1])
  to_date = lubridate::as_date(ifelse(length(dateRange) == 2, lubridate::ymd(dateRange[2]), from_date))

  # Get updated data
  timeSeriesFull = fetchData()

  # Determine first day and last day included in the data
  first_day = min(timeSeriesFull$Date)
  last_day = max(timeSeriesFull$Date)

  # Throw exception if dateRange second date was before dateRange first date
  if(from_date > to_date){
    stop("First date of dateRange shouldn't be ahead of its second date!")
  }

  if(from_date > last_day | to_date < first_day){
    stop("dateRange was out of the acceptable range of data!")
  }

  if(type != "Confirmed" & type != "Death"){
    stop("incorrect value passed for 'type'! It should be one of \"Confirmed\" or \"Death\".")
  }

  # If one side of the dateRange was out of range, change it to the nearest possible date
  from_date = max(from_date, first_day)
  to_date = min(to_date, last_day)

  # Filter data by dateRange, then calculate total number of confirmed and death cases
  timeSeriesFull %>%
    dplyr::filter(Date >= from_date & Date <= to_date) %>%
    dplyr::group_by(Region, Lat, Long) %>%
    dplyr::summarise(Confirmed = sum(Confirmed),
                     Death = sum(Death))-> timeSeries

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
  ggplot() +
    geom_sf(data = worldData, aes(fill = !!sym(type))) +
    scale_fill_stepsn(colors = c("white", "yellow", "red"), show.limits = T,
                      na.value = "gray", oob = scales::squish_infinite,
                      values = scales::rescale(scales::log_breaks(n = 6, base = 2)(c(1,max(worldData$Confirmed))), to = c(0,1)),
                      breaks = scales::log_breaks(n = 6, base = 2)(c(1,max(worldData[[type]])))) +
    labs(x = "Longitude", y = "Latitude") +
    ggtitle(paste(type, "Cases Around The World"), subtitle = paste("From", from_date, "To", to_date)) +
    theme_minimal()
}

# plotWorldMap(c("2020/03/26","2020/05/12"), type = "Confirmed")

options(scipen = 999)


# ggplot() +
#   geom_sf(data = worldData, aes(fill = Confirmed)) +
#   scale_fill_stepsn(colors = c("white", "yellow", "red"), show.limits = T,
#                     na.value = "gray", oob = scales::squish_infinite,
#                     values = scales::rescale(scales::log_breaks(n = 20, base = 10)(c(1,max(worldData$Confirmed))), to = c(0,1)),
#                     breaks = scales::log_breaks(n = 6, base = 10)(c(1,max(worldData$Confirmed)))) +
#   labs(x = "Longitude", y = "Latitude") +
#   # ggtitle(paste(type, "Cases Around The World"), subtitle = paste("From", from_date, "To", to_date)) +
#   theme_minimal()
#
#
# max(worldData[["Confirmed"]])
