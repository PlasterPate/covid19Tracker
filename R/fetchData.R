
#' fetchData
#'
#' @description This function downloads updated coronavirus data. It contains time series of
#' confirmed and death cases of all countries in a tidy format.
#' It also contains geographic coordinates(Lat,Long) for each Region
#'
#' @return A tidy table containing daily information about coronavirus for all countries over the world
#' @export
#'
#' @examples
#' \dontrun{timeSeries <- fetchData()}
#'
fetchData <- function(){
  # Read database of confirmed cases and death cases of coronavirus from JHU github
  confirmed_url = RCurl::getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
  death_url = RCurl::getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
  confirmed_input = readr::read_csv(confirmed_url)
  death_input = readr::read_csv(death_url)

  # Clean confirmed data ,reshape it to tidy format, add daily count and arrange by date
  confirmed_input %>%
    dplyr::select(-"Province/State") %>%
    dplyr::rename(Region = "Country/Region") %>%
    tidyr::gather(key = "Date", value = "CumConfirmed", -Region, -Lat, -Long) %>%
    dplyr::group_by(Region, Date) %>%
    dplyr::summarise(CumConfirmed = sum(CumConfirmed),
                     Lat = dplyr::first(Lat),
                     Long = dplyr::first(Long)) %>%
    dplyr::mutate(Date = lubridate::mdy(Date)) %>%
    dplyr::arrange(Region, Date) %>%
    dplyr::mutate(Confirmed = c(CumConfirmed[1], diff(CumConfirmed[])))-> confirmeds

  # Clean death data ,reshape it to tidy format, add daily count and arrange by date
  death_input %>%
    dplyr::select(-"Province/State") %>%
    dplyr::rename(Region = "Country/Region") %>%
    tidyr::gather(key = "Date", value = "CumDeath", -Region, -Lat, -Long) %>%
    dplyr::group_by(Region, Date) %>%
    dplyr::summarise(CumDeath = sum(CumDeath),
                     Lat = dplyr::first(Lat),
                     Long = dplyr::first(Long)) %>%
    dplyr::mutate(Date = lubridate::mdy(Date)) %>%
    dplyr::arrange(Region, Date) %>%
    dplyr::mutate(Death = c(CumDeath[1], diff(CumDeath[])))-> deaths


  # Join confirmed and death tables, move Lat&Long columns to the end
  confirmeds %>%
    dplyr::full_join(deaths, by = c("Region", "Date", "Lat", "Long")) %>%
    dplyr::relocate(Lat, .after = Death) %>%
    dplyr::relocate(Long, .after = Lat)-> timeSeries
}
?fetchData
