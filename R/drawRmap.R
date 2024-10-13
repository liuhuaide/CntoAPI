#' Draw a Map with a Specified Location
#'
#' This function retrieves the geographical coordinates for a given location
#' and plots a map using the specified map type and zoom level. The function
#' utilizes the Stadia Maps service for rendering the map. A valid API key
#' is required for accessing the Stadia Maps service.
#'
#' @param location A character string representing the location to be plotted.
#' @param maptype A character string specifying the type of map to be used.
#'        Valid options are "stamen_toner", "stamen_toner_lite", "stamen_watercolor",
#'        "stamen_terrain", "stamen_terrain_background", "stamen_terrain_labels",
#'        "stamen_terrain_lines", "outdoors", "alidade_smooth", and "alidade_smooth_dark".
#'        Defaults to "stamen_toner".
#' @param zoom An integer representing the zoom level of the map, ranging from 1 to 20.
#'        Defaults to 12.
#'
#' @return A ggplot object displaying the map with the specified location marked.
#'
#' @import ggmap
#' @import httr
#' @import jsonlite
#' @import ggplot2
#' @importFrom utils URLencode
#'
#' @examples
#' \dontrun{
#' # Draw map for Asia
#' drawRmap("Asia", maptype = "stamen_toner", zoom = 10)
#'
#' # Draw map for Harvard University
#' drawRmap("Harvard University", maptype = "outdoors", zoom = 11)
#' }
#' @export
drawRmap <- function(location, maptype = "stamen_toner", zoom = 12) {
  register_stadiamaps(key = "8ae72004-57e1-4266-a66a-3c2d2ba4aef4")

  valid_maptypes <- c("stamen_toner", "stamen_toner_lite", "stamen_watercolor",
                      "stamen_terrain", "stamen_terrain_background",
                      "stamen_terrain_labels", "stamen_terrain_lines",
                      "outdoors", "alidade_smooth", "alidade_smooth_dark")

  if (!maptype %in% valid_maptypes) {
    stop("Invalid maptype. Please use the following valid values: ",
         paste(valid_maptypes, collapse = ", "))
  }

  if (!is.numeric(zoom) || zoom < 1 || zoom > 20) {
    stop("Invalid zoom value. Please provide a number between 1 and 20.")
  }

  response <- GET(paste0("https://nominatim.openstreetmap.org/search?format=json&q=",
                         URLencode(location)))
  if (http_status(response)$category != "Success") {
    stop("Failed to retrieve latitude and longitude. Please check the input location.")
  }
  location_data <- fromJSON(content(response, as = "text"))

  if (length(location_data) == 0 || !is.data.frame(location_data)) {
    stop("Unable to retrieve the coordinates for the given location. Please check the input location.")
  }

  lat <- as.numeric(location_data$lat[1])
  lon <- as.numeric(location_data$lon[1])

  if (is.na(lat) || is.na(lon) || lat < -90 || lat > 90 || lon < -180 || lon > 180) {
    stop("The retrieved latitude and longitude are invalid. Please check the input location.")
  }

  bbox_radius <- 0.5  # Adjust the display area size (radius)
  bbox <- c(left = lon - bbox_radius, bottom = lat - bbox_radius,
            right = lon + bbox_radius, top = lat + bbox_radius)

  stadia_map <- get_stadiamap(bbox = bbox,
                              zoom = zoom,
                              maptype = maptype)

  p <- ggmap(stadia_map) +
    ggtitle(paste("Map of", location)) +
    xlab("Longitude") +
    ylab("Latitude")

  # Add marker point
  #p <- p + geom_point(aes(x = lon, y = lat), color = "red", size = 5, shape = 19) +
  #geom_text(aes(x = lon, y = lat, label = location), vjust = -1, color = "red", size = 5)

  # Print the map
  print(p)
}
