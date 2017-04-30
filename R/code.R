#'Clean location Column
#'
#'Cleans up a column by removiewing everything up to and including the rightmost column, removing excess whitespace, and transforming the text to title case.
#'
#'@param column_id The column to clean up.  Uses notation data_frame$column
#'
#'@return The cleaned up column. Note that the function does not return the entire dataframe
#'
#' @examples
#' \dontrun{
#' data_set <- data("earthquakes")
#' eq_location_clean(data_set$LOCATION_NAME)
#' }

eq_location_clean <- function(column_id){
        clean_column <- column_id %>% stringr::str_replace(pattern = "^.*:", replacement = "") %>% stringr::str_trim() %>% stringi::stri_trans_totitle()
        clean_column
}
#'
#'
#'Clean up earthquake data
#'
#'Cleans up data frame by creating a date column, converting lat/long, deaths, and magnitude to numeric rather than character, and applying eq_location_clean.  Function assumes data splits
#'date into YEAR, MONTH, and DAY column.  Location data needs to be in LATITUDE and LONGITUDE columns.
#' Function is case sensitive.  Future work should focus on making eq_clean_data operate like dplyr, taking column names rather than strings.
#'
#'@param raw_data the data frame to be cleaned
#'
#'@param column_id the column containting location  as a string
#'
#'@return a cleaned data frame
#'
#'@examples
#'\dontrun{
#' data_set <- data("earthquakes")
#' eq_clean_data(data_set, "LOCATION_NAME")
#' }
#'
#'@export
eq_clean_data <- function(raw_data, column_id){

        new_data <- dplyr::mutate_(raw_data, .dots = setNames(list( ~ paste(YEAR, MONTH, DAY, sep = "/")), "DATE"))
        new_data <- dplyr::select_(new_data, .dots = c("-YEAR", "-MONTH", "DAY"))
        new_data$DATE <- lubridate::ymd(new_data$DATE)
        new_data$LATITUDE <- as.numeric(new_data$LATITUDE)
        new_data$LONGITUDE <- as.numeric(new_data$LONGITUDE)
        new_data$EQ_PRIMARY <- as.numeric(new_data$EQ_PRIMARY)
        new_data$TOTAL_DEATHS <- as.numeric(new_data$TOTAL_DEATHS)
        new_data[[column_id]] <- eq_location_clean(new_data[[column_id]])
        new_data
}
#'
#'Draw earthquake timeline
#'
#'Function draws an earthquake timeline using a dataframe cleaned with eq_clean_data.  This function is used by geom_timeline rather than called directly.
#'In the interest of citing one's sources, most of the point implementation is recycling code from ggplot2's geom_point.
#'
#'@param aes column names containing variables to be mapped to x (intended use:date), y, size, and colour.  Other aes are available, but the user shouldn't
#'change defaults. They're only there becasue hard-coding them in resulted in errors.
#'
GeomTimeline <- ggplot2::ggproto("GeomTimeline", Geom,
                       required_aes = c("x"),
                       default_aes = ggplot2::aes(y = 0.5, size = 1, colour = "gray", alpha = 0.8, stroke = 1, fill = NA, shape = 19),
                       draw_key = ggplot2::draw_key_point,
                       draw_panel = function(data, panel_scales, coord) {
                               # Transform the data first
                               coords <- coord$transform(data, panel_scales)
                               #browser()


                                       # Create the line at the y levels of stratification
                                       line_grobs <- grid::polylineGrob(x = grid::unit(rep(c(0, 1), length(coords$y)), "npc"), y = rep(coords$y, each = 2),
                                                                        id.length = rep(2,length(coords$y)),
                                                                        gp = grid::gpar(col = "black", lwd = 0.2, lty = 1))

                                       # Draw the points (note that this is basically reusing geom_point)
                                       points_grobs <- grid::pointsGrob(
                                               x = coords$x,
                                               y = coords$y,
                                               pch = coords$shape,
                                               gp = grid::gpar(col = alpha(coords$colour, coords$alpha),
                                                               fill = alpha(coords$fill, coords$alpha),
                                               fontsize = coords$size * .pt + coords$stroke * .stroke / 2,
                                               lwd = coords$stroke * .stroke / 2)
                                       )

                                       grid::gTree(children = grid::gList(points_grobs, line_grobs))
                       })

#' Earthquake timeline
#'
#' Function draws an earthquake timeline using a dataframe cleaned with eq_clean_data.
#'
#'@param mapping Set of aesthetic mappings created by aes or aes_. If specified and inherit.aes = TRUE (the default),
#'it is combined with the default mapping at the top level of the plot. You must supply mapping if there is no plot mapping.
#'
#'@param data The data to be displayed in this layer. There are three options:
#'If NULL, the default, the data is inherited from the plot data as specified in the call to ggplot.
#'A data.frame, or other object, will override the plot data. All objects will be fortified to produce a data frame. See fortify for which variables will be created.
#'A function will be called with a single argument, the plot data. The return value must be a data.frame., and will be used as the layer data.
#'
#'@param stat The statistical transformation to use on the data for this layer, as a string.
#'
#'@param position Position adjustment, either as a string, or the result of a call to a position adjustment function.
#'
#'@param na.rm If FALSE, the default, missing values are removed with a warning. If TRUE, missing values are silently removed.
#'
#'@param show.legend logical. Should this layer be included in the legends? NA, the default, includes if any aesthetics are mapped.
#'FALSE never includes, and TRUE always includes.
#'
#'@param inherit.aes If FALSE, overrides the default aesthetics, rather than combining with them.
#'This is most useful for helper functions that define both data and aesthetics and shouldn't inherit behaviour from the default plot specification, e.g. borders.
#'
#'@param ... other arguments passed on to layer. These are often aesthetics, used to set an aesthetic to a fixed value, like color = "red" or size = 3.
#'They may also be parameters to the paired geom/stat.
#'
#' @return adds timeline to ggplot object (technically, returns NULL)
#'
#' @examples
#' \dontrun{
#' data("earthquakes")
#' earthquakes %>%
#' dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
#' ggplot() +
#' geom_timeline(aes(x = DATE, size = EQ_PRIMARY, colour = TOTAL_DEATHS, fill = TOTAL_DEATHS))
#' }
#'
#' @export
geom_timeline <- function(mapping = NULL, data = NULL, stat = "identity",
                         position = "identity", na.rm = FALSE,
                         show.legend = NA, inherit.aes = TRUE, ...) {
        ggplot2::layer(
                geom = GeomTimeline, mapping = mapping,
                data = data, stat = stat, position = position,
                show.legend = show.legend, inherit.aes = inherit.aes,
                params = list(na.rm = na.rm, ...)
        )
}
#'
#'Add annotation to earthquake timeline
#'
#'Function adds an annotation to an earthquake timeline built with geom_timeline.  Annotation shares some aes with geom_timeline. User is responsible for making sure
#'values are the same between the two geoms. This function is used by geom_timeline_label rather than called directly.  With more time, may look into implementing
#'n_max as a stat rather than as an aes.
#'
#'@param aes column names containing variables to be mapped to x (intended use:date), label (location name), n_max (an integer),
#'max_aes(aes to apply n_max, magnitude), and y)
#'
GeomTimelineLabel <- ggplot2::ggproto("GeomTimelineLabel", Geom,
                       required_aes = c("x", "label"),
                       default_aes = ggplot2::aes(y = 0.5, n_max = NULL, max_aes = NULL),
                       draw_key = draw_key_text,
                       draw_panel = function(data, panel_scales, coord) {

                               #To do: return error if n_max isn't an integer and either one n_max and max_aes are NULL but the other isn't

                               if (is.null(data$n_max) == FALSE){
                                       #subset to max
                                       n_max <- data$n_max[1]
                                       data <- data %>% dplyr::arrange_(.dots = "desc(max_aes)") %>% dplyr::slice_(.dots = "1:n_max")
                               }

                               # Transform the data
                               coords <- coord$transform(data, panel_scales)


                               #set length of vertical lines
                               line_length <- 0.04/length(unique(coords$y))


                                       # draw line segments
                                       seg_grobs <- grid::segmentsGrob(x0 = grid::unit(coords$x, "npc"), y0 = grid::unit(coords$y, "npc"),
                                                     x1 = grid::unit(coords$x, "npc"), y1 = grid::unit(coords$y + line_length, "npc"),
                                                     default.units = "npc",
                                                     arrow = NULL,
                                                     name = NULL, gp = grid::gpar(), vp = NULL)

                                       #add text
                                       text_grobs <- grid::textGrob(label = coords$label, x = unit(coords$x, "npc"), y = unit(coords$y + line_length, "npc"),
                                                       just = "left", rot = 60, gp = grid::gpar(fontsize = 8))
                                       grid::gTree(children = grid::gList(seg_grobs, text_grobs))
                               }
                       )

#' Add annotation to earthquake timeline
#'
#'#'Function adds an annotation to an earthquake timeline built with geom_timeline.  Annotation shares some aes with geom_timeline. User is responsible for making sure
#'values are the same between the two geoms.
#'
#'@param mapping Set of aesthetic mappings created by aes or aes_. If specified and inherit.aes = TRUE (the default),
#'it is combined with the default mapping at the top level of the plot. You must supply mapping if there is no plot mapping.
#'
#'@param data The data to be displayed in this layer. There are three options:
#'If NULL, the default, the data is inherited from the plot data as specified in the call to ggplot.
#'A data.frame, or other object, will override the plot data. All objects will be fortified to produce a data frame. See fortify for which variables will be created.
#'A function will be called with a single argument, the plot data. The return value must be a data.frame., and will be used as the layer data.
#'
#'@param stat The statistical transformation to use on the data for this layer, as a string.
#'
#'@param position Position adjustment, either as a string, or the result of a call to a position adjustment function.
#'
#'@param na.rm If FALSE, the default, missing values are removed with a warning. If TRUE, missing values are silently removed.
#'
#'@param show.legend logical. Should this layer be included in the legends? NA, the default, includes if any aesthetics are mapped.
#'FALSE never includes, and TRUE always includes.
#'
#'@param inherit.aes If FALSE, overrides the default aesthetics, rather than combining with them.
#'This is most useful for helper functions that define both data and aesthetics and shouldn't inherit behaviour from the default plot specification, e.g. borders.
#'
#'@param ... other arguments passed on to layer. These are often aesthetics, used to set an aesthetic to a fixed value, like color = "red" or size = 3.
#'They may also be parameters to the paired geom/stat.
#'
#' @return adds annotation to ggplot object (technically, returns NULL)
#'
#' @examples
#' \dontrun{
#' data("earthquakes")
#' earthquakes %>% eq_clean_data("LOCATION_NAME") %>%
#' dplyr::filter((COUNTRY == "MEXICO" | COUNTRY =="CANADA") & lubridate::year(DATE) >= 2000) %>%
#' ggplot() +
#' geom_timeline(aes(x = DATE, y = COUNTRY, size = EQ_PRIMARY, colour = TOTAL_DEATHS, fill = TOTAL_DEATHS)) +
#' geom_timeline_label(aes(x = DATE, y = COUNTRY, label = LOCATION_NAME, n_max = 10, max_aes = EQ_PRIMARY))
#'}
#'
#' @export
geom_timeline_label <- function(mapping = NULL, data = NULL, stat = "identity",
                         position = "identity", na.rm = FALSE,
                         show.legend = NA, inherit.aes = TRUE, ...) {


        ggplot2::layer(
                geom = GeomTimelineLabel, mapping = mapping,
                data = data, stat = stat, position = position,
                show.legend = show.legend, inherit.aes = inherit.aes,
                params = list(na.rm = na.rm, ...)
        )
}


#' Create earthquake map
#'
#' Function takes a data frame cleaned with eq_clean_data and creates a leadlet map that plots earthquakes with size based on the magnitude of the earthquake.
#' The function also takes a column name (as a string) to use to create hoverover labels for the point. Future work should focus on making eq_clean_data operate
#' like dplyr, taking column names rather than strings.
#'
#' @param clean_data_frame Data frame cleaned with eq_clean_data
#' @param annot_col Column to be used for creating label
#'
#' @return Leaflet map (technically returns NULL and draws map to plotting object)
#' @examples
#' \dontrun{
#' data("earthquakes")
#' earthquakes %>% eq_clean_data("LOCATION_NAME") %>%
#' dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
#' eq_map(annot_col = "DATE")
#'}
#'
#' @export
eq_map <- function(clean_data_frame, annot_col){
        leaflet_map <- leaflet::leaflet() %>% leaflet::addTiles() %>% leaflet::addCircleMarkers(data = clean_data_frame, radius = ~ EQ_PRIMARY * 1.5, lng = ~ LONGITUDE,
                                                                     lat = ~ LATITUDE, popup  = ~ clean_data_frame[[annot_col]])
        leaflet_map
}

#' Create label for mapping earthquake data
#'
#'Function creates a label column ("popup_text") in a data frame that has been cleaned with eq_clean_data.  The column contains html formatted text for use as a label
#'with the eq_map function. The label contains location, maginitude and deaths of each earthquake in the data frame.  If any earthquake is missing one of those things,
#'the label omits that line.
#'
#' @param clean_data_frame  Data frame of earthquake data that has been processed with eq_clean_data.
#'
#' @return List of character objects that can be used to create labels in eq_map.
#' @examples
#' \dontrun{
#' data("earthquakes")
#' earthquakes %>% eq_clean_data("LOCATION_NAME") %>%
#' dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
#' dplyr::mutate(popup_text = eq_create_label(.)) %>%
#' eq_map(annot_col = "popup_text")
#'}
#'
#' @export
eq_create_label <- function(clean_data_frame){

        # Create elements of label
        loc_line <- ifelse(is.na(clean_data_frame$LOCATION_NAME),"", paste0("<b>Locations: </b>",clean_data_frame$LOCATION_NAME,"<br/>"))
        mag_line <- ifelse(is.na(clean_data_frame$EQ_PRIMARY), "", paste0("<b>Magnitude: </b>",clean_data_frame$EQ_PRIMARY,"<br/>"))
        death_line <- ifelse(is.na(clean_data_frame$TOTAL_DEATHS), "",paste0("<b>Total deaths: </b>",clean_data_frame$TOTAL_DEATHS,"<br/>"))

        #Join elements to form label
        final_label <- paste0(loc_line, mag_line, death_line)
        final_label

}
