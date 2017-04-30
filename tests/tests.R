library(testthat)
library(tidyverse)
library(lubridate)
library(swdevcapstone)

#test eq_clean_data
test_that("data_clean",{
        #should warn about some lines failing to parse
        expect_warning(eq_clean_data(earthquakes, "LOCATION_NAME"), regexp = "parse")

        data_clean_test <- eq_clean_data(earthquakes, "LOCATION_NAME")
        expect_that(data_clean_test$DATE, is_a("Date"))
        expect_that(data_clean_test$LATITUDE, is_a("numeric"))
        expect_that(data_clean_test$LOCATION_NAME, is_a("character"))
})

#test both geoms
test_that("geom_tests",{
        #check for expected warning
                g <-  earthquakes %>%  eq_clean_data("LOCATION_NAME") %>%
                ggplot2::ggplot() +
                geom_timeline(aes(x = DATE, y = COUNTRY, size = EQ_PRIMARY, colour = TOTAL_DEATHS, fill = TOTAL_DEATHS)) +
                geom_timeline_label(aes(x = DATE, y = COUNTRY, label = LOCATION_NAME, n_max = 10, max_aes = LOCATION_NAME))

        expect_that(g, is_a("ggplot"))
})

#test map functions
test_that("map_tests",{
        test_map_data <- earthquakes %>%
                eq_clean_data("LOCATION_NAME") %>%
                dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
                dplyr::mutate(popup_text = eq_create_label(.))
        expect_that(test_map_data$popup_text, is_a("character"))

        test_map <- test_map_data %>% eq_map(annot_col = "popup_text")
        expect_that(test_map, is_a("leaflet"))
})


