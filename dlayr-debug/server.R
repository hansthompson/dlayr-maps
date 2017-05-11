library(shiny);library(leaflet);library(RColorBrewer)
library(htmltools);library(dlayr);library(dplyr);library(XML);library(lubridate)

source("http://akdata.org/tidy-gps-functions/anchorage-ak-peoplemover.R")
function(input, output, session) {

#    get_data <- reactive({
#      load("tidy_gps_obj.rda")
#      load("my_daily_gtfs.rda") #gtfs
#      (get_data <- list(tidy_gps_obj, my_daily_gtfs))
#    })

  output$map <- renderLeaflet({
    leaflet() %>% setView(lat = 61.15, lng = -149.8,zoom = 11) %>% addTiles()
  })
  
  observe({
    invalidateLater(4000)
    load("tidy_gps_obj.rda")
    #tidy_gps_obj <- get_tidy_gps()
    load("my_daily_gtfs.rda") #gtfs
    get_data <- list(tidy_gps_obj, my_daily_gtfs)
    gps_n_gtfs <- get_data
    delay_data <- calculate_delays(my_gps_data = gps_n_gtfs[[1]], gtfs_today = gps_n_gtfs[[2]], lat_factor = 2.1)
    
    if(input$route != "All") {
      gps_points <- delay_data[as.character(delay_data$route) == input$route,]
      trip_ids <- gps_n_gtfs[[2]]$todays_trip_departures %>% ungroup() %>% mutate(route_id = as.character(route_id)) %>% 
        filter(route_id == input$route) %>% .$trip_id
      route_to_map <- gps_n_gtfs[[2]]$today_stop_times %>% filter(trip_id == trip_ids[1]) %>% inner_join(gps_n_gtfs[[2]]$stops, by = "stop_id")
      gps_delays <- gps_points %>% mutate(html = paste(paste0("delay is: ", delay), 
                                                       paste0("estimated trip id is: ", trip_id), 
                                                       paste0("scheduled departure time from last stop: ", departure_time), 
                                                       paste0("scheduled arrival time at next stop: ", arrival_time), 
                                                       paste0("proportion between stops: ", ratio_complete), 
                                                       sep = "<br/>"))
      
      leafletProxy("map") %>%  addTiles()  %>%
        clearShapes() %>% clearMarkers() %>%
        addMarkers(data = gps_delays, lng = ~gps_lon, lat = ~gps_lat, popup = gps_delays$html) %>%
        addCircleMarkers(data = gps_delays, lng = ~A_lon, lat = ~A_lat, radius = ~A_dist * 2000, color = "green") %>%
        addCircleMarkers(data = gps_delays, lng = ~B_lon, lat = ~B_lat, radius = ~B_dist * 2000, color = "red") %>%
        addPolylines(dat = route_to_map, lng = ~stop_lon, lat = ~stop_lat, color = "green")
      
    } else {
      gps_points <- delay_data
      
      gps_delays <- gps_points %>% mutate(html = paste(paste0("delay is: ", delay), 
                                                       paste0("estimated trip id is: ", trip_id), 
                                                       paste0("scheduled departure time from last stop: ", departure_time), 
                                                       paste0("scheduled arrival time at next stop: ", arrival_time), 
                                                       paste0("proportion between stops: ", ratio_complete), 
                                                       sep = "<br/>"))
      
      leafletProxy("map") %>%  addTiles()  %>%
        clearShapes() %>% clearMarkers() %>%
        addMarkers(data = gps_delays, lng = ~gps_lon, lat = ~gps_lat, popup = gps_delays$html) %>%
        addCircleMarkers(data = gps_delays, lng = ~A_lon, lat = ~A_lat, radius = ~A_dist * 2000, color = "green") %>%
        addCircleMarkers(data = gps_delays, lng = ~B_lon, lat = ~B_lat, radius = ~B_dist * 2000, color = "red") 
      }

  })
}

#  leaflet(surrounding_stops) %>% addTiles() %>% 
#    addCircles(~A_lon, ~A_lat, radius = ~A_dist * 50000, weight = 1, color = "green") %>%
#    addMarkers(~gps_lon, ~gps_lat) %>% 
#    addCircles(~B_lon, ~B_lat, radius = ~B_dist * 50000, weight = 1, color = "red") %>%
#    addPolylines(data = inner_join((gtfs_today$today_stop_times   %>% filter(trip_id %in% surrounding_stops$trip_id)), 
#                                   (gtfs_today$all_stop_sequences  %>% ungroup()%>% select(-stop_sequence)), by = "stop_id") %>% 
#                   group_by(stop_sequence) %>% 
#                   filter(row_number() == 1), lng = ~stop_lon, lat = ~stop_lat, color = "yellow") 



