venues %>% mutate(venue_lon=as.numeric(mapply(geocode,paste(venue, venue_state, "Australia", sep=", "), sensor=TRUE)[1,]),
                  venue_lat=as.numeric(mapply(geocode,paste(venue, venue_state, "Australia", sep=", "))[2,])) -> venues


venues %>% mutate_geocode(venue) -> venues
