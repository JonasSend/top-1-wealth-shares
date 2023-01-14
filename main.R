

library(leaflet)
library(tidyverse)
library(readxl)
library(geojsonio)
library(htmltools)
library(htmlwidgets)

# downloaded from wid.world on 2023-01-10
top_shares_dta_wide <- read_excel("top_1_share_2021.xlsx")
# downloaded from geojson-maps.ash.ms on 2023-01-11
countries <- geojson_read("countries.json", what = "sp")

country_name <- top_shares_dta_wide %>% 
  names() %>% 
  tail(n = -2) %>% 
  sub(pattern=".*\n", replacement="")

# adjust names
country_name[country_name == "USA"] <- "United States"
country_name[country_name == "Korea"] <- "Republic of Korea"
country_name[country_name == "North Korea"] <- "Dem. Rep. Korea"
country_name[country_name == "Cote d’Ivoire"] <- "Côte d'Ivoire"
country_name[country_name == "Gambia"] <- "The Gambia"
country_name[country_name == "Syrian Arab Republic"] <- "Syria"
country_name[country_name == "Sao Tome and Principe"] <- "São Tomé and Principe"
country_name[country_name == "Cabo Verde"] <- "Republic of Cabo Verde"
country_name[country_name == "Congo"] <- "Republic of the Congo"
country_name[country_name == "DR Congo"] <- "Democratic Republic of the Congo"
country_name[country_name == "Swaziland"] <- "Kingdom of eSwatini"
country_name[country_name == "Viet Nam"] <- "Vietnam"
# add Greenland to Denmark
countries@data$name_long[countries@data$name_long == "Greenland"] <- "Denmark"

top_1_wealth_share <- top_shares_dta_wide[-c(1, 2)] %>% 
  unlist() %>% unname()

countries@data$top_1_wealth_share <- top_1_wealth_share[match(countries@data$name_long, country_name)]*100

labels <- lapply(ifelse(!(is.na(countries@data$top_1_wealth_share)),
                        sprintf("<strong>%s</strong><br/>Top 1&percnt; wealth share: %g&percnt;",
                         countries@data$name_long, countries@data$top_1_wealth_share),
                        sprintf("<strong>%s</strong><br/>No data available",
                                countries@data$name_long)),
                 HTML)

colorFactors = colorBin(c("darkblue", "khaki", "darkred"), bins = 10,
                        domain = countries@data$top_1_wealth_share)

map <- leaflet(options = leafletOptions(minZoom = 1.5)) %>%
  addProviderTiles("Esri.WorldGrayCanvas") %>%
  addPolygons(data = countries, 
              fillColor = ~colorFactors(countries@data$top_1_wealth_share), color = "black",
              fillOpacity = 0.8, opacity = 1,
              weight = .5, highlightOptions = highlightOptions(weight = 3,
                                                               bringToFront = TRUE),
              label = labels,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto", sticky = FALSE)) %>%
  addLegend("bottomleft", pal = colorFactors, values = countries@data$top_1_wealth_share,
            title = HTML("Top 1% wealth<br/>share in %"))

saveWidget(map, file = "map.html")




