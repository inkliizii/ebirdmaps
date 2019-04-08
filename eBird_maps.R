library(auk)
library(ggplot2)
library(rgdal)
library(tidyverse)
library(plyr)
library(lubridate)
library(shiny)
library(shinyWidgets)
library(rsconnect)


# read in data----
DICK <- read_ebd("~/Data/eBird_data_species/ebd_dickci_relFeb-2019.txt")
GRSP <- read_ebd("~/Data/eBird_data_species/ebd_graspa_relFeb-2019.txt")
EAME <- read_ebd("~/Data/eBird_data_species/ebd_easmea_relFeb-2019.txt")

write.csv(DICK_points,file = "~/Data/ebird/DICK_ebird.csv")
write.csv(GRSP_points,file = "~/Data/ebird/GRSP_ebird.csv")
write.csv(EAME_points,file = "~/Data/ebird/EAME_ebird.csv")

bcr <- readOGR("~/Data/bcr_terrestrial_shape", "BCR_Terrestrial_master")
bcr@data$id <- rownames(bcr@data)
bcr_points <- fortify(bcr, region = "id")
bcr_df <- join(bcr_points, bcr@data, by = "id")

us <- readOGR("~/Data/Map stuff/USA", "states")
us@data$id <- rownames(us@data)
us_points <- fortify(us, region = "id")
us_df <- join(us_points, us@data, by = "id")
usa <- subset(us_df, STATE_NAME != "Hawaii" & STATE_NAME != "Alaska")
usa <- usa[c(1, 2, 7, 8)]
names(usa) <- c("long", "lat", "group", "name")
ca <- readOGR("~/Data/Map stuff/Canada", "Canada_AL263")
ca@data$id <- rownames(ca@data)
ca_points <- fortify(ca, region = "id")
ca_df <- join(ca_points, ca@data, by = "id")
canada <- subset(ca_df, name == "British Columbia" | name == "Saskatchewan" | name == "Alberta" | name == "Manitoba" | name == "Ontario")
canada <- canada[c(1, 2, 7, 9)]
canada$group <- as.numeric(as.character(canada$group))
canada$group <- canada$group + 50
canada$group <- as.factor(canada$group)


na <- rbind(usa, canada)


bcr_df_subset <- subset(bcr_df, BCR == 20 | BCR == 21 | BCR == 18 | BCR == 19 | BCR == 22 | BCR == 17 | BCR == 11)

# GRSP map----
GRSP_bcr <- subset(GRSP, bcr_code == 20 | bcr_code == 21 | bcr_code == 18 | bcr_code == 19 | bcr_code == 22 | bcr_code == 17 | bcr_code == 11)
GRSP_bcr$observation_count <- as.numeric(GRSP_bcr$observation_count)
GRSP_points <- GRSP_bcr[c(6, 8, 25, 26, 27)]
GRSP_points <- GRSP_points[complete.cases(GRSP_points), ]
GRSP_points$observation_count <- as.numeric(GRSP_points$observation_count)
GRSP_points$date <- yday(GRSP_points$observation_date)
GRSP_points$year <- substr(GRSP_points$observation_date, 1, 4)
GRSP_points <- subset(GRSP_points, GRSP_points$date > 91 & GRSP_points$date < 212 & GRSP_points$year >= "2008")
ggplot() +
  geom_polygon(data = na, aes(x = long, y = lat, group = group), color = "black", fill = NA) +
  # geom_path(data=bcr_df_subset,aes(x=long,y=lat))+
  coord_equal() +
  geom_point(data = GRSP_points, aes(x = longitude, y = latitude, alpha = observation_count)) +
  theme_void()

# DICK map----
DICK_bcr <- subset(DICK, bcr_code == 20 | bcr_code == 21 | bcr_code == 18 | bcr_code == 19 | bcr_code == 22 | bcr_code == 17 | bcr_code == 11)
DICK_bcr$observation_count <- as.numeric(DICK_bcr$observation_count)
DICK_points <- DICK_bcr[c(6, 8, 25, 26, 27)]
DICK_points <- DICK_points[complete.cases(DICK_points), ]
DICK_points$observation_count <- as.numeric(DICK_points$observation_count)
DICK_points$date <- yday(DICK_points$observation_date)
DICK_points <- subset(DICK_points, DICK_points$date > 91 & DICK_points$date < 212)
DICK_points$year <- substr(DICK_points$observation_date, 1, 4)
DICK_points <- subset(DICK_points, DICK_points$date > 91 & DICK_points$date < 212 & DICK_points$year >= "2008")
ggplot() +
  geom_polygon(data = bcr_df_subset, aes(x = long, y = lat, group = group), color = "black", fill = NA) +
  # geom_path(data=bcr_df_subset,aes(x=long,y=lat))+
  coord_equal() +
  geom_point(data = DICK_points, aes(x = longitude, y = latitude, alpha = observation_count)) +
  theme_void()

# EAME map----
EAME_bcr <- subset(EAME, bcr_code == 20 | bcr_code == 21 | bcr_code == 18 | bcr_code == 19 | bcr_code == 22 | bcr_code == 17 | bcr_code == 11)
EAME_bcr$observation_count <- as.numeric(EAME_bcr$observation_count)
EAME_points <- EAME_bcr[c(6, 8, 25, 26, 27)]
EAME_points <- EAME_points[complete.cases(EAME_points), ]
EAME_points$observation_count <- as.numeric(EAME_points$observation_count)
EAME_points$date <- yday(EAME_points$observation_date)
EAME_points <- subset(EAME_points, EAME_points$date > 91 & EAME_points$date < 212)
EAME_points$year <- substr(EAME_points$observation_date, 1, 4)
EAME_points <- subset(EAME_points, EAME_points$date > 91 & EAME_points$date < 212 & EAME_points$year >= "2008")
ggplot() +
  geom_polygon(data = bcr_df_subset, aes(x = long, y = lat, group = group), color = "black", fill = NA) +
  # geom_path(data=bcr_df_subset,aes(x=long,y=lat))+
  coord_equal() +
  geom_point(data = EAME_points, aes(x = longitude, y = latitude, alpha = observation_count)) +
  theme_void()


# shiny map----

# data----
# read ebird data
DICK <- read_ebd("~/Data/eBird_data_species/ebd_dickci_relFeb-2019.txt")
GRSP <- read_ebd("~/Data/eBird_data_species/ebd_graspa_relFeb-2019.txt")
EAME <- read_ebd("~/Data/eBird_data_species/ebd_easmea_relFeb-2019.txt")
# spatial extent
bcr <- readOGR("~/Data/bcr_terrestrial_shape", "BCR_Terrestrial_master")
bcr@data$id <- rownames(bcr@data)
bcr_points <- fortify(bcr, region = "id")
bcr_df <- join(bcr_points, bcr@data, by = "id")
bcr_df_subset <- subset(bcr_df, BCR == 20 | BCR == 21 | BCR == 18 | BCR == 19 | BCR == 22 | BCR == 17 | BCR == 11)
# map data
us <- readOGR("~/Data/Map stuff/USA", "states")
us@data$id <- rownames(us@data)
us_points <- fortify(us, region = "id")
us_df <- join(us_points, us@data, by = "id")
usa <- subset(us_df, STATE_NAME != "Hawaii" & STATE_NAME != "Alaska")
usa <- usa[c(1, 2, 7, 8)]
names(usa) <- c("long", "lat", "group", "name")
ca <- readOGR("~/Data/Map stuff/Canada", "Canada_AL263")
ca@data$id <- rownames(ca@data)
ca_points <- fortify(ca, region = "id")
ca_df <- join(ca_points, ca@data, by = "id")
canada <- subset(ca_df, name == "British Columbia" | name == "Saskatchewan" | name == "Alberta" | name == "Manitoba" | name == "Ontario")
canada <- canada[c(1, 2, 7, 9)]
canada$group <- as.numeric(as.character(canada$group))
canada$group <- canada$group + 50
canada$group <- as.factor(canada$group)


na <- rbind(usa, canada)
# GRSP data
GRSP_bcr <- subset(GRSP, bcr_code == 20 | bcr_code == 21 | bcr_code == 18 | bcr_code == 19 | bcr_code == 22 | bcr_code == 17 | bcr_code == 11)
gp <- readOGR("~/Data/Great_Plains_LCC_Boundary")
gp@data$id <- rownames(gp@data)
gp_points <- fortify(gp, region = "id")
gp_df <- join(gp_points, gp@data, by = "id")
GRSP_bcr$observation_count <- as.numeric(GRSP_bcr$observation_count)
GRSP_points <- GRSP_bcr[c(6, 8, 25, 26, 27)]
GRSP_points <- GRSP_points[complete.cases(GRSP_points), ]
GRSP_points$observation_count <- as.numeric(GRSP_points$observation_count)
GRSP_points$date <- yday(GRSP_points$observation_date)
GRSP_points <- subset(GRSP_points, GRSP_points$date > 91 & GRSP_points$date < 212)
# DICK data
DICK_bcr <- subset(DICK, bcr_code == 20 | bcr_code == 21 | bcr_code == 18 | bcr_code == 19 | bcr_code == 22 | bcr_code == 17 | bcr_code == 11)
DICK_bcr$observation_count <- as.numeric(DICK_bcr$observation_count)
DICK_points <- DICK_bcr[c(6, 8, 25, 26, 27)]
DICK_points <- DICK_points[complete.cases(DICK_points), ]
DICK_points$observation_count <- as.numeric(DICK_points$observation_count)
DICK_points$date <- yday(DICK_points$observation_date)
DICK_points <- subset(DICK_points, DICK_points$date > 91 & DICK_points$date < 212)
# EAME data
EAME_bcr <- subset(EAME, bcr_code == 20 | bcr_code == 21 | bcr_code == 18 | bcr_code == 19 | bcr_code == 22 | bcr_code == 17 | bcr_code == 11)
EAME_bcr$observation_count <- as.numeric(EAME_bcr$observation_count)
EAME_points <- EAME_bcr[c(6, 8, 25, 26, 27)]
EAME_points <- EAME_points[complete.cases(EAME_points), ]
EAME_points$observation_count <- as.numeric(EAME_points$observation_count)
EAME_points$date <- yday(EAME_points$observation_date)
EAME_points <- subset(EAME_points, EAME_points$date > 91 & EAME_points$date < 212)

# app----
shinyApp(
  ui <- fixedPage({
    titlePanel(title = h4("Abundance Map", align = "center"))
    fixedRow(
      column(
        3,
        sliderTextInput("year", h3("Year"),
          choices = list("2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019"),
          animate= animationOptions(loop=TRUE)
        )
      ),
      fixedRow(
        column(
          3,
          radioButtons("radio", h3("Species"),
            choices = list("Grasshopper Sparrow", "Dickcissel", "Eastern Meadowlark")
          )
        ),
        mainPanel(
          column(10,
          textOutput("total"),
          hr(),
          plotOutput("Map", height = 800, width = 800)
        )
      )
    )
    )
  }),
  server <- function(input, output) {
    species <- reactive({
      switch(input$radio,
        "Grasshopper Sparrow" = subset(GRSP_points, year == input$year),
        "Dickcissel" = subset(DICK_points, year == input$year),
        "Eastern Meadowlark" = subset(EAME_points, year == input$year)
      )
    })
    output$Map <- renderPlot({
      p <- ggplot() +
        geom_path(data = na, aes(x = long, y = lat, group = group), color = "black") +
        coord_equal() +
        geom_point(data = species(), aes(x = longitude, y = latitude, alpha = .5)) +
        theme_void() +
        theme(panel.background = element_blank(), legend.position = "none")
      p
    })
  output$total <- renderText({
    paste("Total number of individuals:",
    sum(species()$observation_count))
  })
  }
)
