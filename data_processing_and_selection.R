# load packages ####
library(readxl)
library(dplyr)

# upload files ####
setwd("C:/Users/tara/Desktop/carbon_project_1/")
data <- read.csv("ff_cond_coordinates.csv")

# select data ####
colnames(data)

selected_data <- data
selected_data$plot_ID <- paste(data$STATECD, 
                               data$COUNTYCD, 
                               data$PLOT, 
                               sep = "_")
selected_data <- unique(selected_data[c("plot_ID", "LAT", "LON")])

selected_data <- selected_data[!is.na(selected_data$LAT),]

write.csv(selected_data, file = "fuzzy_coordinates.csv", row.names = F )


# Selected coordinates ####
raw_coord <- read.csv("ff_cond_coordinates.csv")
mod_coor <- raw_coord
mod_coor$plot_ID <- paste(mod_coor$STATECD, 
                          mod_coor$COUNTYCD, 
                          mod_coor$PLOT, 
                          #selected_coordinates$CONDID,
                          sep = "_")

fuzzy_coordinates <- read.csv("fuzzy_coordinates.csv") 

selected_coordinates <- final_plot_data
selected_coordinates$plot_ID <- paste(selected_coordinates$STATECD, 
                                      selected_coordinates$COUNTYCD, 
                                      selected_coordinates$PLOT, 
                                      #selected_coordinates$CONDID,
                                      sep = "_")
selected_coordinates <- unique(selected_coordinates[c("plot_ID")])
selected_coordinates <- right_join(fuzzy_coordinates, selected_coordinates)

selected_coordinates <- selected_coordinates[!is.na(selected_coordinates$LAT),]
selected_coordinates <- as.data.frame(unique(selected_coordinates))

write.csv(selected_coordinates, "selected_coordinates.csv")

# raw coordinate handling ####

# upload files ####
setwd("C:/Users/tara/Desktop/carbon_project_1/")
data <- read_xlsx("fed forest cond data 04-03-2026.xlsx")

# select data ####
colnames(data)

selected_data <- data
selected_data$plot_ID <- paste(data$STATECD, 
                               data$COUNTYCD, 
                               data$PLOT, 
                               sep = "_")
selected_data <- unique(selected_data[c("plot_ID", "SDS_ACTUAL_LAT", "SDS_ACTUAL_LON", "MEASYEAR")])
# Source - https://stackoverflow.com/a/35469576
# Posted by Xi Liang, modified by community. See post 'Timeline' for change history
# Retrieved 2026-04-09, License - CC BY-SA 4.0
selected_data <- selected_data %>% group_by(plot_ID) %>% top_n(1, MEASYEAR) # gets most recent year of inventory plots
selected_data <- selected_data[,-4]


selected_data <- selected_data[!is.na(selected_data$SDS_ACTUAL_LAT),]

#write.csv(selected_data, file = "fuzzy_coordinates.csv", row.names = F )


# Selected coordinates ####

fuzzy_coordinates <- selected_data

selected_coordinates <- final_plot_data
selected_coordinates$plot_ID <- paste(selected_coordinates$STATECD, 
                                      selected_coordinates$COUNTYCD, 
                                      selected_coordinates$PLOT, 
                                      #selected_coordinates$CONDID,
                                      sep = "_")
selected_coordinates <- unique(selected_coordinates[c("plot_ID")])
selected_coordinates <- right_join(fuzzy_coordinates, selected_coordinates)

selected_coordinates <- selected_coordinates[!is.na(selected_coordinates$SDS_ACTUAL_LAT),]

selected_coordinates <- as.data.frame(unique(selected_coordinates))

write.csv(selected_coordinates, "selected_coordinates_raw.csv")

# combine single point buffer and raw coordinates
single <- read.csv("5280.0ftsingle.csv")
raw <- read.csv("selected_coordinates_raw.csv")
single <- single[,-1]
raw <- raw[,-1]
combined <- bind_rows(single, raw)
write.csv(combined, "combined_coordinates.csv")

