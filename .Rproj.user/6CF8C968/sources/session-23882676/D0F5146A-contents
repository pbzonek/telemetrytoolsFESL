#working script
library(tidyverse)
library(data.table)
test_data <- data_det_raw %>%
 mutate(FishID = as.numeric(as.factor(animal_id)),
        ReceiverID=transmitter_id,
        lat=deploy_lat,
        long=deploy_long)

test_network_df <- network_summary(data = test_data,
                                FishID=test_data$FishID,
                                ReceiverID=test_data$ReceiverID,
                                lat = test_data$lat,
                                long = test_data$long)

test_network_df$receiver.locations
test_network_df$individual.moves
test_network_df$plot.data


test_plot <- network_plot(data = test_network_df,
                     Min.traffic=3, #specify if you want ignore any lines below traffic threshold
                     shapefile=HH_shapefile_WGS84,  #specify shapefile, and potential domain limits
                     #colour.gradient.low="grey", colour.gradient.high="purple", #pick your own colour scale
                     line.min=1, line.max=3, #pick your own line weights
                     #receiver.shape=22, receiver.size=6, receiver.stroke=4, #customize the look of your nodes
                     plot.title="Network plot", #speficy optional figure title (useful if looping across fish)
                     #labels=TRUE, label.size=1.8, label.transparency=0.5, label.nudge=0.00009, #add labels, and specify details
                     )
test_plot <- test_plot+
  scale_x_continuous(limits=c(min(test_network_df$plot.data$to.x)-0.1, min(test_network_df$plot.data$to.x)+0.2))+
  scale_y_continuous(limits=c(min(test_network_df$plot.data$to.y)-0.01, max(test_network_df$plot.data$to.y)+0.01))

test_plot
