
#-------------------------------------------------------------#
#-------------------------------------------------------------#
### Functions built by Paul Bzonek to summarize and plot network analysis data
### Functions were made by modifying code from:
        #Author:Kim Whoriskey    |   Whoriskey et al. 2019
        #https://doi.org/10.1111/2041-210X.13188
              #code used to get network movements between receivers

        #Author:Christopher Chizinski
        #https://chrischizinski.github.io/rstats/igraph-ggplotll/
              #code used to plot networks with a ggplot base
#-------------------------------------------------------------#
#-------------------------------------------------------------#


#-------------------------------------------------------------#
### Function to add a console message when functions are run
#-------------------------------------------------------------#
#' @import shiny
#' @importFrom shinyjs runjs




#Bring in packages used in functions
library(ggplot2) #plotting package
#library(igraph) #plotting package
#library(rgdal) #geospatial package


#-------------------------------------------------------------#
### Function to summarize network data for analysis and plotting
#-------------------------------------------------------------#
#' @title Summarize Network Plot Data
#' @name network_summary
#'
#' @description #Summarize an input datatable to produce a dafaframe for plotting
#'
#' @param data The input datatable with FishID, ReceiverID, lat, and long columns
#' @param FishID A column in data describing fish identity. Save as a factor
#' @param ReceiverID A column in data describing acoustic reciever identity. Save as a factor
#' @param lat numeric
#' @param long numeric
#'
#' @returns receiver.location; moves.matrix; individual.moves; plot.data
#'
#' @export
#'
network_summary <- function(data, FishID, ReceiverID, lat, long, ...){
  data$FishID<- as.integer(FishID) #Show function where to find data
  data$ReceiverNames <- ReceiverID #Show function where to find data
  data$ReceiverID<-as.integer(as.factor(ReceiverID)) #Show function where to find data
  data$lat <- as.numeric(lat) #Show function where to find data
  data$long <- as.numeric(long) #Show function where to find data

  #Look for unique reciever-location pairs. This is important if one reciever has been moved to multiple locations
  data$UniqueID <- as.numeric(as.factor(interaction(data$ReceiverID, data$lat, data$long))) #Combine lat and long values to find unique combinations. Convert text to numbers
  data$UniqueID <- as.integer(as.numeric(as.factor(data$UniqueID))) #Make UniqueID numbers sequential

  fishunique <- unique(data$FishID) #build vector of unique fish in dataset
  from <- numeric() #the UniqueID the fish moved FROM
  to <- numeric() #the UniqueID the fish moved TO
  fish <- numeric() #the fish

    for(i in 1:length(fishunique)){ #Build loop to track the receiver movements per fish#
      fishsub <- data[data$FishID==fishunique[i],] ##subset data$FishID to the run of detections for one fish
      from <- append(from, rle(fishsub$UniqueID)$values[-length(rle(fishsub$UniqueID)$values)]) #build a list of UniqueIDs, and show all but last, making a 'from' vector
      to <- append(to, rle(fishsub$UniqueID)$values[-1]) #build a list of UniqueIDs, and show all but first, making a 'to' vector
      fish <- append(fish, rep(unique(fishsub$FishID), length(rle(fishsub$UniqueID)$values)-1)) #record the fish id
      } #end loop tracking fish movements

  #record the data
  individual.moves <- data.frame(from, to, fish) #movements between UniqueIDs
  moves.matrix <- table(individual.moves[,1:2]) #gives the pairwise counts of all fish moving from one UniqueID to another, in matrix form
  moves <- data.frame(moves.matrix) #summarizes the pairwise counts
  moves <- moves[moves$Freq!=0,] #summarizes the pairwise counts

  Receiver.locations <- data.frame(table(data[,c('ReceiverNames','UniqueID','lat','long')])) #Summarize frequency of pings at unique Receiver locations
  Receiver.locations <- Receiver.locations[Receiver.locations$Freq!=0,] #Remove 0s made by non-existenent receiver-lat/long combinations
  Receiver.locations$lat <- as.numeric(as.character(Receiver.locations$lat)) #add latitude to dataframe
  Receiver.locations$long <- as.numeric(as.character(Receiver.locations$long)) #add longitude to dataframe


  #Build the dataframe that will be used for plotting
  plot.data<-as.data.frame(moves) #New dataframe
  plot.data$from.x <- Receiver.locations$long[match(plot.data$from, Receiver.locations$UniqueID)] #match the matrix data with the 'reciver.locations' data
  plot.data$from.y <- Receiver.locations$lat[match(plot.data$from, Receiver.locations$UniqueID)] #match the matrix data with the 'reciver.locations' data
  plot.data$to.x <- Receiver.locations$long[match(plot.data$to, Receiver.locations$UniqueID)] #match the matrix data with the 'reciver.locations' data
  plot.data$to.y <- Receiver.locations$lat[match(plot.data$to, Receiver.locations$UniqueID)] #match the matrix data with the 'reciver.locations' data

  ###
  return(list(receiver.locations=Receiver.locations, moves.matrix=moves.matrix, individual.moves=individual.moves, plot.data=plot.data )) #Store function data in useful structure
  ###
} #end function






#-------------------------------------------------------------#
### Function to summarize network data for analysis and plotting
#-------------------------------------------------------------#
#' @title Plot Network Data
#' @name network_plot
#'
#' @description Summarize an input datatable to produce a dataframe for plotting
#'
#' @param data The input list created by the network_summary function. The list must include a dataframe titled `plot.data`.
#' @param Min.traffic Numeric. Specify if you want to ignore any lines below the traffic threshold. Default is 1.
#' @param shapefile A spatial object to overlay on the plot. Default is NA.
#' @param xlim Numeric. Optional limits for the x-axis.
#' @param ylim Numeric. Optional limits for the y-axis.
#' @param line.min Numeric. The minimum line width for the plot.
#' @param line.max Numeric. The maximum line width for the plot.
#' @param plot.title Character. Optional plot title.
#' @param y.axis Character. Label for the y-axis. Default is "latitude".
#' @param x.axis Character. Label for the x-axis. Default is "longitude".
#' @param labels Logical. If TRUE, adds labels to the receivers on the plot. Default is FALSE.
#' @param label.size Numeric. The size of the labels, if added. Default is 2.
#' @param label.transparency Numeric. Transparency of labels, if added. Default is 0.6.
#' @param label.nudge Numeric. Nudge the labels horizontally. Default is 0.
#' @param ... Other arguments passed to internal functions.
#'
#' @return A single ggplot object displaying the network.
#'
#' @import ggplot2
#'
#' @export
#'
network_plot <- function(data, #specify previously created network matrix
                         Min.traffic=1, #specify if you want ignore any lines below traffic threshold
                         shapefile=NA, xlim=NULL, ylim=NULL, #specify shapefile, and potential domain limits
                         #colour.gradient.low="blue", colour.gradient.high="red", #pick your own colour scale
                         line.min=0.5, line.max=2.5, #pick your own line weights
                         #receiver.shape=21, receiver.size=4, receiver.stroke=2, #customize the look of your nodes
                         plot.title=NULL, y.axis="latitude", x.axis="longitude", #speficy optional figure title (useful if looping across fish)
                         labels=FALSE, label.size=2, label.transparency=0.6, label.nudge=0, #add labels, and specify details
                         ...){ #Start function

  plot.data<-subset(data$plot.data, data$plot.data$Freq>Min.traffic-1) #Filter out network lines/edges below specified traffic



  #Plot network
  a<- ggplot()+ #Use ggplot base
    #geom_polygon(data = shapefile, aes(x = long, y = lat, group = group), colour = "dark grey", fill = NA)+ #plot provided shapefile
    geom_sf(data=shapefile, colour = "black", fill=NA, inherit.aes = FALSE)+
    geom_segment(data=plot.data, aes(x=from.x, xend = to.x, y=from.y, yend = to.y, size=Freq), colour='burlywood4', alpha=0.6) +  #Plot movement lines
    scale_size("line", range = c(line.min, line.max))+  #Set line-size range
    geom_point(data=data$receiver.locations, aes(x=long, y=lat, fill=Freq), size=3, shape=21, stroke=1.3)+  #Add reciever circles, colour by matrix frequency
    scale_fill_viridis_c()+
    #coord_fixed(xlim=xlim, ylim=ylim)+  #Lock aspect ratio. Specify limits if shapefile is too big
    ylab(y.axis)+ #y axis label
    xlab(x.axis)+ # x axis label
    ggtitle(as.character(plot.title))+ #add optional title
    theme_classic() #simple plot theme


  #If else loop to look for optional labels.
  if(isFALSE(labels)){c <- NULL} else{ #Ignore the lines below if labels are not wanted
    c<-geom_label(data=data$receiver.locations, #add labels for receiver names
          aes(x=long, y=lat, label=(data$receiver.locations$ReceiverNames)), #add labels for receiver names
          hjust = 0, size=label.size, alpha=label.transparency, nudge_x=label.nudge) #add labels for receiver names
    } #end if else loop
  ###
  print(a + c) #print plot with optional shapefile and labels
  ###
} #end function
