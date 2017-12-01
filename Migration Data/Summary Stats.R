#Clear Memory
rm(list = ls())


#Load Libraries
library(sna)
library(visNetwork)
library(threejs)
library(networkD3)
library(ggplot2)
library(ggstance)
library(ggmcmc)
library(coda)
library(gdata)
library(readxl)
library(XLConnect)
library(dplyr)
library(foreign)
library(haven)
library(plyr)
library(statnet)
library(igraph)
library(network)

#Set working directory
setwd('~/Dropbox/github/Migration Data')

#Loading Migration Data
states0509 <-get(load('states0509.Rdata'))
states0610 <-get(load('states0610.Rdata'))
states0711 <-get(load('states0711.Rdata'))
states0812 <-get(load('states0812.Rdata'))
states0913 <-get(load('states0913.Rdata'))
states1014 <-get(load('states1014.Rdata'))
states1115 <-get(load('states1115.Rdata'))

#Loading Covariate Data
county_covariate <-get(load('county_covariate.Rdata'))

#Merge covariate information with migration data 
#Changing column name for merge
colnames(county_covariate)[1] <- "sendfips"

#Exclusing Alaska and Hawaii (sending and receiving)
#Identifying counties in Alaska
subset(county_covariate$sendfips,county_covariate$state==2)

#Removing them from data frame

alaska <- list("2013", "2016", "2020", "2050", "2060", "2068", "2070", "2090", "2100",
               "2105", "2110", "2122", "2130", "2150", "2164", "2170", "2180", "2185",
               "2188", "2195", "2198", "2220", "2230", "2240", "2261", "2270", "2275",
               "2282", "2290")

states1115 <- states1115[!(states1115$sendfips %in% alaska), ]
states1115 <- states1115[!(states1115$recfips %in% alaska), ]

#Identifying counties in Hawaii
subset(county_covariate$sendfips,county_covariate$state==15)

#Removing them from data frame
hawaii <-list("15001", "15003", "15005", "15007", "15009")

states1115 <- states1115[!(states1115$sendfips %in% hawaii), ]
states1115 <- states1115[!(states1115$recfips %in% hawaii), ]

#Subsetting covariate information to exclude Alaska and Hawaii 
states0509 <- subset(states0509, states0509$sendfips!="NA")
county_covariate <- subset(county_covariate, county_covariate$state!=2)
county_covariate <- subset(county_covariate, county_covariate$state!=15)

#Creating edgelist from data
#2011 - 2015
edgelist_states1115 <- as.matrix(states1115)

graph_states1115 = graph.edgelist(edgelist_states1115[,1:2])

#Adding weights 
E(graph_states1115)$weight = as.numeric(edgelist_states1115[,3])

#Extracing adjacency matrix
adjacency_matrix_states_1115 <- as_adjacency_matrix(graph_states1115, names=T, type="both", attr="weight")

#Visualize the data
library(maps)
library(geosphere)

usa <- map_data("state")


p1 <-ggplot()+geom_polygon(data=usa, aes(x=long, y=lat, group=group)) + 
  geom_point(data=county_covariate, aes(x=Longitude, y=Latitude)) 

p1


