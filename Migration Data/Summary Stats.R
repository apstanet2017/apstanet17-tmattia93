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
library(sand)
library(intergraph)
library(ergm)
library(maps)
library(geosphere)

#Set working directory
setwd('~/Dropbox/github/Migration Data')

#Loading Migration Data
#states0509 <-get(load('states0509.Rdata'))
#states0610 <-get(load('states0610.Rdata'))
#states0711 <-get(load('states0711.Rdata'))
#states0812 <-get(load('states0812.Rdata'))
#states0913 <-get(load('states0913.Rdata'))
#states1014 <-get(load('states1014.Rdata'))
states1115 <-get(load('states1115.Rdata'))

#Loading Covariate Data
county_covariate <-get(load('county_covariate.Rdata'))

#Merge covariate information with migration data 

#Changing column name for merge
colnames(county_covariate)[1] <- "sendfips"

#Exclusing Alaska and Hawaii (sending and receiving)
#Identifying counties in Alaska
subset(county_covariate$sendfips, county_covariate$state==2)

#Removing them from data frame

alaska <- list("2013", "2016", "2020", "2050", "2060", "2068", "2070", "2090", "2100",
               "2105", "2110", "2122", "2130", "2150", "2158", "2164", "2170", "2180", "2185",
               "2188", "2195", "2198", "2220", "2230", "2240", "2261", "2270", "2275",
               "2282", "2290")

states1115 <- states1115[!(states1115$sendfips %in% alaska), ]
states1115 <- states1115[!(states1115$recfips %in% alaska), ]

#Identifying counties in Hawaii
subset(county_covariate$sendfips,county_covariate$state==15)

#Removing them from data frame
hawaii <-list("15001", "15003", "15005", "15007", "15009", "46102")

states1115 <- states1115[!(states1115$sendfips %in% hawaii), ]
states1115 <- states1115[!(states1115$recfips %in% hawaii), ]

#Subsetting covariate information to exclude Alaska and Hawaii 
states1115 <- subset(states1115, states1115$sendfips!="NA")
county_covariate <- subset(county_covariate, county_covariate$state!=2)
county_covariate <- subset(county_covariate, county_covariate$state!=15)

#Merging migration information with covariate data
states1115_migration_complete <- merge(states1115, county_covariate, by="sendfips", all.x=T)

###########################################################################################################
###########################################################################################################
########################################### Constructing Network ########################################## 
###########################################################################################################
###########################################################################################################
#1) Migration from 2011 - 2015

#Creating edgelist from data
edgelist_states1115 <- as.matrix(states1115)

#Creating graph from the edgelist
graph_states1115 = graph.edgelist(edgelist_states1115[,1:2], directed=T)

#Adding weights 
E(graph_states1115)$weight = as.numeric(edgelist_states1115[,3])

#Extracing adjacency matrix
adjacency_matrix_states_1115 <- as_adjacency_matrix(graph_states1115, names=T, type="both", attr="weight")

#Setting vertex attributes (nodal covariates)
#1) Need to remove spaces from name string
V(graph_states1115)$name <- gsub(" ", "", V(graph_states1115)$name, fixed = TRUE)

#2) Setting vertex attributes
#Location Descriptors
V(graph_states1115)$state_name = as.character(county_covariate$statename[match(V(graph_states1115)$name, county_covariate$sendfips)])
V(graph_states1115)$state_number = as.character(county_covariate$state[match(V(graph_states1115)$name, county_covariate$sendfips)])
V(graph_states1115)$county_name = as.character(county_covariate$countyname[match(V(graph_states1115)$name, county_covariate$sendfips)])
V(graph_states1115)$county_string = as.character(county_covariate$countystring[match(V(graph_states1115)$name, county_covariate$sendfips)])
V(graph_states1115)$latitude = as.character(county_covariate$Latitude[match(V(graph_states1115)$name, county_covariate$sendfips)])
V(graph_states1115)$longitude = as.character(county_covariate$Longitude[match(V(graph_states1115)$name, county_covariate$sendfips)])

#Household income (2009 - 2015)
V(graph_states1115)$hhincome_2009 = as.character(county_covariate$hhincome_2009[match(V(graph_states1115)$name, county_covariate$sendfips)])
V(graph_states1115)$hhincome_2010 = as.character(county_covariate$hhincome_2010[match(V(graph_states1115)$name, county_covariate$sendfips)])
V(graph_states1115)$hhincome_2011 = as.character(county_covariate$hhincome_2011[match(V(graph_states1115)$name, county_covariate$sendfips)])
V(graph_states1115)$hhincome_2012 = as.character(county_covariate$hhincome_2012[match(V(graph_states1115)$name, county_covariate$sendfips)])
V(graph_states1115)$hhincome_2013 = as.character(county_covariate$hhincome_2013[match(V(graph_states1115)$name, county_covariate$sendfips)])
V(graph_states1115)$hhincome_2014 = as.character(county_covariate$hhincome_2013[match(V(graph_states1115)$name, county_covariate$sendfips)])
V(graph_states1115)$hhincome_2015 = as.character(county_covariate$hhincome_2013[match(V(graph_states1115)$name, county_covariate$sendfips)])

#Unemployment rate (2009 - 2015)
V(graph_states1115)$unemployment_rate_2009 = as.character(county_covariate$unemploymentrate_2009[match(V(graph_states1115)$name, county_covariate$sendfips)])
V(graph_states1115)$unemployment_rate_2010 = as.character(county_covariate$unemploymentrate_2010[match(V(graph_states1115)$name, county_covariate$sendfips)])
V(graph_states1115)$unemployment_rate_2011 = as.character(county_covariate$unemploymentrate_2011[match(V(graph_states1115)$name, county_covariate$sendfips)])
V(graph_states1115)$unemployment_rate_2012 = as.character(county_covariate$unemploymentrate_2012[match(V(graph_states1115)$name, county_covariate$sendfips)])
V(graph_states1115)$unemployment_rate_2013 = as.character(county_covariate$unemploymentrate_2013[match(V(graph_states1115)$name, county_covariate$sendfips)])
V(graph_states1115)$unemployment_rate_2014 = as.character(county_covariate$unemploymentrate_2014[match(V(graph_states1115)$name, county_covariate$sendfips)])
V(graph_states1115)$unemployment_rate_2015 = as.character(county_covariate$unemploymentrate_2015[match(V(graph_states1115)$name, county_covariate$sendfips)])

#High School Graduation Rate (2009 - 2015)
V(graph_states1115)$hs_gradrate_2009 = as.character(county_covariate$hs_gradrate_2009[match(V(graph_states1115)$name, county_covariate$sendfips)])
V(graph_states1115)$hs_gradrate_2010 = as.character(county_covariate$hs_gradrate_2010[match(V(graph_states1115)$name, county_covariate$sendfips)])
V(graph_states1115)$hs_gradrate_2011 = as.character(county_covariate$hs_gradrate_2011[match(V(graph_states1115)$name, county_covariate$sendfips)])
V(graph_states1115)$hs_gradrate_2012 = as.character(county_covariate$hs_gradrate_2012[match(V(graph_states1115)$name, county_covariate$sendfips)])
V(graph_states1115)$hs_gradrate_2013 = as.character(county_covariate$hs_gradrate_2013[match(V(graph_states1115)$name, county_covariate$sendfips)])
V(graph_states1115)$hs_gradrate_2014 = as.character(county_covariate$hs_gradrate_2014[match(V(graph_states1115)$name, county_covariate$sendfips)])
V(graph_states1115)$hs_gradrate_2015 = as.character(county_covariate$hs_gradrate_2015[match(V(graph_states1115)$name, county_covariate$sendfips)])

#Household Poverty Rate (2009 - 2015)
V(graph_states1115)$hh_poverty_rate_2009 = as.character(county_covariate$hh_poverty_rate_2009[match(V(graph_states1115)$name, county_covariate$sendfips)])
V(graph_states1115)$hh_poverty_rate_2010 = as.character(county_covariate$hh_poverty_rate_2010[match(V(graph_states1115)$name, county_covariate$sendfips)])
V(graph_states1115)$hh_poverty_rate_2011 = as.character(county_covariate$hh_poverty_rate_2011[match(V(graph_states1115)$name, county_covariate$sendfips)])
V(graph_states1115)$hh_poverty_rate_2012 = as.character(county_covariate$hh_poverty_rate_2012[match(V(graph_states1115)$name, county_covariate$sendfips)])
V(graph_states1115)$hh_poverty_rate_2013 = as.character(county_covariate$hh_poverty_rate_2013[match(V(graph_states1115)$name, county_covariate$sendfips)])
V(graph_states1115)$hh_poverty_rate_2014 = as.character(county_covariate$hh_poverty_rate_2014[match(V(graph_states1115)$name, county_covariate$sendfips)])
V(graph_states1115)$hh_poverty_rate_2015 = as.character(county_covariate$hh_poverty_rate_2014[match(V(graph_states1115)$name, county_covariate$sendfips)])

#Total population (2010 - 2015)
V(graph_states1115)$total_pop_2010 = as.character(county_covariate$total_pop_2010[match(V(graph_states1115)$name, county_covariate$sendfips)])
V(graph_states1115)$total_pop_2011 = as.character(county_covariate$total_pop_2011[match(V(graph_states1115)$name, county_covariate$sendfips)])
V(graph_states1115)$total_pop_2012 = as.character(county_covariate$total_pop_2012[match(V(graph_states1115)$name, county_covariate$sendfips)])
V(graph_states1115)$total_pop_2013 = as.character(county_covariate$total_pop_2013[match(V(graph_states1115)$name, county_covariate$sendfips)])
V(graph_states1115)$total_pop_2014 = as.character(county_covariate$total_pop_2014[match(V(graph_states1115)$name, county_covariate$sendfips)])
V(graph_states1115)$total_pop_2015 = as.character(county_covariate$total_pop_2015[match(V(graph_states1115)$name, county_covariate$sendfips)])

# Racial information 
V(graph_states1115)$perc_white_2010 = as.character(county_covariate$perc_white_2010[match(V(graph_states1115)$name, county_covariate$sendfips)])
V(graph_states1115)$perc_white_2011 = as.character(county_covariate$perc_white_2011[match(V(graph_states1115)$name, county_covariate$sendfips)])
V(graph_states1115)$perc_white_2012 = as.character(county_covariate$perc_white_2012[match(V(graph_states1115)$name, county_covariate$sendfips)])
V(graph_states1115)$perc_white_2013 = as.character(county_covariate$perc_white_2013[match(V(graph_states1115)$name, county_covariate$sendfips)])
V(graph_states1115)$perc_white_2014 = as.character(county_covariate$perc_white_2014[match(V(graph_states1115)$name, county_covariate$sendfips)])
V(graph_states1115)$perc_white_2015 = as.character(county_covariate$perc_white_2015[match(V(graph_states1115)$name, county_covariate$sendfips)])
V(graph_states1115)$perc_black_2010 = as.character(county_covariate$perc_black_2010[match(V(graph_states1115)$name, county_covariate$sendfips)])
V(graph_states1115)$perc_black_2011 = as.character(county_covariate$perc_black_2011[match(V(graph_states1115)$name, county_covariate$sendfips)])
V(graph_states1115)$perc_black_2012 = as.character(county_covariate$perc_black_2012[match(V(graph_states1115)$name, county_covariate$sendfips)])
V(graph_states1115)$perc_black_2013 = as.character(county_covariate$perc_black_2013[match(V(graph_states1115)$name, county_covariate$sendfips)])
V(graph_states1115)$perc_black_2014 = as.character(county_covariate$perc_black_2014[match(V(graph_states1115)$name, county_covariate$sendfips)])
V(graph_states1115)$perc_black_2015 = as.character(county_covariate$perc_black_2015[match(V(graph_states1115)$name, county_covariate$sendfips)])
V(graph_states1115)$perc_latino_2010 = as.character(county_covariate$perc_latino_2010[match(V(graph_states1115)$name, county_covariate$sendfips)])
V(graph_states1115)$perc_latino_2011 = as.character(county_covariate$perc_latino_2011[match(V(graph_states1115)$name, county_covariate$sendfips)])
V(graph_states1115)$perc_latino_2012 = as.character(county_covariate$perc_latino_2012[match(V(graph_states1115)$name, county_covariate$sendfips)])
V(graph_states1115)$perc_latino_2013 = as.character(county_covariate$perc_latino_2013[match(V(graph_states1115)$name, county_covariate$sendfips)])
V(graph_states1115)$perc_latino_2014 = as.character(county_covariate$perc_latino_2014[match(V(graph_states1115)$name, county_covariate$sendfips)])
V(graph_states1115)$perc_latino_2015 = as.character(county_covariate$perc_latino_2015[match(V(graph_states1115)$name, county_covariate$sendfips)])


###########################################################################################################
###########################################################################################################
########################################### Data Visualization ############################################
###########################################################################################################
###########################################################################################################

#1) Intra-state migration (Texas)

# Plot a map of the united states:
map("state", region="texas", col="grey20", fill=TRUE, bg="black", lwd=0.1)

#Generating color gradient for edges in the network 
col.1 <- adjustcolor("orange red", alpha=0.4)
col.2 <- adjustcolor("orange", alpha=0.4)
edge.pal <- colorRampPalette(c(col.1, col.2), alpha = TRUE)
edge.col <- edge.pal(100)


#Within Texas Migration (data prep)

#Subset all migration to and from Texas Counties 
states1115_texas_complete <-subset(states1115_migration_complete, states1115_migration_complete$state==48)
states1115_texas_complete <- states1115_texas_complete[grepl("^48[0-9]", states1115_texas_complete$sendfips), ]
states1115_texas_complete <- states1115_texas_complete[grepl("^48[0-9]", states1115_texas_complete$recfips), ]
states1115_texas_complete$movers <- as.numeric(as.factor(states1115_texas_complete$movers))

#Generating shortest distance "arcs"
for(i in 1:nrow(states1115_texas_complete))  {
  node1 <- county_covariate[county_covariate$sendfips == states1115_texas_complete[i,]$sendfips,]
  node2 <- county_covariate[county_covariate$sendfips == states1115_texas_complete[i,]$recfips,]
  
  arc <- gcIntermediate( c(node1[1,]$Longitude, node1[1,]$Latitude), 
                         c(node2[1,]$Longitude, node2[1,]$Latitude), 
                         n=100, addStartEnd=TRUE )
  edge.ind <- round(25*states1115_texas_complete[i,]$movers / max(states1115_texas_complete$movers))
  
  lines(arc, col=edge.col[edge.ind], lwd=edge.ind/30)
}

#Saving image
dev.copy(png,"texas.png",width=8,height=6,units="in",res=100)
dev.off()


#2) Migration to Colorado (data prep)
states1115_migration_to_colorado <- states1115_migration_complete[grepl("^8[0-9]", states1115_migration_complete$recfips), ] 
states1115_migration_to_colorado$movers <- as.numeric(as.factor(states1115_migration_to_colorado$movers ))
  
#Plot map of the United States
map("state", col="grey20", fill=TRUE, bg="black", lwd=0.1)

#Show migration to Colorado from all other counties in the U.S. 
for(i in 1:nrow(states1115_migration_to_colorado))  {
  node1 <- county_covariate[county_covariate$sendfips == states1115_migration_to_colorado[i,]$sendfips,]
  node2 <- county_covariate[county_covariate$sendfips == states1115_migration_to_colorado[i,]$recfips,]
  
  arc <- gcIntermediate( c(node1[1,]$Longitude, node1[1,]$Latitude), 
                         c(node2[1,]$Longitude, node2[1,]$Latitude), 
                         n=100, addStartEnd=TRUE )
  edge.ind <- round(10*states1115_migration_to_colorado[i,]$movers / max(states1115_migration_to_colorado$movers))
  
  lines(arc, col=edge.col[edge.ind], lwd=edge.ind/30)
}

# Save image
dev.copy(png,"colorado.png",width=8,height=6,units="in",res=100)
dev.off()

#Migration from counties in NYC (all five boroughs) to other parts of the country (data prep)
states1115_migration_from_ny <- states1115_migration_complete[states1115_migration_complete$sendfips %in% c("36005", "36047", "36061", "36081", "36085"), ] 
states1115_migration_from_ny$movers <- as.numeric(as.factor(states1115_migration_from_ny$movers))

#Loading map
map("state", col="grey20", fill=TRUE, bg="black", lwd=0.1)

#Generating weighted edges 
for(i in 1:nrow(states1115_migration_from_ny))  {
  node1 <- states1115_migration_complete[states1115_migration_complete$sendfips == states1115_migration_from_ny[i,]$sendfips,]
  node2 <- states1115_migration_complete[states1115_migration_complete$sendfips == states1115_migration_from_ny[i,]$recfips,]
  
  arc <- gcIntermediate( c(node1[1,]$Longitude, node1[1,]$Latitude), 
                         c(node2[1,]$Longitude, node2[1,]$Latitude), 
                         n=1000, addStartEnd=TRUE )
  edge.ind <- round(25*states1115_migration_from_ny[i,]$movers / max(states1115_migration_from_ny$movers))
  
  lines(arc, col=edge.col[edge.ind], lwd=edge.ind/30)
}

#Save image
dev.copy(png,"nyc.png",width=8,height=6,units="in",res=100)
dev.off()


#########################################################################################################
############################### Centralization Measures ################################################
#########################################################################################################

#1) Centralization 
Cd <- function(g) {
  n <- igraph::vcount(g)
  sum(max(igraph::degree(g)) - igraph::degree(g))/((n-1)*(n-2))
}

Cd(graph_states1115)
sna::centralization(net_migration_states1115, degree)

#2) Prestige
#a) Declare igraph object a network object
net_migration_states1115 <-intergraph::asNetwork(graph_states1115)

#b) Degree prestige (Receiving more migration from others)
degree_prestige <- sna::prestige(net_migration_states1115, cmode="indegree")
node_county_names <- V(graph_states1115)$county_name 
node_state_names <- V(graph_states1115)$state_name

degree_prestige_display <- rbind(degree_prestige, node_county_names, node_state_names)
degree_prestige_display_transpose <- t(degree_prestige_display)
degree_prestige_data <- as.data.frame(degree_prestige_display_transpose, stringsAsFactors = F)
degree_prestige_data$degree_prestige <- as.numeric(as.character(degree_prestige_data$degree_prestige))


head(dplyr::arrange(degree_prestige_data, desc(degree_prestige)), n = 10)

#Displaying results

#c) Proximity prestige (More nodes can reach i via shortest paths)
proximity_prestige <- sna::prestige(net_migration_states1115, cmode="domain.proximity")


#d) Status or rank prestige(sum of prestige of neighbors pointing to node)
rank_presitge <- sna::prestige(net_migration_states1115, cmode="eigenvector")

rank_prestige_display <- rbind(rank_presitge, node_county_names, node_state_names)
rank_prestige_display_transpose <- t(rank_prestige_display)
rank_prestige_data <- as.data.frame(rank_prestige_display_transpose, stringsAsFactors=F)
rank_prestige_data$rank_presitge <- as.numeric(as.character(rank_prestige_data$rank_presitge))

head(dplyr::arrange(rank_prestige_data, desc(rank_presitge)), n = 10)

#3) Weighted measures
#Degree of each county (total)
degree_weighted <- strength(graph_states1115)
head(sort(degree_weighted, decreasing=T), n = 10)


#In-degree
in_degree_weighted <-strength(graph_states1115, mode=c("in"))
head(sort(in_degree_weighted, decreasing=T), n=10)

#Out-degree
out_degree_weighted <-strength(graph_states1115, mode=c("out"))
head(sort(out_degree_weighted, decreasing=T), n=10)

#4) Centrality
eigen_centrality <- eigen_centrality(graph_states1115, directed=T, weights=E(graph_states1115)$weight)$vector


eigen_centrality_display <- rbind(eigen_centrality, node_county_names, node_state_names)
eigen_centrality_display_transpose <- t(eigen_centrality_display)
eigen_centrality_data <- as.data.frame(eigen_centrality_display_transpose, stringsAsFactors=F)
eigen_centrality_data$eigen_centrality <- as.numeric(as.character(eigen_centrality_data$eigen_centrality))

head(dplyr::arrange(eigen_centrality_data, desc(eigen_centrality)), n = 10)
