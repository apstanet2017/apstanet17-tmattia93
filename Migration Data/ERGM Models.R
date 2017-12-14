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
library(ergm.count)
library(ergm.rank)
library(GERGM)
library(bootnet)
library(glasso)
library(qgraph)


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


#########################################################################################################
############################################ ERGM MODEL  ################################################
#########################################################################################################
############################
#Sampling
states1115$movers <- as.character(states1115$movers)
states1115$movers <- as.numeric(as.character(states1115$movers))

net_migration_states1115 <-intergraph::asNetwork(graph_states1115)

#Converting attributes to numeric types
net_migration_states1115 %v% "unemployment_rate_2010" <- as.numeric(net_migration_states1115 %v% "unemployment_rate_2010")
net_migration_states1115 %v% "hhincome_2010" <- as.numeric(net_migration_states1115 %v% "hhincome_2010")
net_migration_states1115 %v% "hh_poverty_rate_2010" <- as.numeric(net_migration_states1115 %v% "hh_poverty_rate_2010")
net_migration_states1115 %v% "perc_white_2010" <- as.numeric(net_migration_states1115 %v% "perc_white_2010")
net_migration_states1115 %v% "perc_black_2010" <- as.numeric(net_migration_states1115 %v% "perc_black_2010")
net_migration_states1115 %v% "perc_latino_2010" <- as.numeric(net_migration_states1115 %v% "perc_latino_2010")
net_migration_states1115 %v% "total_pop_2010" <- as.numeric(net_migration_states1115 %v% "total_pop_2010")


#1) Covariates (in and out)

#Inital model used for coefficeints
model_sender_and_receiver_fit_1 <- ergm((net_migration_states1115 ~ edges +  nodeicov("unemployment_rate_2010") + nodeocov("unemployment_rate_2010")
                                         + nodeicov("hhincome_2010") + nodeocov("hhincome_2010")
                                         + nodeicov("hh_poverty_rate_2010") + nodeocov("hh_poverty_rate_2010")
                                         + nodeicov("perc_white_2010") + nodeocov("perc_white_2010")
                                         + nodeicov("total_pop_2010") + nodeocov("total_pop_2010") +
                                           + nodeicov("perc_black_2010") + nodeocov("perc_black_2010") +
                                           + nodeicov("perc_latino_2010") + nodeocov("perc_latino_2010") + 
                                           + nodematch("state_name", diff=F)), 
                                        response="weight",
                                        control = control.ergm(MCMLE.steplength.margin=0.25), 
                                        reference=~Poisson,
                                        eval.loglik=TRUE)



#Preferred model 
model_sender_and_receiver_fit_2 <- ergm((net_migration_states1115 ~ edges +  nodeicov("unemployment_rate_2010") + nodeocov("unemployment_rate_2010")
                                         + nodeicov("hhincome_2010") + nodeocov("hhincome_2010")
                                         + nodeicov("hh_poverty_rate_2010") + nodeocov("hh_poverty_rate_2010")
                                         + nodeicov("perc_white_2010") + nodeocov("perc_white_2010")
                                         + nodeicov("total_pop_2010") + nodeocov("total_pop_2010") +
                                           + nodeicov("perc_black_2010") + nodeocov("perc_black_2010") +
                                           + nodeicov("perc_latino_2010") + nodeocov("perc_latino_2010") + 
                                           + nodematch("state_name", diff=F)), 
                                        response="weight",
                                        control = control.ergm(init=coef(model_sender_and_receiver_fit_1),
                                                               MCMLE.steplength.margin=0.25,
                                                               MCMC.samplesize=50000,
                                                               MCMC.interval=5000),
                                        reference=~Poisson,
                                        eval.loglik=TRUE)
#2)Absolute Difference Model

#Inital model used for coefficeints
model_absolute_difference_fit_1 <- ergm((net_migration_states1115 ~ edges +
                                           + absdiff("unemployment_rate_2010") + absdiff("hhincome_2010") 
                                         + absdiff("hh_poverty_rate_2010")
                                         + absdiff("perc_white_2010") + absdiff("total_pop_2010")
                                         + absdiff("perc_black_2010") + absdiff("perc_black_2010") +
                                           + nodematch("state_name", diff=F)),
                                        response="weight",
                                        control =  control = control.ergm(MCMLE.steplength.margin=0.25),
                                        reference=~Poisson,
                                        eval.loglik=TRUE)
#Model for inference
model_absolute_difference_fit_2 <- ergm((net_migration_states1115 ~ edges +
                                           + absdiff("unemployment_rate_2010") + absdiff("hhincome_2010") 
                                         + absdiff("hh_poverty_rate_2010")
                                         + absdiff("perc_white_2010") + absdiff("total_pop_2010")
                                         + absdiff("perc_black_2010") + absdiff("perc_black_2010") +
                                           + nodematch("state_name", diff=F)),
                                        response="weight",
                                        control = control.ergm(init=coef(model_absolute_difference_fit_1),
                                                               MCMLE.steplength.margin=0.25,
                                                               MCMC.samplesize=50000,
                                                               MCMC.interval=5000),
                                        reference=~Poisson,
                                        eval.loglik=TRUE)