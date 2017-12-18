#Clear Memory
rm(list = ls())

#Load Libraries
library(sna)
library(ggplot2)
library(ggmcmc)
library(coda)
library(dplyr)
library(foreign)
library(haven)
library(statnet)
library(igraph)
library(network)
library(intergraph)
library(ergm)
library(ergm.count)
library(ergm.rank)

#Set working directory
setwd('~/Dropbox/github/Migration Data')

#setwd('C:\\Users\\trm354.POLITICS.011\\Dropbox\\github\\Migration Data')

#Load the data
network_1115 <- get(load('net_migration_states1115.RData'))

#######################
# P1 Model: Texas

# Creating a network of migration within Texas 
texas_1115 <- get.inducedSubgraph(network_1115, which(network_1115 %v% 'state_name'=="TX"))

#Creating Adjacency Matrix
states1115_texas_complete <- get(load('states1115_texas_complete.RData'))

texas_adj_matrix <- subset(states1115_texas_complete, select=c("sendfips", "recfips", "movers"))

#Reshaping into long format
texas_adj_matrix <- reshape(texas_adj_matrix, idvar = "sendfips",  timevar="recfips", direction="wide")

#Renaming columns
colnames(texas_adj_matrix) <- gsub("movers.", "", colnames(texas_adj_matrix), fixed = TRUE)

#Making first column the name of the first 
rownames(texas_adj_matrix) <- texas_adj_matrix[,1]
texas_adj_matrix[,1] <- NULL

#Removing counties to make matrix square
texas_adj_matrix <- texas_adj_matrix[!rownames(texas_adj_matrix) %in% c("48269", "48301"), ]

# Creating sender and receiver matrix

#1) Creating a unique id for each sender and receiver
n <- nrow(texas_adj_matrix)

#2) Senders and Receivers
senders <- c(matrix((as.numeric(rownames(texas_adj_matrix))), n, n))

receivers <- c(t(matrix((as.numeric(rownames(texas_adj_matrix))), n, n)))

#3) Extracting outcome variable
texas_adj_matrix <- as.matrix(texas_adj_matrix)
y <- c(texas_adj_matrix)


#Adding covariates
#1) Unemployment in time t-1 (2010)
texas_1115_unemployment <- subset(states1115_texas_complete, select=c("sendfips", "unemploymentrate_2010"))
texas_1115_unemployment <- unique(texas_1115_unemployment)

#a) Sending county unemployment 
senders_unemployment <- as.matrix(senders)
senders_unemployment <- as.data.frame(senders_unemployment)
colnames(senders_unemployment) <- c("sendfips")


#Merging the datasets
texas_unemployment_matrix_s <- join(senders_unemployment, texas_1115_unemployment, by = "sendfips")

#Reshaping for analysis
texas_senders_umemployment <- texas_unemployment_matrix_s$unemploymentrate_2010 
texas_senders_umemployment <- as.numeric(texas_senders_umemployment)

#b) Receiveing county unemployment
receivers_unemployment <- as.matrix(receivers)
receivers_unemployment <- as.data.frame(receivers_unemployment)
colnames(receivers_unemployment) <- c("sendfips")

#Merging the data sets
texas_unemployment_matrix_r <- join(receivers_unemployment, texas_1115_unemployment, by =c("sendfips"))

#Reshaping for analysis
texas_receivers_umemployment <- texas_unemployment_matrix_r$unemploymentrate_2010 
texas_receivers_umemployment <- as.numeric(texas_receivers_umemployment)

#2) Percentage of the county whose residents identify as Latino
texas_1115_perc_latino <- subset(states1115_texas_complete, select=c("sendfips", "perc_latino_2010"))
texas_1115_perc_latino <- unique(texas_1115_perc_latino)

#a) Sending county
senders_perc_latino <- as.matrix(senders)
senders_perc_latino <- as.data.frame(senders_perc_latino)
colnames(senders_perc_latino) <- c("sendfips")

#Merging the datasets
texas_perc_latino_matrix_s <- join(senders_perc_latino, texas_1115_perc_latino, by = "sendfips")

#Reshaping for analysis
texas_senders_perc_latino <- texas_perc_latino_matrix_s$perc_latino_2010 
texas_senders_perc_latino <- as.numeric(texas_senders_perc_latino)

#b) Receiving county
receivers_perc_latino <- as.matrix(receivers)
receivers_perc_latino <- as.data.frame(receivers_perc_latino)
colnames(receivers_perc_latino) <- c("sendfips")

#Merging the data sets
texas_perc_latino_matrix_r <- join(receivers_perc_latino, texas_1115_perc_latino, by =c("sendfips"))

#Reshaping for analysis
texas_receivers_perc_latino <- texas_perc_latino_matrix_r$perc_latino_2010 
texas_receivers_perc_latino <- as.numeric(texas_receivers_perc_latino)

#3) Percentage of the county whose residents identify as white
texas_1115_perc_white <- subset(states1115_texas_complete, select=c("sendfips", "perc_white_2010"))
texas_1115_perc_white <- unique(texas_1115_perc_white)

#a) Sending county
senders_perc_white <- as.matrix(senders)
senders_perc_white <- as.data.frame(senders_perc_white)
colnames(senders_perc_white) <- c("sendfips")

#Merging the datasets
texas_perc_white_matrix_s <- join(senders_perc_white, texas_1115_perc_white, by = "sendfips")

#Reshaping for analysis
texas_senders_perc_white <- texas_perc_white_matrix_s$perc_white_2010 
texas_senders_perc_white <- as.numeric(texas_senders_perc_white)

#b) Receiving county
receivers_perc_white <- as.matrix(receivers)
receivers_perc_white <- as.data.frame(receivers_perc_white)
colnames(receivers_perc_white) <- c("sendfips")

#Merging the data sets
texas_perc_white_matrix_r <- join(receivers_perc_white, texas_1115_perc_white, by =c("sendfips"))

#Reshaping for analysis
texas_receivers_perc_white <- texas_perc_white_matrix_r$perc_white_2010 
texas_receivers_perc_white <- as.numeric(texas_receivers_perc_white)

#4) High school graduation rate
texas_1115_grad_rate <- subset(states1115_texas_complete, select=c("sendfips", "hs_gradrate_2010"))
texas_1115_grad_rate <- unique(texas_1115_grad_rate)

#a) Sending county
senders_grad_rate <- as.matrix(senders)
senders_grad_rate <- as.data.frame(senders_grad_rate)
colnames(senders_grad_rate) <- c("sendfips")

#Merging the datasets
texas_grad_rate_matrix_s <- join(senders_grad_rate, texas_1115_grad_rate, by = "sendfips")

#Reshaping for analysis
texas_senders_grad_rate <- texas_grad_rate_matrix_s$hs_gradrate_2010 
texas_senders_grad_rate <- as.numeric(texas_senders_grad_rate)

#b) Receiving county
receivers_grad_rate <- as.matrix(receivers)
receivers_grad_rate <- as.data.frame(receivers_grad_rate)
colnames(receivers_grad_rate) <- c("sendfips")

#Merging the data sets
texas_grad_rate_matrix_r <- join(receivers_grad_rate, texas_1115_grad_rate, by =c("sendfips"))

#Reshaping for analysis
texas_receivers_grad_rate <- texas_grad_rate_matrix_r$hs_gradrate_2010 
texas_receivers_grad_rate <- as.numeric(texas_receivers_grad_rate)

#5) Household Income
texas_1115_hh_income <- subset(states1115_texas_complete, select=c("sendfips", "hhincome_2010"))
texas_1115_hh_income <- unique(texas_1115_hh_income)

#a) Sending county
senders_hh_income <- as.matrix(senders)
senders_hh_income <- as.data.frame(senders_hh_income)
colnames(senders_hh_income) <- c("sendfips")

#Merging the datasets
texas_hh_income_s <- join(senders_hh_income, texas_1115_hh_income, by = "sendfips")

#Reshaping for analysis
texas_senders_hh_income <- texas_hh_income_s$hhincome_2010 
texas_senders_hh_income <- as.numeric(texas_senders_hh_income)

#b) Receiving county
receivers_hh_income <- as.matrix(receivers)
receivers_hh_income <- as.data.frame(receivers_hh_income)
colnames(receivers_hh_income) <- c("sendfips")

#Merging the data sets
texas_hh_income_r <- join(receivers_hh_income, texas_1115_hh_income, by =c("sendfips"))

#Reshaping for analysis
texas_receivers_hh_income<- texas_hh_income_r$hhincome_2010 
texas_receivers_hh_income <- as.numeric(texas_receivers_hh_income)

#Fit 6
model1 <- glm(y ~ texas_senders_umemployment + texas_receivers_umemployment + texas_senders_perc_white
            + texas_receivers_perc_white + texas_senders_grad_rate + texas_receivers_grad_rate +
            + texas_senders_hh_income + texas_receivers_hh_income +
            + texas_senders_perc_latino + texas_receivers_perc_latino , family=poisson)

summary(model1)


