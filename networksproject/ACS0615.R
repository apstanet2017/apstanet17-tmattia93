#Clear Memory

rm(list = ls())

#Load Libraries
library(ggplot2)
library(ggstance)
library(ggmcmc)
library(coda)
library(gdata)
library(statnet)
library(readxl)
library(XLConnect)
library(dplyr)
set.seed(12345678)

sessionInfo()

#Set working directory
setwd('~/Dropbox/github/networksproject')

#Load the data
read_excel_sheets <- function(filename) {
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet=X))
  names(x) <-sheets
  x
}


inflow0509 <- read_excel_sheets("inflow2005.xls")
inflow0610 <- read_excel_sheets("inflow2006.xls")
inflow0711 <- read_excel_sheets("inflow2007.xls")
inflow0812 <- read_excel_sheets("inflow2008.xls")
inflow0913 <- read_excel_sheets("inflow2009.xlsx")
inflow1014 <- read_excel_sheets("inflow2010.xlsx")
inflow1115 <- read_excel_sheets("inflow2011.xlsx")


#Function to clean data
cleaning <-function(dat) {
  dat <- subset(dat, select=c("Table with column headers in rows 2 through 4.", "X__1", "X__2", "X__3", "X__36", "X__37"))
  colnames(dat) <-c("Current State FIPS", "Current County FIPS", "Previous State FIPS", "Previous County FIPS", "movers", "MOE")
  dat <- dat[-c(1,2,3), ]
  dat$recfips <-as.numeric(paste(dat$`Current State FIPS`, dat$`Current County FIPS`, sep=""))
  dat$sendfips <-as.numeric(paste(dat$`Previous State FIPS`, dat$`Previous County FIPS`, sep=""))
  dat <-data.frame(sendfips = dat$sendfips, recfips = dat$recfips, movers=dat$movers)
}

#2011 - 2015 
list2env(inflow1115,envir=.GlobalEnv)

states <- list(Alabama, Alaska, Arizona, Arkansas, California, Colorado, Connecticut,
               Delaware, `District of Columbia`, Florida, Georgia, Hawaii, Idaho, 
               Illinois, Indiana, Iowa, Kansas, Kentucky, Louisiana, Maine,
               Maryland, Massachusetts, Michigan, Minnesota, Mississippi, Missouri,
               Montana, Nebraska, Nevada, `New Hampshire`, `New Jersey`, `New Mexico`, 
               `New York`, `North Carolina`, `North Dakota`, Ohio, Oklahoma, Oregon,
               Pennsylvania, `Rhode Island`, `South Carolina`, `South Dakota`, 
               Tennessee, Texas, Utah, Vermont, Virginia, Washington, `West Virginia`,
               Wisconsin, Wyoming)

#Applying cleaning function to all states in list 
states1115 <-lapply(states, FUN=cleaning)

#Combine all data frames in list
states1115 <-ldply(states1115, data.frame)

states1115=reshape(states1115 ,timevar = 'recfips',idvar = 'sendfips',v.names = 'movers',direction = 'wide')

rm(Alabama, Alaska, Arizona, Arkansas, California, Colorado, Connecticut,
   Delaware, `District of Columbia`, Florida, Georgia, Hawaii, Idaho, 
   Illinois, Indiana, Iowa, Kansas, Kentucky, Louisiana, Maine,
   Maryland, Massachusetts, Michigan, Minnesota, Mississippi, Missouri,
   Montana, Nebraska, Nevada, `New Hampshire`, `New Jersey`, `New Mexico`, 
   `New York`, `North Carolina`, `North Dakota`, Ohio, Oklahoma, Oregon, `Puerto Rico`,
   Pennsylvania, `Rhode Island`, `South Carolina`, `South Dakota`, 
   Tennessee, Texas, Utah, Vermont, Virginia, Washington, `West Virginia`,
   Wisconsin, Wyoming)

#2010 - 2014
list2env(inflow1014,envir=.GlobalEnv)

states <- list(Alabama, Alaska, Arizona, Arkansas, California, Colorado, Connecticut,
               Delaware, `District of Columbia`, Florida, Georgia, Hawaii, Idaho, 
               Illinois, Indiana, Iowa, Kansas, Kentucky, Louisiana, Maine,
               Maryland, Massachusetts, Michigan, Minnesota, Mississippi, Missouri,
               Montana, Nebraska, Nevada, `New Hampshire`, `New Jersey`, `New Mexico`, 
               `New York`, `North Carolina`, `North Dakota`, Ohio, Oklahoma, Oregon,
               Pennsylvania, `Rhode Island`, `South Carolina`, `South Dakota`, 
               Tennessee, Texas, Utah, Vermont, Virginia, Washington, `West Virginia`,
               Wisconsin, Wyoming)

#Applying cleaning function to all states in list 
states1014 <-lapply(states, FUN=cleaning)

#Combine all data frames in list
states1014 <-ldply(states1014, data.frame)

states1014=reshape(states1014 ,timevar = 'recfips',idvar = 'sendfips',v.names = 'movers',direction = 'wide')


rm(Alabama, Alaska, Arizona, Arkansas, California, Colorado, Connecticut,
   Delaware, `District of Columbia`, Florida, Georgia, Hawaii, Idaho, 
   Illinois, Indiana, Iowa, Kansas, Kentucky, Louisiana, Maine,
   Maryland, Massachusetts, Michigan, Minnesota, Mississippi, Missouri,
   Montana, Nebraska, Nevada, `New Hampshire`, `New Jersey`, `New Mexico`, 
   `New York`, `North Carolina`, `North Dakota`, Ohio, Oklahoma, Oregon, `Puerto Rico`,
   Pennsylvania, `Rhode Island`, `South Carolina`, `South Dakota`, 
   Tennessee, Texas, Utah, Vermont, Virginia, Washington, `West Virginia`,
   Wisconsin, Wyoming)

#2009 - 2013

list2env(inflow0913,envir=.GlobalEnv)

states <- list(Alabama, Alaska, Arizona, Arkansas, California, Colorado, Connecticut,
               Delaware, `District of Columbia`, Florida, Georgia, Hawaii, Idaho, 
               Illinois, Indiana, Iowa, Kansas, Kentucky, Louisiana, Maine,
               Maryland, Massachusetts, Michigan, Minnesota, Mississippi, Missouri,
               Montana, Nebraska, Nevada, `New Hampshire`, `New Jersey`, `New Mexico`, 
               `New York`, `North Carolina`, `North Dakota`, Ohio, Oklahoma, Oregon,
               Pennsylvania, `Rhode Island`, `South Carolina`, `South Dakota`, 
               Tennessee, Texas, Utah, Vermont, Virginia, Washington, `West Virginia`,
               Wisconsin, Wyoming)

#Applying cleaning function to all states in list 
states0913 <-lapply(states, FUN=cleaning)

#Combine all data frames in list
states0913 <-ldply(states0913, data.frame)

states0913=reshape(states0913 ,timevar = 'recfips',idvar = 'sendfips',v.names = 'movers',direction = 'wide')


rm(Alabama, Alaska, Arizona, Arkansas, California, Colorado, Connecticut,
   Delaware, `District of Columbia`, Florida, Georgia, Hawaii, Idaho, 
   Illinois, Indiana, Iowa, Kansas, Kentucky, Louisiana, Maine,
   Maryland, Massachusetts, Michigan, Minnesota, Mississippi, Missouri,
   Montana, Nebraska, Nevada, `New Hampshire`, `New Jersey`, `New Mexico`, 
   `New York`, `North Carolina`, `North Dakota`, Ohio, Oklahoma, Oregon, `Puerto Rico`,
   Pennsylvania, `Rhode Island`, `South Carolina`, `South Dakota`, 
   Tennessee, Texas, Utah, Vermont, Virginia, Washington, `West Virginia`,
   Wisconsin, Wyoming)

#2008 - 2012

ist2env(inflow0812,envir=.GlobalEnv)

states <- list(Alabama, Alaska, Arizona, Arkansas, California, Colorado, Connecticut,
               Delaware, `District of Columbia`, Florida, Georgia, Hawaii, Idaho, 
               Illinois, Indiana, Iowa, Kansas, Kentucky, Louisiana, Maine,
               Maryland, Massachusetts, Michigan, Minnesota, Mississippi, Missouri,
               Montana, Nebraska, Nevada, `New Hampshire`, `New Jersey`, `New Mexico`, 
               `New York`, `North Carolina`, `North Dakota`, Ohio, Oklahoma, Oregon,
               Pennsylvania, `Rhode Island`, `South Carolina`, `South Dakota`, 
               Tennessee, Texas, Utah, Vermont, Virginia, Washington, `West Virginia`,
               Wisconsin, Wyoming)

#Applying cleaning function to all states in list 
states0812 <-lapply(states, FUN=cleaning)

#Combine all data frames in list
states0812 <-ldply(states0812, data.frame)

states0812=reshape(states0812 ,timevar = 'recfips',idvar = 'sendfips',v.names = 'movers',direction = 'wide')

rm(Alabama, Alaska, Arizona, Arkansas, California, Colorado, Connecticut,
   Delaware, `District of Columbia`, Florida, Georgia, Hawaii, Idaho, 
   Illinois, Indiana, Iowa, Kansas, Kentucky, Louisiana, Maine,
   Maryland, Massachusetts, Michigan, Minnesota, Mississippi, Missouri,
   Montana, Nebraska, Nevada, `New Hampshire`, `New Jersey`, `New Mexico`, 
   `New York`, `North Carolina`, `North Dakota`, Ohio, Oklahoma, Oregon, `Puerto Rico`,
   Pennsylvania, `Rhode Island`, `South Carolina`, `South Dakota`, 
   Tennessee, Texas, Utah, Vermont, Virginia, Washington, `West Virginia`,
   Wisconsin, Wyoming)

#2007 - 2011

list2env(inflow0711,envir=.GlobalEnv)

states <- list(Alabama, Alaska, Arizona, Arkansas, California, Colorado, Connecticut,
               Delaware, `District of Columbia`, Florida, Georgia, Hawaii, Idaho, 
               Illinois, Indiana, Iowa, Kansas, Kentucky, Louisiana, Maine,
               Maryland, Massachusetts, Michigan, Minnesota, Mississippi, Missouri,
               Montana, Nebraska, Nevada, `New Hampshire`, `New Jersey`, `New Mexico`, 
               `New York`, `North Carolina`, `North Dakota`, Ohio, Oklahoma, Oregon,
               Pennsylvania, `Rhode Island`, `South Carolina`, `South Dakota`, 
               Tennessee, Texas, Utah, Vermont, Virginia, Washington, `West Virginia`,
               Wisconsin, Wyoming)

#Applying cleaning function to all states in list 
states0711 <-lapply(states, FUN=cleaning)

#Combine all data frames in list
states0711 <-ldply(states0711, data.frame)

states0711=reshape(states0711 ,timevar = 'recfips',idvar = 'sendfips',v.names = 'movers',direction = 'wide')

rm(Alabama, Alaska, Arizona, Arkansas, California, Colorado, Connecticut,
   Delaware, `District of Columbia`, Florida, Georgia, Hawaii, Idaho, 
   Illinois, Indiana, Iowa, Kansas, Kentucky, Louisiana, Maine,
   Maryland, Massachusetts, Michigan, Minnesota, Mississippi, Missouri,
   Montana, Nebraska, Nevada, `New Hampshire`, `New Jersey`, `New Mexico`, 
   `New York`, `North Carolina`, `North Dakota`, Ohio, Oklahoma, Oregon, `Puerto Rico`,
   Pennsylvania, `Rhode Island`, `South Carolina`, `South Dakota`, 
   Tennessee, Texas, Utah, Vermont, Virginia, Washington, `West Virginia`,
   Wisconsin, Wyoming)

#2006 - 2010
list2env(inflow0610,envir=.GlobalEnv)

states <- list(Alabama, Alaska, Arizona, Arkansas, California, Colorado, Connecticut,
               Delaware, `District of Columbia`, Florida, Georgia, Hawaii, Idaho, 
               Illinois, Indiana, Iowa, Kansas, Kentucky, Louisiana, Maine,
               Maryland, Massachusetts, Michigan, Minnesota, Mississippi, Missouri,
               Montana, Nebraska, Nevada, `New Hampshire`, `New Jersey`, `New Mexico`, 
               `New York`, `North Carolina`, `North Dakota`, Ohio, Oklahoma, Oregon,
               Pennsylvania, `Rhode Island`, `South Carolina`, `South Dakota`, 
               Tennessee, Texas, Utah, Vermont, Virginia, Washington, `West Virginia`,
               Wisconsin, Wyoming)

#Applying cleaning function to all states in list 
states0610 <-lapply(states, FUN=cleaning)

#Combine all data frames in list
states0610 <-ldply(states0610, data.frame)

states0610=reshape(states0610 ,timevar = 'recfips',idvar = 'sendfips',v.names = 'movers',direction = 'wide')

rm(Alabama, Alaska, Arizona, Arkansas, California, Colorado, Connecticut,
   Delaware, `District of Columbia`, Florida, Georgia, Hawaii, Idaho, 
   Illinois, Indiana, Iowa, Kansas, Kentucky, Louisiana, Maine,
   Maryland, Massachusetts, Michigan, Minnesota, Mississippi, Missouri,
   Montana, Nebraska, Nevada, `New Hampshire`, `New Jersey`, `New Mexico`, 
   `New York`, `North Carolina`, `North Dakota`, Ohio, Oklahoma, Oregon, `Puerto Rico`,
   Pennsylvania, `Rhode Island`, `South Carolina`, `South Dakota`, 
   Tennessee, Texas, Utah, Vermont, Virginia, Washington, `West Virginia`,
   Wisconsin, Wyoming)

#2005 - 2009 
list2env(inflow0509,envir=.GlobalEnv)

states <- list(Alabama, Alaska, Arizona, Arkansas, California, Colorado, Connecticut,
               Delaware, `District of Columbia`, Florida, Georgia, Hawaii, Idaho, 
               Illinois, Indiana, Iowa, Kansas, Kentucky, Louisiana, Maine,
               Maryland, Massachusetts, Michigan, Minnesota, Mississippi, Missouri,
               Montana, Nebraska, Nevada, `New Hampshire`, `New Jersey`, `New Mexico`, 
               `New York`, `North Carolina`, `North Dakota`, Ohio, Oklahoma, Oregon,
               Pennsylvania, `Rhode Island`, `South Carolina`, `South Dakota`, 
               Tennessee, Texas, Utah, Vermont, Virginia, Washington, `West Virginia`,
               Wisconsin, Wyoming)

#Applying cleaning function to all states in list 
states0509 <-lapply(states, FUN=cleaning)

#Combine all data frames in list
states0509 <-ldply(states0509, data.frame)

states0509 = reshape(states0509 ,timevar = 'recfips',idvar = 'sendfips',v.names = 'movers',direction = 'wide')

rm(Alabama, Alaska, Arizona, Arkansas, California, Colorado, Connecticut,
   Delaware, `District of Columbia`, Florida, Georgia, Hawaii, Idaho, 
   Illinois, Indiana, Iowa, Kansas, Kentucky, Louisiana, Maine,
   Maryland, Massachusetts, Michigan, Minnesota, Mississippi, Missouri,
   Montana, Nebraska, Nevada, `New Hampshire`, `New Jersey`, `New Mexico`, 
   `New York`, `North Carolina`, `North Dakota`, Ohio, Oklahoma, Oregon, `Puerto Rico`,
   Pennsylvania, `Rhode Island`, `South Carolina`, `South Dakota`, 
   Tennessee, Texas, Utah, Vermont, Virginia, Washington, `West Virginia`,
   Wisconsin, Wyoming)

view(states0509)