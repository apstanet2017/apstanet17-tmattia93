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
library(foreign)
library(haven)
library(plyr)
library(sna)
set.seed(12345678)

sessionInfo()

#Set working directory
setwd('~/Dropbox/github/Covariate')

#Load county level data (basic)
county_fips <- read_dta('~/Dropbox/github/Covariate/countyfips.dta')

#Unique FIPS Identifier
county_fips$fips_complete <-as.numeric(paste(county_fips$state, county_fips$countystring, sep=""))

#Remove uneeded variables
county_fips <-county_fips[,-7]

#Merge with latitude and longitude coordinates 
coordinates <-read.csv('~/Dropbox/github/Covariate/countylatlong.csv')
coordinates$Longitude = -(coordinates$Longitude)
county_fips <-merge(county_fips, coordinates, by=c("fips_complete"))

#Income Data by Year
income_2009 <- read.csv('~/Dropbox/github/Covariate/income2009.csv', stringsAsFactors = F)
income_2010 <- read.csv('~/Dropbox/github/Covariate/income2010.csv', stringsAsFactors = F)
income_2011 <- read.csv('~/Dropbox/github/Covariate/income2011.csv', stringsAsFactors = F)
income_2012 <- read.csv('~/Dropbox/github/Covariate/income2012.csv', stringsAsFactors = F)
income_2013 <- read.csv('~/Dropbox/github/Covariate/income2013.csv', stringsAsFactors = F)
income_2014 <- read.csv('~/Dropbox/github/Covariate/income2014.csv', stringsAsFactors = F)
income_2015 <- read.csv('~/Dropbox/github/Covariate/income2015.csv', stringsAsFactors = F)

#Function to clean income data
income_clean <-function(dat) {
  dat <- subset(dat, select=c("GEO.id2", "HC01_EST_VC01"))
  dat <- dat[-c(1), ]
  #dat$GEO.id2 <- lapply(dat$GEO.id2, function(y) sub('^0+(?=[1-9])', '', y, perl=TRUE))
  #rename(dat, c("GEO.id2"="fips_complete"))
}  

#Rename column variables and prepare data for merging 
income_2009 <-income_clean(income_2009)
colnames(income_2009) <- c("fips_complete", "hhincome_2009")
income_2009$fips_complete <- lapply(income_2009$fips_complete, function(y) sub('^0+(?=[1-9])', '', y, perl=TRUE))


income_2010 <-income_clean(income_2010)
colnames(income_2010) <- c("fips_complete", "hhincome_2010")
income_2010$fips_complete <- lapply(income_2010$fips_complete, function(y) sub('^0+(?=[1-9])', '', y, perl=TRUE))

income_2011 <-income_clean(income_2011)
colnames(income_2011) <- c("fips_complete", "hhincome_2011")
income_2011$fips_complete <- lapply(income_2011$fips_complete, function(y) sub('^0+(?=[1-9])', '', y, perl=TRUE))

income_2012 <-income_clean(income_2012)
colnames(income_2012) <- c("fips_complete", "hhincome_2012")
income_2012$fips_complete <- lapply(income_2012$fips_complete, function(y) sub('^0+(?=[1-9])', '', y, perl=TRUE))


income_2013 <-income_clean(income_2013)
colnames(income_2013) <- c("fips_complete", "hhincome_2013")
income_2013$fips_complete <- lapply(income_2013$fips_complete, function(y) sub('^0+(?=[1-9])', '', y, perl=TRUE))

income_2014 <-income_clean(income_2014)
colnames(income_2014) <- c("fips_complete", "hhincome_2014")
income_2014$fips_complete <- lapply(income_2014$fips_complete, function(y) sub('^0+(?=[1-9])', '', y, perl=TRUE))


income_2015 <-income_clean(income_2015)
colnames(income_2015) <- c("fips_complete", "hhincome_2015")
income_2015$fips_complete <- lapply(income_2015$fips_complete, function(y) sub('^0+(?=[1-9])', '', y, perl=TRUE))

#Merging income data with master data set
county_fips <- Reduce(function(x, y) merge(x, y, by="fips_complete", all.x=T), list(county_fips, income_2009, income_2010, income_2011, income_2012, income_2013, income_2014, income_2015))

#Unemployment
#2009 
unemployment_2009 <- read.csv('~/Dropbox/github/Covariate/unemployment2009.csv', stringsAsFactors = F)
unemployment_2009 <- subset(unemployment_2009, select=c("GEO.id2", "HC04_EST_VC01"))
colnames(unemployment_2009) <- c("fips_complete", "unemploymentrate_2009")
unemployment_2009 <- unemployment_2009[-c(1), ]
unemployment_2009$fips_complete <- lapply(unemployment_2009$fips_complete, function(y) sub('^0+(?=[1-9])', '', y, perl=TRUE))

#2010
unemployment_2010 <- read.csv('~/Dropbox/github/Covariate/unemployment2010.csv', stringsAsFactors = F)
unemployment_2010 <- subset(unemployment_2010, select=c("GEO.id2", "HC04_EST_VC01"))
colnames(unemployment_2010) <- c("fips_complete", "unemploymentrate_2010")
unemployment_2010 <- unemployment_2010[-c(1), ]
unemployment_2010$fips_complete <- lapply(unemployment_2010$fips_complete, function(y) sub('^0+(?=[1-9])', '', y, perl=TRUE))

#2011
unemployment_2011 <- read.csv('~/Dropbox/github/Covariate/unemployment2011.csv', stringsAsFactors = F)
unemployment_2011 <- subset(unemployment_2011, select=c("GEO.id2", "HC04_EST_VC01"))
colnames(unemployment_2011) <- c("fips_complete", "unemploymentrate_2011")
unemployment_2011 <- unemployment_2011[-c(1), ]
unemployment_2011$fips_complete <- lapply(unemployment_2011$fips_complete, function(y) sub('^0+(?=[1-9])', '', y, perl=TRUE))

#2012
unemployment_2012 <- read.csv('~/Dropbox/github/Covariate/unemployment2012.csv', stringsAsFactors = F)
unemployment_2012 <- subset(unemployment_2012, select=c("GEO.id2", "HC04_EST_VC01"))
colnames(unemployment_2012) <- c("fips_complete", "unemploymentrate_2012")
unemployment_2012 <- unemployment_2012[-c(1), ]
unemployment_2012$fips_complete <- lapply(unemployment_2012$fips_complete, function(y) sub('^0+(?=[1-9])', '', y, perl=TRUE))

#2013
unemployment_2013 <- read.csv('~/Dropbox/github/Covariate/unemployment2013.csv', stringsAsFactors = F)
unemployment_2013 <- subset(unemployment_2013, select=c("GEO.id2", "HC04_EST_VC01"))
colnames(unemployment_2013) <- c("fips_complete", "unemploymentrate_2013")
unemployment_2013 <- unemployment_2013[-c(1), ]
unemployment_2013$fips_complete <- lapply(unemployment_2013$fips_complete, function(y) sub('^0+(?=[1-9])', '', y, perl=TRUE))

#2014
unemployment_2014 <- read.csv('~/Dropbox/github/Covariate/unemployment2014.csv', stringsAsFactors = F)
unemployment_2014 <- subset(unemployment_2014, select=c("GEO.id2", "HC04_EST_VC01"))
colnames(unemployment_2014) <- c("fips_complete", "unemploymentrate_2014")
unemployment_2014 <- unemployment_2014[-c(1), ]
unemployment_2014$fips_complete <- lapply(unemployment_2014$fips_complete, function(y) sub('^0+(?=[1-9])', '', y, perl=TRUE))

#2015
unemployment_2015 <- read.csv('~/Dropbox/github/Covariate/unemployment2015.csv', stringsAsFactors = F)
unemployment_2015 <- subset(unemployment_2015, select=c("GEO.id2", "HC04_EST_VC01"))
colnames(unemployment_2015) <- c("fips_complete", "unemploymentrate_2015")
unemployment_2015 <- unemployment_2015[-c(1), ]
unemployment_2015$fips_complete <- lapply(unemployment_2015$fips_complete, function(y) sub('^0+(?=[1-9])', '', y, perl=TRUE))

#Merge with master data set
county_fips <- Reduce(function(x, y) merge(x, y, by="fips_complete", all.x=T), list(county_fips, unemployment_2009, unemployment_2010, unemployment_2011, unemployment_2012, unemployment_2013, unemployment_2014, unemployment_2015))

#Eductional attainment 
#2009
education_2009 <- read.csv('~/Dropbox/github/Covariate/education_2009.csv', stringsAsFactors = F)
education_2009 <- subset(education_2009, select=c("GEO.id2", "HC01_EST_VC03"))
colnames(education_2009) <- c("fips_complete", "hs_gradrate_2009")
education_2009 <- education_2009[-c(1), ]
education_2009$fips_complete <- lapply(education_2009$fips_complete, function(y) sub('^0+(?=[1-9])', '', y, perl=TRUE))

#2010
education_2010 <- read.csv('~/Dropbox/github/Covariate/education_2010.csv', stringsAsFactors = F)
education_2010 <- subset(education_2010, select=c("GEO.id2", "HC01_EST_VC03"))
colnames(education_2010) <- c("fips_complete", "hs_gradrate_2010")
education_2010 <- education_2010[-c(1), ]
education_2010$fips_complete <- lapply(education_2010$fips_complete, function(y) sub('^0+(?=[1-9])', '', y, perl=TRUE))

#2011
education_2011 <- read.csv('~/Dropbox/github/Covariate/education_2011.csv', stringsAsFactors = F)
education_2011 <- subset(education_2011, select=c("GEO.id2", "HC01_EST_VC03"))
colnames(education_2011) <- c("fips_complete", "hs_gradrate_2011")
education_2011 <- education_2011[-c(1), ]
education_2011$fips_complete <- lapply(education_2011$fips_complete, function(y) sub('^0+(?=[1-9])', '', y, perl=TRUE))

#2012
education_2012 <- read.csv('~/Dropbox/github/Covariate/education_2012.csv', stringsAsFactors = F)
education_2012 <- subset(education_2012, select=c("GEO.id2", "HC01_EST_VC03"))
colnames(education_2012) <- c("fips_complete", "hs_gradrate_2012")
education_2012 <- education_2012[-c(1), ]
education_2012$fips_complete <- lapply(education_2012$fips_complete, function(y) sub('^0+(?=[1-9])', '', y, perl=TRUE))

#2013
education_2013 <- read.csv('~/Dropbox/github/Covariate/education_2013.csv', stringsAsFactors = F)
education_2013 <- subset(education_2013, select=c("GEO.id2", "HC01_EST_VC03"))
colnames(education_2013) <- c("fips_complete", "hs_gradrate_2013")
education_2013 <- education_2013[-c(1), ]
education_2013$fips_complete <- lapply(education_2013$fips_complete, function(y) sub('^0+(?=[1-9])', '', y, perl=TRUE))

#2014
education_2014 <- read.csv('~/Dropbox/github/Covariate/education_2014.csv', stringsAsFactors = F)
education_2014 <- subset(education_2014, select=c("GEO.id2", "HC01_EST_VC03"))
colnames(education_2014) <- c("fips_complete", "hs_gradrate_2014")
education_2014 <- education_2014[-c(1), ]
education_2014$fips_complete <- lapply(education_2014$fips_complete, function(y) sub('^0+(?=[1-9])', '', y, perl=TRUE))

#2015
education_2015 <- read.csv('~/Dropbox/github/Covariate/education_2015.csv', stringsAsFactors = F)
education_2015 <- subset(education_2015, select=c("GEO.id2", "HC01_EST_VC03"))
colnames(education_2015) <- c("fips_complete", "hs_gradrate_2015")
education_2015 <- education_2015[-c(1), ]
education_2015$fips_complete <- lapply(education_2015$fips_complete, function(y) sub('^0+(?=[1-9])', '', y, perl=TRUE))

#Merge with master data set
county_fips <- Reduce(function(x, y) merge(x, y, by="fips_complete", all.x=T), list(county_fips, education_2009, education_2010, education_2011, education_2012, education_2013, education_2014, education_2015))

#Poverty level
#2009
poverty_2009 <- read.csv('~/Dropbox/github/Covariate/poverty2009.csv', stringsAsFactors = F)
poverty_2009 <- subset(poverty_2009, select=c("GEO.id2", "HC01_EST_VC05"))
colnames(poverty_2009) <- c("fips_complete", "hh_poverty_rate_2009")
poverty_2009 <- poverty_2009[-c(1), ]
poverty_2009$fips_complete <- lapply(poverty_2009$fips_complete, function(y) sub('^0+(?=[1-9])', '', y, perl=TRUE))

#2010
poverty_2010 <- read.csv('~/Dropbox/github/Covariate/poverty2010.csv', stringsAsFactors = F)
poverty_2010 <- subset(poverty_2010, select=c("GEO.id2", "HC01_EST_VC06"))
colnames(poverty_2010) <- c("fips_complete", "hh_poverty_rate_2010")
poverty_2010 <- poverty_2010[-c(1), ]
poverty_2010$fips_complete <- lapply(poverty_2010$fips_complete, function(y) sub('^0+(?=[1-9])', '', y, perl=TRUE))

#2011
poverty_2011 <- read.csv('~/Dropbox/github/Covariate/poverty2011.csv', stringsAsFactors = F)
poverty_2011 <- subset(poverty_2011, select=c("GEO.id2", "HC01_EST_VC06"))
colnames(poverty_2011) <- c("fips_complete", "hh_poverty_rate_2011")
poverty_2011 <- poverty_2011[-c(1), ]
poverty_2011$fips_complete <- lapply(poverty_2011$fips_complete, function(y) sub('^0+(?=[1-9])', '', y, perl=TRUE))

#2012
poverty_2012 <- read.csv('~/Dropbox/github/Covariate/poverty2012.csv', stringsAsFactors = F)
poverty_2012 <- subset(poverty_2012, select=c("GEO.id2", "HC01_EST_VC06"))
colnames(poverty_2012) <- c("fips_complete", "hh_poverty_rate_2012")
poverty_2012 <- poverty_2012[-c(1), ]
poverty_2012$fips_complete <- lapply(poverty_2012$fips_complete, function(y) sub('^0+(?=[1-9])', '', y, perl=TRUE))

#2013
poverty_2013 <- read.csv('~/Dropbox/github/Covariate/poverty2013.csv', stringsAsFactors = F)
poverty_2013 <- subset(poverty_2013, select=c("GEO.id2", "HC01_EST_VC06"))
colnames(poverty_2013) <- c("fips_complete", "hh_poverty_rate_2013")
poverty_2013 <- poverty_2013[-c(1), ]
poverty_2013$fips_complete <- lapply(poverty_2013$fips_complete, function(y) sub('^0+(?=[1-9])', '', y, perl=TRUE))

#2014
poverty_2014 <- read.csv('~/Dropbox/github/Covariate/poverty2014.csv', stringsAsFactors = F)
poverty_2014 <- subset(poverty_2014, select=c("GEO.id2", "HC01_EST_VC06"))
colnames(poverty_2014) <- c("fips_complete", "hh_poverty_rate_2014")
poverty_2014 <- poverty_2014[-c(1), ]
poverty_2014$fips_complete <- lapply(poverty_2014$fips_complete, function(y) sub('^0+(?=[1-9])', '', y, perl=TRUE))

#Merge with master data set 
county_fips <- Reduce(function(x, y) merge(x, y, by="fips_complete", all.x=T), list(county_fips, poverty_2009, poverty_2010, poverty_2011, poverty_2012, poverty_2013, poverty_2014))

#Race
#2009
race_2009 <- read.csv('~/Dropbox/github/Covariate/race_2009.csv' , stringsAsFactors = F)
race_2009 <- subset(race_2009, select=c("GEO.id2", "HD01_VD02", "HD01_VD03"))
colnames(race_2009) <- c("fips_complete", "white_pop_2009", "black_pop_2009")
race_2009 <- race_2009[-c(1), ]
race_2009$fips_complete <- lapply(race_2009$fips_complete, function(y) sub('^0+(?=[1-9])', '', y, perl=TRUE))

#2010
race_2010 <- read.csv('~/Dropbox/github/Covariate/race2010.csv', stringsAsFactors = F)
race_2010 <- subset(race_2010, select=c("GEO.id2", "HD01_VD02", "HD01_VD03"))
colnames(race_2010) <- c("fips_complete", "white_pop_2010", "black_pop_2010")
race_2010 <- race_2010[-c(1), ]
race_2010$fips_complete <- lapply(race_2010$fips_complete, function(y) sub('^0+(?=[1-9])', '', y, perl=TRUE))

#2011
race_2011 <- read.csv('~/Dropbox/github/Covariate/race2011.csv', stringsAsFactors = F)
race_2011 <- subset(race_2011, select=c("GEO.id2", "HD01_VD02", "HD01_VD03"))
colnames(race_2011) <- c("fips_complete", "white_pop_2011", "black_pop_2011")
race_2011 <- race_2011[-c(1), ]
race_2011$fips_complete <- lapply(race_2011$fips_complete, function(y) sub('^0+(?=[1-9])', '', y, perl=TRUE))

#2012
race_2012 <- read.csv('~/Dropbox/github/Covariate/race2012.csv', stringsAsFactors = F)
race_2012 <- subset(race_2012, select=c("GEO.id2", "HD01_VD02", "HD01_VD03"))
colnames(race_2012) <- c("fips_complete", "white_pop_2012", "black_pop_2012")
race_2012 <- race_2012[-c(1), ]
race_2012$fips_complete <- lapply(race_2012$fips_complete, function(y) sub('^0+(?=[1-9])', '', y, perl=TRUE))

#2013
race_2013 <- read.csv('~/Dropbox/github/Covariate/race2013.csv', stringsAsFactors = F)
race_2013 <- subset(race_2013, select=c("GEO.id2", "HD01_VD02", "HD01_VD03"))
colnames(race_2013) <- c("fips_complete", "white_pop_2013", "black_pop_2013")
race_2013 <- race_2013[-c(1), ]
race_2013$fips_complete <- lapply(race_2013$fips_complete, function(y) sub('^0+(?=[1-9])', '', y, perl=TRUE))

#2014
race_2014 <- read.csv('~/Dropbox/github/Covariate/race2014.csv', stringsAsFactors = F)
race_2014 <- subset(race_2014, select=c("GEO.id2", "HD01_VD02", "HD01_VD03"))
colnames(race_2014) <- c("fips_complete", "white_pop_2014", "black_pop_2014")
race_2014 <- race_2014[-c(1), ]
race_2014$fips_complete <- lapply(race_2014$fips_complete, function(y) sub('^0+(?=[1-9])', '', y, perl=TRUE))

#2015
race_2015 <- read.csv('~/Dropbox/github/Covariate/race2015.csv', stringsAsFactors = F)
race_2015 <- subset(race_2015, select=c("GEO.id2", "HD01_VD02", "HD01_VD03"))
colnames(race_2015) <- c("fips_complete", "white_pop_2015", "black_pop_2015")
race_2015 <- race_2015[-c(1), ]
race_2015$fips_complete <- lapply(race_2015$fips_complete, function(y) sub('^0+(?=[1-9])', '', y, perl=TRUE))

#Merge with master data set 
county_fips <- Reduce(function(x, y) merge(x, y, by="fips_complete", all.x=T), list(county_fips, race_2009, race_2010, race_2011, race_2012, race_2013, race_2014, race_2015))

#Latino population
#2010
population_2010 <-read.csv('~/Dropbox/github/Covariate/population2010.csv', stringsAsFactors = F)
population_2010 <- subset(population_2010, select=c("GEO.id2", "HC01_VC03", "HC03_VC43", "HC03_VC44", "HC01_VC82", "HC03_VC82"))
colnames(population_2010) <- c("fips_complete", "total_pop_2010", "perc_white_2010", "perc_black_2010", "latino_pop_2010", "perc_latino_2010")
population_2010 <- population_2010[-c(1), ]
population_2010$fips_complete <- lapply(population_2010$fips_complete, function(y) sub('^0+(?=[1-9])', '', y, perl=TRUE))

#2011
population_2011 <-read.csv('~/Dropbox/github/Covariate/population2011.csv', stringsAsFactors = F)
population_2011 <- subset(population_2011, select=c("GEO.id2", "HC01_VC03", "HC03_VC43", "HC03_VC44", "HC01_VC82", "HC03_VC82"))
colnames(population_2011) <- c("fips_complete", "total_pop_2011", "perc_white_2011", "perc_black_2011", "latino_pop_2011", "perc_latino_2011")
population_2011 <- population_2011[-c(1), ]
population_2011$fips_complete <- lapply(population_2011$fips_complete, function(y) sub('^0+(?=[1-9])', '', y, perl=TRUE))

#2012
population_2012 <-read.csv('~/Dropbox/github/Covariate/population2012.csv', stringsAsFactors = F)
population_2012 <- subset(population_2012, select=c("GEO.id2", "HC01_VC03", "HC03_VC43", "HC03_VC44", "HC01_VC82", "HC03_VC82"))
colnames(population_2012) <- c("fips_complete", "total_pop_2012", "perc_white_2012", "perc_black_2012", "latino_pop_2012", "perc_latino_2012")
population_2012 <- population_2012[-c(1), ]
population_2012$fips_complete <- lapply(population_2012$fips_complete, function(y) sub('^0+(?=[1-9])', '', y, perl=TRUE))

#2013
population_2013 <-read.csv('~/Dropbox/github/Covariate/population2013.csv', stringsAsFactors = F)
population_2013 <- subset(population_2013, select=c("GEO.id2", "HC01_VC03", "HC03_VC49", "HC03_VC50", "HC01_VC88", "HC03_VC88"))
colnames(population_2013) <- c("fips_complete", "total_pop_2013", "perc_white_2013", "perc_black_2013", "latino_pop_2013", "perc_latino_2013")
population_2013 <- population_2013[-c(1), ]
population_2013$fips_complete <- lapply(population_2013$fips_complete, function(y) sub('^0+(?=[1-9])', '', y, perl=TRUE))

#2014
population_2014 <-read.csv('~/Dropbox/github/Covariate/population2014.csv', stringsAsFactors = F)
population_2014 <- subset(population_2014, select=c("GEO.id2", "HC01_VC03", "HC03_VC49", "HC03_VC50", "HC01_VC88", "HC03_VC88"))
colnames(population_2014) <- c("fips_complete", "total_pop_2014", "perc_white_2014", "perc_black_2014", "latino_pop_2014", "perc_latino_2014")
population_2014 <- population_2014[-c(1), ]
population_2014$fips_complete <- lapply(population_2014$fips_complete, function(y) sub('^0+(?=[1-9])', '', y, perl=TRUE))

#2015
population_2015 <-read.csv('~/Dropbox/github/Covariate/population2015.csv', stringsAsFactors = F)
population_2015 <- subset(population_2015, select=c("GEO.id2", "HC01_VC03", "HC03_VC49", "HC03_VC50", "HC01_VC88", "HC03_VC88"))
colnames(population_2015) <- c("fips_complete", "total_pop_2015", "perc_white_2015", "perc_black_2015", "latino_pop_2015", "perc_latino_2015")
population_2015 <- population_2015[-c(1), ]
population_2015$fips_complete <- lapply(population_2015$fips_complete, function(y) sub('^0+(?=[1-9])', '', y, perl=TRUE))

#Merge with master data set 
county_fips <- Reduce(function(x, y) merge(x, y, by="fips_complete", all.x=T), list(county_fips, population_2010, population_2011, population_2012, population_2013, population_2014, population_2015))

save(county_fips, file = "county_covariate.Rdata")
