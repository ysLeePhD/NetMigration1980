
#http://www.econ.uiuc.edu/~lab/workshop/Spatial_in_R.html
install.packages("maptools", dependencies = TRUE)
install.packages("spdep", dependencies = TRUE)
install.packages("leaflet", dependencies = TRUE)
install.packages("RColorBrewer", dependencies = TRUE)
install.packages("McSpatial", dependencies = TRUE)
install.packages('spDataLarge',
                 repos='https://nowosad.github.io/drat/', type='source')
#install.packages("stargazer")
install.packages("tidyverse", dependencies = TRUE)
#install.packages("dplyr")

library(spdep)
library(spDataLarge)
library(maptools)
library(leaflet)
library(RColorBrewer)

#library(foreign)
library(sas7bdat)
library(rgdal)
library(McSpatial)
#library(stargazer)
library(tidyverse)
library(dplyr)

setwd("C:/Users/ylee366/Dropbox (GaTech)/3b_ResearchPersonal/11_BSL/Millennial_panel/09_NetMigration1980/Scratch")



#1. import the main dataset 

temp  <- read.sas7bdat("C:/Users/ylee366/Dropbox (GaTech)/3b_ResearchPersonal/11_BSL/Millennial_panel/05_Scratch/pool3.sas7bdat")
colnames(temp)
nrow(temp)



#2.1. process the main dataset 

varlist <- c("decade", "UA", "UAname", "stcnttr", 
             "Migr_youngest", "Migr_young", "Migr_midlife", "Migr_middle", 
             #since all four variables are here, # of cases would not differ by cohort
             "ln_dist_cbd8090", "ln_dist_cbd9000", "ln_dist_cbd0010",
             "cc8090", "cc9000", "cc0010",
             "cc8090_3", "cc9000_3", "cc0010_3",
             "cc8090_5", "cc9000_5", "cc0010_5",
             "lnpden8090", "lnpden9000", "lnpden0010", 
             "One_Transit8090", "One_Transit9000", "One_Transit0010", 
             "f_ccity9000b", "f_ccity0010b", "f_pden9000b", "f_pden0010b", 
             "f_pt9000b", "f_pt0010b", "lncden9000b", "lncden0010b", 
             "lnpop100", "pctyoungestbg", "pctyoungbg", "pctmidlifebg", "pctmiddlebg", 
             "pctnhw", "pctforeign", #"pctcoll", "lnrent", 
             "pctmulti", "yr8090", "yr9000", "yr0010", 
             "UA01", "UA02", "UA03", "UA04", "UA05", 
             "UA06", "UA07", "UA08", "UA09", "UA10", 
             "UA11", "UA12", "UA13", "UA14", "UA15", 
             "UA16", "UA17", "UA18", "UA19", "UA20")

varlist2 <- c(varlist, "elem")    
complete_rows <- temp[, varlist] %>% 
  complete.cases()

migrdata <- temp[, varlist2]    # there are many cases with missing elem 
# regression without elem will use those cases
migrdata <- migrdata[complete_rows, ]

migrdata$StCntTr <- migrdata$stcnttr 
migrdata$StCntTrYr <- paste0(migrdata$stcnttr, migrdata$decade)

migrdata <- migrdata[, c(1:4, 67:68, 5:42, 66, 43:65)]
#head(migrdata)
colnames(migrdata)


#2.2. process three shapefiles 

shp.YR8090 <- readOGR("C:/Users/ylee366/Dropbox (GaTech)/3b_ResearchPersonal/11_BSL/Millennial_panel/15_GIS/CTUA", layer = "1980CTUA")
shp.YR8090@data <- shp.YR8090@data["StCntTr"]
shp.YR8090@data$StCntTrYr <- paste0(shp.YR8090@data$StCntTr, "yr8090") 
shp.YR8090 <- spChFIDs(shp.YR8090, shp.YR8090@data$StCntTrYr) # change the fid field 

shp.YR9000 <- readOGR("C:/Users/ylee366/Dropbox (GaTech)/3b_ResearchPersonal/11_BSL/Millennial_panel/15_GIS/CTUA", layer = "1990CTUA")
shp.YR9000@data <- shp.YR9000@data["StCntTr"]
shp.YR9000@data$StCntTrYr <- paste0(shp.YR9000@data$StCntTr, "yr9000")
shp.YR9000 <- spChFIDs(shp.YR9000, shp.YR9000@data$StCntTrYr) # change the fid field 

shp.YR0010 <- readOGR("C:/Users/ylee366/Dropbox (GaTech)/3b_ResearchPersonal/11_BSL/Millennial_panel/15_GIS/CTUA", layer = "2000CTUA")
shp.YR0010@data <- shp.YR0010@data["StCntTr"]
shp.YR0010@data$StCntTrYr <- paste0(shp.YR0010@data$StCntTr, "yr0010")
shp.YR0010 <- spChFIDs(shp.YR0010, shp.YR0010@data$StCntTrYr) # change the fid field 



#2.3. Prepare a SpatialPolygonsDataFrame (without missing observations) for each decade.  

geodata8090 <- merge(shp.YR8090, subset(migrdata, yr8090==1), by="StCntTrYr")#, duplicateGeoms = TRUE) <- inner join
head(geodata8090@data)
geodata8090 <- subset(geodata8090, is.na(geodata8090@data[, 8])==0) 
geodata8090@data <- geodata8090@data[order(geodata8090@data$UA, geodata8090@data$StCntTrYr), ] # sorting 
head(geodata8090@data)

wmat01a <- makew(shpfile=subset(geodata8090, geodata8090@data$UA01==1), method="queen")$wmat
wmat02a <- makew(shpfile=subset(geodata8090, geodata8090@data$UA02==1), method="queen")$wmat
wmat03a <- makew(shpfile=subset(geodata8090, geodata8090@data$UA03==1), method="queen")$wmat
wmat04a <- makew(shpfile=subset(geodata8090, geodata8090@data$UA04==1), method="queen")$wmat
wmat05a <- makew(shpfile=subset(geodata8090, geodata8090@data$UA05==1), method="queen")$wmat
wmat06a <- makew(shpfile=subset(geodata8090, geodata8090@data$UA06==1), method="queen")$wmat
wmat07a <- makew(shpfile=subset(geodata8090, geodata8090@data$UA07==1), method="queen")$wmat
wmat08a <- makew(shpfile=subset(geodata8090, geodata8090@data$UA08==1), method="queen")$wmat
wmat09a <- makew(shpfile=subset(geodata8090, geodata8090@data$UA09==1), method="queen")$wmat
wmat10a <- makew(shpfile=subset(geodata8090, geodata8090@data$UA10==1), method="queen")$wmat
wmat11a <- makew(shpfile=subset(geodata8090, geodata8090@data$UA11==1), method="queen")$wmat
wmat12a <- makew(shpfile=subset(geodata8090, geodata8090@data$UA12==1), method="queen")$wmat
wmat13a <- makew(shpfile=subset(geodata8090, geodata8090@data$UA13==1), method="queen")$wmat
wmat14a <- makew(shpfile=subset(geodata8090, geodata8090@data$UA14==1), method="queen")$wmat
wmat15a <- makew(shpfile=subset(geodata8090, geodata8090@data$UA15==1), method="queen")$wmat
wmat16a <- makew(shpfile=subset(geodata8090, geodata8090@data$UA16==1), method="queen")$wmat
wmat17a <- makew(shpfile=subset(geodata8090, geodata8090@data$UA17==1), method="queen")$wmat
wmat18a <- makew(shpfile=subset(geodata8090, geodata8090@data$UA18==1), method="queen")$wmat
wmat19a <- makew(shpfile=subset(geodata8090, geodata8090@data$UA19==1), method="queen")$wmat
wmat20a <- makew(shpfile=subset(geodata8090, geodata8090@data$UA20==1), method="queen")$wmat

geodata9000 <- merge(shp.YR9000, subset(migrdata, yr9000==1), by="StCntTrYr")#, duplicateGeoms = TRUE)
head(geodata9000@data)
geodata9000 <- subset(geodata9000, is.na(geodata9000@data[, 8])==0)
geodata9000@data <- geodata9000@data[order(geodata9000@data$UA, geodata9000@data$StCntTrYr), ] # sorting 
head(geodata9000@data)

wmat01b <- makew(shpfile=subset(geodata9000, geodata9000@data$UA01==1), method="queen")$wmat
wmat02b <- makew(shpfile=subset(geodata9000, geodata9000@data$UA02==1), method="queen")$wmat
wmat03b <- makew(shpfile=subset(geodata9000, geodata9000@data$UA03==1), method="queen")$wmat
wmat04b <- makew(shpfile=subset(geodata9000, geodata9000@data$UA04==1), method="queen")$wmat
wmat05b <- makew(shpfile=subset(geodata9000, geodata9000@data$UA05==1), method="queen")$wmat
wmat06b <- makew(shpfile=subset(geodata9000, geodata9000@data$UA06==1), method="queen")$wmat
wmat07b <- makew(shpfile=subset(geodata9000, geodata9000@data$UA07==1), method="queen")$wmat
wmat08b <- makew(shpfile=subset(geodata9000, geodata9000@data$UA08==1), method="queen")$wmat
wmat09b <- makew(shpfile=subset(geodata9000, geodata9000@data$UA09==1), method="queen")$wmat
wmat10b <- makew(shpfile=subset(geodata9000, geodata9000@data$UA10==1), method="queen")$wmat
wmat11b <- makew(shpfile=subset(geodata9000, geodata9000@data$UA11==1), method="queen")$wmat
wmat12b <- makew(shpfile=subset(geodata9000, geodata9000@data$UA12==1), method="queen")$wmat
wmat13b <- makew(shpfile=subset(geodata9000, geodata9000@data$UA13==1), method="queen")$wmat
wmat14b <- makew(shpfile=subset(geodata9000, geodata9000@data$UA14==1), method="queen")$wmat
wmat15b <- makew(shpfile=subset(geodata9000, geodata9000@data$UA15==1), method="queen")$wmat
wmat16b <- makew(shpfile=subset(geodata9000, geodata9000@data$UA16==1), method="queen")$wmat
wmat17b <- makew(shpfile=subset(geodata9000, geodata9000@data$UA17==1), method="queen")$wmat
wmat18b <- makew(shpfile=subset(geodata9000, geodata9000@data$UA18==1), method="queen")$wmat
wmat19b <- makew(shpfile=subset(geodata9000, geodata9000@data$UA19==1), method="queen")$wmat
wmat20b <- makew(shpfile=subset(geodata9000, geodata9000@data$UA20==1), method="queen")$wmat

geodata0010 <- merge(shp.YR0010, subset(migrdata, yr0010==1), by="StCntTrYr")#, duplicateGeoms = TRUE)
head(geodata0010@data)
geodata0010 <- subset(geodata0010, is.na(geodata0010@data[, 8])==0)
geodata0010@data <- geodata0010@data[order(geodata0010@data$UA, geodata0010@data$StCntTrYr), ] # sorting 
head(geodata0010@data)

wmat01c <- makew(shpfile=subset(geodata0010, geodata0010@data$UA01==1), method="queen")$wmat
wmat02c <- makew(shpfile=subset(geodata0010, geodata0010@data$UA02==1), method="queen")$wmat
wmat03c <- makew(shpfile=subset(geodata0010, geodata0010@data$UA03==1), method="queen")$wmat
wmat04c <- makew(shpfile=subset(geodata0010, geodata0010@data$UA04==1), method="queen")$wmat
wmat05c <- makew(shpfile=subset(geodata0010, geodata0010@data$UA05==1), method="queen")$wmat
wmat06c <- makew(shpfile=subset(geodata0010, geodata0010@data$UA06==1), method="queen")$wmat
wmat07c <- makew(shpfile=subset(geodata0010, geodata0010@data$UA07==1), method="queen")$wmat
wmat08c <- makew(shpfile=subset(geodata0010, geodata0010@data$UA08==1), method="queen")$wmat
wmat09c <- makew(shpfile=subset(geodata0010, geodata0010@data$UA09==1), method="queen")$wmat
wmat10c <- makew(shpfile=subset(geodata0010, geodata0010@data$UA10==1), method="queen")$wmat
wmat11c <- makew(shpfile=subset(geodata0010, geodata0010@data$UA11==1), method="queen")$wmat
wmat12c <- makew(shpfile=subset(geodata0010, geodata0010@data$UA12==1), method="queen")$wmat
wmat13c <- makew(shpfile=subset(geodata0010, geodata0010@data$UA13==1), method="queen")$wmat
wmat14c <- makew(shpfile=subset(geodata0010, geodata0010@data$UA14==1), method="queen")$wmat
wmat15c <- makew(shpfile=subset(geodata0010, geodata0010@data$UA15==1), method="queen")$wmat
wmat16c <- makew(shpfile=subset(geodata0010, geodata0010@data$UA16==1), method="queen")$wmat
wmat17c <- makew(shpfile=subset(geodata0010, geodata0010@data$UA17==1), method="queen")$wmat
wmat18c <- makew(shpfile=subset(geodata0010, geodata0010@data$UA18==1), method="queen")$wmat
wmat19c <- makew(shpfile=subset(geodata0010, geodata0010@data$UA19==1), method="queen")$wmat
wmat20c <- makew(shpfile=subset(geodata0010, geodata0010@data$UA20==1), method="queen")$wmat

#fit <- makew(shpfile=geodata, method="queen")
#wmat <- fit$wmat 
#dim(wmat)



# 2.4. generate block-diagonal contiguity matrices for each decade

#https://www.r-bloggers.com/block-diagonal-matrices-in-r/
blockMatrixDiagonal<-function(...){  
  matrixList<-list(...)
  if(is.list(matrixList[[1]])) matrixList<-matrixList[[1]]
  
  dimensions<-sapply(matrixList,FUN=function(x) dim(x)[1])
  finalDimension<-sum(dimensions)
  finalMatrix<-matrix(0,nrow=finalDimension,ncol=finalDimension)
  index<-1
  for(k in 1:length(dimensions)){
    finalMatrix[index:(index+dimensions[k]-1),index:(index+dimensions[k]-1)]<-matrixList[[k]]
    index<-index+dimensions[k]
  }
  finalMatrix
}

wmat8090 <- blockMatrixDiagonal(wmat01a, wmat02a, wmat03a, wmat04a, wmat05a, wmat06a, wmat07a, wmat08a, wmat09a, wmat10a, 
                                wmat11a, wmat12a, wmat13a, wmat14a, wmat15a, wmat16a, wmat17a, wmat18a, wmat19a, wmat20a) 

wmat9000 <- blockMatrixDiagonal(wmat01b, wmat02b, wmat03b, wmat04b, wmat05b, wmat06b, wmat07b, wmat08b, wmat09b, wmat10b, 
                                wmat11b, wmat12b, wmat13b, wmat14b, wmat15b, wmat16b, wmat17b, wmat18b, wmat19b, wmat20b)

wmat0010 <- blockMatrixDiagonal(wmat01c, wmat02c, wmat03c, wmat04c, wmat05c, wmat06c, wmat07c, wmat08c, wmat09c, wmat10c, 
                                wmat11c, wmat12c, wmat13c, wmat14c, wmat15c, wmat16c, wmat17c, wmat18c, wmat19c, wmat20c)



# 3.1. Estimate three-decade models: Youngest

form8090.youngest <- Migr_youngest ~  
  cc8090_5 + lnpden8090 +  One_Transit8090 + 
  lnpop100 + pctyoungestbg + pctnhw + pctforeign + pctpro +  
  lnmedhhinc + pctunemp + pctmulti + #elem +  
  UA01 + UA02 + UA03 + UA04 + UA05 + UA06 + UA07 + UA08 + UA09 + UA10 + 
  UA11 + UA12 + UA13 + UA14 + UA15 + UA16 + UA17 + UA18 + UA19

form9000.youngest <-  Migr_youngest ~  
  cc9000_5 + lnpden9000 + One_Transit9000 + 
  lnpop100 + pctyoungestbg + pctnhw + pctforeign + pctpro +  
  lnmedhhinc + pctunemp + pctmulti + #elem +  
  UA01 + UA02 + UA03 + UA04 + UA05 + UA06 + UA07 + UA08 + UA09 + UA10 + 
  UA11 + UA12 + UA13 + UA14 + UA15 + UA16 + UA17 + UA18 + UA19

form0010.youngest <-  Migr_youngest ~   
  cc0010_5 + lnpden0010 + One_Transit0010 + 
  lnpop100 + pctyoungestbg + pctnhw + pctforeign + pctpro +  
  lnmedhhinc + pctunemp + pctmulti + #elem +  
  UA01 + UA02 + UA03 + UA04 + UA05 + UA06 + UA07 + UA08 + UA09 + UA10 + 
  UA11 + UA12 + UA13 + UA14 + UA15 + UA16 + UA17 + UA18 + UA19

myfunction <- function(x, y, z="sig"){
  temp <- as.data.frame(x)
  temp$sig <- ifelse(temp[, 4] <= 0.01, "***", 
                     ifelse(temp[, 4] <= 0.05, "**", 
                            ifelse(temp[, 4] <= 0.1, "*", "")))
  temp2 <- temp[, c(1,7)]
  colnames(temp2)<- c(y, z)
  temp2 
}



# 3.1a. the 1980s: Youngest

q25a.youngest <- myfunction(qregspiv(form8090.youngest,  wmat=wmat8090, tau=0.25, data=geodata8090@data, silent=TRUE), "25", "sig25")
q50a.youngest <- myfunction(qregspiv(form8090.youngest,  wmat=wmat8090, tau=0.50, data=geodata8090@data, silent=TRUE), "50", "sig50")
q75a.youngest <- myfunction(qregspiv(form8090.youngest,  wmat=wmat8090, tau=0.75, data=geodata8090@data, silent=TRUE), "75", "sig75")

q25a.youngest$rn <- rownames(q25a.youngest)
q50a.youngest$rn <- rownames(q50a.youngest)
q75a.youngest$rn <- rownames(q75a.youngest)

df <- plyr::join_all(list(q25a.youngest, q50a.youngest, q75a.youngest), by='rn', type='full') 
row.names(df) <- df$rn
df$rn <- NULL
df8090.youngest <- df[2:4, ] # <- to generate regression tables 
df8090.youngest



# 3.1b. the 1990s: Youngest

q25b.youngest <- myfunction(qregspiv(form9000.youngest,  wmat=wmat9000, tau=0.25, data=geodata9000@data, silent=TRUE), "25", "sig25")
q50b.youngest <- myfunction(qregspiv(form9000.youngest,  wmat=wmat9000, tau=0.50, data=geodata9000@data, silent=TRUE), "50", "sig50")
q75b.youngest <- myfunction(qregspiv(form9000.youngest,  wmat=wmat9000, tau=0.75, data=geodata9000@data, silent=TRUE), "75", "sig75")

q25b.youngest$rn <- rownames(q25b.youngest)
q50b.youngest$rn <- rownames(q50b.youngest)
q75b.youngest$rn <- rownames(q75b.youngest)

df<- NULL
df <- plyr::join_all(list(q25b.youngest, q50b.youngest, q75b.youngest), by='rn', type='full') 
row.names(df) <- df$rn
df$rn <- NULL
df9000.youngest <- df[2:4, ] # <- to generate regression tables 
df9000.youngest



# 3.1c. the 2000s: Youngest 

q25c.youngest <- myfunction(qregspiv(form0010.youngest,  wmat=wmat0010, tau=0.25, data=geodata0010@data, silent=TRUE), "25", "sig25")
q50c.youngest <- myfunction(qregspiv(form0010.youngest,  wmat=wmat0010, tau=0.50, data=geodata0010@data, silent=TRUE), "50", "sig50")
q75c.youngest <- myfunction(qregspiv(form0010.youngest,  wmat=wmat0010, tau=0.75, data=geodata0010@data, silent=TRUE), "75", "sig75")

q25c.youngest$rn <- rownames(q25c.youngest)
q50c.youngest$rn <- rownames(q50c.youngest)
q75c.youngest$rn <- rownames(q75c.youngest)

df <- NULL
df <- plyr::join_all(list(q25c.youngest, q50c.youngest, q75c.youngest), by='rn', type='full')  
row.names(df) <- df$rn
df$rn <- NULL
df0010.youngest <- df[2:4, ] # <- to generate regression tables 
df0010.youngest




# 3.2. Estimate three-decade models: Young

form8090.young <- Migr_young ~    
  cc8090_5 + lnpden8090 +  One_Transit8090 + 
  lnpop100 + pctyoungestbg + pctnhw + pctforeign + pctpro +  
  lnmedhhinc + pctunemp + pctmulti + #elem +  
  UA01 + UA02 + UA03 + UA04 + UA05 + UA06 + UA07 + UA08 + UA09 + UA10 + 
  UA11 + UA12 + UA13 + UA14 + UA15 + UA16 + UA17 + UA18 + UA19

form9000.young <-  Migr_young ~   
  cc9000_5 + lnpden9000 + One_Transit9000 + 
  lnpop100 + pctyoungestbg + pctnhw + pctforeign + pctpro +  
  lnmedhhinc + pctunemp + pctmulti + #elem +  
  UA01 + UA02 + UA03 + UA04 + UA05 + UA06 + UA07 + UA08 + UA09 + UA10 + 
  UA11 + UA12 + UA13 + UA14 + UA15 + UA16 + UA17 + UA18 + UA19

form0010.young <-  Migr_young ~  
  lnpop100 + pctyoungestbg + pctnhw + pctforeign + pctpro +  
  lnmedhhinc + pctunemp + pctmulti + #elem +  
  UA01 + UA02 + UA03 + UA04 + UA05 + UA06 + UA07 + UA08 + UA09 + UA10 + 
  UA11 + UA12 + UA13 + UA14 + UA15 + UA16 + UA17 + UA18 + UA19

myfunction <- function(x, y, z="sig"){
  temp <- as.data.frame(x)
  temp$sig <- ifelse(temp[, 4] <= 0.01, "***", 
                     ifelse(temp[, 4] <= 0.05, "**", 
                            ifelse(temp[, 4] <= 0.1, "*", "")))
  temp2 <- temp[, c(1,7)]
  colnames(temp2)<- c(y, z)
  temp2 
}



# 3.2a. the 1980s: Young

q25a.young <- myfunction(qregspiv(form8090.young,  wmat=wmat8090, tau=0.25, data=geodata8090@data, silent=TRUE), "25", "sig25")
q50a.young <- myfunction(qregspiv(form8090.young,  wmat=wmat8090, tau=0.50, data=geodata8090@data, silent=TRUE), "50", "sig50")
q75a.young <- myfunction(qregspiv(form8090.young,  wmat=wmat8090, tau=0.75, data=geodata8090@data, silent=TRUE), "75", "sig75")

q25a.young$rn <- rownames(q25a.young)
q50a.young$rn <- rownames(q50a.young)
q75a.young$rn <- rownames(q75a.young)

df <- plyr::join_all(list(q25a.young, q50a.young, q75a.young), by='rn', type='full') 
row.names(df) <- df$rn
df$rn <- NULL
df8090.young <- df[2:4, ] # <- to generate regression tables 
df8090.young



# 3.2b. the 1990s: Young

q25b.young <- myfunction(qregspiv(form9000.young,  wmat=wmat9000, tau=0.25, data=geodata9000@data, silent=TRUE), "25", "sig25")
q50b.young <- myfunction(qregspiv(form9000.young,  wmat=wmat9000, tau=0.50, data=geodata9000@data, silent=TRUE), "50", "sig50")
q75b.young <- myfunction(qregspiv(form9000.young,  wmat=wmat9000, tau=0.75, data=geodata9000@data, silent=TRUE), "75", "sig75")

q25b.young$rn <- rownames(q25b.young)
q50b.young$rn <- rownames(q50b.young)
q75b.young$rn <- rownames(q75b.young)

df<- NULL
df <- plyr::join_all(list(q25b.young, q50b.young, q75b.young), by='rn', type='full') 
row.names(df) <- df$rn
df$rn <- NULL
df9000.young <- df[2:4, ] # <- to generate regression tables 
df9000.young



# 3.2c. the 2000s: Young 

q25c.young <- myfunction(qregspiv(form0010.young,  wmat=wmat0010, tau=0.25, data=geodata0010@data, silent=TRUE), "25", "sig25")
q50c.young <- myfunction(qregspiv(form0010.young,  wmat=wmat0010, tau=0.50, data=geodata0010@data, silent=TRUE), "50", "sig50")
q75c.young <- myfunction(qregspiv(form0010.young,  wmat=wmat0010, tau=0.75, data=geodata0010@data, silent=TRUE), "75", "sig75")

q25c.young$rn <- rownames(q25c.young)
q50c.young$rn <- rownames(q50c.young)
q75c.young$rn <- rownames(q75c.young)

df <- NULL
df <- plyr::join_all(list(q25c.young, q50c.young, q75c.young), by='rn', type='full')  
row.names(df) <- df$rn
df$rn <- NULL
df0010.young <- df[2:4, ] # <- to generate regression tables 
df0010.young



# 3.3. Estimate three-decade models: Midlife

form8090.midlife <- Migr_midlife ~   
  cc8090_5 + lnpden8090 +  One_Transit8090 + 
  lnpop100 + pctmidlifebg + pctnhw + pctforeign + pctmulti + #elem +  
  UA01 + UA02 + UA03 + UA04 + UA05 + UA06 + UA07 + UA08 + UA09 + UA10 + 
  UA11 + UA12 + UA13 + UA14 + UA15 + UA16 + UA17 + UA18 + UA19

form9000.midlife <-  Migr_midlife ~  
  cc9000_5 + lnpden9000 + One_Transit9000 + 
  lnpop100 + pctmidlifebg + pctnhw + pctforeign + pctmulti + #elem +  
  UA01 + UA02 + UA03 + UA04 + UA05 + UA06 + UA07 + UA08 + UA09 + UA10 + 
  UA11 + UA12 + UA13 + UA14 + UA15 + UA16 + UA17 + UA18 + UA19

form0010.midlife <-  Migr_midlife ~  
  cc0010_5 + lnpden0010 + One_Transit0010 + 
  lnpop100 + pctmidlifebg + pctnhw + pctforeign + pctmulti + #elem +  
  UA01 + UA02 + UA03 + UA04 + UA05 + UA06 + UA07 + UA08 + UA09 + UA10 + 
  UA11 + UA12 + UA13 + UA14 + UA15 + UA16 + UA17 + UA18 + UA19

myfunction <- function(x, y, z="sig"){
  temp <- as.data.frame(x)
  temp$sig <- ifelse(temp[, 4] <= 0.01, "***", 
                     ifelse(temp[, 4] <= 0.05, "**", 
                            ifelse(temp[, 4] <= 0.1, "*", "")))
  temp2 <- temp[, c(1,7)]
  colnames(temp2)<- c(y, z)
  temp2 
}



# 3.3a. the 1980s: Midlife

q25a.midlife <- myfunction(qregspiv(form8090.midlife,  wmat=wmat8090, tau=0.25, data=geodata8090@data, silent=TRUE), "25", "sig25")
q50a.midlife <- myfunction(qregspiv(form8090.midlife,  wmat=wmat8090, tau=0.50, data=geodata8090@data, silent=TRUE), "50", "sig50")
q75a.midlife <- myfunction(qregspiv(form8090.midlife,  wmat=wmat8090, tau=0.75, data=geodata8090@data, silent=TRUE), "75", "sig75")

q25a.midlife$rn <- rownames(q25a.midlife)
q50a.midlife$rn <- rownames(q50a.midlife)
q75a.midlife$rn <- rownames(q75a.midlife)

df <- plyr::join_all(list(q25a.midlife, q50a.midlife, q75a.midlife), by='rn', type='full') 
row.names(df) <- df$rn
df$rn <- NULL
df8090.midlife <- df[2:4, ] # <- to generate regression tables 
df8090.midlife



# 3.3b. the 1990s: Midlife

q25b.midlife <- myfunction(qregspiv(form9000.midlife,  wmat=wmat9000, tau=0.25, data=geodata9000@data, silent=TRUE), "25", "sig25")
q50b.midlife <- myfunction(qregspiv(form9000.midlife,  wmat=wmat9000, tau=0.50, data=geodata9000@data, silent=TRUE), "50", "sig50")
q75b.midlife <- myfunction(qregspiv(form9000.midlife,  wmat=wmat9000, tau=0.75, data=geodata9000@data, silent=TRUE), "75", "sig75")

q25b.midlife$rn <- rownames(q25b.midlife)
q50b.midlife$rn <- rownames(q50b.midlife)
q75b.midlife$rn <- rownames(q75b.midlife)

df<- NULL
df <- plyr::join_all(list(q25b.midlife, q50b.midlife, q75b.midlife), by='rn', type='full') 
row.names(df) <- df$rn
df$rn <- NULL
df9000.midlife <- df[2:4, ] # <- to generate regression tables 
df9000.midlife



# 3.3c. the 2000s: Midlife 

q25c.midlife <- myfunction(qregspiv(form0010.midlife,  wmat=wmat0010, tau=0.25, data=geodata0010@data, silent=TRUE), "25", "sig25")
q50c.midlife <- myfunction(qregspiv(form0010.midlife,  wmat=wmat0010, tau=0.50, data=geodata0010@data, silent=TRUE), "50", "sig50")
q75c.midlife <- myfunction(qregspiv(form0010.midlife,  wmat=wmat0010, tau=0.75, data=geodata0010@data, silent=TRUE), "75", "sig75")

q25c.midlife$rn <- rownames(q25c.midlife)
q50c.midlife$rn <- rownames(q50c.midlife)
q75c.midlife$rn <- rownames(q75c.midlife)

df <- NULL
df <- plyr::join_all(list(q25c.midlife, q50c.midlife, q75c.midlife), by='rn', type='full')  
row.names(df) <- df$rn
df$rn <- NULL
df0010.midlife <- df[2:4, ] # <- to generate regression tables 
df0010.midlife



# 3.4. Estimate three-decade models: Middle

form8090.middle <- Migr_middle ~   
  cc8090_5 + lnpden8090 +  One_Transit8090 + 
  lnpop100 + pctmiddlebg + pctnhw + pctforeign + pctmulti + #elem +  
  UA01 + UA02 + UA03 + UA04 + UA05 + UA06 + UA07 + UA08 + UA09 + UA10 + 
  UA11 + UA12 + UA13 + UA14 + UA15 + UA16 + UA17 + UA18 + UA19

form9000.middle <-  Migr_middle ~  
  cc9000_5 + lnpden9000 + One_Transit9000 + 
  lnpop100 + pctmiddlebg + pctnhw + pctforeign + pctmulti + #elem +  
  UA01 + UA02 + UA03 + UA04 + UA05 + UA06 + UA07 + UA08 + UA09 + UA10 + 
  UA11 + UA12 + UA13 + UA14 + UA15 + UA16 + UA17 + UA18 + UA19

form0010.middle <-  Migr_middle ~  
  cc0010_5 + lnpden0010 + One_Transit0010 + 
  lnpop100 + pctmiddlebg + pctnhw + pctforeign + pctmulti + #elem +  
  UA01 + UA02 + UA03 + UA04 + UA05 + UA06 + UA07 + UA08 + UA09 + UA10 + 
  UA11 + UA12 + UA13 + UA14 + UA15 + UA16 + UA17 + UA18 + UA19

myfunction <- function(x, y, z="sig"){
  temp <- as.data.frame(x)
  temp$sig <- ifelse(temp[, 4] <= 0.01, "***", 
                     ifelse(temp[, 4] <= 0.05, "**", 
                            ifelse(temp[, 4] <= 0.1, "*", "")))
  temp2 <- temp[, c(1,7)]
  colnames(temp2)<- c(y, z)
  temp2 
}



# 3.4a. the 1980s: Middle

q25a.middle <- myfunction(qregspiv(form8090.middle,  wmat=wmat8090, tau=0.25, data=geodata8090@data, silent=TRUE), "25", "sig25")
q50a.middle <- myfunction(qregspiv(form8090.middle,  wmat=wmat8090, tau=0.50, data=geodata8090@data, silent=TRUE), "50", "sig50")
q75a.middle <- myfunction(qregspiv(form8090.middle,  wmat=wmat8090, tau=0.75, data=geodata8090@data, silent=TRUE), "75", "sig75")

q25a.middle$rn <- rownames(q25a.middle)
q50a.middle$rn <- rownames(q50a.middle)
q75a.middle$rn <- rownames(q75a.middle)

df <- plyr::join_all(list(q25a.middle, q50a.middle, q75a.middle), by='rn', type='full') 
row.names(df) <- df$rn
df$rn <- NULL
df8090.middle <- df[2:4, ] # <- to generate regression tables 
df8090.middle



# 3.4b. the 1990s: Middle

q25b.middle <- myfunction(qregspiv(form9000.middle,  wmat=wmat9000, tau=0.25, data=geodata9000@data, silent=TRUE), "25", "sig25")
q50b.middle <- myfunction(qregspiv(form9000.middle,  wmat=wmat9000, tau=0.50, data=geodata9000@data, silent=TRUE), "50", "sig50")
q75b.middle <- myfunction(qregspiv(form9000.middle,  wmat=wmat9000, tau=0.75, data=geodata9000@data, silent=TRUE), "75", "sig75")

q25b.middle$rn <- rownames(q25b.middle)
q50b.middle$rn <- rownames(q50b.middle)
q75b.middle$rn <- rownames(q75b.middle)

df<- NULL
df <- plyr::join_all(list(q25b.middle, q50b.middle, q75b.middle), by='rn', type='full') 
row.names(df) <- df$rn
df$rn <- NULL
df9000.middle <- df[2:4, ] # <- to generate regression tables 
df9000.middle



# 3.4c. the 2000s: Middle 

q25c.middle <- myfunction(qregspiv(form0010.middle,  wmat=wmat0010, tau=0.25, data=geodata0010@data, silent=TRUE), "25", "sig25")
q50c.middle <- myfunction(qregspiv(form0010.middle,  wmat=wmat0010, tau=0.50, data=geodata0010@data, silent=TRUE), "50", "sig50")
q75c.middle <- myfunction(qregspiv(form0010.middle,  wmat=wmat0010, tau=0.75, data=geodata0010@data, silent=TRUE), "75", "sig75")

q25c.middle$rn <- rownames(q25c.middle)
q50c.middle$rn <- rownames(q50c.middle)
q75c.middle$rn <- rownames(q75c.middle)

df <- NULL
df <- plyr::join_all(list(q25c.middle, q50c.middle, q75c.middle), by='rn', type='full')  
row.names(df) <- df$rn
df$rn <- NULL
df0010.middle <- df[2:4, ] # <- to generate regression tables 
df0010.middle



# 3.5. Merge all outcomes from three-decade models 

#https://stackoverflow.com/questions/16138693/rbind-multiple-data-sets
df8010.youngest <- do.call("rbind", list(
  df8090.youngest, df9000.youngest, df0010.youngest))
df8010.youngest <- df8010.youngest[c(1, 4, 7, 2, 5, 8, 3, 6, 9), ]

df8010.young <- do.call("rbind", list(
  df8090.young, df9000.young, df0010.young))
df8010.young <- df8010.young[c(1, 4, 7, 2, 5, 8, 3, 6, 9), ]

df8010.midlife <- do.call("rbind", list(
  df8090.midlife, df9000.midlife, df0010.midlife))
df8010.midlife <- df8010.midlife[c(1, 4, 7, 2, 5, 8, 3, 6, 9), ]

df8010.middle <- do.call("rbind", list(
  df8090.middle, df9000.middle, df0010.middle))
df8010.middle <- df8010.middle[c(1, 4, 7, 2, 5, 8, 3, 6, 9), ]



df8010.youngest 
df8010.young 
df8010.midlife 
df8010.middle

write.csv(df8010.youngest, file="M:/Millennial_panel/09_NetMigration1980/scrach/df8010_youngest.csv")
write.csv(df8010.young,    file="M:/Millennial_panel/09_NetMigration1980/scrach/df8010_young.csv")
write.csv(df8010.midlife,  file="M:/Millennial_panel/09_NetMigration1980/scrach/df8010_midlife.csv")
write.csv(df8010.middle,   file="M:/Millennial_panel/09_NetMigration1980/scrach/df8010_middle.csv")


df8010.youngest[1:3, ]
df8010.young[1:3, ]
df8010.midlife[1:3, ]
df8010.middle[1:3, ]

df8010.youngest[4:6, ]
df8010.young[4:6, ]
df8010.midlife[4:6, ]
df8010.middle[4:6, ]

df8010.youngest[7:9, ]
df8010.young[7:9, ]
df8010.midlife[7:9, ]
df8010.middle[7:9, ]



# 4.1. Estimate two-decade models: Youngest

form9000t.youngest <-  Migr_youngest ~  
  f_ccity9000b + f_pden9000b +  f_pt9000b + lncden9000b + 
  lnpop100 + pctyoungestbg + pctnhw + pctforeign + pctmulti +  
  UA01 + UA02 + UA03 + UA04 + UA05 + UA06 + UA07 + UA08 + UA09 + UA10 + 
  UA11 + UA12 + UA13 + UA14 + UA15 + UA16 + UA17 + UA18 + UA19

form0010t.youngest <-  Migr_youngest ~  
  f_ccity0010b + f_pden0010b +  f_pt0010b + lncden0010b + 
  lnpop100 + pctyoungestbg + pctnhw + pctforeign + pctmulti +  
  UA01 + UA02 + UA03 + UA04 + UA05 + UA06 + UA07 + UA08 + UA09 + UA10 + 
  UA11 + UA12 + UA13 + UA14 + UA15 + UA16 + UA17 + UA18 + UA19

myfunction <- function(x, y, z="sig"){
  temp <- as.data.frame(x)
  temp$sig <- ifelse(temp[, 4] <= 0.01, "***", 
                     ifelse(temp[, 4] <= 0.05, "**", 
                            ifelse(temp[, 4] <= 0.1, "*", "")))
  temp2 <- temp[, c(1,7)]
  colnames(temp2)<- c(y, z)
  temp2 
}



# 4.1a. the 1990s: Youngest

q25bt.youngest <- myfunction(qregspiv(form9000t.youngest,  wmat=wmat9000, tau=0.25, data=geodata9000@data, silent=TRUE), "25", "sig25")
q50bt.youngest <- myfunction(qregspiv(form9000t.youngest,  wmat=wmat9000, tau=0.50, data=geodata9000@data, silent=TRUE), "50", "sig50")
q75bt.youngest <- myfunction(qregspiv(form9000t.youngest,  wmat=wmat9000, tau=0.75, data=geodata9000@data, silent=TRUE), "75", "sig75")

q25bt.youngest$rn <- rownames(q25bt.youngest)
q50bt.youngest$rn <- rownames(q50bt.youngest)
q75bt.youngest$rn <- rownames(q75bt.youngest)

df<- NULL
df <- plyr::join_all(list(q25bt.youngest, q50bt.youngest, q75bt.youngest), by='rn', type='full') 
row.names(df) <- df$rn
df$rn <- NULL
df9000t.youngest <- df[2:5, ] # <- to generate regression tables 
df9000t.youngest



# 4.1b. the 2000s: Youngest 

q25ct.youngest <- myfunction(qregspiv(form0010t.youngest,  wmat=wmat0010, tau=0.25, data=geodata0010@data, silent=TRUE), "25", "sig25")
q50ct.youngest <- myfunction(qregspiv(form0010t.youngest,  wmat=wmat0010, tau=0.50, data=geodata0010@data, silent=TRUE), "50", "sig50")
q75ct.youngest <- myfunction(qregspiv(form0010t.youngest,  wmat=wmat0010, tau=0.75, data=geodata0010@data, silent=TRUE), "75", "sig75")

q25ct.youngest$rn <- rownames(q25ct.youngest)
q50ct.youngest$rn <- rownames(q50ct.youngest)
q75ct.youngest$rn <- rownames(q75ct.youngest)

df <- NULL
df <- plyr::join_all(list(q25ct.youngest, q50ct.youngest, q75ct.youngest), by='rn', type='full')  
row.names(df) <- df$rn
df$rn <- NULL
df0010t.youngest <- df[2:5, ] # <- to generate regression tables 
df0010t.youngest




# 4.2. Estimate two-decade models: Young

form9000t.young <-  Migr_young ~   
  f_ccity9000b + f_pden9000b +  f_pt9000b + lncden9000b + 
  lnpop100 + pctyoungbg + pctnhw + pctforeign + pctmulti + #elem +  
  UA01 + UA02 + UA03 + UA04 + UA05 + UA06 + UA07 + UA08 + UA09 + UA10 + 
  UA11 + UA12 + UA13 + UA14 + UA15 + UA16 + UA17 + UA18 + UA19

form0010t.young <-  Migr_young ~  
  f_ccity0010b + f_pden0010b +  f_pt0010b + lncden0010b + 
  lnpop100 + pctyoungbg + pctnhw + pctforeign + pctmulti + #elem +  
  UA01 + UA02 + UA03 + UA04 + UA05 + UA06 + UA07 + UA08 + UA09 + UA10 + 
  UA11 + UA12 + UA13 + UA14 + UA15 + UA16 + UA17 + UA18 + UA19

myfunction <- function(x, y, z="sig"){
  temp <- as.data.frame(x)
  temp$sig <- ifelse(temp[, 4] <= 0.01, "***", 
                     ifelse(temp[, 4] <= 0.05, "**", 
                            ifelse(temp[, 4] <= 0.1, "*", "")))
  temp2 <- temp[, c(1,7)]
  colnames(temp2)<- c(y, z)
  temp2 
}



# 4.2a. the 1990s: Young

q25bt.young <- myfunction(qregspiv(form9000t.young,  wmat=wmat9000, tau=0.25, data=geodata9000@data, silent=TRUE), "25", "sig25")
q50bt.young <- myfunction(qregspiv(form9000t.young,  wmat=wmat9000, tau=0.50, data=geodata9000@data, silent=TRUE), "50", "sig50")
q75bt.young <- myfunction(qregspiv(form9000t.young,  wmat=wmat9000, tau=0.75, data=geodata9000@data, silent=TRUE), "75", "sig75")

q25bt.young$rn <- rownames(q25bt.young)
q50bt.young$rn <- rownames(q50bt.young)
q75bt.young$rn <- rownames(q75bt.young)

df<- NULL
df <- plyr::join_all(list(q25bt.young, q50bt.young, q75bt.young), by='rn', type='full') 
row.names(df) <- df$rn
df$rn <- NULL
df9000t.young <- df[2:5, ] # <- to generate regression tables 
df9000t.young



# 4.2b. the 2000s: Young 

q25ct.young <- myfunction(qregspiv(form0010t.young,  wmat=wmat0010, tau=0.25, data=geodata0010@data, silent=TRUE), "25", "sig25")
q50ct.young <- myfunction(qregspiv(form0010t.young,  wmat=wmat0010, tau=0.50, data=geodata0010@data, silent=TRUE), "50", "sig50")
q75ct.young <- myfunction(qregspiv(form0010t.young,  wmat=wmat0010, tau=0.75, data=geodata0010@data, silent=TRUE), "75", "sig75")

q25ct.young$rn <- rownames(q25ct.young)
q50ct.young$rn <- rownames(q50ct.young)
q75ct.young$rn <- rownames(q75ct.young)

df <- NULL
df <- plyr::join_all(list(q25ct.young, q50ct.young, q75ct.young), by='rn', type='full')  
row.names(df) <- df$rn
df$rn <- NULL
df0010t.young <- df[2:5, ] # <- to generate regression tables 
df0010t.young



# 4.3. Estimate two-decade models: Midlife

form9000t.midlife <-  Migr_midlife ~  
  f_ccity9000b + f_pden9000b +  f_pt9000b + lncden9000b + 
  lnpop100 + pctmidlifebg + pctnhw + pctforeign + pctmulti + #elem +  
  UA01 + UA02 + UA03 + UA04 + UA05 + UA06 + UA07 + UA08 + UA09 + UA10 + 
  UA11 + UA12 + UA13 + UA14 + UA15 + UA16 + UA17 + UA18 + UA19

form0010t.midlife <-  Migr_midlife ~  
  f_ccity0010b + f_pden0010b +  f_pt0010b + lncden0010b + 
  lnpop100 + pctmidlifebg + pctnhw + pctforeign + pctmulti + #elem +  
  UA01 + UA02 + UA03 + UA04 + UA05 + UA06 + UA07 + UA08 + UA09 + UA10 + 
  UA11 + UA12 + UA13 + UA14 + UA15 + UA16 + UA17 + UA18 + UA19

myfunction <- function(x, y, z="sig"){
  temp <- as.data.frame(x)
  temp$sig <- ifelse(temp[, 4] <= 0.01, "***", 
                     ifelse(temp[, 4] <= 0.05, "**", 
                            ifelse(temp[, 4] <= 0.1, "*", "")))
  temp2 <- temp[, c(1,7)]
  colnames(temp2)<- c(y, z)
  temp2 
}



# 4.3a. the 1990s: Midlife

q25bt.midlife <- myfunction(qregspiv(form9000t.midlife,  wmat=wmat9000, tau=0.25, data=geodata9000@data, silent=TRUE), "25", "sig25")
q50bt.midlife <- myfunction(qregspiv(form9000t.midlife,  wmat=wmat9000, tau=0.50, data=geodata9000@data, silent=TRUE), "50", "sig50")
q75bt.midlife <- myfunction(qregspiv(form9000t.midlife,  wmat=wmat9000, tau=0.75, data=geodata9000@data, silent=TRUE), "75", "sig75")

q25bt.midlife$rn <- rownames(q25bt.midlife)
q50bt.midlife$rn <- rownames(q50bt.midlife)
q75bt.midlife$rn <- rownames(q75bt.midlife)

df<- NULL
df <- plyr::join_all(list(q25bt.midlife, q50bt.midlife, q75bt.midlife), by='rn', type='full') 
row.names(df) <- df$rn
df$rn <- NULL
df9000t.midlife <- df[2:5, ] # <- to generate regression tables 
df9000t.midlife



# 4.3b. the 2000s: Midlife 

q25ct.midlife <- myfunction(qregspiv(form0010t.midlife,  wmat=wmat0010, tau=0.25, data=geodata0010@data, silent=TRUE), "25", "sig25")
q50ct.midlife <- myfunction(qregspiv(form0010t.midlife,  wmat=wmat0010, tau=0.50, data=geodata0010@data, silent=TRUE), "50", "sig50")
q75ct.midlife <- myfunction(qregspiv(form0010t.midlife,  wmat=wmat0010, tau=0.75, data=geodata0010@data, silent=TRUE), "75", "sig75")

q25ct.midlife$rn <- rownames(q25ct.midlife)
q50ct.midlife$rn <- rownames(q50ct.midlife)
q75ct.midlife$rn <- rownames(q75ct.midlife)

df <- NULL
df <- plyr::join_all(list(q25ct.midlife, q50ct.midlife, q75ct.midlife), by='rn', type='full')  
row.names(df) <- df$rn
df$rn <- NULL
df0010t.midlife <- df[2:5, ] # <- to generate regression tables 
df0010t.midlife




# 4.4. Estimate two-decade models: Middle

form9000t.middle <-  Migr_middle ~  
  f_ccity9000b + f_pden9000b +  f_pt9000b + lncden9000b + 
  lnpop100 + pctmiddlebg + pctnhw + pctforeign + pctmulti + #elem +  
  UA01 + UA02 + UA03 + UA04 + UA05 + UA06 + UA07 + UA08 + UA09 + UA10 + 
  UA11 + UA12 + UA13 + UA14 + UA15 + UA16 + UA17 + UA18 + UA19

form0010t.middle <-  Migr_middle ~  
  f_ccity0010b + f_pden0010b +  f_pt0010b + lncden0010b + 
  lnpop100 + pctmiddlebg + pctnhw + pctforeign + pctmulti + #elem +  
  UA01 + UA02 + UA03 + UA04 + UA05 + UA06 + UA07 + UA08 + UA09 + UA10 + 
  UA11 + UA12 + UA13 + UA14 + UA15 + UA16 + UA17 + UA18 + UA19

myfunction <- function(x, y, z="sig"){
  temp <- as.data.frame(x)
  temp$sig <- ifelse(temp[, 4] <= 0.01, "***", 
                     ifelse(temp[, 4] <= 0.05, "**", 
                            ifelse(temp[, 4] <= 0.1, "*", "")))
  temp2 <- temp[, c(1,7)]
  colnames(temp2)<- c(y, z)
  temp2 
}



# 4.4a. the 1990s: Middle

q25bt.middle <- myfunction(qregspiv(form9000t.middle,  wmat=wmat9000, tau=0.25, data=geodata9000@data, silent=TRUE), "25", "sig25")
q50bt.middle <- myfunction(qregspiv(form9000t.middle,  wmat=wmat9000, tau=0.50, data=geodata9000@data, silent=TRUE), "50", "sig50")
q75bt.middle <- myfunction(qregspiv(form9000t.middle,  wmat=wmat9000, tau=0.75, data=geodata9000@data, silent=TRUE), "75", "sig75")

q25bt.middle$rn <- rownames(q25bt.middle)
q50bt.middle$rn <- rownames(q50bt.middle)
q75bt.middle$rn <- rownames(q75bt.middle)

df<- NULL
df <- plyr::join_all(list(q25bt.middle, q50bt.middle, q75bt.middle), by='rn', type='full') 
row.names(df) <- df$rn
df$rn <- NULL
df9000t.middle <- df[2:5, ] # <- to generate regression tables 
df9000t.middle



# 4.4b. the 2000s: Middle 

q25ct.middle <- myfunction(qregspiv(form0010t.middle,  wmat=wmat0010, tau=0.25, data=geodata0010@data, silent=TRUE), "25", "sig25")
q50ct.middle <- myfunction(qregspiv(form0010t.middle,  wmat=wmat0010, tau=0.50, data=geodata0010@data, silent=TRUE), "50", "sig50")
q75ct.middle <- myfunction(qregspiv(form0010t.middle,  wmat=wmat0010, tau=0.75, data=geodata0010@data, silent=TRUE), "75", "sig75")

q25ct.middle$rn <- rownames(q25ct.middle)
q50ct.middle$rn <- rownames(q50ct.middle)
q75ct.middle$rn <- rownames(q75ct.middle)

df <- NULL
df <- plyr::join_all(list(q25ct.middle, q50ct.middle, q75ct.middle), by='rn', type='full')  
row.names(df) <- df$rn
df$rn <- NULL
df0010t.middle <- df[2:5, ] # <- to generate regression tables 
df0010t.middle



# 4.5. Merge all outcomes from three-decade models 

#https://stackoverflow.com/questions/16138693/rbind-multiple-data-sets
df9010t.youngest <- do.call("rbind", list(
  df9000t.youngest, df0010t.youngest))
df9010t.youngest <- df9010t.youngest[c(1, 5, 2, 6, 3, 7, 4, 8), ]

df9010t.young <- do.call("rbind", list(
  df9000t.young, df0010t.young))
df9010t.young <- df9010t.young[c(1, 5, 2, 6, 3, 7, 4, 8), ]

df9010t.midlife <- do.call("rbind", list(
  df9000t.midlife, df0010t.midlife))
df9010t.midlife <- df9010t.midlife[c(1, 5, 2, 6, 3, 7, 4, 8), ]

df9010t.middle <- do.call("rbind", list(
  df9000t.middle, df0010t.middle))
df9010t.middle <- df9010t.middle[c(1, 5, 2, 6, 3, 7, 4, 8), ]



df9010t.youngest 
df9010t.young 
df9010t.midlife 
df9010t.middle 

write.csv(df9010t.youngest, file="M:/Millennial_panel/09_NetMigration1980/scrach/df9010t_youngest.csv")
write.csv(df9010t.young,    file="M:/Millennial_panel/09_NetMigration1980/scrach/df9010t_young.csv")
write.csv(df9010t.midlife,  file="M:/Millennial_panel/09_NetMigration1980/scrach/df9010t_midlife.csv")
write.csv(df9010t.middle,   file="M:/Millennial_panel/09_NetMigration1980/scrach/df9010t_middle.csv")


df9010t.youngest[1:2, ]
df9010t.young[1:2, ]
df9010t.midlife[1:2, ]
df9010t.middle[1:2, ]

df9010t.youngest[3:4, ]
df9010t.young[3:4, ]
df9010t.midlife[3:4, ]
df9010t.middle[3:4, ]

df9010t.youngest[5:6, ]
df9010t.young[5:6, ]
df9010t.midlife[5:6, ]
df9010t.middle[5:6, ]

df9010t.youngest[7:8, ]
df9010t.young[7:8, ]
df9010t.midlife[7:8, ]
df9010t.middle[7:8, ]


# 5. Estimate UA-specific models for 1990-2010 (to test lncden9000b & lncden0010b)

# 5.1. Build block diagonal matrices for 20 UAs 

wmat01 <- blockMatrixDiagonal(wmat01b, wmat01c)
wmat02 <- blockMatrixDiagonal(wmat02b, wmat02c)
wmat03 <- blockMatrixDiagonal(wmat03b, wmat03c)
wmat04 <- blockMatrixDiagonal(wmat04b, wmat04c)
wmat05 <- blockMatrixDiagonal(wmat05b, wmat05c)
wmat06 <- blockMatrixDiagonal(wmat06b, wmat06c)
wmat07 <- blockMatrixDiagonal(wmat07b, wmat07c)
wmat08 <- blockMatrixDiagonal(wmat08b, wmat08c)
wmat09 <- blockMatrixDiagonal(wmat09b, wmat09c)
wmat10 <- blockMatrixDiagonal(wmat10b, wmat10c)

wmat11 <- blockMatrixDiagonal(wmat11b, wmat11c)
wmat12 <- blockMatrixDiagonal(wmat12b, wmat12c)
wmat13 <- blockMatrixDiagonal(wmat13b, wmat13c)
wmat14 <- blockMatrixDiagonal(wmat14b, wmat14c)
wmat15 <- blockMatrixDiagonal(wmat15b, wmat15c)
wmat16 <- blockMatrixDiagonal(wmat16b, wmat16c)
wmat17 <- blockMatrixDiagonal(wmat17b, wmat17c)
wmat18 <- blockMatrixDiagonal(wmat18b, wmat18c)
wmat19 <- blockMatrixDiagonal(wmat19b, wmat19c)
wmat20 <- blockMatrixDiagonal(wmat20b, wmat20c)



# 5.2. Generate data for each UA (pool three decades)

geodataUA01 <- rbind(subset(geodata9000@data, geodata9000@data$UA01==1), subset(geodata0010@data, geodata0010@data$UA01==1))
geodataUA02 <- rbind(subset(geodata9000@data, geodata9000@data$UA02==1), subset(geodata0010@data, geodata0010@data$UA02==1))
geodataUA03 <- rbind(subset(geodata9000@data, geodata9000@data$UA03==1), subset(geodata0010@data, geodata0010@data$UA03==1))
geodataUA04 <- rbind(subset(geodata9000@data, geodata9000@data$UA04==1), subset(geodata0010@data, geodata0010@data$UA04==1))
geodataUA05 <- rbind(subset(geodata9000@data, geodata9000@data$UA05==1), subset(geodata0010@data, geodata0010@data$UA05==1))
geodataUA06 <- rbind(subset(geodata9000@data, geodata9000@data$UA06==1), subset(geodata0010@data, geodata0010@data$UA06==1))
geodataUA07 <- rbind(subset(geodata9000@data, geodata9000@data$UA07==1), subset(geodata0010@data, geodata0010@data$UA07==1))
geodataUA08 <- rbind(subset(geodata9000@data, geodata9000@data$UA08==1), subset(geodata0010@data, geodata0010@data$UA08==1))
geodataUA09 <- rbind(subset(geodata9000@data, geodata9000@data$UA09==1), subset(geodata0010@data, geodata0010@data$UA09==1))
geodataUA10 <- rbind(subset(geodata9000@data, geodata9000@data$UA10==1), subset(geodata0010@data, geodata0010@data$UA10==1))

geodataUA11 <- rbind(subset(geodata9000@data, geodata9000@data$UA11==1), subset(geodata0010@data, geodata0010@data$UA11==1))
geodataUA12 <- rbind(subset(geodata9000@data, geodata9000@data$UA12==1), subset(geodata0010@data, geodata0010@data$UA12==1))
geodataUA13 <- rbind(subset(geodata9000@data, geodata9000@data$UA13==1), subset(geodata0010@data, geodata0010@data$UA13==1))
geodataUA14 <- rbind(subset(geodata9000@data, geodata9000@data$UA14==1), subset(geodata0010@data, geodata0010@data$UA14==1))
geodataUA15 <- rbind(subset(geodata9000@data, geodata9000@data$UA15==1), subset(geodata0010@data, geodata0010@data$UA15==1))
geodataUA16 <- rbind(subset(geodata9000@data, geodata9000@data$UA16==1), subset(geodata0010@data, geodata0010@data$UA16==1))
geodataUA17 <- rbind(subset(geodata9000@data, geodata9000@data$UA17==1), subset(geodata0010@data, geodata0010@data$UA17==1))
geodataUA18 <- rbind(subset(geodata9000@data, geodata9000@data$UA18==1), subset(geodata0010@data, geodata0010@data$UA18==1))
geodataUA19 <- rbind(subset(geodata9000@data, geodata9000@data$UA19==1), subset(geodata0010@data, geodata0010@data$UA19==1))
geodataUA20 <- rbind(subset(geodata9000@data, geodata9000@data$UA20==1), subset(geodata0010@data, geodata0010@data$UA20==1))



# 5.3. Run spatial quantile regression models by UA 

byUA9010 <- Migr_young ~  
  f_ccity9000b + f_ccity0010b + f_pden9000b + f_pden0010b + 
  f_pt9000b + f_pt0010b + lncden9000b + lncden0010b + 
  lnpop100 + pctyoungbg + pctnhw + pctforeign + pctmulti +  yr0010

myfunction <- function(x, y, z="sig"){
  temp <- as.data.frame(x)
  temp$sig <- ifelse(temp[, 4] <= 0.01, "***", 
                     ifelse(temp[, 4] <= 0.05, "**", 
                            ifelse(temp[, 4] <= 0.1, "*", "")))
  temp2 <- temp[, c(1,7)]
  colnames(temp2)<- c(y, z)
  temp2 
}

mysummary9010 <- function(city, inputwmat, inputdata) {
  q25 <<- myfunction(qregspiv(byUA9010,  wmat=inputwmat, tau=0.25, data=inputdata, silent=TRUE), paste0(city, "25"), "sig25")
  q50 <<- myfunction(qregspiv(byUA9010,  wmat=inputwmat, tau=0.50, data=inputdata, silent=TRUE), paste0(city, "50"), "sig50")
  q75 <<- myfunction(qregspiv(byUA9010,  wmat=inputwmat, tau=0.75, data=inputdata, silent=TRUE), paste0(city, "75"), "sig75")
  q25$rn <<- rownames(q25)
  q50$rn <<- rownames(q50)
  q75$rn <<- rownames(q75)
  df <<- join_all(list(q25, q50, q75), by='rn', type='full') 
  row.names(df) <- df$rn
  df$rn <<- NULL
  df
}

UA01summary9010 <- mysummary9010("Atlanta",   wmat01, geodataUA01)
colnames(UA01summary9010) <- c("Coef25", "sig25", "rn", "Coef50", "sig50", "Coef75", "sig75")
UA02summary9010 <- mysummary9010("Baltimore", wmat02, geodataUA02)
colnames(UA02summary9010) <- c("Coef25", "sig25", "rn", "Coef50", "sig50", "Coef75", "sig75")
UA03summary9010 <- mysummary9010("Boston", wmat03, geodataUA03)
colnames(UA03summary9010) <- c("Coef25", "sig25", "rn", "Coef50", "sig50", "Coef75", "sig75")
UA04summary9010 <- mysummary9010("Chicago", wmat04, geodataUA04)
colnames(UA04summary9010) <- c("Coef25", "sig25", "rn", "Coef50", "sig50", "Coef75", "sig75")
UA05summary9010 <- mysummary9010("Cleveland", wmat05, geodataUA05)
colnames(UA05summary9010) <- c("Coef25", "sig25", "rn", "Coef50", "sig50", "Coef75", "sig75")
UA06summary9010 <- mysummary9010("Dallas", wmat06, geodataUA06)
colnames(UA06summary9010) <- c("Coef25", "sig25", "rn", "Coef50", "sig50", "Coef75", "sig75")
UA07summary9010 <- mysummary9010("Detroit", wmat07, geodataUA07)
colnames(UA07summary9010) <- c("Coef25", "sig25", "rn", "Coef50", "sig50", "Coef75", "sig75")
UA08summary9010 <- mysummary9010("Houston", wmat08, geodataUA08)
colnames(UA08summary9010) <- c("Coef25", "sig25", "rn", "Coef50", "sig50", "Coef75", "sig75")
UA09summary9010 <- mysummary9010("LA", wmat09, geodataUA09)
colnames(UA09summary9010) <- c("Coef25", "sig25", "rn", "Coef50", "sig50", "Coef75", "sig75")
UA10summary9010 <- mysummary9010("Miami", wmat10, geodataUA10)
colnames(UA10summary9010) <- c("Coef25", "sig25", "rn", "Coef50", "sig50", "Coef75", "sig75")

UA11summary9010 <- mysummary9010("Minneapolis",   wmat11, geodataUA11)
colnames(UA11summary9010) <- c("Coef25", "sig25", "rn", "Coef50", "sig50", "Coef75", "sig75")
UA12summary9010 <- mysummary9010("New York", wmat12, geodataUA12)
colnames(UA12summary9010) <- c("Coef25", "sig25", "rn", "Coef50", "sig50", "Coef75", "sig75")
UA13summary9010 <- mysummary9010("Philadelphia", wmat13, geodataUA13)
colnames(UA13summary9010) <- c("Coef25", "sig25", "rn", "Coef50", "sig50", "Coef75", "sig75")
UA14summary9010 <- mysummary9010("Phoenix", wmat14, geodataUA14)
colnames(UA14summary9010) <- c("Coef25", "sig25", "rn", "Coef50", "sig50", "Coef75", "sig75")
UA15summary9010 <- mysummary9010("St Louis", wmat15, geodataUA15)
colnames(UA15summary9010) <- c("Coef25", "sig25", "rn", "Coef50", "sig50", "Coef75", "sig75")
UA16summary9010 <- mysummary9010("San Diego", wmat16, geodataUA16)
colnames(UA16summary9010) <- c("Coef25", "sig25", "rn", "Coef50", "sig50", "Coef75", "sig75")
UA17summary9010 <- mysummary9010("San Francisco", wmat17, geodataUA17)
colnames(UA17summary9010) <- c("Coef25", "sig25", "rn", "Coef50", "sig50", "Coef75", "sig75")
UA18summary9010 <- mysummary9010("Seattle", wmat18, geodataUA18)
colnames(UA18summary9010) <- c("Coef25", "sig25", "rn", "Coef50", "sig50", "Coef75", "sig75")
UA19summary9010 <- mysummary9010("Tampa", wmat19, geodataUA19)
colnames(UA19summary9010) <- c("Coef25", "sig25", "rn", "Coef50", "sig50", "Coef75", "sig75")
UA20summary9010 <- mysummary9010("Washington DC", wmat20, geodataUA20)
colnames(UA20summary9010) <- c("Coef25", "sig25", "rn", "Coef50", "sig50", "Coef75", "sig75")

UAsummary9000 <- do.call("rbind", list(UA01summary9010[8, ], UA02summary9010[8, ], UA03summary9010[8, ], UA04summary9010[8, ], UA05summary9010[8, ],
                                       UA06summary9010[8, ], UA07summary9010[8, ], UA08summary9010[8, ], UA09summary9010[8, ], UA10summary9010[8, ],
                                       UA11summary9010[8, ], UA12summary9010[8, ], UA13summary9010[8, ], UA14summary9010[8, ], UA15summary9010[8, ],
                                       UA16summary9010[8, ], UA17summary9010[8, ], UA18summary9010[8, ], UA19summary9010[8, ], UA20summary9010[8, ]))
UAsummary9000$rn <- NULL 
UAsummary0010 <- do.call("rbind", list(UA01summary9010[9, ], UA02summary9010[9, ], UA03summary9010[9, ], UA04summary9010[9, ], UA05summary9010[9, ],
                                       UA06summary9010[9, ], UA07summary9010[9, ], UA08summary9010[9, ], UA09summary9010[9, ], UA10summary9010[9, ],
                                       UA11summary9010[9, ], UA12summary9010[9, ], UA13summary9010[9, ], UA14summary9010[9, ], UA15summary9010[9, ],
                                       UA16summary9010[9, ], UA17summary9010[9, ], UA18summary9010[9, ], UA19summary9010[9, ], UA20summary9010[9, ]))
UAsummary0010$rn <- NULL
UAsummary9010 <- cbind(UAsummary9000, UAsummary0010)

colnames(UAsummary9010) <- c("Coef.9000.25", "sig.9000.25", "Coef.9000.50", "sig.9000.50", "Coef.9000.75", "sig.9000.75", 
                             "Coef.0010.25", "sig.0010.25", "Coef.0010.50", "sig.0010.50", "Coef.0010.75", "sig.0010.75")
row.names(UAsummary9010) <- c("Atlanta", "Baltimore", "Boston", "Chicago", "Cleveland", 
                              "Dallas", "Detroit", "Houston", "LA", "Miami", 
                              "Minneapolis", "New York", "Philadelphia", "Phoenix", "St Louis", 
                              "San Diego", "San Francisco", "Seattle", "Tampa", "Washington DC")
UAsummary9010

write.csv(UA01summary9010, file="M:/Millennial_panel/09_NetMigration1980/scrach/UA01summary.csv")
write.csv(UA02summary9010, file="M:/Millennial_panel/09_NetMigration1980/scrach/UA02summary.csv")
write.csv(UA03summary9010, file="M:/Millennial_panel/09_NetMigration1980/scrach/UA03summary.csv")
write.csv(UA04summary9010, file="M:/Millennial_panel/09_NetMigration1980/scrach/UA04summary.csv")
write.csv(UA05summary9010, file="M:/Millennial_panel/09_NetMigration1980/scrach/UA05summary.csv")
write.csv(UA06summary9010, file="M:/Millennial_panel/09_NetMigration1980/scrach/UA06summary.csv")
write.csv(UA07summary9010, file="M:/Millennial_panel/09_NetMigration1980/scrach/UA07summary.csv")
write.csv(UA08summary9010, file="M:/Millennial_panel/09_NetMigration1980/scrach/UA08summary.csv")
write.csv(UA09summary9010, file="M:/Millennial_panel/09_NetMigration1980/scrach/UA09summary.csv")
write.csv(UA10summary9010, file="M:/Millennial_panel/09_NetMigration1980/scrach/UA10summary.csv")

write.csv(UA11summary9010, file="M:/Millennial_panel/09_NetMigration1980/scrach/UA11summary.csv")
write.csv(UA12summary9010, file="M:/Millennial_panel/09_NetMigration1980/scrach/UA12summary.csv")
write.csv(UA13summary9010, file="M:/Millennial_panel/09_NetMigration1980/scrach/UA13summary.csv")
write.csv(UA14summary9010, file="M:/Millennial_panel/09_NetMigration1980/scrach/UA14summary.csv")
write.csv(UA15summary9010, file="M:/Millennial_panel/09_NetMigration1980/scrach/UA15summary.csv")
write.csv(UA16summary9010, file="M:/Millennial_panel/09_NetMigration1980/scrach/UA16summary.csv")
write.csv(UA17summary9010, file="M:/Millennial_panel/09_NetMigration1980/scrach/UA17summary.csv")
write.csv(UA18summary9010, file="M:/Millennial_panel/09_NetMigration1980/scrach/UA18summary.csv")
write.csv(UA19summary9010, file="M:/Millennial_panel/09_NetMigration1980/scrach/UA19summary.csv")
write.csv(UA20summary9010, file="M:/Millennial_panel/09_NetMigration1980/scrach/UA20summary.csv")



