
#0. install packages 

install.packages("sas7bdat")
install.packages("stargazer")

install.packages("SparseM")
install.packages("quantreg")
install.packages("car")
install.packages("directlabels")
install.packages("ggrepel")
install.packages("tidyverse")
install.packages("magrittr")
install.packages("bindrcpp")
install.packages("RColorBrewer")
install.packages("wesanderson")

install.packages("lattice")
install.packages("locfit")
install.packages("rgeos")
install.packages("maptools")
install.packages("sp")
install.packages("RANN")
install.packages("spdep")
install.packages("rgdal")
install.packages("McSpatial")

#0. load packages 

library(stats)
library(utils)
library(sas7bdat)
library(foreign)
library(stargazer)

library(SparseM)
library(quantreg)
library(carData)
library(car)
library(directlabels)
library(ggrepel)
library(ggplot2)
library(plyr)
library(dplyr)
library(magrittr)
library(RColorBrewer)
library(wesanderson)

library(lattice)
library(locfit)
library(rgeos)
library(sp)
library(maptools)
#gpclibPermit()
library(RANN)
library(Matrix)
library(spData)
library(spdep)
library(rgdal)
library(McSpatial)

library(reshape2)

setwd("M:/Millennial_panel/09_R")

#1. import the main dataset 

temp  <- read.sas7bdat("M:/Millennial_panel/05_Scratch/pool3.sas7bdat")
colnames(temp)


#2. three-decade model 

#2.1. process the main dataset 

#migrdata <- subset(temp, pctforeign<=100)# & yr8090==1 & UA01==1)
varlist <- c("decade", "UA", "UAname", "stcnttr", 
             "Migr_youngest", "Migr_young", "Migr_midlife", "Migr_middle", 
             "ln_dist_cbd8090", "ln_dist_cbd9000", "ln_dist_cbd0010",
             "cc8090", "cc9000", "cc0010",
             "cc8090_3", "cc9000_3", "cc0010_3",
             "cc8090_5", "cc9000_5", "cc0010_5",
             "lnpden8090", "lnpden9000", "lnpden0010", 
             "One_Transit8090", "One_Transit9000", "One_Transit0010", 
             "f_ccity9000b", "f_ccity0010b", "f_pden9000b", "f_pden0010b", 
             "f_pt9000b", "f_pt0010b", "lncden9000b", "lncden0010b", 
             "lnpop100", "pctyoungestbg", "pctyoungbg", "pctmidlifebg", "pctmiddlebg", 
             "pctnhw", "pctforeign", "pctmulti", "elem", "yr8090", "yr9000", "yr0010", 
             "UA01", "UA02", "UA03", "UA04", "UA05", 
             "UA06", "UA07", "UA08", "UA09", "UA10", 
             "UA11", "UA12", "UA13", "UA14", "UA15", 
             "UA16", "UA17", "UA18", "UA19", "UA20")

complete_rows <- temp[, varlist] %>% 
  complete.cases()

migrdata <- temp[, varlist]
migrdata <- migrdata[complete_rows, ]

migrdata$StCntTr <- migrdata$stcnttr 
migrdata$StCntTrYr <- paste0(migrdata$stcnttr, migrdata$decade)
migrdata <- migrdata[, c(1:4, 67:68, 5:66)]
head(migrdata)



#2.2. process three shapefiles 

# file.exists("M://Millennial_panel/15_GIS/CTUA/YR8090_UA01.shp")
# shp.UA01 <- readShapePoly("M://Millennial_panel/15_GIS/CTUA/YR8090_UA01.shp")

shp.YR8090 <- readOGR("M://Millennial_panel/15_GIS/CTUA", layer = "1980CTUA")
shp.YR8090@data <- shp.YR8090@data["StCntTr"]
shp.YR8090@data$StCntTrYr <- paste0(shp.YR8090@data$StCntTr, "yr8090") 
shp.YR8090 <- spChFIDs(shp.YR8090, shp.YR8090@data$StCntTrYr) # change the fid field 
head(shp.YR8090@data)

shp.YR9000 <- readOGR("M://Millennial_panel/15_GIS/CTUA", layer = "1990CTUA")
shp.YR9000@data <- shp.YR9000@data["StCntTr"]
shp.YR9000@data$StCntTrYr <- paste0(shp.YR9000@data$StCntTr, "yr9000")
shp.YR9000 <- spChFIDs(shp.YR9000, shp.YR9000@data$StCntTrYr) # change the fid field 
head(shp.YR9000@data)

shp.YR0010 <- readOGR("M://Millennial_panel/15_GIS/CTUA", layer = "2000CTUA")
shp.YR0010@data <- shp.YR0010@data["StCntTr"]
shp.YR0010@data$StCntTrYr <- paste0(shp.YR0010@data$StCntTr, "yr0010")
shp.YR0010 <- spChFIDs(shp.YR0010, shp.YR0010@data$StCntTrYr) # change the fid field 
head(shp.YR0010@data)

colnames(shp.YR8090@data)
head(shp.YR8090@data)
head(shp.YR9000@data)
head(shp.YR0010@data)

#shp.YRALL <- spRbind(spRbind(shp.YR8090, shp.YR9000), shp.YR0010)
#crswgs84=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
#shp.UA01=readShapePoly("M://Millennial_panel/15_GIS/CTUA/YR8090_UA01.shp", proj4string=crswgs84,verbose=TRUE)

#class(shp.UA01)
#str(shp.UA01@data)
#str(shp.UA01@polygons[[1]])
#shp.UA01@proj4string
#plot(shp.UA01)

#sum(duplicated(shp.YR8090$StCntTr))
#sum(duplicated(subset(migrdata, yr8090==1)$StCntTr))



#2.3. generate contiguity matrices for each UA and each decade 

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



# 2.4. generate block-diagonal contiguity matrices 

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

wmat01 <- blockMatrixDiagonal(wmat01a, wmat01b, wmat01c)
wmat02 <- blockMatrixDiagonal(wmat02a, wmat02b, wmat02c)
wmat03 <- blockMatrixDiagonal(wmat03a, wmat03b, wmat03c)
wmat04 <- blockMatrixDiagonal(wmat04a, wmat04b, wmat04c)
wmat05 <- blockMatrixDiagonal(wmat05a, wmat05b, wmat05c)
wmat06 <- blockMatrixDiagonal(wmat06a, wmat06b, wmat06c)
wmat07 <- blockMatrixDiagonal(wmat07a, wmat07b, wmat07c)
wmat08 <- blockMatrixDiagonal(wmat08a, wmat08b, wmat08c)
wmat09 <- blockMatrixDiagonal(wmat09a, wmat09b, wmat09c)
wmat10 <- blockMatrixDiagonal(wmat10a, wmat10b, wmat10c)

wmat11 <- blockMatrixDiagonal(wmat11a, wmat11b, wmat11c)
wmat12 <- blockMatrixDiagonal(wmat12a, wmat12b, wmat12c)
wmat13 <- blockMatrixDiagonal(wmat13a, wmat13b, wmat13c)
wmat14 <- blockMatrixDiagonal(wmat14a, wmat14b, wmat14c)
wmat15 <- blockMatrixDiagonal(wmat15a, wmat15b, wmat15c)
wmat16 <- blockMatrixDiagonal(wmat16a, wmat16b, wmat16c)
wmat17 <- blockMatrixDiagonal(wmat17a, wmat17b, wmat17c)
wmat18 <- blockMatrixDiagonal(wmat18a, wmat18b, wmat18c)
wmat19 <- blockMatrixDiagonal(wmat19a, wmat19b, wmat19c)
wmat20 <- blockMatrixDiagonal(wmat20a, wmat20b, wmat20c)



# 2.5. generate data for each UA (pool three decades)

geodataUA01 <- rbind(subset(geodata8090@data, geodata8090@data$UA01==1), subset(geodata9000@data, geodata9000@data$UA01==1), subset(geodata0010@data, geodata0010@data$UA01==1))
geodataUA02 <- rbind(subset(geodata8090@data, geodata8090@data$UA02==1), subset(geodata9000@data, geodata9000@data$UA02==1), subset(geodata0010@data, geodata0010@data$UA02==1))
geodataUA03 <- rbind(subset(geodata8090@data, geodata8090@data$UA03==1), subset(geodata9000@data, geodata9000@data$UA03==1), subset(geodata0010@data, geodata0010@data$UA03==1))
geodataUA04 <- rbind(subset(geodata8090@data, geodata8090@data$UA04==1), subset(geodata9000@data, geodata9000@data$UA04==1), subset(geodata0010@data, geodata0010@data$UA04==1))
geodataUA05 <- rbind(subset(geodata8090@data, geodata8090@data$UA05==1), subset(geodata9000@data, geodata9000@data$UA05==1), subset(geodata0010@data, geodata0010@data$UA05==1))
geodataUA06 <- rbind(subset(geodata8090@data, geodata8090@data$UA06==1), subset(geodata9000@data, geodata9000@data$UA06==1), subset(geodata0010@data, geodata0010@data$UA06==1))
geodataUA07 <- rbind(subset(geodata8090@data, geodata8090@data$UA07==1), subset(geodata9000@data, geodata9000@data$UA07==1), subset(geodata0010@data, geodata0010@data$UA07==1))
geodataUA08 <- rbind(subset(geodata8090@data, geodata8090@data$UA08==1), subset(geodata9000@data, geodata9000@data$UA08==1), subset(geodata0010@data, geodata0010@data$UA08==1))
geodataUA09 <- rbind(subset(geodata8090@data, geodata8090@data$UA09==1), subset(geodata9000@data, geodata9000@data$UA09==1), subset(geodata0010@data, geodata0010@data$UA09==1))
geodataUA10 <- rbind(subset(geodata8090@data, geodata8090@data$UA10==1), subset(geodata9000@data, geodata9000@data$UA10==1), subset(geodata0010@data, geodata0010@data$UA10==1))

geodataUA11 <- rbind(subset(geodata8090@data, geodata8090@data$UA11==1), subset(geodata9000@data, geodata9000@data$UA11==1), subset(geodata0010@data, geodata0010@data$UA11==1))
geodataUA12 <- rbind(subset(geodata8090@data, geodata8090@data$UA12==1), subset(geodata9000@data, geodata9000@data$UA12==1), subset(geodata0010@data, geodata0010@data$UA12==1))
geodataUA13 <- rbind(subset(geodata8090@data, geodata8090@data$UA13==1), subset(geodata9000@data, geodata9000@data$UA13==1), subset(geodata0010@data, geodata0010@data$UA13==1))
geodataUA14 <- rbind(subset(geodata8090@data, geodata8090@data$UA14==1), subset(geodata9000@data, geodata9000@data$UA14==1), subset(geodata0010@data, geodata0010@data$UA14==1))
geodataUA15 <- rbind(subset(geodata8090@data, geodata8090@data$UA15==1), subset(geodata9000@data, geodata9000@data$UA15==1), subset(geodata0010@data, geodata0010@data$UA15==1))
geodataUA16 <- rbind(subset(geodata8090@data, geodata8090@data$UA16==1), subset(geodata9000@data, geodata9000@data$UA16==1), subset(geodata0010@data, geodata0010@data$UA16==1))
geodataUA17 <- rbind(subset(geodata8090@data, geodata8090@data$UA17==1), subset(geodata9000@data, geodata9000@data$UA17==1), subset(geodata0010@data, geodata0010@data$UA17==1))
geodataUA18 <- rbind(subset(geodata8090@data, geodata8090@data$UA18==1), subset(geodata9000@data, geodata9000@data$UA18==1), subset(geodata0010@data, geodata0010@data$UA18==1))
geodataUA19 <- rbind(subset(geodata8090@data, geodata8090@data$UA19==1), subset(geodata9000@data, geodata9000@data$UA19==1), subset(geodata0010@data, geodata0010@data$UA19==1))
geodataUA20 <- rbind(subset(geodata8090@data, geodata8090@data$UA20==1), subset(geodata9000@data, geodata9000@data$UA20==1), subset(geodata0010@data, geodata0010@data$UA20==1))



# 2.6. run quantile regression models at 19 quantile points (from 5th to 95th with the 5%tile interval) by UA 

form <- Migr_young ~  
  cc8090_5 + cc9000_5 + cc0010_5 + lnpden8090 + lnpden9000 + lnpden0010 + One_Transit8090 + One_Transit9000 + One_Transit0010 + 
  lnpop100 + pctyoungbg + pctnhw + pctforeign + pctmulti + elem + yr9000 + yr0010 # + 
#  UA01 + UA02 + UA03 + UA04 + UA05 + UA06 + UA07 + UA08 + UA09 + UA10 + 
#  UA11 + UA12 + UA13 + UA14 + UA15 + UA16 + UA17 + UA18 + UA19
form2 <- Migr_young ~ 
  cc8090_5 + cc9000_5 + cc0010_5 + lnpden8090 + lnpden9000 + lnpden0010 + One_Transit9000 + One_Transit0010 + 
  lnpop100 + pctyoungbg + pctnhw + pctforeign + pctmulti + elem + yr9000 + yr0010  
form3 <- Migr_young ~  
  cc8090_5 + cc9000_5 + cc0010_5 + lnpden8090 + lnpden9000 + lnpden0010 + One_Transit0010 + 
  lnpop100 + pctyoungbg + pctnhw + pctforeign + pctmulti + elem + yr9000 + yr0010   
form4 <- Migr_young ~ 
  cc8090_5 + cc9000_5 + cc0010_5 + lnpden8090 + lnpden9000 + lnpden0010 + 
  lnpop100 + pctyoungbg + pctnhw + pctforeign + pctmulti + elem + yr9000 + yr0010   

myfunction <- function(x, y, z="sig"){
  temp <- as.data.frame(x)
  temp$sig <- ifelse(temp[, 4] <= 0.01, "***", 
                     ifelse(temp[, 4] <= 0.05, "**", 
                            ifelse(temp[, 4] <= 0.1, "*", "")))
  temp2 <- temp[, c(1,7)]
  colnames(temp2)<- c(y, z)
  temp2 
}

mysummary <- function(city, inputwmat, inputdata) {
  #q05 <- myfunction(qregspiv(form,  wmat=inputwmat, tau=0.05, data=inputdata, silent=TRUE), paste0(city, "05"), "sig05")
  #q10 <- myfunction(qregspiv(form,  wmat=inputwmat, tau=0.10, data=inputdata, silent=TRUE), paste0(city, "10"), "sig10")
  #q15 <- myfunction(qregspiv(form,  wmat=inputwmat, tau=0.15, data=inputdata, silent=TRUE), paste0(city, "15"), "sig15")
  #q20 <- myfunction(qregspiv(form,  wmat=inputwmat, tau=0.20, data=inputdata, silent=TRUE), paste0(city, "20"), "sig20")
  q25 <- myfunction(qregspiv(form,  wmat=inputwmat, tau=0.25, data=inputdata, silent=TRUE), paste0(city, "25"), "sig25")
  #q30 <- myfunction(qregspiv(form,  wmat=inputwmat, tau=0.30, data=inputdata, silent=TRUE), paste0(city, "30"), "sig30")
  #q35 <- myfunction(qregspiv(form,  wmat=inputwmat, tau=0.35, data=inputdata, silent=TRUE), paste0(city, "35"), "sig35")
  #q40 <- myfunction(qregspiv(form,  wmat=inputwmat, tau=0.40, data=inputdata, silent=TRUE), paste0(city, "40"), "sig40")
  #q45 <- myfunction(qregspiv(form,  wmat=inputwmat, tau=0.45, data=inputdata, silent=TRUE), paste0(city, "45"), "sig45")
  q50 <- myfunction(qregspiv(form,  wmat=inputwmat, tau=0.50, data=inputdata, silent=TRUE), paste0(city, "50"), "sig50")
  #q55 <- myfunction(qregspiv(form,  wmat=inputwmat, tau=0.55, data=inputdata, silent=TRUE), paste0(city, "55"), "sig55")
  #q60 <- myfunction(qregspiv(form,  wmat=inputwmat, tau=0.60, data=inputdata, silent=TRUE), paste0(city, "60"), "sig60")
  #q65 <- myfunction(qregspiv(form,  wmat=inputwmat, tau=0.65, data=inputdata, silent=TRUE), paste0(city, "65"), "sig65")
  #q70 <- myfunction(qregspiv(form,  wmat=inputwmat, tau=0.70, data=inputdata, silent=TRUE), paste0(city, "70"), "sig70")
  q75 <- myfunction(qregspiv(form,  wmat=inputwmat, tau=0.75, data=inputdata, silent=TRUE), paste0(city, "75"), "sig75")
  #q80 <- myfunction(qregspiv(form,  wmat=inputwmat, tau=0.80, data=inputdata, silent=TRUE), paste0(city, "80"), "sig80")
  #q85 <- myfunction(qregspiv(form,  wmat=inputwmat, tau=0.85, data=inputdata, silent=TRUE), paste0(city, "85"), "sig85")
  #q90 <- myfunction(qregspiv(form,  wmat=inputwmat, tau=0.90, data=inputdata, silent=TRUE), paste0(city, "90"), "sig90")
  #q95 <- myfunction(qregspiv(form,  wmat=inputwmat, tau=0.95, data=inputdata, silent=TRUE), paste0(city, "95"), "sig95")
  
  #q05$rn <- rownames(q05)
  #q10$rn <- rownames(q10)
  #q15$rn <- rownames(q15)
  #q20$rn <- rownames(q20)
  q25$rn <- rownames(q25)
  #q30$rn <- rownames(q30)
  #q35$rn <- rownames(q35)
  #q40$rn <- rownames(q40)
  #q45$rn <- rownames(q45)
  q50$rn <- rownames(q50)
  #q55$rn <- rownames(q55)
  #q60$rn <- rownames(q60)
  #q65$rn <- rownames(q65)
  #q70$rn <- rownames(q70)
  q75$rn <- rownames(q75)
  #q80$rn <- rownames(q80)
  #q85$rn <- rownames(q85)
  #q90$rn <- rownames(q90)
  #q95$rn <- rownames(q95)
  df <- join_all(list(#q05, q10, q15, q20, 
                      q25, #q30, q35, q40, q45, 
                      q50, #q55, q60, q65, q70, 
                      q75), by='rn', type='full') #, q80, q85, q90, q95)  
  df$rn <- NULL
  df
}

mysummary2 <- function(city, inputwmat, inputdata) {
  #q05 <- myfunction(qregspiv(form2,  wmat=inputwmat, tau=0.05, data=inputdata, silent=TRUE), paste0(city, "05"), "sig05")
  #q10 <- myfunction(qregspiv(form2,  wmat=inputwmat, tau=0.10, data=inputdata, silent=TRUE), paste0(city, "10"), "sig10")
  #q15 <- myfunction(qregspiv(form2,  wmat=inputwmat, tau=0.15, data=inputdata, silent=TRUE), paste0(city, "15"), "sig15")
  #q20 <- myfunction(qregspiv(form2,  wmat=inputwmat, tau=0.20, data=inputdata, silent=TRUE), paste0(city, "20"), "sig20")
  q25 <- myfunction(qregspiv(form2,  wmat=inputwmat, tau=0.25, data=inputdata, silent=TRUE), paste0(city, "25"), "sig25")
  #q30 <- myfunction(qregspiv(form2,  wmat=inputwmat, tau=0.30, data=inputdata, silent=TRUE), paste0(city, "30"), "sig30")
  #q35 <- myfunction(qregspiv(form2,  wmat=inputwmat, tau=0.35, data=inputdata, silent=TRUE), paste0(city, "35"), "sig35")
  #q40 <- myfunction(qregspiv(form2,  wmat=inputwmat, tau=0.40, data=inputdata, silent=TRUE), paste0(city, "40"), "sig40")
  #q45 <- myfunction(qregspiv(form2,  wmat=inputwmat, tau=0.45, data=inputdata, silent=TRUE), paste0(city, "45"), "sig45")
  q50 <- myfunction(qregspiv(form2,  wmat=inputwmat, tau=0.50, data=inputdata, silent=TRUE), paste0(city, "50"), "sig50")
  #q55 <- myfunction(qregspiv(form2,  wmat=inputwmat, tau=0.55, data=inputdata, silent=TRUE), paste0(city, "55"), "sig55")
  #q60 <- myfunction(qregspiv(form2,  wmat=inputwmat, tau=0.60, data=inputdata, silent=TRUE), paste0(city, "60"), "sig60")
  #q65 <- myfunction(qregspiv(form2,  wmat=inputwmat, tau=0.65, data=inputdata, silent=TRUE), paste0(city, "65"), "sig65")
  #q70 <- myfunction(qregspiv(form2,  wmat=inputwmat, tau=0.70, data=inputdata, silent=TRUE), paste0(city, "70"), "sig70")
  q75 <- myfunction(qregspiv(form2,  wmat=inputwmat, tau=0.75, data=inputdata, silent=TRUE), paste0(city, "75"), "sig75")
  #q80 <- myfunction(qregspiv(form2,  wmat=inputwmat, tau=0.80, data=inputdata, silent=TRUE), paste0(city, "80"), "sig80")
  #q85 <- myfunction(qregspiv(form2,  wmat=inputwmat, tau=0.85, data=inputdata, silent=TRUE), paste0(city, "85"), "sig85")
  #q90 <- myfunction(qregspiv(form2,  wmat=inputwmat, tau=0.90, data=inputdata, silent=TRUE), paste0(city, "90"), "sig90")
  #q95 <- myfunction(qregspiv(form2,  wmat=inputwmat, tau=0.95, data=inputdata, silent=TRUE), paste0(city, "95"), "sig95")
  
  #q05$rn <- rownames(q05)
  #q10$rn <- rownames(q10)
  #q15$rn <- rownames(q15)
  #q20$rn <- rownames(q20)
  q25$rn <- rownames(q25)
  #q30$rn <- rownames(q30)
  #q35$rn <- rownames(q35)
  #q40$rn <- rownames(q40)
  #q45$rn <- rownames(q45)
  q50$rn <- rownames(q50)
  #q55$rn <- rownames(q55)
  #q60$rn <- rownames(q60)
  #q65$rn <- rownames(q65)
  #q70$rn <- rownames(q70)
  q75$rn <- rownames(q75)
  #q80$rn <- rownames(q80)
  #q85$rn <- rownames(q85)
  #q90$rn <- rownames(q90)
  #q95$rn <- rownames(q95)
  df <- join_all(list(#q05, q10, q15, q20, 
    q25, #q30, q35, q40, q45, 
    q50, #q55, q60, q65, q70, 
    q75), by='rn', type='full') #, q80, q85, q90, q95)  
  #df$rn <- NULL
  df
}

mysummary3 <- function(city, inputwmat, inputdata) {
  #q05 <- myfunction(qregspiv(form3,  wmat=inputwmat, tau=0.05, data=inputdata, silent=TRUE), paste0(city, "05"), "sig05")
  #q10 <- myfunction(qregspiv(form3,  wmat=inputwmat, tau=0.10, data=inputdata, silent=TRUE), paste0(city, "10"), "sig10")
  #q15 <- myfunction(qregspiv(form3,  wmat=inputwmat, tau=0.15, data=inputdata, silent=TRUE), paste0(city, "15"), "sig15")
  #q20 <- myfunction(qregspiv(form3,  wmat=inputwmat, tau=0.20, data=inputdata, silent=TRUE), paste0(city, "20"), "sig20")
  q25 <- myfunction(qregspiv(form3,  wmat=inputwmat, tau=0.25, data=inputdata, silent=TRUE), paste0(city, "25"), "sig25")
  #q30 <- myfunction(qregspiv(form3,  wmat=inputwmat, tau=0.30, data=inputdata, silent=TRUE), paste0(city, "30"), "sig30")
  #q35 <- myfunction(qregspiv(form3,  wmat=inputwmat, tau=0.35, data=inputdata, silent=TRUE), paste0(city, "35"), "sig35")
  #q40 <- myfunction(qregspiv(form3,  wmat=inputwmat, tau=0.40, data=inputdata, silent=TRUE), paste0(city, "40"), "sig40")
  #q45 <- myfunction(qregspiv(form3,  wmat=inputwmat, tau=0.45, data=inputdata, silent=TRUE), paste0(city, "45"), "sig45")
  q50 <- myfunction(qregspiv(form3,  wmat=inputwmat, tau=0.50, data=inputdata, silent=TRUE), paste0(city, "50"), "sig50")
  #q55 <- myfunction(qregspiv(form3,  wmat=inputwmat, tau=0.55, data=inputdata, silent=TRUE), paste0(city, "55"), "sig55")
  #q60 <- myfunction(qregspiv(form3,  wmat=inputwmat, tau=0.60, data=inputdata, silent=TRUE), paste0(city, "60"), "sig60")
  #q65 <- myfunction(qregspiv(form3,  wmat=inputwmat, tau=0.65, data=inputdata, silent=TRUE), paste0(city, "65"), "sig65")
  #q70 <- myfunction(qregspiv(form3,  wmat=inputwmat, tau=0.70, data=inputdata, silent=TRUE), paste0(city, "70"), "sig70")
  q75 <- myfunction(qregspiv(form3,  wmat=inputwmat, tau=0.75, data=inputdata, silent=TRUE), paste0(city, "75"), "sig75")
  #q80 <- myfunction(qregspiv(form3,  wmat=inputwmat, tau=0.80, data=inputdata, silent=TRUE), paste0(city, "80"), "sig80")
  #q85 <- myfunction(qregspiv(form3,  wmat=inputwmat, tau=0.85, data=inputdata, silent=TRUE), paste0(city, "85"), "sig85")
  #q90 <- myfunction(qregspiv(form3,  wmat=inputwmat, tau=0.90, data=inputdata, silent=TRUE), paste0(city, "90"), "sig90")
  #q95 <- myfunction(qregspiv(form3,  wmat=inputwmat, tau=0.95, data=inputdata, silent=TRUE), paste0(city, "95"), "sig95")
  
  #q05$rn <- rownames(q05)
  #q10$rn <- rownames(q10)
  #q15$rn <- rownames(q15)
  #q20$rn <- rownames(q20)
  q25$rn <- rownames(q25)
  #q30$rn <- rownames(q30)
  #q35$rn <- rownames(q35)
  #q40$rn <- rownames(q40)
  #q45$rn <- rownames(q45)
  q50$rn <- rownames(q50)
  #q55$rn <- rownames(q55)
  #q60$rn <- rownames(q60)
  #q65$rn <- rownames(q65)
  #q70$rn <- rownames(q70)
  q75$rn <- rownames(q75)
  #q80$rn <- rownames(q80)
  #q85$rn <- rownames(q85)
  #q90$rn <- rownames(q90)
  #q95$rn <- rownames(q95)
  df <- join_all(list(#q05, q10, q15, q20, 
    q25, #q30, q35, q40, q45, 
    q50, #q55, q60, q65, q70, 
    q75), by='rn', type='full') #, q80, q85, q90, q95)  
  #df$rn <- NULL
  df
}

mysummary4 <- function(city, inputwmat, inputdata) {
  #q05 <- myfunction(qregspiv(form4,  wmat=inputwmat, tau=0.05, data=inputdata, silent=TRUE), paste0(city, "05"), "sig05")
  #q10 <- myfunction(qregspiv(form4,  wmat=inputwmat, tau=0.10, data=inputdata, silent=TRUE), paste0(city, "10"), "sig10")
  #q15 <- myfunction(qregspiv(form4,  wmat=inputwmat, tau=0.15, data=inputdata, silent=TRUE), paste0(city, "15"), "sig15")
  #q20 <- myfunction(qregspiv(form4,  wmat=inputwmat, tau=0.20, data=inputdata, silent=TRUE), paste0(city, "20"), "sig20")
  q25 <- myfunction(qregspiv(form4,  wmat=inputwmat, tau=0.25, data=inputdata, silent=TRUE), paste0(city, "25"), "sig25")
  #q30 <- myfunction(qregspiv(form4,  wmat=inputwmat, tau=0.30, data=inputdata, silent=TRUE), paste0(city, "30"), "sig30")
  #q35 <- myfunction(qregspiv(form4,  wmat=inputwmat, tau=0.35, data=inputdata, silent=TRUE), paste0(city, "35"), "sig35")
  #q40 <- myfunction(qregspiv(form4,  wmat=inputwmat, tau=0.40, data=inputdata, silent=TRUE), paste0(city, "40"), "sig40")
  #q45 <- myfunction(qregspiv(form4,  wmat=inputwmat, tau=0.45, data=inputdata, silent=TRUE), paste0(city, "45"), "sig45")
  q50 <- myfunction(qregspiv(form4,  wmat=inputwmat, tau=0.50, data=inputdata, silent=TRUE), paste0(city, "50"), "sig50")
  #q55 <- myfunction(qregspiv(form4,  wmat=inputwmat, tau=0.55, data=inputdata, silent=TRUE), paste0(city, "55"), "sig55")
  #q60 <- myfunction(qregspiv(form4,  wmat=inputwmat, tau=0.60, data=inputdata, silent=TRUE), paste0(city, "60"), "sig60")
  #q65 <- myfunction(qregspiv(form4,  wmat=inputwmat, tau=0.65, data=inputdata, silent=TRUE), paste0(city, "65"), "sig65")
  #q70 <- myfunction(qregspiv(form4,  wmat=inputwmat, tau=0.70, data=inputdata, silent=TRUE), paste0(city, "70"), "sig70")
  q75 <- myfunction(qregspiv(form4,  wmat=inputwmat, tau=0.75, data=inputdata, silent=TRUE), paste0(city, "75"), "sig75")
  #q80 <- myfunction(qregspiv(form4,  wmat=inputwmat, tau=0.80, data=inputdata, silent=TRUE), paste0(city, "80"), "sig80")
  #q85 <- myfunction(qregspiv(form4,  wmat=inputwmat, tau=0.85, data=inputdata, silent=TRUE), paste0(city, "85"), "sig85")
  #q90 <- myfunction(qregspiv(form4,  wmat=inputwmat, tau=0.90, data=inputdata, silent=TRUE), paste0(city, "90"), "sig90")
  #q95 <- myfunction(qregspiv(form4,  wmat=inputwmat, tau=0.95, data=inputdata, silent=TRUE), paste0(city, "95"), "sig95")
  
  #q05$rn <- rownames(q05)
  #q10$rn <- rownames(q10)
  #q15$rn <- rownames(q15)
  #q20$rn <- rownames(q20)
  q25$rn <- rownames(q25)
  #q30$rn <- rownames(q30)
  #q35$rn <- rownames(q35)
  #q40$rn <- rownames(q40)
  #q45$rn <- rownames(q45)
  q50$rn <- rownames(q50)
  #q55$rn <- rownames(q55)
  #q60$rn <- rownames(q60)
  #q65$rn <- rownames(q65)
  #q70$rn <- rownames(q70)
  q75$rn <- rownames(q75)
  #q80$rn <- rownames(q80)
  #q85$rn <- rownames(q85)
  #q90$rn <- rownames(q90)
  #q95$rn <- rownames(q95)
  df <- join_all(list(#q05, q10, q15, q20, 
    q25, #q30, q35, q40, q45, 
    q50, #q55, q60, q65, q70, 
    q75), by='rn', type='full') #, q80, q85, q90, q95)  
  #df$rn <- NULL
  df
}

UA01summary <- mysummary("Atlanta",   wmat01, geodataUA01)
UA02summary <- mysummary("Baltimore", wmat02, geodataUA02)
UA03summary <- mysummary("Boston", wmat03, geodataUA03)
UA04summary <- mysummary("Chicago", wmat04, geodataUA04)
UA05summary <- mysummary("Cleveland", wmat05, geodataUA05)
UA06summary <- mysummary3("Dallas", wmat06, geodataUA06)
UA07summary <- mysummary2("Detroit", wmat07, geodataUA07)
UA08summary <- mysummary4("Houston", wmat08, geodataUA08)
UA09summary <- mysummary2("LA", wmat09, geodataUA09)
UA10summary <- mysummary2("Miama", wmat10, geodataUA10)

UA11summary <- mysummary4("Minneapolis",   wmat11, geodataUA11)
UA12summary <- mysummary("New York", wmat12, geodataUA12)
UA13summary <- mysummary("Philadelphia", wmat13, geodataUA13)
UA14summary <- mysummary4("Phoenix", wmat14, geodataUA14)
UA15summary <- mysummary3("St Louis", wmat15, geodataUA15)
UA16summary <- mysummary2("San Diego", wmat16, geodataUA16)
UA17summary <- mysummary("San Francisco", wmat17, geodataUA17)
UA18summary <- mysummary4("Seattle", wmat18, geodataUA18)
UA19summary <- mysummary4("Tampa", wmat19, geodataUA19)
UA20summary <- mysummary("Washington DC", wmat20, geodataUA20)



# 2.7. generate charts for each UA 

mycleaning <- function(UAsummary) {
  temp01 <- UAsummary[2:10, c(1, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30, 32, 34, 36, 38)]
  temp01[10, ] <- c(5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95)
  temp01 <- as.data.frame(t(temp01))
  colnames(temp01) <- c("CC8090", "CC9000", "CC0010", "lnpden8090", "lnpden9000", "lnpden0010", 
                        "One_Transit8090", "One_Transit9000", "One_Transit0010","quantile")
  temp01 
}

mychart <- function(UAsummary, UAno, cohort, UAname){
  temp01 <- mycleaning(UAsummary)
  ggplot() + 
    scale_x_continuous(breaks = seq(0, 100, by = 10)) + 
    scale_y_continuous(breaks = seq(-400, 400, by = 200)) + 
    coord_cartesian(ylim=c(-1, 1))+ 
    geom_point(data=temp01, aes(x=quantile, y=CC8090), color="orangered4", size = 1.5) + 
    geom_smooth(data=temp01, aes(x=quantile, y=CC8090), color="orangered1", size = 0.25, alpha=0.4, method="loess", se = FALSE) + 
    geom_point(data=temp01, aes(x=quantile, y=CC9000), color="green4", size = 1.5) +
    geom_smooth(data=temp01, aes(x=quantile, y=CC9000), color="green1", size = 0.25, alpha=0.4, method="loess", se = FALSE) +
    geom_point(data=temp01, aes(x=quantile, y=CC0010), color="royalblue4", size = 1.5) + 
    geom_smooth(data=temp01, aes(x=quantile, y=CC0010), color="royalblue1", size = 0.25, alpha=0.4, method="loess", se = FALSE) + 
    labs(x="Quantiles", y="Coefficients for being in the Central City", size=4) + 
    ggtitle(paste("  Net migration of", cohort, "in", UAname)) +
    theme(plot.title = element_text(size=14, hjust=0))
  
  ggsave(paste0("M:\\Millennial_panel\\23_chart_figure_3decades\\qregspiv_ccity_", cohort, UAno, ".png"), 
         width = 8.5, height = 8, units = 'in', dpi=300)
}

mychart(UA01summary, "UA01", "the 25-34 cohort", "Atlanta")
mychart(UA02summary, "UA02", "the 25-34 cohort", "Baltimore")
mychart(UA03summary, "UA03", "the 25-34 cohort", "Boston")
mychart(UA04summary, "UA04", "the 25-34 cohort", "Chicago")
mychart(UA05summary, "UA05", "the 25-34 cohort", "Cleveland")

mychart(UA06summary, "UA06", "the 25-34 cohort", "Dallas")
mychart(UA07summary, "UA07", "the 25-34 cohort", "Detroit")
mychart(UA08summary, "UA08", "the 25-34 cohort", "Houston")
mychart(UA09summary, "UA09", "the 25-34 cohort", "Los Angeles")
mychart(UA10summary, "UA10", "the 25-34 cohort", "Miami")

mychart(UA11summary, "UA11", "the 25-34 cohort", "Minneapolis")
mychart(UA12summary, "UA12", "the 25-34 cohort", "New York")
mychart(UA13summary, "UA13", "the 25-34 cohort", "Philadelphia")
mychart(UA14summary, "UA14", "the 25-34 cohort", "Phoenix")
mychart(UA15summary, "UA15", "the 25-34 cohort", "St. Louis")

mychart(UA16summary, "UA16", "the 25-34 cohort", "San Diego")
mychart(UA17summary, "UA17", "the 25-34 cohort", "San Francisco")
mychart(UA18summary, "UA18", "the 25-34 cohort", "Seattle")
mychart(UA19summary, "UA19", "the 25-34 cohort", "Tampa")
mychart(UA20summary, "UA20", "the 25-34 cohort", "Washington, DC")



# 2.8. run quantile regression by decade  

wmat8090 <- blockMatrixDiagonal(wmat01a, wmat02a, wmat03a, wmat04a, wmat05a, wmat06a, wmat07a, wmat08a, wmat09a, wmat10a, 
                                wmat11a, wmat12a, wmat13a, wmat14a, wmat15a, wmat16a, wmat17a, wmat18a, wmat19a, wmat20a) 

wmat9000 <- blockMatrixDiagonal(wmat01b, wmat02b, wmat03b, wmat04b, wmat05b, wmat06b, wmat07b, wmat08b, wmat09b, wmat10b, 
                                wmat11b, wmat12b, wmat13b, wmat14b, wmat15b, wmat16b, wmat17b, wmat18b, wmat19b, wmat20b)

wmat0010 <- blockMatrixDiagonal(wmat01c, wmat02c, wmat03c, wmat04c, wmat05c, wmat06c, wmat07c, wmat08c, wmat09c, wmat10c, 
                                wmat11c, wmat12c, wmat13c, wmat14c, wmat15c, wmat16c, wmat17c, wmat18c, wmat19c, wmat20c)

form8090 <- Migr_young ~ # Migr_young ~  
  cc8090_5 + lnpden8090 +  One_Transit8090 + 
  lnpop100 + pctyoungbg + pctnhw + pctforeign + pctmulti + elem +  
  UA01 + UA02 + UA03 + UA04 + UA05 + UA06 + UA07 + UA08 + UA09 + UA10 + 
  UA11 + UA12 + UA13 + UA14 + UA15 + UA16 + UA17 + UA18 + UA19

form9000 <-  Migr_young ~ # Migr_young ~  
  cc9000_5 + lnpden9000 + One_Transit9000 + 
  lnpop100 + pctyoungbg + pctnhw + pctforeign + pctmulti + elem +  
  UA01 + UA02 + UA03 + UA04 + UA05 + UA06 + UA07 + UA08 + UA09 + UA10 + 
  UA11 + UA12 + UA13 + UA14 + UA15 + UA16 + UA17 + UA18 + UA19

form0010 <-  Migr_young ~ # Migr_young ~  
  cc0010_5 + lnpden0010 + One_Transit0010 + 
  lnpop100 + pctyoungbg + pctnhw + pctforeign + pctmulti + elem +  
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

# 2.8.1. the 1980s 
#q05 <- myfunction(qregspiv(form8090,  wmat=wmat8090, tau=0.05, data=geodata8090@data, silent=TRUE), "05", "sig05")
#q10 <- myfunction(qregspiv(form8090,  wmat=wmat8090, tau=0.10, data=geodata8090@data, silent=TRUE), "10", "sig10")
#q15 <- myfunction(qregspiv(form8090,  wmat=wmat8090, tau=0.15, data=geodata8090@data, silent=TRUE), "15", "sig15")
#q20 <- myfunction(qregspiv(form8090,  wmat=wmat8090, tau=0.20, data=geodata8090@data, silent=TRUE), "20", "sig20")
q25 <- myfunction(qregspiv(form8090,  wmat=wmat8090, tau=0.25, data=geodata8090@data, silent=TRUE), "25", "sig25")
#q30 <- myfunction(qregspiv(form8090,  wmat=wmat8090, tau=0.30, data=geodata8090@data, silent=TRUE), "30", "sig30")
#q35 <- myfunction(qregspiv(form8090,  wmat=wmat8090, tau=0.35, data=geodata8090@data, silent=TRUE), "35", "sig35")
#q40 <- myfunction(qregspiv(form8090,  wmat=wmat8090, tau=0.40, data=geodata8090@data, silent=TRUE), "40", "sig40")
#q45 <- myfunction(qregspiv(form8090,  wmat=wmat8090, tau=0.45, data=geodata8090@data, silent=TRUE), "45", "sig45")
q50 <- myfunction(qregspiv(form8090,  wmat=wmat8090, tau=0.50, data=geodata8090@data, silent=TRUE), "50", "sig50")
#q55 <- myfunction(qregspiv(form8090,  wmat=wmat8090, tau=0.55, data=geodata8090@data, silent=TRUE), "55", "sig55")
#q60 <- myfunction(qregspiv(form8090,  wmat=wmat8090, tau=0.60, data=geodata8090@data, silent=TRUE), "60", "sig60")
#q65 <- myfunction(qregspiv(form8090,  wmat=wmat8090, tau=0.65, data=geodata8090@data, silent=TRUE), "65", "sig65")
#q70 <- myfunction(qregspiv(form8090,  wmat=wmat8090, tau=0.70, data=geodata8090@data, silent=TRUE), "70", "sig70")
q75 <- myfunction(qregspiv(form8090,  wmat=wmat8090, tau=0.75, data=geodata8090@data, silent=TRUE), "75", "sig75")
#q80 <- myfunction(qregspiv(form8090,  wmat=wmat8090, tau=0.80, data=geodata8090@data, silent=TRUE), "80", "sig80")
#q85 <- myfunction(qregspiv(form8090,  wmat=wmat8090, tau=0.85, data=geodata8090@data, silent=TRUE), "85", "sig85")
#q90 <- myfunction(qregspiv(form8090,  wmat=wmat8090, tau=0.90, data=geodata8090@data, silent=TRUE), "90", "sig90")
#q95 <- myfunction(qregspiv(form8090,  wmat=wmat8090, tau=0.95, data=geodata8090@data, silent=TRUE), "95", "sig95")

#q05$rn <- rownames(q05)
#q10$rn <- rownames(q10)
#q15$rn <- rownames(q15)
#q20$rn <- rownames(q20)
q25$rn <- rownames(q25)
#q30$rn <- rownames(q30)
#q35$rn <- rownames(q35)
#q40$rn <- rownames(q40)
#q45$rn <- rownames(q45)
q50$rn <- rownames(q50)
#q55$rn <- rownames(q55)
#q60$rn <- rownames(q60)
#q65$rn <- rownames(q65)
#q70$rn <- rownames(q70)
q75$rn <- rownames(q75)
#q80$rn <- rownames(q80)
#q85$rn <- rownames(q85)
#q90$rn <- rownames(q90)
#q95$rn <- rownames(q95)

#write.csv(q05, file="./key_output/by_decade/qregspiv_young_2000s_q05.csv")
#write.csv(q10, file="./key_output/by_decade/qregspiv_young_2000s_q10.csv")
#write.csv(q15, file="./key_output/by_decade/qregspiv_young_2000s_q15.csv")
#write.csv(q20, file="./key_output/by_decade/qregspiv_young_2000s_q20.csv")
write.csv(q25, file="./key_output/by_decade/qregspiv_young_1980s_q25.csv")
#write.csv(q30, file="./key_output/by_decade/qregspiv_young_2000s_q30.csv")
#write.csv(q35, file="./key_output/by_decade/qregspiv_young_2000s_q35.csv")
#write.csv(q40, file="./key_output/by_decade/qregspiv_young_2000s_q40.csv")
#write.csv(q45, file="./key_output/by_decade/qregspiv_young_2000s_q45.csv")
write.csv(q50, file="./key_output/by_decade/qregspiv_young_1980s_q50.csv")
#write.csv(q55, file="./key_output/by_decade/qregspiv_young_2000s_q55.csv")
#write.csv(q60, file="./key_output/by_decade/qregspiv_young_2000s_q60.csv")
#write.csv(q65, file="./key_output/by_decade/qregspiv_young_2000s_q65.csv")
#write.csv(q70, file="./key_output/by_decade/qregspiv_young_2000s_q70.csv")
write.csv(q75, file="./key_output/by_decade/qregspiv_young_1980s_q75.csv")
#write.csv(q80, file="./key_output/by_decade/qregspiv_young_2000s_q80.csv")
#write.csv(q85, file="./key_output/by_decade/qregspiv_young_2000s_q85.csv")
#write.csv(q90, file="./key_output/by_decade/qregspiv_young_2000s_q90.csv")
#write.csv(q95, file="./key_output/by_decade/qregspiv_young_2000s_q95.csv")

df <- join_all(list(#q05, q10, q15, q20, 
  q25, #q30, q35, q40, q45, 
  q50, #q55, q60, q65, q70, 
  q75), by='rn', type='full') #, q80, q85, q90, q95), by='rn', type='full') 
row.names(df) <- df$rn
df
df$rn <- NULL
df <- as.data.frame(t(df))
#df8090 <- df[c(seq(1, 37, by=2)), 2:4]
df8090 <- df[c(1, 3, 5), c(2:4, 30)] # <- to generate regression tables 
df8090$quantile <- row.names(df8090)
df8090

# 2.8.2. the 1990s 

#q05b <- myfunction(qregspiv(form9000,  wmat=wmat9000, tau=0.05, data=geodata9000@data, silent=TRUE), "05", "sig05")
#q10b <- myfunction(qregspiv(form9000,  wmat=wmat9000, tau=0.10, data=geodata9000@data, silent=TRUE), "10", "sig10")
#q15b <- myfunction(qregspiv(form9000,  wmat=wmat9000, tau=0.15, data=geodata9000@data, silent=TRUE), "15", "sig15")
#q20b <- myfunction(qregspiv(form9000,  wmat=wmat9000, tau=0.20, data=geodata9000@data, silent=TRUE), "20", "sig20")
q25b <- myfunction(qregspiv(form9000,  wmat=wmat9000, tau=0.25, data=geodata9000@data, silent=TRUE), "25", "sig25")
#q30b <- myfunction(qregspiv(form9000,  wmat=wmat9000, tau=0.30, data=geodata9000@data, silent=TRUE), "30", "sig30")
#q35b <- myfunction(qregspiv(form9000,  wmat=wmat9000, tau=0.35, data=geodata9000@data, silent=TRUE), "35", "sig35")
#q40b <- myfunction(qregspiv(form9000,  wmat=wmat9000, tau=0.40, data=geodata9000@data, silent=TRUE), "40", "sig40")
#q45b <- myfunction(qregspiv(form9000,  wmat=wmat9000, tau=0.45, data=geodata9000@data, silent=TRUE), "45", "sig45")
q50b <- myfunction(qregspiv(form9000,  wmat=wmat9000, tau=0.50, data=geodata9000@data, silent=TRUE), "50", "sig50")
#q55b <- myfunction(qregspiv(form9000,  wmat=wmat9000, tau=0.55, data=geodata9000@data, silent=TRUE), "55", "sig55")
#q60b <- myfunction(qregspiv(form9000,  wmat=wmat9000, tau=0.60, data=geodata9000@data, silent=TRUE), "60", "sig60")
#q65b <- myfunction(qregspiv(form9000,  wmat=wmat9000, tau=0.65, data=geodata9000@data, silent=TRUE), "65", "sig65")
#q70b <- myfunction(qregspiv(form9000,  wmat=wmat9000, tau=0.70, data=geodata9000@data, silent=TRUE), "70", "sig70")
q75b <- myfunction(qregspiv(form9000,  wmat=wmat9000, tau=0.75, data=geodata9000@data, silent=TRUE), "75", "sig75")
#q80b <- myfunction(qregspiv(form9000,  wmat=wmat9000, tau=0.80, data=geodata9000@data, silent=TRUE), "80", "sig80")
#q85b <- myfunction(qregspiv(form9000,  wmat=wmat9000, tau=0.85, data=geodata9000@data, silent=TRUE), "85", "sig85")
#q90b <- myfunction(qregspiv(form9000,  wmat=wmat9000, tau=0.90, data=geodata9000@data, silent=TRUE), "90", "sig90")
#q95b <- myfunction(qregspiv(form9000,  wmat=wmat9000, tau=0.95, data=geodata9000@data, silent=TRUE), "95", "sig95")

#q05b$rn <- rownames(q05b)
#q10b$rn <- rownames(q10b)
#q15b$rn <- rownames(q15b)
#q20b$rn <- rownames(q20b)
q25b$rn <- rownames(q25b)
#q30b$rn <- rownames(q30b)
#q35b$rn <- rownames(q35b)
#q40b$rn <- rownames(q40b)
#q45b$rn <- rownames(q45b)
q50b$rn <- rownames(q50b)
#q55b$rn <- rownames(q55b)
#q60b$rn <- rownames(q60b)
#q65b$rn <- rownames(q65b)
#q70b$rn <- rownames(q70b)
q75b$rn <- rownames(q75b)
#q80b$rn <- rownames(q80b)
#q85b$rn <- rownames(q85b)
#q90b$rn <- rownames(q90b)
#q95b$rn <- rownames(q95b)

#write.csv(q05b, file="./key_output/by_decade/qregspiv_young_1990s_q05b.csv")
#write.csv(q10b, file="./key_output/by_decade/qregspiv_young_1990s_q10b.csv")
#write.csv(q15b, file="./key_output/by_decade/qregspiv_young_1990s_q15b.csv")
#write.csv(q20b, file="./key_output/by_decade/qregspiv_young_1990s_q20b.csv")
write.csv(q25b, file="./key_output/by_decade/qregspiv_young_1990s_q25b.csv")
#write.csv(q30b, file="./key_output/by_decade/qregspiv_young_2000s_q30b.csv")
#write.csv(q35b, file="./key_output/by_decade/qregspiv_young_2000s_q35b.csv")
#write.csv(q40b, file="./key_output/by_decade/qregspiv_young_2000s_q40b.csv")
#write.csv(q45b, file="./key_output/by_decade/qregspiv_young_2000s_q45b.csv")
write.csv(q50b, file="./key_output/by_decade/qregspiv_young_1990s_q50b.csv")
#write.csv(q55b, file="./key_output/by_decade/qregspiv_young_2000s_q55b.csv")
#write.csv(q60b, file="./key_output/by_decade/qregspiv_young_2000s_q60b.csv")
#write.csv(q65b, file="./key_output/by_decade/qregspiv_young_2000s_q65b.csv")
#write.csv(q70b, file="./key_output/by_decade/qregspiv_young_2000s_q70b.csv")
write.csv(q75b, file="./key_output/by_decade/qregspiv_young_1990s_q75b.csv")
#write.csv(q80b, file="./key_output/by_decade/qregspiv_young_2000s_q80b.csv")
#write.csv(q85b, file="./key_output/by_decade/qregspiv_young_2000s_q85b.csv")
#write.csv(q90b, file="./key_output/by_decade/qregspiv_young_2000s_q90b.csv")
#write.csv(q95b, file="./key_output/by_decade/qregspiv_young_2000s_q95b.csv")

df<- NULL
df <- join_all(list(#q05b, q10b, q15b, q20b, 
  q25b, #q30b, q35b, q40b, q45b, 
  q50b, #q55b, q60b, q65b, q70b, 
  q75b), by='rn', type='full') #q80b, q85b, q90b, q95b), by='rn', type='full') 
row.names(df) <- df$rn
df$rn <- NULL
df <- as.data.frame(t(df))
#df9000 <- df[c(seq(1, 37, by=2)), 2:4]
df9000 <- df[c(1, 3, 5), c(2:4, 30)] # <- to generate regression tables 
df9000$quantile <- row.names(df9000)
df9000

# 2.8.3. the 2000s 
#q05c <- myfunction(qregspiv(form0010,  wmat=wmat0010, tau=0.05, data=geodata0010@data, silent=TRUE), "05", "sig05")
#q10c <- myfunction(qregspiv(form0010,  wmat=wmat0010, tau=0.10, data=geodata0010@data, silent=TRUE), "10", "sig10")
#q15c <- myfunction(qregspiv(form0010,  wmat=wmat0010, tau=0.15, data=geodata0010@data, silent=TRUE), "15", "sig15")
#q20c <- myfunction(qregspiv(form0010,  wmat=wmat0010, tau=0.20, data=geodata0010@data, silent=TRUE), "20", "sig20")
q25c <- myfunction(qregspiv(form0010,  wmat=wmat0010, tau=0.25, data=geodata0010@data, silent=TRUE), "25", "sig25")
#q30c <- myfunction(qregspiv(form0010,  wmat=wmat0010, tau=0.30, data=geodata0010@data, silent=TRUE), "30", "sig30")
#q35c <- myfunction(qregspiv(form0010,  wmat=wmat0010, tau=0.35, data=geodata0010@data, silent=TRUE), "35", "sig35")
#q40c <- myfunction(qregspiv(form0010,  wmat=wmat0010, tau=0.40, data=geodata0010@data, silent=TRUE), "40", "sig40")
#q45c <- myfunction(qregspiv(form0010,  wmat=wmat0010, tau=0.45, data=geodata0010@data, silent=TRUE), "45", "sig45")
q50c <- myfunction(qregspiv(form0010,  wmat=wmat0010, tau=0.50, data=geodata0010@data, silent=TRUE), "50", "sig50")
#q55c <- myfunction(qregspiv(form0010,  wmat=wmat0010, tau=0.55, data=geodata0010@data, silent=TRUE), "55", "sig55")
#q60c <- myfunction(qregspiv(form0010,  wmat=wmat0010, tau=0.60, data=geodata0010@data, silent=TRUE), "60", "sig60")
#q65c <- myfunction(qregspiv(form0010,  wmat=wmat0010, tau=0.65, data=geodata0010@data, silent=TRUE), "65", "sig65")
#q70c <- myfunction(qregspiv(form0010,  wmat=wmat0010, tau=0.70, data=geodata0010@data, silent=TRUE), "70", "sig70")
q75c <- myfunction(qregspiv(form0010,  wmat=wmat0010, tau=0.75, data=geodata0010@data, silent=TRUE), "75", "sig75")
#q80c <- myfunction(qregspiv(form0010,  wmat=wmat0010, tau=0.80, data=geodata0010@data, silent=TRUE), "80", "sig80")
#q85c <- myfunction(qregspiv(form0010,  wmat=wmat0010, tau=0.85, data=geodata0010@data, silent=TRUE), "85", "sig85")
#q90c <- myfunction(qregspiv(form0010,  wmat=wmat0010, tau=0.90, data=geodata0010@data, silent=TRUE), "90", "sig90")
#q95c <- myfunction(qregspiv(form0010,  wmat=wmat0010, tau=0.95, data=geodata0010@data, silent=TRUE), "95", "sig95")

#q05c$rn <- rownames(q05c)
#q10c$rn <- rownames(q10c)
#q15c$rn <- rownames(q15c)
#q20c$rn <- rownames(q20c)
q25c$rn <- rownames(q25c)
#q30c$rn <- rownames(q30c)
#q35c$rn <- rownames(q35c)
#q40c$rn <- rownames(q40c)
#q45c$rn <- rownames(q45c)
q50c$rn <- rownames(q50c)
#q55c$rn <- rownames(q55c)
#q60c$rn <- rownames(q60c)
#q65c$rn <- rownames(q65c)
#q70c$rn <- rownames(q70c)
q75c$rn <- rownames(q75c)
#q80c$rn <- rownames(q80c)
#q85c$rn <- rownames(q85c)
#q90c$rn <- rownames(q90c)
#q95c$rn <- rownames(q95c)

#write.csv(q05c, file="./key_output/by_decade/qregspiv_young_2000s_q05c.csv")
#write.csv(q10c, file="./key_output/by_decade/qregspiv_young_2000s_q10c.csv")
#write.csv(q15c, file="./key_output/by_decade/qregspiv_young_2000s_q15c.csv")
#write.csv(q20c, file="./key_output/by_decade/qregspiv_young_2000s_q20c.csv")
write.csv(q25c, file="./key_output/by_decade/qregspiv_young_2000s_q25c.csv")
#write.csv(q30c, file="./key_output/by_decade/qregspiv_young_2000s_q30c.csv")
#write.csv(q35c, file="./key_output/by_decade/qregspiv_young_2000s_q35c.csv")
#write.csv(q40c, file="./key_output/by_decade/qregspiv_young_2000s_q40c.csv")
#write.csv(q45c, file="./key_output/by_decade/qregspiv_young_2000s_q45c.csv")
write.csv(q50c, file="./key_output/by_decade/qregspiv_young_2000s_q50c.csv")
#write.csv(q55c, file="./key_output/by_decade/qregspiv_young_2000s_q55c.csv")
#write.csv(q60c, file="./key_output/by_decade/qregspiv_young_2000s_q60c.csv")
#write.csv(q65c, file="./key_output/by_decade/qregspiv_young_2000s_q65c.csv")
#write.csv(q70c, file="./key_output/by_decade/qregspiv_young_2000s_q70c.csv")
write.csv(q75c, file="./key_output/by_decade/qregspiv_young_2000s_q75c.csv")
#write.csv(q80c, file="./key_output/by_decade/qregspiv_young_2000s_q80c.csv")
#write.csv(q85c, file="./key_output/by_decade/qregspiv_young_2000s_q85c.csv")
#write.csv(q90c, file="./key_output/by_decade/qregspiv_young_2000s_q90c.csv")
#write.csv(q95c, file="./key_output/by_decade/qregspiv_young_2000s_q95c.csv")
df <- NULL
df <- join_all(list(#q05c, q10c, q15c, q20c, 
  q25c, #q30c, q35c, q40c, q45c, 
  q50c, #q55c, q60c, q65c, q70c, 
  q75c), by='rn', type='full')  #, q80c, q85c, q90c, q95c), by='rn', type='full') 
row.names(df) <- df$rn
df$rn <- NULL
df <- as.data.frame(t(df))
#df0010 <- df[c(seq(1, 37, by=2)), 2:4]
df0010 <- df[c(1, 3, 5), c(2:4, 30)] # <- to generate regression tables 
df0010$quantile <- row.names(df0010)
df0010 



# 2.8.4. merge & export 

temp01 <- join_all(list(df8090, df9000, df0010), by='quantile', type='full')
row.names(temp01) <- temp01$quantile 
temp01$quantile <- NULL 
temp01$WY <- NULL 
#temp01$quantile <- c(seq(25, 25, by=25))
indx <- sapply(temp01, is.factor)
temp01[indx] <- lapply(temp01[indx], function(x) as.numeric(as.character(x)))
temp01 <- temp01[, c(1, 4, 7, 2, 5, 8, 3, 6, 9)]
temp01

#test[20, ] <- colnames(test)
#row.names(test)[20]<- "header"
#write.csv(test, file="./key_output/qregspiv_young_3decades.csv")
write.csv(temp01, file="./key_output/by_decade/qregspiv_youngest_3decades.csv")



# 2.9. generate charts 

#temp01 <- read.csv(file = './key_output/qregspiv_young_3decades.csv')
#temp01 <- temp01[-20, ] 
#temp01 <- temp01[, -1]
#indx <- sapply(temp01, is.factor)
#temp01[indx] <- lapply(temp01[indx], function(x) as.numeric(as.character(x)))

#https://stackoverflow.com/questions/10349206/add-legend-to-ggplot2-line-plot
ggplot(data=temp01, aes(x = quantile)) +
  scale_x_continuous(breaks = seq(0, 100, by = 10)) + 
  #scale_y_continuous(breaks = seq(-400, 100, by = 100)) + 
  coord_cartesian(ylim=c(-50, 150))+ 
  #scale_y_continuous(breaks = seq(-1400, 600, by = 200)) + 
  #coord_cartesian(ylim=c(-1400, 600))+ 
  geom_point(aes(y=CC8090, colour="1980s"), size = 1.5) + 
  geom_smooth(aes(y=CC8090, colour="1980s"), size = 0.25, alpha=0.4, method="loess", se = FALSE) + 
  geom_point(aes(y=CC9000, colour="1990s"), size = 1.5) +
  geom_smooth(aes(y=CC9000, colour="1990s"), size = 0.25, alpha=0.4, method="loess", se = FALSE) +
  geom_point(aes(y=CC0010, colour="2000s"), size = 1.5) + 
  geom_smooth(aes(y=CC0010, colour="2000s"), size = 0.25, alpha=0.4, method="loess", se = FALSE) + 
  scale_colour_manual("", breaks = c("2000s", "1990s", "1980s"), 
                          values = c("red", "green", "blue")) + 
  labs(x="Quantile points", y="Central city ", size=4) + 
  #ggtitle("  Net migration of the 20-24 cohort") +
  theme(plot.title = element_text(size=14, hjust=0))

ggsave(paste0("M:\\Millennial_panel\\23_chart_figure_3decades\\qregspiv_ccity_", "25-34", ".png"), 
       width = 9.5, height = 8, units = 'in', dpi=300)

ggplot(data=temp01, aes(x = quantile)) +
  scale_x_continuous(breaks = seq(0, 100, by = 10)) + 
  #scale_y_continuous(breaks = seq(-400, 100, by = 100)) + 
  coord_cartesian(ylim=c(-400, 50))+ 
  #scale_y_continuous(breaks = seq(-1400, 600, by = 200)) + 
  #coord_cartesian(ylim=c(-1400, 600))+ 
  geom_point(aes(y=lnpden8090, colour="1980s"), size = 1.5) + 
  geom_smooth(aes(y=lnpden8090, colour="1980s"), size = 0.25, alpha=0.4, method="loess", se = FALSE) + 
  geom_point(aes(y=lnpden9000, colour="1990s"), size = 1.5) +
  geom_smooth(aes(y=lnpden9000, colour="1990s"), size = 0.25, alpha=0.4, method="loess", se = FALSE) +
  geom_point(aes(y=lnpden0010, colour="2000s"), size = 1.5) + 
  geom_smooth(aes(y=lnpden0010, colour="2000s"), size = 0.25, alpha=0.4, method="loess", se = FALSE) + 
  scale_colour_manual("", breaks = c("2000s", "1990s", "1980s"), 
                      values = c("red", "green", "blue")) + 
  labs(x="Quantile points", y="ln(population density) ", size=4) + 
  #ggtitle("  Net migration of the 20-24 cohort") +
  theme(plot.title = element_text(size=14, hjust=0))

ggsave(paste0("M:\\Millennial_panel\\23_chart_figure_3decades\\qregspiv_lnpden_", "25-34", ".png"), 
       width = 9.5, height = 8, units = 'in', dpi=300)

ggplot(data=temp01, aes(x = quantile)) +
  scale_x_continuous(breaks = seq(0, 100, by = 10)) + 
  #scale_y_continuous(breaks = seq(-400, 100, by = 100)) + 
  coord_cartesian(ylim=c(0, 60))+ 
  #scale_y_continuous(breaks = seq(-1400, 600, by = 200)) + 
  #coord_cartesian(ylim=c(-1400, 600))+ 
  geom_point(aes(y=One_Transit8090, colour="1980s"), size = 1.5) + 
  geom_smooth(aes(y=One_Transit8090, colour="1980s"), size = 0.25, alpha=0.4, method="loess", se = FALSE) + 
  geom_point(aes(y=One_Transit9000, colour="1990s"), size = 1.5) +
  geom_smooth(aes(y=One_Transit9000, colour="1990s"), size = 0.25, alpha=0.4, method="loess", se = FALSE) +
  geom_point(aes(y=One_Transit0010, colour="2000s"), size = 1.5) + 
  geom_smooth(aes(y=One_Transit0010, colour="2000s"), size = 0.25, alpha=0.4, method="loess", se = FALSE) + 
  scale_colour_manual("", breaks = c("2000s", "1990s", "1980s"), 
                      values = c("red", "green", "blue")) + 
  labs(x="Quantile points", y="1/(Distance to public transit) ", size=4) + 
  #ggtitle("  Net migration of the 20-24 cohort") +
  theme(plot.title = element_text(size=14, hjust=0))

ggsave(paste0("M:\\Millennial_panel\\23_chart_figure_3decades\\qregspiv_transit_", "25-34", ".png"), 
       width = 9.5, height = 8, units = 'in', dpi=300)



# 3. two-decade model 

# 3.1. process the main dataset 

varlist2 <- c("decade", "UA", "UAname", "stcnttr", 
             "Migr_youngest", "Migr_young", "Migr_midlife", "Migr_middle", 
             "f_ccity9000b", "f_ccity0010b", "f_pden9000b", "f_pden0010b", 
             "f_pt9000b", "f_pt0010b", "lncden9000b", "lncden0010b", 
             "lnpop100", "pctyoungestbg", "pctyoungbg", "pctmidlifebg", "pctmiddlebg", 
             "pctnhw", "pctforeign", "pctmulti", "elem", "yr9000", "yr0010", 
             "UA01", "UA02", "UA03", "UA04", "UA05", 
             "UA06", "UA07", "UA08", "UA09", "UA10", 
             "UA11", "UA12", "UA13", "UA14", "UA15", 
             "UA16", "UA17", "UA18", "UA19", "UA20")

migrdata2 <- temp[temp$yr8090==0, varlist2]
complete_rows2 <- migrdata2[, varlist2] %>% 
  complete.cases()
migrdata2 <- migrdata2[complete_rows2, ]

migrdata2$StCntTr <- migrdata2$stcnttr 
migrdata2$StCntTrYr <- paste0(migrdata2$stcnttr, migrdata2$decade)

migrdata2 <- migrdata2[, c(1:4, 48:49, 5:47)]
head(migrdata2)


# 3.2. process three shapefiles 

# file.exists("M://Millennial_panel/15_GIS/CTUA/YR8090_UA01.shp")
# shp.UA01 <- readShapePoly("M://Millennial_panel/15_GIS/CTUA/YR8090_UA01.shp")

shp.YR8090 <- readOGR("M://Millennial_panel/15_GIS/CTUA", layer = "1980CTUA")
shp.YR8090@data <- shp.YR8090@data["StCntTr"]
shp.YR8090@data$StCntTrYr <- paste0(shp.YR8090@data$StCntTr, "yr8090") 
shp.YR8090 <- spChFIDs(shp.YR8090, shp.YR8090@data$StCntTrYr) # change the fid field 
head(shp.YR8090@data)

shp.YR9000 <- readOGR("M://Millennial_panel/15_GIS/CTUA", layer = "1990CTUA")
shp.YR9000@data <- shp.YR9000@data["StCntTr"]
shp.YR9000@data$StCntTrYr <- paste0(shp.YR9000@data$StCntTr, "yr9000")
shp.YR9000 <- spChFIDs(shp.YR9000, shp.YR9000@data$StCntTrYr) # change the fid field 
head(shp.YR9000@data)

shp.YR0010 <- readOGR("M://Millennial_panel/15_GIS/CTUA", layer = "2000CTUA")
shp.YR0010@data <- shp.YR0010@data["StCntTr"]
shp.YR0010@data$StCntTrYr <- paste0(shp.YR0010@data$StCntTr, "yr0010")
shp.YR0010 <- spChFIDs(shp.YR0010, shp.YR0010@data$StCntTrYr) # change the fid field 
head(shp.YR0010@data)

#colnames(shp.YR8090@data)
#head(shp.YR8090@data)
#head(shp.YR9000@data)
#head(shp.YR0010@data)




#3.3. generate contiguity matrices for each UA and each decade 

geodata90002 <- merge(shp.YR9000, subset(migrdata2, yr9000==1), by="StCntTrYr")#, duplicateGeoms = TRUE)
head(geodata90002@data)
geodata90002 <- subset(geodata90002, is.na(geodata90002@data[, 8])==0)
geodata90002@data <- geodata90002@data[order(geodata90002@data$UA, geodata90002@data$StCntTrYr), ] # sorting 
head(geodata90002@data)

wmat01b2 <- makew(shpfile=subset(geodata90002, geodata90002@data$UA01==1), method="queen")$wmat
wmat02b2 <- makew(shpfile=subset(geodata90002, geodata90002@data$UA02==1), method="queen")$wmat
wmat03b2 <- makew(shpfile=subset(geodata90002, geodata90002@data$UA03==1), method="queen")$wmat
wmat04b2 <- makew(shpfile=subset(geodata90002, geodata90002@data$UA04==1), method="queen")$wmat
wmat05b2 <- makew(shpfile=subset(geodata90002, geodata90002@data$UA05==1), method="queen")$wmat
wmat06b2 <- makew(shpfile=subset(geodata90002, geodata90002@data$UA06==1), method="queen")$wmat
wmat07b2 <- makew(shpfile=subset(geodata90002, geodata90002@data$UA07==1), method="queen")$wmat
wmat08b2 <- makew(shpfile=subset(geodata90002, geodata90002@data$UA08==1), method="queen")$wmat
wmat09b2 <- makew(shpfile=subset(geodata90002, geodata90002@data$UA09==1), method="queen")$wmat
wmat10b2 <- makew(shpfile=subset(geodata90002, geodata90002@data$UA10==1), method="queen")$wmat
wmat11b2 <- makew(shpfile=subset(geodata90002, geodata90002@data$UA11==1), method="queen")$wmat
wmat12b2 <- makew(shpfile=subset(geodata90002, geodata90002@data$UA12==1), method="queen")$wmat
wmat13b2 <- makew(shpfile=subset(geodata90002, geodata90002@data$UA13==1), method="queen")$wmat
wmat14b2 <- makew(shpfile=subset(geodata90002, geodata90002@data$UA14==1), method="queen")$wmat
wmat15b2 <- makew(shpfile=subset(geodata90002, geodata90002@data$UA15==1), method="queen")$wmat
wmat16b2 <- makew(shpfile=subset(geodata90002, geodata90002@data$UA16==1), method="queen")$wmat
wmat17b2 <- makew(shpfile=subset(geodata90002, geodata90002@data$UA17==1), method="queen")$wmat
wmat18b2 <- makew(shpfile=subset(geodata90002, geodata90002@data$UA18==1), method="queen")$wmat
wmat19b2 <- makew(shpfile=subset(geodata90002, geodata90002@data$UA19==1), method="queen")$wmat
wmat20b2 <- makew(shpfile=subset(geodata90002, geodata90002@data$UA20==1), method="queen")$wmat

geodata00102 <- merge(shp.YR0010, subset(migrdata2, yr0010==1), by="StCntTrYr")#, duplicateGeoms = TRUE)
head(geodata00102@data)
geodata00102 <- subset(geodata00102, is.na(geodata00102@data[, 8])==0)
geodata00102@data <- geodata00102@data[order(geodata00102@data$UA, geodata00102@data$StCntTrYr), ] # sorting 
head(geodata00102@data)

wmat01c2 <- makew(shpfile=subset(geodata00102, geodata00102@data$UA01==1), method="queen")$wmat
wmat02c2 <- makew(shpfile=subset(geodata00102, geodata00102@data$UA02==1), method="queen")$wmat
wmat03c2 <- makew(shpfile=subset(geodata00102, geodata00102@data$UA03==1), method="queen")$wmat
wmat04c2 <- makew(shpfile=subset(geodata00102, geodata00102@data$UA04==1), method="queen")$wmat
wmat05c2 <- makew(shpfile=subset(geodata00102, geodata00102@data$UA05==1), method="queen")$wmat
wmat06c2 <- makew(shpfile=subset(geodata00102, geodata00102@data$UA06==1), method="queen")$wmat
wmat07c2 <- makew(shpfile=subset(geodata00102, geodata00102@data$UA07==1), method="queen")$wmat
wmat08c2 <- makew(shpfile=subset(geodata00102, geodata00102@data$UA08==1), method="queen")$wmat
wmat09c2 <- makew(shpfile=subset(geodata00102, geodata00102@data$UA09==1), method="queen")$wmat
wmat10c2 <- makew(shpfile=subset(geodata00102, geodata00102@data$UA10==1), method="queen")$wmat
wmat11c2 <- makew(shpfile=subset(geodata00102, geodata00102@data$UA11==1), method="queen")$wmat
wmat12c2 <- makew(shpfile=subset(geodata00102, geodata00102@data$UA12==1), method="queen")$wmat
wmat13c2 <- makew(shpfile=subset(geodata00102, geodata00102@data$UA13==1), method="queen")$wmat
wmat14c2 <- makew(shpfile=subset(geodata00102, geodata00102@data$UA14==1), method="queen")$wmat
wmat15c2 <- makew(shpfile=subset(geodata00102, geodata00102@data$UA15==1), method="queen")$wmat
wmat16c2 <- makew(shpfile=subset(geodata00102, geodata00102@data$UA16==1), method="queen")$wmat
wmat17c2 <- makew(shpfile=subset(geodata00102, geodata00102@data$UA17==1), method="queen")$wmat
wmat18c2 <- makew(shpfile=subset(geodata00102, geodata00102@data$UA18==1), method="queen")$wmat
wmat19c2 <- makew(shpfile=subset(geodata00102, geodata00102@data$UA19==1), method="queen")$wmat
wmat20c2 <- makew(shpfile=subset(geodata00102, geodata00102@data$UA20==1), method="queen")$wmat



# 3.4. generate block-diagonal contiguity matrices 

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

wmat012 <- blockMatrixDiagonal(wmat01b2, wmat01c2)
wmat022 <- blockMatrixDiagonal(wmat02b2, wmat02c2)
wmat032 <- blockMatrixDiagonal(wmat03b2, wmat03c2)
wmat042 <- blockMatrixDiagonal(wmat04b2, wmat04c2)
wmat052 <- blockMatrixDiagonal(wmat05b2, wmat05c2)
wmat062 <- blockMatrixDiagonal(wmat06b2, wmat06c2)
wmat072 <- blockMatrixDiagonal(wmat07b2, wmat07c2)
wmat082 <- blockMatrixDiagonal(wmat08b2, wmat08c2)
wmat092 <- blockMatrixDiagonal(wmat09b2, wmat09c2)
wmat102 <- blockMatrixDiagonal(wmat10b2, wmat10c2)

wmat112 <- blockMatrixDiagonal(wmat11b2, wmat11c2)
wmat122 <- blockMatrixDiagonal(wmat12b2, wmat12c2)
wmat132 <- blockMatrixDiagonal(wmat13b2, wmat13c2)
wmat142 <- blockMatrixDiagonal(wmat14b2, wmat14c2)
wmat152 <- blockMatrixDiagonal(wmat15b2, wmat15c2)
wmat162 <- blockMatrixDiagonal(wmat16b2, wmat16c2)
wmat172 <- blockMatrixDiagonal(wmat17b2, wmat17c2)
wmat182 <- blockMatrixDiagonal(wmat18b2, wmat18c2)
wmat192 <- blockMatrixDiagonal(wmat19b2, wmat19c2)
wmat202 <- blockMatrixDiagonal(wmat20b2, wmat20c2)



# 3.5. generate data for each UA (pool two decades)

geodataUA012 <- rbind(subset(geodata90002@data, geodata90002@data$UA01==1), subset(geodata00102@data, geodata00102@data$UA01==1))
geodataUA022 <- rbind(subset(geodata90002@data, geodata90002@data$UA02==1), subset(geodata00102@data, geodata00102@data$UA02==1))
geodataUA032 <- rbind(subset(geodata90002@data, geodata90002@data$UA03==1), subset(geodata00102@data, geodata00102@data$UA03==1))
geodataUA042 <- rbind(subset(geodata90002@data, geodata90002@data$UA04==1), subset(geodata00102@data, geodata00102@data$UA04==1))
geodataUA052 <- rbind(subset(geodata90002@data, geodata90002@data$UA05==1), subset(geodata00102@data, geodata00102@data$UA05==1))
geodataUA062 <- rbind(subset(geodata90002@data, geodata90002@data$UA06==1), subset(geodata00102@data, geodata00102@data$UA06==1))
geodataUA072 <- rbind(subset(geodata90002@data, geodata90002@data$UA07==1), subset(geodata00102@data, geodata00102@data$UA07==1))
geodataUA082 <- rbind(subset(geodata90002@data, geodata90002@data$UA08==1), subset(geodata00102@data, geodata00102@data$UA08==1))
geodataUA092 <- rbind(subset(geodata90002@data, geodata90002@data$UA09==1), subset(geodata00102@data, geodata00102@data$UA09==1))
geodataUA102 <- rbind(subset(geodata90002@data, geodata90002@data$UA10==1), subset(geodata00102@data, geodata00102@data$UA10==1))

geodataUA112 <- rbind(subset(geodata90002@data, geodata90002@data$UA11==1), subset(geodata00102@data, geodata00102@data$UA11==1))
geodataUA122 <- rbind(subset(geodata90002@data, geodata90002@data$UA12==1), subset(geodata00102@data, geodata00102@data$UA12==1))
geodataUA132 <- rbind(subset(geodata90002@data, geodata90002@data$UA13==1), subset(geodata00102@data, geodata00102@data$UA13==1))
geodataUA142 <- rbind(subset(geodata90002@data, geodata90002@data$UA14==1), subset(geodata00102@data, geodata00102@data$UA14==1))
geodataUA152 <- rbind(subset(geodata90002@data, geodata90002@data$UA15==1), subset(geodata00102@data, geodata00102@data$UA15==1))
geodataUA162 <- rbind(subset(geodata90002@data, geodata90002@data$UA16==1), subset(geodata00102@data, geodata00102@data$UA16==1))
geodataUA172 <- rbind(subset(geodata90002@data, geodata90002@data$UA17==1), subset(geodata00102@data, geodata00102@data$UA17==1))
geodataUA182 <- rbind(subset(geodata90002@data, geodata90002@data$UA18==1), subset(geodata00102@data, geodata00102@data$UA18==1))
geodataUA192 <- rbind(subset(geodata90002@data, geodata90002@data$UA19==1), subset(geodata00102@data, geodata00102@data$UA19==1))
geodataUA202 <- rbind(subset(geodata90002@data, geodata90002@data$UA20==1), subset(geodata00102@data, geodata00102@data$UA20==1))



# 3.6. run quantile regression models at 19 quantile points (from 5th to 95th with the 5%tile interval) by UA 

form1_2de <- Migr_young ~  
  f_ccity9000b + f_ccity0010b + f_pden9000b + f_pden0010b + f_pt9000b + f_pt0010b + lncden9000b + lncden0010b + 
  lnpop100 + pctyoungbg + pctnhw + pctforeign + pctmulti + elem + yr0010 

#form2_2de <- Migr_young ~ 
#  f_ccity9000b + f_ccity0010b + f_pden9000b + f_pden0010b +             f_pt0010b + lncden0010b + lncden0010b +
#  lnpop100 + pctyoungbg + pctnhw + pctforeign + pctmulti + elem + yr0010  
#form3_2de <- Migr_young ~  
#  f_ccity9000b + f_ccity0010b + f_pden9000b + f_pden0010b +                         lncden0010b + lncden0010b +
#  lnpop100 + pctyoungbg + pctnhw + pctforeign + pctmulti + elem + yr0010   

myfunction <- function(x, y, z="sig"){
  temp <- as.data.frame(x)
  temp$sig <- ifelse(temp[, 4] <= 0.01, "***", 
                     ifelse(temp[, 4] <= 0.05, "**", 
                            ifelse(temp[, 4] <= 0.1, "*", "")))
  temp2 <- temp[, c(1,7)]
  colnames(temp2)<- c(y, z)
  temp2 
}

mysummary1 <- function(city, inputwmat, inputdata) {
  q05 <- myfunction(qregspiv(form1_2de,  wmat=inputwmat, tau=0.05, data=inputdata, silent=TRUE), paste0(city, "05"), "sig05")
  q10 <- myfunction(qregspiv(form1_2de,  wmat=inputwmat, tau=0.10, data=inputdata, silent=TRUE), paste0(city, "10"), "sig10")
  q15 <- myfunction(qregspiv(form1_2de,  wmat=inputwmat, tau=0.15, data=inputdata, silent=TRUE), paste0(city, "15"), "sig15")
  q20 <- myfunction(qregspiv(form1_2de,  wmat=inputwmat, tau=0.20, data=inputdata, silent=TRUE), paste0(city, "20"), "sig20")
  q25 <- myfunction(qregspiv(form1_2de,  wmat=inputwmat, tau=0.25, data=inputdata, silent=TRUE), paste0(city, "25"), "sig25")
  q30 <- myfunction(qregspiv(form1_2de,  wmat=inputwmat, tau=0.30, data=inputdata, silent=TRUE), paste0(city, "30"), "sig30")
  q35 <- myfunction(qregspiv(form1_2de,  wmat=inputwmat, tau=0.35, data=inputdata, silent=TRUE), paste0(city, "35"), "sig35")
  q40 <- myfunction(qregspiv(form1_2de,  wmat=inputwmat, tau=0.40, data=inputdata, silent=TRUE), paste0(city, "40"), "sig40")
  q45 <- myfunction(qregspiv(form1_2de,  wmat=inputwmat, tau=0.45, data=inputdata, silent=TRUE), paste0(city, "45"), "sig45")
  q50 <- myfunction(qregspiv(form1_2de,  wmat=inputwmat, tau=0.50, data=inputdata, silent=TRUE), paste0(city, "50"), "sig50")
  q55 <- myfunction(qregspiv(form1_2de,  wmat=inputwmat, tau=0.55, data=inputdata, silent=TRUE), paste0(city, "55"), "sig55")
  q60 <- myfunction(qregspiv(form1_2de,  wmat=inputwmat, tau=0.60, data=inputdata, silent=TRUE), paste0(city, "60"), "sig60")
  q65 <- myfunction(qregspiv(form1_2de,  wmat=inputwmat, tau=0.65, data=inputdata, silent=TRUE), paste0(city, "65"), "sig65")
  q70 <- myfunction(qregspiv(form1_2de,  wmat=inputwmat, tau=0.70, data=inputdata, silent=TRUE), paste0(city, "70"), "sig70")
  q75 <- myfunction(qregspiv(form1_2de,  wmat=inputwmat, tau=0.75, data=inputdata, silent=TRUE), paste0(city, "75"), "sig75")
  q80 <- myfunction(qregspiv(form1_2de,  wmat=inputwmat, tau=0.80, data=inputdata, silent=TRUE), paste0(city, "80"), "sig80")
  q85 <- myfunction(qregspiv(form1_2de,  wmat=inputwmat, tau=0.85, data=inputdata, silent=TRUE), paste0(city, "85"), "sig85")
  q90 <- myfunction(qregspiv(form1_2de,  wmat=inputwmat, tau=0.90, data=inputdata, silent=TRUE), paste0(city, "90"), "sig90")
  q95 <- myfunction(qregspiv(form1_2de,  wmat=inputwmat, tau=0.95, data=inputdata, silent=TRUE), paste0(city, "95"), "sig95")
  
  q05$rn <- rownames(q05)
  q10$rn <- rownames(q10)
  q15$rn <- rownames(q15)
  q20$rn <- rownames(q20)
  q25$rn <- rownames(q25)
  q30$rn <- rownames(q30)
  q35$rn <- rownames(q35)
  q40$rn <- rownames(q40)
  q45$rn <- rownames(q45)
  q50$rn <- rownames(q50)
  q55$rn <- rownames(q55)
  q60$rn <- rownames(q60)
  q65$rn <- rownames(q65)
  q70$rn <- rownames(q70)
  q75$rn <- rownames(q75)
  q80$rn <- rownames(q80)
  q85$rn <- rownames(q85)
  q90$rn <- rownames(q90)
  q95$rn <- rownames(q95)
  df <- join_all(list(q05, q10, q15, q20, q25, q30, q35, q40, q45, q50, 
                      q55, q60, q65, q70, q75, q80, q85, q90, q95)  , by='rn', type='full') 
  #df$rn <- NULL
  df
}

mysummary2 <- function(city, inputwmat, inputdata) {
  q05 <- myfunction(qregspiv(form2_2de,  wmat=inputwmat, tau=0.05, data=inputdata, silent=TRUE), paste0(city, "05"), "sig05")
  q10 <- myfunction(qregspiv(form2_2de,  wmat=inputwmat, tau=0.10, data=inputdata, silent=TRUE), paste0(city, "10"), "sig10")
  q15 <- myfunction(qregspiv(form2_2de,  wmat=inputwmat, tau=0.15, data=inputdata, silent=TRUE), paste0(city, "15"), "sig15")
  q20 <- myfunction(qregspiv(form2_2de,  wmat=inputwmat, tau=0.20, data=inputdata, silent=TRUE), paste0(city, "20"), "sig20")
  q25 <- myfunction(qregspiv(form2_2de,  wmat=inputwmat, tau=0.25, data=inputdata, silent=TRUE), paste0(city, "25"), "sig25")
  q30 <- myfunction(qregspiv(form2_2de,  wmat=inputwmat, tau=0.30, data=inputdata, silent=TRUE), paste0(city, "30"), "sig30")
  q35 <- myfunction(qregspiv(form2_2de,  wmat=inputwmat, tau=0.35, data=inputdata, silent=TRUE), paste0(city, "35"), "sig35")
  q40 <- myfunction(qregspiv(form2_2de,  wmat=inputwmat, tau=0.40, data=inputdata, silent=TRUE), paste0(city, "40"), "sig40")
  q45 <- myfunction(qregspiv(form2_2de,  wmat=inputwmat, tau=0.45, data=inputdata, silent=TRUE), paste0(city, "45"), "sig45")
  q50 <- myfunction(qregspiv(form2_2de,  wmat=inputwmat, tau=0.50, data=inputdata, silent=TRUE), paste0(city, "50"), "sig50")
  q55 <- myfunction(qregspiv(form2_2de,  wmat=inputwmat, tau=0.55, data=inputdata, silent=TRUE), paste0(city, "55"), "sig55")
  q60 <- myfunction(qregspiv(form2_2de,  wmat=inputwmat, tau=0.60, data=inputdata, silent=TRUE), paste0(city, "60"), "sig60")
  q65 <- myfunction(qregspiv(form2_2de,  wmat=inputwmat, tau=0.65, data=inputdata, silent=TRUE), paste0(city, "65"), "sig65")
  q70 <- myfunction(qregspiv(form2_2de,  wmat=inputwmat, tau=0.70, data=inputdata, silent=TRUE), paste0(city, "70"), "sig70")
  q75 <- myfunction(qregspiv(form2_2de,  wmat=inputwmat, tau=0.75, data=inputdata, silent=TRUE), paste0(city, "75"), "sig75")
  q80 <- myfunction(qregspiv(form2_2de,  wmat=inputwmat, tau=0.80, data=inputdata, silent=TRUE), paste0(city, "80"), "sig80")
  q85 <- myfunction(qregspiv(form2_2de,  wmat=inputwmat, tau=0.85, data=inputdata, silent=TRUE), paste0(city, "85"), "sig85")
  q90 <- myfunction(qregspiv(form2_2de,  wmat=inputwmat, tau=0.90, data=inputdata, silent=TRUE), paste0(city, "90"), "sig90")
  q95 <- myfunction(qregspiv(form2_2de,  wmat=inputwmat, tau=0.95, data=inputdata, silent=TRUE), paste0(city, "95"), "sig95")
  
  q05$rn <- rownames(q05)
  q10$rn <- rownames(q10)
  q15$rn <- rownames(q15)
  q20$rn <- rownames(q20)
  q25$rn <- rownames(q25)
  q30$rn <- rownames(q30)
  q35$rn <- rownames(q35)
  q40$rn <- rownames(q40)
  q45$rn <- rownames(q45)
  q50$rn <- rownames(q50)
  q55$rn <- rownames(q55)
  q60$rn <- rownames(q60)
  q65$rn <- rownames(q65)
  q70$rn <- rownames(q70)
  q75$rn <- rownames(q75)
  q80$rn <- rownames(q80)
  q85$rn <- rownames(q85)
  q90$rn <- rownames(q90)
  q95$rn <- rownames(q95)
  df <- join_all(list(q05, q10, q15, q20, q25, q30, q35, q40, q45, q50, 
                      q55, q60, q65, q70, q75, q80, q85, q90, q95)  , by='rn', type='full') 
  #df$rn <- NULL
  df
}

mysummary3 <- function(city, inputwmat, inputdata) {
  q05 <- myfunction(qregspiv(form3_2de,  wmat=inputwmat, tau=0.05, data=inputdata, silent=TRUE), paste0(city, "05"), "sig05")
  q10 <- myfunction(qregspiv(form3_2de,  wmat=inputwmat, tau=0.10, data=inputdata, silent=TRUE), paste0(city, "10"), "sig10")
  q15 <- myfunction(qregspiv(form3_2de,  wmat=inputwmat, tau=0.15, data=inputdata, silent=TRUE), paste0(city, "15"), "sig15")
  q20 <- myfunction(qregspiv(form3_2de,  wmat=inputwmat, tau=0.20, data=inputdata, silent=TRUE), paste0(city, "20"), "sig20")
  q25 <- myfunction(qregspiv(form3_2de,  wmat=inputwmat, tau=0.25, data=inputdata, silent=TRUE), paste0(city, "25"), "sig25")
  q30 <- myfunction(qregspiv(form3_2de,  wmat=inputwmat, tau=0.30, data=inputdata, silent=TRUE), paste0(city, "30"), "sig30")
  q35 <- myfunction(qregspiv(form3_2de,  wmat=inputwmat, tau=0.35, data=inputdata, silent=TRUE), paste0(city, "35"), "sig35")
  q40 <- myfunction(qregspiv(form3_2de,  wmat=inputwmat, tau=0.40, data=inputdata, silent=TRUE), paste0(city, "40"), "sig40")
  q45 <- myfunction(qregspiv(form3_2de,  wmat=inputwmat, tau=0.45, data=inputdata, silent=TRUE), paste0(city, "45"), "sig45")
  q50 <- myfunction(qregspiv(form3_2de,  wmat=inputwmat, tau=0.50, data=inputdata, silent=TRUE), paste0(city, "50"), "sig50")
  q55 <- myfunction(qregspiv(form3_2de,  wmat=inputwmat, tau=0.55, data=inputdata, silent=TRUE), paste0(city, "55"), "sig55")
  q60 <- myfunction(qregspiv(form3_2de,  wmat=inputwmat, tau=0.60, data=inputdata, silent=TRUE), paste0(city, "60"), "sig60")
  q65 <- myfunction(qregspiv(form3_2de,  wmat=inputwmat, tau=0.65, data=inputdata, silent=TRUE), paste0(city, "65"), "sig65")
  q70 <- myfunction(qregspiv(form3_2de,  wmat=inputwmat, tau=0.70, data=inputdata, silent=TRUE), paste0(city, "70"), "sig70")
  q75 <- myfunction(qregspiv(form3_2de,  wmat=inputwmat, tau=0.75, data=inputdata, silent=TRUE), paste0(city, "75"), "sig75")
  q80 <- myfunction(qregspiv(form3_2de,  wmat=inputwmat, tau=0.80, data=inputdata, silent=TRUE), paste0(city, "80"), "sig80")
  q85 <- myfunction(qregspiv(form3_2de,  wmat=inputwmat, tau=0.85, data=inputdata, silent=TRUE), paste0(city, "85"), "sig85")
  q90 <- myfunction(qregspiv(form3_2de,  wmat=inputwmat, tau=0.90, data=inputdata, silent=TRUE), paste0(city, "90"), "sig90")
  q95 <- myfunction(qregspiv(form3_2de,  wmat=inputwmat, tau=0.95, data=inputdata, silent=TRUE), paste0(city, "95"), "sig95")
  
  q05$rn <- rownames(q05)
  q10$rn <- rownames(q10)
  q15$rn <- rownames(q15)
  q20$rn <- rownames(q20)
  q25$rn <- rownames(q25)
  q30$rn <- rownames(q30)
  q35$rn <- rownames(q35)
  q40$rn <- rownames(q40)
  q45$rn <- rownames(q45)
  q50$rn <- rownames(q50)
  q55$rn <- rownames(q55)
  q60$rn <- rownames(q60)
  q65$rn <- rownames(q65)
  q70$rn <- rownames(q70)
  q75$rn <- rownames(q75)
  q80$rn <- rownames(q80)
  q85$rn <- rownames(q85)
  q90$rn <- rownames(q90)
  q95$rn <- rownames(q95)
  df <- join_all(list(q05, q10, q15, q20, q25, q30, q35, q40, q45, q50, 
                      q55, q60, q65, q70, q75, q80, q85, q90, q95)  , by='rn', type='full') 
  #df$rn <- NULL
  df
}

UA01summary <- mysummary1("Atlanta",   wmat012, geodataUA012)
UA02summary <- mysummary1("Baltimore", wmat022, geodataUA022)
UA03summary <- mysummary1("Boston", wmat032, geodataUA032)
UA04summary <- mysummary1("Chicago", wmat042, geodataUA042)
UA05summary <- mysummary1("Cleveland", wmat052, geodataUA052)
UA06summary <- mysummary1("Dallas", wmat062, geodataUA062)
UA07summary <- mysummary1("Detroit", wmat072, geodataUA072)
UA08summary <- mysummary1("Houston", wmat082, geodataUA082)
UA09summary <- mysummary1("LA", wmat092, geodataUA092)
UA10summary <- mysummary1("Miami", wmat102, geodataUA102)

UA11summary <- mysummary1("Minneapolis",   wmat112, geodataUA112)
UA12summary <- mysummary1("New York", wmat122, geodataUA122)
UA13summary <- mysummary1("Philadelphia", wmat132, geodataUA132)
UA14summary <- mysummary1("Phoenix", wmat142, geodataUA142)
UA15summary <- mysummary1("St Louis", wmat152, geodataUA152)
UA16summary <- mysummary1("San Diego", wmat162, geodataUA162)
UA17summary <- mysummary1("San Francisco", wmat172, geodataUA172)
UA18summary <- mysummary1("Seattle", wmat182, geodataUA182)
UA19summary <- mysummary1("Tampa", wmat192, geodataUA192)
UA20summary <- mysummary1("Washington DC", wmat202, geodataUA202)

#colnames(UA01summary)

mycleaning <- function(UAsummary, UAname) {
  temp01 <- UAsummary[2:9, c(1, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30, 32, 34, 36, 38)]
  temp01[9, ] <- c(5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95)
  temp01 <- as.data.frame(t(temp01))
  temp01[, 10] <- UAname 
  colnames(temp01) <- c(UA01summary[2:9, 3], "quantile", "UAname")
  temp01 
}

UA01cleansum <- mycleaning(UA01summary, "Atlanta")
UA02cleansum <- mycleaning(UA02summary, "Baltimore")
UA03cleansum <- mycleaning(UA03summary, "Boston")
UA04cleansum <- mycleaning(UA04summary, "Chicago")
UA05cleansum <- mycleaning(UA05summary, "Cleveland")
UA06cleansum <- mycleaning(UA06summary, "Dallas")
UA07cleansum <- mycleaning(UA07summary, "Detroit")
UA08cleansum <- mycleaning(UA08summary, "Houston")
UA09cleansum <- mycleaning(UA09summary, "Los Angeles")
UA10cleansum <- mycleaning(UA10summary, "Miami")

UA11cleansum <- mycleaning(UA11summary, "Minneapolis")
UA12cleansum <- mycleaning(UA12summary, "New York")
UA13cleansum <- mycleaning(UA13summary, "Philadelphia")
UA14cleansum <- mycleaning(UA14summary, "Phoenix")
UA15cleansum <- mycleaning(UA15summary, "St. Louis")
UA16cleansum <- mycleaning(UA16summary, "San Diego")
UA17cleansum <- mycleaning(UA17summary, "San Francisco")
UA18cleansum <- mycleaning(UA18summary, "Seattle")
UA19cleansum <- mycleaning(UA19summary, "Tampa")
UA20cleansum <- mycleaning(UA20summary, "Washington")


# 3.6.2. run quantile regression models at three quantile points (25, 50, &, 75) by UA 

mysummary11 <- function(inputwmat, inputdata, inputcity) {
  df_q25 <- myfunction(qregspiv(form1_2de,  wmat=inputwmat, tau=0.25, data=inputdata, silent=TRUE), "25th percentile", "sig25")
  df_q50 <- myfunction(qregspiv(form1_2de,  wmat=inputwmat, tau=0.50, data=inputdata, silent=TRUE), "50th percentile", "sig50")
  df_q75 <- myfunction(qregspiv(form1_2de,  wmat=inputwmat, tau=0.75, data=inputdata, silent=TRUE), "75th percentile", "sig75")
  df_q25$rn <- rownames(df)
  df_q50$rn <- rownames(df)
  df_q75$rn <- rownames(df)
  df <- join_all(list(df_q25, df_q50, df_q75)  , by='rn', type='full') 
  rownames(df) <- df$rn 
  #df$rn <- NULL
  df$city <- inputcity
  df
}

qregspivUA01 <- mysummary11(wmat012, geodataUA012, "Atlanta")
qregspivUA02 <- mysummary11(wmat022, geodataUA022, "Baltimore")
qregspivUA03 <- mysummary11(wmat032, geodataUA032, "Boston")
qregspivUA04 <- mysummary11(wmat042, geodataUA042, "Chicago")
qregspivUA05 <- mysummary11(wmat052, geodataUA052, "Cleveland")
qregspivUA06 <- mysummary11(wmat062, geodataUA062, "Dallas")
qregspivUA07 <- mysummary11(wmat072, geodataUA072, "Detriot")
qregspivUA08 <- mysummary11(wmat082, geodataUA082, "Houston")
qregspivUA09 <- mysummary11(wmat092, geodataUA092, "LA")
qregspivUA10 <- mysummary11(wmat102, geodataUA102, "Miami")

qregspivUA11 <- mysummary11(wmat112, geodataUA112, "Minneapolis")
qregspivUA12 <- mysummary11(wmat122, geodataUA122, "New York")
qregspivUA13 <- mysummary11(wmat132, geodataUA132, "Philadelphia")
qregspivUA14 <- mysummary11(wmat142, geodataUA142, "Phoenix")
qregspivUA15 <- mysummary11(wmat152, geodataUA152, "St Louis")
qregspivUA16 <- mysummary11(wmat162, geodataUA162, "San Diego")
qregspivUA17 <- mysummary11(wmat172, geodataUA172, "San Francisco")
qregspivUA18 <- mysummary11(wmat182, geodataUA182, "Seattle")
qregspivUA19 <- mysummary11(wmat192, geodataUA192, "Tampa")
qregspivUA20 <- mysummary11(wmat202, geodataUA202, "Washington DC")

df <- rbind(qregspivUA01, qregspivUA02, qregspivUA03, qregspivUA04, qregspivUA05, 
            qregspivUA06, qregspivUA07, qregspivUA08, qregspivUA09, qregspivUA10, 
            qregspivUA11, qregspivUA12, qregspivUA13, qregspivUA14, qregspivUA15, 
            qregspivUA16, qregspivUA17, qregspivUA18, qregspivUA19, qregspivUA20)

write.csv(df, file="./key_output/by_UA/qregspiv_young_2decades_byUA.csv")

df[df$rn == "f_ccity0010b", ]
df[df$rn == "f_pden0010b", ]
df[df$rn == "f_pt0010b", ]
df[df$rn == "lncden0010b", ]



# 3.7. generate charts for each UA 

# 3.7.1. for youngest <- running *only* for analyses on the youngest 

All20sum1youngest <- rbind(UA12cleansum, UA13cleansum, UA02cleansum, UA20cleansum, UA03cleansum)
All20sum1youngest$UAname <- as.factor(All20sum1youngest$UAname)

All20sum2youngest <- rbind(UA11cleansum, UA15cleansum, UA04cleansum, UA05cleansum, UA07cleansum)
All20sum2youngest$UAname <- as.factor(All20sum2youngest$UAname)

All20sum3youngest <- rbind(UA01cleansum, UA10cleansum, UA19cleansum, UA06cleansum, UA08cleansum)
All20sum3youngest$UAname <- as.factor(All20sum3youngest$UAname)

All20sum4youngest <- rbind(UA14cleansum, UA16cleansum, UA17cleansum, UA18cleansum, UA09cleansum)
All20sum4youngest$UAname <- as.factor(All20sum4youngest$UAname)

# export qregspiv results by UA for four regions 

write.csv(All20sum1youngest, file="./key_output/qregspiv_youngest_2decades_byUA_Northeast.csv")
write.csv(All20sum2youngest, file="./key_output/qregspiv_youngest_2decades_byUA_Midwest.csv")
write.csv(All20sum3youngest, file="./key_output/qregspiv_youngest_2decades_byUA_South.csv")
write.csv(All20sum4youngest, file="./key_output/qregspiv_youngest_2decades_byUA_West.csv")

# import qregspiv results by UA for four regions 

All20sum1youngest <- read.csv(file = './key_output/qregspiv_youngest_2decades_byUA_Northeast.csv')
All20sum2youngest <- read.csv(file = './key_output/qregspiv_youngest_2decades_byUA_Midwest.csv')
All20sum3youngest <- read.csv(file = './key_output/qregspiv_youngest_2decades_byUA_South.csv')
All20sum4youngest <- read.csv(file = './key_output/qregspiv_youngest_2decades_byUA_West.csv')

# draw charts 

mychart <- function(dataset, var1990s, var2000s, var1, cohort, var2, region){
  ggplot(data=dataset, aes(x=var1990s, y= var2000s, color=UAname)) + 
    #coord_cartesian(xlim=c(-200, 200), ylim=c(-200, 200)) + 
    #scale_x_continuous(breaks = seq(0, 100, by = 10)) + 
    #scale_y_continuous(breaks = seq(-1400, 600, by = 200)) + 
    geom_abline(colour="black", size=0.25) + 
    geom_hline(yintercept=0, colour="grey") + 
    geom_vline(xintercept=0, colour="grey") + 
    geom_point(size=2) + 
    geom_smooth(se=FALSE, size=0.5) + 
    labs(x=paste0(var1, " in the 1990s"), y=paste0(var1, " in the 2000s"), size=4) + 
    #ggtitle("  Net migration of 25-34 cohort in the top 20 Urban Areas") +
    theme(plot.title = element_text(size=14, hjust=0))
  
  ggsave(paste0("M:\\Millennial_panel\\23_chart_figure_2decades\\qregspiv_", cohort, var2, "_20UA", region, ".png"), 
         width = 8.5, height = 8, units = 'in', dpi=300)
}
# this mychart function does not specify xy bounds 

mychart(All20sum1youngest, All20sum1youngest$lncden9000b, All20sum1youngest$lncden0010b, "Consumption amenities", "2024_", "amenity", "_Northest")
mychart(All20sum2youngest, All20sum2youngest$lncden9000b, All20sum2youngest$lncden0010b, "Consumption amenities", "2024_", "amenity", "_Midwest")
mychart(All20sum3youngest, All20sum3youngest$lncden9000b, All20sum3youngest$lncden0010b, "Consumption amenities", "2024_", "amenity", "_South")
mychart(All20sum4youngest, All20sum4youngest$lncden9000b, All20sum4youngest$lncden0010b, "Consumption amenities", "2024_", "amenity", "_West")

# 3.7.2. for young <- running *only* for analyses on the young

All20sum1young <- rbind(UA12cleansum, UA13cleansum, UA02cleansum, UA20cleansum, UA03cleansum)
All20sum1young$UAname <- as.factor(All20sum1young$UAname)

All20sum2young <- rbind(UA11cleansum, UA15cleansum, UA04cleansum, UA05cleansum, UA07cleansum)
All20sum2young$UAname <- as.factor(All20sum2young$UAname)

All20sum3young <- rbind(UA01cleansum, UA10cleansum, UA19cleansum, UA06cleansum, UA08cleansum)
All20sum3young$UAname <- as.factor(All20sum3young$UAname)

All20sum4young <- rbind(UA14cleansum, UA16cleansum, UA17cleansum, UA18cleansum, UA09cleansum)
All20sum4young$UAname <- as.factor(All20sum4young$UAname)

# export qregspiv results by UA for four regions 

write.csv(All20sum1young, file="./key_output/qregspiv_young_2decades_byUA_Northeast.csv")
write.csv(All20sum2young, file="./key_output/qregspiv_young_2decades_byUA_Midwest.csv")
write.csv(All20sum3young, file="./key_output/qregspiv_young_2decades_byUA_South.csv")
write.csv(All20sum4young, file="./key_output/qregspiv_young_2decades_byUA_West.csv")

# import qregspiv results by UA for four regions 

All20sum1young <- read.csv(file = './key_output/qregspiv_young_2decades_byUA_Northeast.csv')
All20sum2young <- read.csv(file = './key_output/qregspiv_young_2decades_byUA_Midwest.csv')
All20sum3young <- read.csv(file = './key_output/qregspiv_young_2decades_byUA_South.csv')
All20sum4young <- read.csv(file = './key_output/qregspiv_young_2decades_byUA_West.csv')

mychart <- function(dataset, var1990s, var2000s, var1, cohort, var2, region){
  ggplot(data=dataset, aes(x=var1990s, y= var2000s, color=UAname)) + 
    coord_cartesian(xlim=c(-200, 200), ylim=c(-200, 200)) + 
    scale_x_continuous(breaks = seq(-200, 200, by = 50)) + 
    scale_y_continuous(breaks = seq(-200, 200, by = 50)) + 
    geom_abline(colour="black", size=0.25) + 
    geom_hline(yintercept=0, colour="grey") + 
    geom_vline(xintercept=0, colour="grey") + 
    geom_point(size=2) + 
    #geom_line() + 
    geom_smooth(se=FALSE, size=0.5) + 
    labs(x=paste0(var1, " in the 1990s"), y=paste0(var1, " in the 2000s"), size=4) + 
    #ggtitle("  Net migration of 25-34 cohort in the top 20 Urban Areas") +
    theme(plot.title = element_text(size=14, hjust=0))
  
  ggsave(paste0("M:\\Millennial_panel\\23_chart_figure_2decades\\qregspiv_", cohort, var2, "_20UA", region, ".png"), 
         width = 8.5, height = 8, units = 'in', dpi=300)
} 

mychart(All20sum1young, All20sum1young$lncden9000b, All20sum1young$lncden0010b, "Consumption amenities", "2534_", "amenity", "_Northest")
mychart(All20sum2young, All20sum2young$lncden9000b, All20sum2young$lncden0010b, "Consumption amenities", "2534_", "amenity", "_Midwest")
mychart(All20sum3young, All20sum3young$lncden9000b, All20sum3young$lncden0010b, "Consumption amenities", "2534_", "amenity", "_South")
mychart(All20sum4young, All20sum4young$lncden9000b, All20sum4young$lncden0010b, "Consumption amenities", "2534_", "amenity", "_West")

mychart(All20sum1young, All20sum1young$f_ccity9000b, All20sum1young$f_ccity0010b, "Central city", "2534_", "ccity", "_Northest")
mychart(All20sum2young, All20sum2young$f_ccity9000b, All20sum2young$f_ccity0010b, "Central city", "2534_", "ccity", "_Midwest")
mychart(All20sum3young, All20sum3young$f_ccity9000b, All20sum3young$f_ccity0010b, "Central city", "2534_", "ccity", "_South")
mychart(All20sum4young, All20sum4young$f_ccity9000b, All20sum4young$f_ccity0010b, "Central city", "2534_", "ccity", "_West")



# 3.8. run quantile regression by decade 

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

wmat90002 <- blockMatrixDiagonal(wmat01b2, wmat02b2, wmat03b2, wmat04b2, wmat05b2, wmat06b2, wmat07b2, wmat08b2, wmat09b2, wmat10b2, 
                                 wmat11b2, wmat12b2, wmat13b2, wmat14b2, wmat15b2, wmat16b2, wmat17b2, wmat18b2, wmat19b2, wmat20b2)

wmat00102 <- blockMatrixDiagonal(wmat01c2, wmat02c2, wmat03c2, wmat04c2, wmat05c2, wmat06c2, wmat07c2, wmat08c2, wmat09c2, wmat10c2, 
                                 wmat11c2, wmat12c2, wmat13c2, wmat14c2, wmat15c2, wmat16c2, wmat17c2, wmat18c2, wmat19c2, wmat20c2)

form90002 <- Migr_young ~  
  f_ccity9000b + f_pden9000b + f_pt9000b + lncden9000b +
  lnpop100 + pctyoungbg + pctnhw + pctforeign + pctmulti + elem +  
  UA01 + UA02 + UA03 + UA04 + UA05 + UA06 + UA07 + UA08 + UA09 + UA10 + 
  UA11 + UA12 + UA13 + UA14 + UA15 + UA16 + UA17 + UA18 + UA19

form00102 <- Migr_young ~  
  f_ccity0010b + f_pden0010b + f_pt0010b + lncden0010b +
  lnpop100 + pctyoungbg + pctnhw + pctforeign + pctmulti + elem +  
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

#q05b2 <- myfunction(qregspiv(form90002,  wmat=wmat90002, tau=0.05, data=geodata90002@data, silent=TRUE), "05", "sig05")
#q10b2 <- myfunction(qregspiv(form90002,  wmat=wmat90002, tau=0.10, data=geodata90002@data, silent=TRUE), "10", "sig10")
#q15b2 <- myfunction(qregspiv(form90002,  wmat=wmat90002, tau=0.15, data=geodata90002@data, silent=TRUE), "15", "sig15")
#q20b2 <- myfunction(qregspiv(form90002,  wmat=wmat90002, tau=0.20, data=geodata90002@data, silent=TRUE), "20", "sig20")
q25b2 <- myfunction(qregspiv(form90002,  wmat=wmat90002, tau=0.25, data=geodata90002@data, silent=TRUE), "25", "sig25")
#q30b2 <- myfunction(qregspiv(form90002,  wmat=wmat90002, tau=0.30, data=geodata90002@data, silent=TRUE), "30", "sig30")
#q35b2 <- myfunction(qregspiv(form90002,  wmat=wmat90002, tau=0.35, data=geodata90002@data, silent=TRUE), "35", "sig35")
#q40b2 <- myfunction(qregspiv(form90002,  wmat=wmat90002, tau=0.40, data=geodata90002@data, silent=TRUE), "40", "sig40")
#q45b2 <- myfunction(qregspiv(form90002,  wmat=wmat90002, tau=0.45, data=geodata90002@data, silent=TRUE), "45", "sig45")
q50b2 <- myfunction(qregspiv(form90002,  wmat=wmat90002, tau=0.50, data=geodata90002@data, silent=TRUE), "50", "sig50")
#q55b2 <- myfunction(qregspiv(form90002,  wmat=wmat90002, tau=0.55, data=geodata90002@data, silent=TRUE), "55", "sig55")
#q60b2 <- myfunction(qregspiv(form90002,  wmat=wmat90002, tau=0.60, data=geodata90002@data, silent=TRUE), "60", "sig60")
#q65b2 <- myfunction(qregspiv(form90002,  wmat=wmat90002, tau=0.65, data=geodata90002@data, silent=TRUE), "65", "sig65")
#q70b2 <- myfunction(qregspiv(form90002,  wmat=wmat90002, tau=0.70, data=geodata90002@data, silent=TRUE), "70", "sig70")
q75b2 <- myfunction(qregspiv(form90002,  wmat=wmat90002, tau=0.75, data=geodata90002@data, silent=TRUE), "75", "sig75")
#q80b2 <- myfunction(qregspiv(form90002,  wmat=wmat90002, tau=0.80, data=geodata90002@data, silent=TRUE), "80", "sig80")
#q85b2 <- myfunction(qregspiv(form90002,  wmat=wmat90002, tau=0.85, data=geodata90002@data, silent=TRUE), "85", "sig85")
#q90b2 <- myfunction(qregspiv(form90002,  wmat=wmat90002, tau=0.90, data=geodata90002@data, silent=TRUE), "90", "sig90")
#q95b2 <- myfunction(qregspiv(form90002,  wmat=wmat90002, tau=0.95, data=geodata90002@data, silent=TRUE), "95", "sig95")

#q05b2$rn <- rownames(q05b2)
#q10b2$rn <- rownames(q10b2)
#q15b2$rn <- rownames(q15b2)
#q20b2$rn <- rownames(q20b2)
q25b2$rn <- rownames(q25b2)
#q30b2$rn <- rownames(q30b2)
#q35b2$rn <- rownames(q35b2)
#q40b2$rn <- rownames(q40b2)
#q45b2$rn <- rownames(q45b2)
q50b2$rn <- rownames(q50b2)
#q55b2$rn <- rownames(q55b2)
#q60b2$rn <- rownames(q60b2)
#q65b2$rn <- rownames(q65b2)
#q70b2$rn <- rownames(q70b2)
q75b2$rn <- rownames(q75b2)
#q80b2$rn <- rownames(q80b2)
#q85b2$rn <- rownames(q85b2)
#q90b2$rn <- rownames(q90b2)
#q95b2$rn <- rownames(q95b2)

df <- join_all(list(#q05b2, q10b2, q15b2, q20b2, 
  q25b2, #q30b2, q35b2, q40b2, q45b2, 
  q50b2, #q55b2, q60b2, q65b2, q70b2, 
  q75b2), by='rn', type='full') #, q80b2, q85b2, q90b2, q95b2
row.names(df) <- df$rn
df$rn <- NULL
df <- as.data.frame(t(df))
#df90002 <- df[c(seq(1, 37, by=2)), 2:5]
df
df90002 <- df[, c(2:5)] # <- to generate regression tables 
#df90002$quantile <- row.names(df90002)
df90002 

#q05c2 <- myfunction(qregspiv(form00102,  wmat=wmat00102, tau=0.05, data=geodata00102@data, silent=TRUE), "05", "sig05")
#q10c2 <- myfunction(qregspiv(form00102,  wmat=wmat00102, tau=0.10, data=geodata00102@data, silent=TRUE), "10", "sig10")
#q15c2 <- myfunction(qregspiv(form00102,  wmat=wmat00102, tau=0.15, data=geodata00102@data, silent=TRUE), "15", "sig15")
#q20c2 <- myfunction(qregspiv(form00102,  wmat=wmat00102, tau=0.20, data=geodata00102@data, silent=TRUE), "20", "sig20")
q25c2 <- myfunction(qregspiv(form00102,  wmat=wmat00102, tau=0.25, data=geodata00102@data, silent=TRUE), "25", "sig25")
#q30c2 <- myfunction(qregspiv(form00102,  wmat=wmat00102, tau=0.30, data=geodata00102@data, silent=TRUE), "30", "sig30")
#q35c2 <- myfunction(qregspiv(form00102,  wmat=wmat00102, tau=0.35, data=geodata00102@data, silent=TRUE), "35", "sig35")
#q40c2 <- myfunction(qregspiv(form00102,  wmat=wmat00102, tau=0.40, data=geodata00102@data, silent=TRUE), "40", "sig40")
#q45c2 <- myfunction(qregspiv(form00102,  wmat=wmat00102, tau=0.45, data=geodata00102@data, silent=TRUE), "45", "sig45")
q50c2 <- myfunction(qregspiv(form00102,  wmat=wmat00102, tau=0.50, data=geodata00102@data, silent=TRUE), "50", "sig50")
#q55c2 <- myfunction(qregspiv(form00102,  wmat=wmat00102, tau=0.55, data=geodata00102@data, silent=TRUE), "55", "sig55")
#q60c2 <- myfunction(qregspiv(form00102,  wmat=wmat00102, tau=0.60, data=geodata00102@data, silent=TRUE), "60", "sig60")
#q65c2 <- myfunction(qregspiv(form00102,  wmat=wmat00102, tau=0.65, data=geodata00102@data, silent=TRUE), "65", "sig65")
#q70c2 <- myfunction(qregspiv(form00102,  wmat=wmat00102, tau=0.70, data=geodata00102@data, silent=TRUE), "70", "sig70")
q75c2 <- myfunction(qregspiv(form00102,  wmat=wmat00102, tau=0.75, data=geodata00102@data, silent=TRUE), "75", "sig75")
#q80c2 <- myfunction(qregspiv(form00102,  wmat=wmat00102, tau=0.80, data=geodata00102@data, silent=TRUE), "80", "sig80")
#q85c2 <- myfunction(qregspiv(form00102,  wmat=wmat00102, tau=0.85, data=geodata00102@data, silent=TRUE), "85", "sig85")
#q90c2 <- myfunction(qregspiv(form00102,  wmat=wmat00102, tau=0.90, data=geodata00102@data, silent=TRUE), "90", "sig90")
#q95c2 <- myfunction(qregspiv(form00102,  wmat=wmat00102, tau=0.95, data=geodata00102@data, silent=TRUE), "95", "sig95")

#q05c2$rn <- rownames(q05c2)
#q10c2$rn <- rownames(q10c2)
#q15c2$rn <- rownames(q15c2)
#q20c2$rn <- rownames(q20c2)
q25c2$rn <- rownames(q25c2)
#q30c2$rn <- rownames(q30c2)
#q35c2$rn <- rownames(q35c2)
#q40c2$rn <- rownames(q40c2)
#q45c2$rn <- rownames(q45c2)
q50c2$rn <- rownames(q50c2)
#q55c2$rn <- rownames(q55c2)
#q60c2$rn <- rownames(q60c2)
#q65c2$rn <- rownames(q65c2)
#q70c2$rn <- rownames(q70c2)
q75c2$rn <- rownames(q75c2)
#q80c2$rn <- rownames(q80c2)
#q85c2$rn <- rownames(q85c2)
#q90c2$rn <- rownames(q90c2)
#q95c2$rn <- rownames(q95c2)

df <- join_all(list(#q05c2, q10c2, q15c2, q20c2, 
  q25c2, #q30c2, q35c2, q40c2, q45c2, 
  q50c2, #q55c2, q60c2, q65c2, q70c2, 
  q75c2), by='rn', type='full') #, q80c2, q85c2, q90c2, q95c2

df <- join_all(list(q25c2, q50c2, q75c2), by ='rn', type='full')
row.names(df) <- df$rn
df$rn <- NULL
df <- as.data.frame(t(df))
#df00102 <- df[c(seq(1, 37, by=2)), 2:5]
df00102 <- df[, c(2:5)] # <- to generate regression tables 
#df00102$quantile <- row.names(df00102)
df00102 

temp01 <- join_all(list(df90002, df00102), by='quantile', type='full')
row.names(temp01) <- temp01$quantile 
temp01$quantile <- NULL 
temp01$quantile <- c(seq(5, 95, by=5))

indx <- sapply(temp01, is.factor)
temp01[indx] <- lapply(temp01[indx], function(x) as.numeric(as.character(x)))

test <- temp01
test[21, ] <- colnames(test)
row.names(test)[21]<- "header"
write.csv(test, file="./key_output/qregspiv_middle_2decades.csv")
temp01



# 3.9. generate charts 

temp02 <- read.csv(file = './key_output/qregspiv_young_2decades.csv')
temp02 <- temp02[-20, ] 
temp02 <- temp02[-20, ] 
temp02 <- temp02[, -1]
indx <- sapply(temp02, is.factor)
temp02[indx] <- lapply(temp02[indx], function(x) as.numeric(as.character(x)))

#https://stackoverflow.com/questions/10349206/add-legend-to-ggplot2-line-plot
ggplot(data=temp02, aes(x = quantile)) +
  scale_x_continuous(breaks = seq(0, 100, by = 10)) + 
  #scale_y_continuous(breaks = seq(-400, 100, by = 100)) + 
  coord_cartesian(ylim=c(-50, 150))+ 
  #scale_y_continuous(breaks = seq(-1400, 600, by = 200)) + 
  #coord_cartesian(ylim=c(-1400, 600))+ 
  geom_point(aes(y=f_ccity9000b, colour="1990s"), size = 1.5) +
  geom_smooth(aes(y=f_ccity9000b, colour="1990s"), size = 0.25, alpha=0.4, method="loess", se = FALSE) +
  geom_point(aes(y=f_ccity0010b, colour="2000s"), size = 1.5) + 
  geom_smooth(aes(y=f_ccity0010b, colour="2000s"), size = 0.25, alpha=0.4, method="loess", se = FALSE) + 
  scale_colour_manual("", breaks = c("2000s", "1990s"), 
                      values = c("green", "blue")) + 
  labs(x="Quantile points", y="Central city (factor)", size=4) + 
  #ggtitle("  Net migration of the 45-64 cohort") +
  theme(plot.title = element_text(size=14, hjust=0))

ggsave(paste0("M:\\Millennial_panel\\23_chart_figure_2decades\\qregspiv_f_ccity_", "25-34", ".png"), 
       width = 9.5, height = 8, units = 'in', dpi=300)

ggplot(data=temp02, aes(x = quantile)) +
  scale_x_continuous(breaks = seq(0, 100, by = 10)) + 
  #scale_y_continuous(breaks = seq(-400, 100, by = 100)) + 
  coord_cartesian(ylim=c(-400, 50))+ 
  #scale_y_continuous(breaks = seq(-1400, 600, by = 200)) + 
  #coord_cartesian(ylim=c(-1400, 600))+ 
  geom_point(aes(y=f_pden9000b, colour="1990s"), size = 1.5) +
  geom_smooth(aes(y=f_pden9000b, colour="1990s"), size = 0.25, alpha=0.4, method="loess", se = FALSE) +
  geom_point(aes(y=f_pden0010b, colour="2000s"), size = 1.5) + 
  geom_smooth(aes(y=f_pden0010b, colour="2000s"), size = 0.25, alpha=0.4, method="loess", se = FALSE) + 
  scale_colour_manual("", breaks = c("2000s", "1990s"), 
                      values = c("green", "blue")) + 
  labs(x="Quantile points", y="Population density (factor) ", size=4) + 
  #ggtitle("  Net migration of the 45-64 cohort") +
  theme(plot.title = element_text(size=14, hjust=0))

ggsave(paste0("M:\\Millennial_panel\\23_chart_figure_2decades\\qregspiv_f_pden_", "25-34", ".png"), 
       width = 9.5, height = 8, units = 'in', dpi=300)

ggplot(data=temp02, aes(x = quantile)) +
  scale_x_continuous(breaks = seq(0, 100, by = 10)) + 
  #scale_y_continuous(breaks = seq(-400, 100, by = 100)) + 
  coord_cartesian(ylim=c(0, 60))+ 
  #scale_y_continuous(breaks = seq(-1400, 600, by = 200)) + 
  #coord_cartesian(ylim=c(-1400, 600))+ 
  geom_point(aes(y=f_pt9000b, colour="1990s"), size = 1.5) +
  geom_smooth(aes(y=f_pt9000b, colour="1990s"), size = 0.25, alpha=0.4, method="loess", se = FALSE) +
  geom_point(aes(y=f_pt0010b, colour="2000s"), size = 1.5) + 
  geom_smooth(aes(y=f_pt0010b, colour="2000s"), size = 0.25, alpha=0.4, method="loess", se = FALSE) + 
  scale_colour_manual("", breaks = c("2000s", "1990s"), 
                      values = c("green", "blue")) + 
  labs(x="Quantile points", y="Proximity to public transit (factor)", size=4) + 
  #ggtitle("  Net migration of the 45-64 cohort") +
  theme(plot.title = element_text(size=14, hjust=0))

ggsave(paste0("M:\\Millennial_panel\\23_chart_figure_2decades\\qregspiv_f_pt_", "25-34", ".png"), 
       width = 9.5, height = 8, units = 'in', dpi=300)

ggplot(data=temp02, aes(x = quantile)) +
  scale_x_continuous(breaks = seq(0, 100, by = 10)) + 
  #scale_y_continuous(breaks = seq(-400, 100, by = 100)) + 
  #coord_cartesian(ylim=c(-400, 100))+ 
  #scale_y_continuous(breaks = seq(-1400, 600, by = 200)) + 
  #coord_cartesian(ylim=c(-1400, 600))+ 
  geom_point(aes(y=lncden9000b, colour="1990s"), size = 1.5) +
  geom_smooth(aes(y=lncden9000b, colour="1990s"), size = 0.25, alpha=0.4, method="loess", se = FALSE) +
  geom_point(aes(y=lncden0010b, colour="2000s"), size = 1.5) + 
  geom_smooth(aes(y=lncden0010b, colour="2000s"), size = 0.25, alpha=0.4, method="loess", se = FALSE) + 
  scale_colour_manual("", breaks = c("2000s", "1990s"), 
                      values = c("green", "blue")) + 
  labs(x="Quantile points", y="Consumption amenity (retail/service job density)", size=4) + 
  #ggtitle("  Net migration of the 45-64 cohort") +
  theme(plot.title = element_text(size=14, hjust=0))

ggsave(paste0("M:\\Millennial_panel\\23_chart_figure_2decades\\qregspiv_amen_", "25-34", ".png"), 
       width = 9.5, height = 8, units = 'in', dpi=300)




# 9. backup scripts

ggplot() + 
  geom_point(data=temp01, aes(x=quantile, y=lnpden8090), color="orangered4", size = 1.5) + 
  geom_smooth(data=temp01, aes(x=quantile, y=lnpden8090), color="orangered1", size = 0.25, alpha=0.4, method="loess", se = FALSE) + 
  geom_point(data=temp01, aes(x=quantile, y=lnpden9000), color="green4", size = 1.5) +
  geom_smooth(data=temp01, aes(x=quantile, y=lnpden9000), color="green1", size = 0.25, alpha=0.4, method="loess", se = FALSE) +
  geom_point(data=temp01, aes(x=quantile, y=lnpden0010), color="royalblue4", size = 1.5) + 
  geom_smooth(data=temp01, aes(x=quantile, y=lnpden0010), color="royalblue1", size = 0.25, alpha=0.4, method="loess", se = FALSE) 

ggplot() + 
  geom_point(data=temp01, aes(x=quantile, y=One_Transit8090), color="orangered4", size = 1.5) + 
  geom_smooth(data=temp01, aes(x=quantile, y=One_Transit8090), color="orangered1", size = 0.25, alpha=0.4, method="loess", se = FALSE) + 
  geom_point(data=temp01, aes(x=quantile, y=One_Transit9000), color="green4", size = 1.5) +
  geom_smooth(data=temp01, aes(x=quantile, y=One_Transit9000), color="green1", size = 0.25, alpha=0.4, method="loess", se = FALSE) +
  geom_point(data=temp01, aes(x=quantile, y=One_Transit0010), color="royalblue4", size = 1.5) + 
  geom_smooth(data=temp01, aes(x=quantile, y=One_Transit0010), color="royalblue1", size = 0.25, alpha=0.4, method="loess", se = FALSE) 

q10 <- qregspiv(form,  wmat=wmat02, tau=t, data=geodataUA02, silent=TRUE)
q10 <- qregspiv(form,  wmat=wmat03, tau=t, data=geodataUA03, silent=TRUE)
q10 <- qregspiv(form,  wmat=wmat04, tau=t, data=geodataUA04, silent=TRUE)
q10 <- qregspiv(form,  wmat=wmat05, tau=t, data=geodataUA05, silent=TRUE)

q10 <- qregspiv(form3, wmat=wmat06, tau=t, data=geodataUA06, silent=TRUE)
q10 <- qregspiv(form2, wmat=wmat07, tau=t, data=geodataUA07, silent=TRUE)
q10 <- qregspiv(form4, wmat=wmat08, tau=t, data=geodataUA08, silent=TRUE)
q10 <- qregspiv(form2, wmat=wmat09, tau=t, data=geodataUA09, silent=TRUE)
q10 <- qregspiv(form2, wmat=wmat10, tau=t, data=geodataUA10, silent=TRUE)

#qregspivUA11 <- qregspiv(form4, wmat=wmat11, tau=t, data=geodataUA11, silent=TRUE)
#qregspivUA12 <- qregspiv(form,  wmat=wmat12, tau=t, data=geodataUA12, silent=TRUE)
#qregspivUA13 <- qregspiv(form,  wmat=wmat13, tau=t, data=geodataUA13, silent=TRUE)
#qregspivUA14 <- qregspiv(form4, wmat=wmat14, tau=t, data=geodataUA14, silent=TRUE)
#qregspivUA15 <- qregspiv(form3, wmat=wmat15, tau=t, data=geodataUA15, silent=TRUE)

#qregspivUA16 <- qregspiv(form2, wmat=wmat16, tau=t, data=geodataUA16, silent=TRUE)
#qregspivUA17 <- qregspiv(form,  wmat=wmat17, tau=t, data=geodataUA17, silent=TRUE)
#qregspivUA18 <- qregspiv(form4, wmat=wmat18, tau=t, data=geodataUA18, silent=TRUE)
#qregspivUA19 <- qregspiv(form4, wmat=wmat19, tau=t, data=geodataUA19, silent=TRUE)
#qregspivUA20 <- qregspiv(form,  wmat=wmat20, tau=t, data=geodataUA20, silent=TRUE)

# 1980s 
form8090  <- Migr_young ~ CC8090 + lnpden8090 + One_Transit8090 + lnpop100 + pctyoungbg + pctnhw  + pctmulti + elem 
form8090b <- Migr_young ~ CC8090 + lnpden8090                   + lnpop100 + pctyoungbg + pctnhw  + pctmulti + elem 

qregspiv8090UA01 <- qregspiv(form8090,  wmat=wmat01a, tau=0.75, data=subset(geodata8090@data, geodata8090@data$UA01==1))
qregspiv8090UA02 <- qregspiv(form8090,  wmat=wmat02a, tau=0.75, data=subset(geodata8090@data, geodata8090@data$UA02==1))
qregspiv8090UA03 <- qregspiv(form8090,  wmat=wmat03a, tau=0.75, data=subset(geodata8090@data, geodata8090@data$UA03==1))
qregspiv8090UA04 <- qregspiv(form8090,  wmat=wmat04a, tau=0.75, data=subset(geodata8090@data, geodata8090@data$UA04==1))
qregspiv8090UA05 <- qregspiv(form8090,  wmat=wmat05a, tau=0.75, data=subset(geodata8090@data, geodata8090@data$UA05==1))

qregspiv8090UA06 <- qregspiv(form8090b,  wmat=wmat06a, tau=0.75, data=subset(geodata8090@data, geodata8090@data$UA06==1))
qregspiv8090UA07 <- qregspiv(form8090b,  wmat=wmat07a, tau=0.75, data=subset(geodata8090@data, geodata8090@data$UA07==1))
qregspiv8090UA08 <- qregspiv(form8090b,  wmat=wmat08a, tau=0.75, data=subset(geodata8090@data, geodata8090@data$UA08==1))
qregspiv8090UA09 <- qregspiv(form8090b,  wmat=wmat09a, tau=0.75, data=subset(geodata8090@data, geodata8090@data$UA09==1))
qregspiv8090UA10 <- qregspiv(form8090b,  wmat=wmat10a, tau=0.75, data=subset(geodata8090@data, geodata8090@data$UA10==1))

qregspiv8090UA11 <- qregspiv(form8090b,  wmat=wmat11a, tau=0.75, data=subset(geodata8090@data, geodata8090@data$UA11==1))
qregspiv8090UA12 <- qregspiv(form8090,  wmat=wmat12a, tau=0.75, data=subset(geodata8090@data, geodata8090@data$UA12==1))
qregspiv8090UA13 <- qregspiv(form8090,  wmat=wmat13a, tau=0.75, data=subset(geodata8090@data, geodata8090@data$UA13==1))
qregspiv8090UA14 <- qregspiv(form8090b,  wmat=wmat14a, tau=0.75, data=subset(geodata8090@data, geodata8090@data$UA14==1))
qregspiv8090UA15 <- qregspiv(form8090b,  wmat=wmat15a, tau=0.75, data=subset(geodata8090@data, geodata8090@data$UA15==1))

qregspiv8090UA16 <- qregspiv(form8090b,  wmat=wmat16a, tau=0.75, data=subset(geodata8090@data, geodata8090@data$UA16==1))
qregspiv8090UA17 <- qregspiv(form8090,  wmat=wmat17a, tau=0.75, data=subset(geodata8090@data, geodata8090@data$UA17==1))
qregspiv8090UA18 <- qregspiv(form8090b,  wmat=wmat18a, tau=0.75, data=subset(geodata8090@data, geodata8090@data$UA18==1))
qregspiv8090UA19 <- qregspiv(form8090b,  wmat=wmat19a, tau=0.75, data=subset(geodata8090@data, geodata8090@data$UA19==1))
qregspiv8090UA20 <- qregspiv(form8090,  wmat=wmat20a, tau=0.75, data=subset(geodata8090@data, geodata8090@data$UA20==1))

# 1990s 
form9000  <- Migr_young ~ CC9000 + lnpden9000 + One_Transit9000 + lnpop100 + pctyoungbg + pctnhw  + pctmulti + elem 
form9000b <- Migr_young ~ CC9000 + lnpden9000                   + lnpop100 + pctyoungbg + pctnhw  + pctmulti + elem 

qregspiv9000UA01 <- qregspiv(form9000,  wmat=wmat01b, tau=0.75, data=subset(geodata9000@data, geodata9000@data$UA01==1))
qregspiv9000UA02 <- qregspiv(form9000,  wmat=wmat02b, tau=0.75, data=subset(geodata9000@data, geodata9000@data$UA02==1))
qregspiv9000UA03 <- qregspiv(form9000,  wmat=wmat03b, tau=0.75, data=subset(geodata9000@data, geodata9000@data$UA03==1))
qregspiv9000UA04 <- qregspiv(form9000,  wmat=wmat04b, tau=0.75, data=subset(geodata9000@data, geodata9000@data$UA04==1))
qregspiv9000UA05 <- qregspiv(form9000,  wmat=wmat05b, tau=0.75, data=subset(geodata9000@data, geodata9000@data$UA05==1))

qregspiv9000UA06 <- qregspiv(form9000b,  wmat=wmat06b, tau=0.75, data=subset(geodata9000@data, geodata9000@data$UA06==1))
qregspiv9000UA07 <- qregspiv(form9000,  wmat=wmat07b, tau=0.75, data=subset(geodata9000@data, geodata9000@data$UA07==1))
qregspiv9000UA08 <- qregspiv(form9000b,  wmat=wmat08b, tau=0.75, data=subset(geodata9000@data, geodata9000@data$UA08==1))
qregspiv9000UA09 <- qregspiv(form9000,  wmat=wmat09b, tau=0.75, data=subset(geodata9000@data, geodata9000@data$UA09==1))
qregspiv9000UA10 <- qregspiv(form9000,  wmat=wmat10b, tau=0.75, data=subset(geodata9000@data, geodata9000@data$UA10==1))

qregspiv9000UA11 <- qregspiv(form9000b,  wmat=wmat11b, tau=0.75, data=subset(geodata9000@data, geodata9000@data$UA11==1))
qregspiv9000UA12 <- qregspiv(form9000,  wmat=wmat12b, tau=0.75, data=subset(geodata9000@data, geodata9000@data$UA12==1))
qregspiv9000UA13 <- qregspiv(form9000,  wmat=wmat13b, tau=0.75, data=subset(geodata9000@data, geodata9000@data$UA13==1))
qregspiv9000UA14 <- qregspiv(form9000b,  wmat=wmat14b, tau=0.75, data=subset(geodata9000@data, geodata9000@data$UA14==1))
qregspiv9000UA15 <- qregspiv(form9000b,  wmat=wmat15b, tau=0.75, data=subset(geodata9000@data, geodata9000@data$UA15==1))

qregspiv9000UA16 <- qregspiv(form9000,  wmat=wmat16b, tau=0.75, data=subset(geodata9000@data, geodata9000@data$UA16==1))
qregspiv9000UA17 <- qregspiv(form9000,  wmat=wmat17b, tau=0.75, data=subset(geodata9000@data, geodata9000@data$UA17==1))
qregspiv9000UA18 <- qregspiv(form9000b,  wmat=wmat18b, tau=0.75, data=subset(geodata9000@data, geodata9000@data$UA18==1))
qregspiv9000UA19 <- qregspiv(form9000b,  wmat=wmat19b, tau=0.75, data=subset(geodata9000@data, geodata9000@data$UA19==1))
qregspiv9000UA20 <- qregspiv(form9000,  wmat=wmat20b, tau=0.75, data=subset(geodata9000@data, geodata9000@data$UA20==1))

# 2000s 
form0010  <- Migr_young ~ CC0010 + lnpden0010 + One_Transit0010 + lnpop100 + pctyoungbg + pctnhw  + pctmulti + elem 
form0010b <- Migr_young ~ CC0010 + lnpden0010                   + lnpop100 + pctyoungbg + pctnhw  + pctmulti + elem 

qregspiv0010UA01 <- qregspiv(form0010,  wmat=wmat01c, tau=0.75, data=subset(geodata0010@data, geodata0010@data$UA01==1))
qregspiv0010UA02 <- qregspiv(form0010,  wmat=wmat02c, tau=0.75, data=subset(geodata0010@data, geodata0010@data$UA02==1))
qregspiv0010UA03 <- qregspiv(form0010,  wmat=wmat03c, tau=0.75, data=subset(geodata0010@data, geodata0010@data$UA03==1))
qregspiv0010UA04 <- qregspiv(form0010,  wmat=wmat04c, tau=0.75, data=subset(geodata0010@data, geodata0010@data$UA04==1))
qregspiv0010UA05 <- qregspiv(form0010,  wmat=wmat05c, tau=0.75, data=subset(geodata0010@data, geodata0010@data$UA05==1))

qregspiv0010UA06 <- qregspiv(form0010,  wmat=wmat06c, tau=0.75, data=subset(geodata0010@data, geodata0010@data$UA06==1))
qregspiv0010UA07 <- qregspiv(form0010,  wmat=wmat07c, tau=0.75, data=subset(geodata0010@data, geodata0010@data$UA07==1))
qregspiv0010UA08 <- qregspiv(form0010b,  wmat=wmat08c, tau=0.75, data=subset(geodata0010@data, geodata0010@data$UA08==1))
qregspiv0010UA09 <- qregspiv(form0010,  wmat=wmat09c, tau=0.75, data=subset(geodata0010@data, geodata0010@data$UA09==1))
qregspiv0010UA10 <- qregspiv(form0010,  wmat=wmat10c, tau=0.75, data=subset(geodata0010@data, geodata0010@data$UA10==1))

qregspiv0010UA11 <- qregspiv(form0010b,  wmat=wmat11c, tau=0.75, data=subset(geodata0010@data, geodata0010@data$UA11==1))
qregspiv0010UA12 <- qregspiv(form0010,  wmat=wmat12c, tau=0.75, data=subset(geodata0010@data, geodata0010@data$UA12==1))
qregspiv0010UA13 <- qregspiv(form0010,  wmat=wmat13c, tau=0.75, data=subset(geodata0010@data, geodata0010@data$UA13==1))
qregspiv0010UA14 <- qregspiv(form0010b,  wmat=wmat14c, tau=0.75, data=subset(geodata0010@data, geodata0010@data$UA14==1))
qregspiv0010UA15 <- qregspiv(form0010,  wmat=wmat15c, tau=0.75, data=subset(geodata0010@data, geodata0010@data$UA15==1))

qregspiv0010UA16 <- qregspiv(form0010,  wmat=wmat16c, tau=0.75, data=subset(geodata0010@data, geodata0010@data$UA16==1))
qregspiv0010UA17 <- qregspiv(form0010,  wmat=wmat17c, tau=0.75, data=subset(geodata0010@data, geodata0010@data$UA17==1))
qregspiv0010UA18 <- qregspiv(form0010b,  wmat=wmat18c, tau=0.75, data=subset(geodata0010@data, geodata0010@data$UA18==1))
qregspiv0010UA19 <- qregspiv(form0010b,  wmat=wmat19c, tau=0.75, data=subset(geodata0010@data, geodata0010@data$UA19==1))
qregspiv0010UA20 <- qregspiv(form0010,  wmat=wmat20c, tau=0.75, data=subset(geodata0010@data, geodata0010@data$UA20==1))
