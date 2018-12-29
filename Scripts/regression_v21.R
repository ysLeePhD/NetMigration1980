 
#install.packages()
install.packages("PerformanceAnalytics")
install.packages("corrplot")
install.packages("Hmisc")

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
library(PerformanceAnalytics)
library(corrplot)
library(Hmisc)

setwd("M:/Millennial_panel/09_NetMigration1980/Scrach")

#1. import the main dataset 

temp  <- read.sas7bdat("M:/Millennial_panel/05_Scratch/pool3.sas7bdat")
colnames(temp)
nrow(temp)

varlistcorr <- c("cc8090_5", "lnpden8090", "One_Transit8090","lnpop100", "pctyoungbg",  
                 "pctnhw", "pctforeign", "pctcoll", "lnrent", "pctmulti")
my_data <- temp[, varlistcorr]
my_data <- my_data[complete.cases(my_data), ] 
round(cor(my_data), 2) 
my_corr <- cor(my_data)
corrplot(my_corr, type="upper", order="hclust",
         tl.col="black", tl.srt=45)
#rcorr(as.matrix(my_data))$P
#chart.Correlation(my_data, histogram=TRUE, pch=19)

#2. three-decade model 

#2.1. process the main dataset 

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
head(migrdata)
colnames(migrdata)


#2.2. process three shapefiles 

shp.YR8090 <- readOGR("M://Millennial_panel/15_GIS/CTUA", layer = "1980CTUA")
shp.YR8090@data <- shp.YR8090@data["StCntTr"]
shp.YR8090@data$StCntTrYr <- paste0(shp.YR8090@data$StCntTr, "yr8090") 
shp.YR8090 <- spChFIDs(shp.YR8090, shp.YR8090@data$StCntTrYr) # change the fid field 

shp.YR9000 <- readOGR("M://Millennial_panel/15_GIS/CTUA", layer = "1990CTUA")
shp.YR9000@data <- shp.YR9000@data["StCntTr"]
shp.YR9000@data$StCntTrYr <- paste0(shp.YR9000@data$StCntTr, "yr9000")
shp.YR9000 <- spChFIDs(shp.YR9000, shp.YR9000@data$StCntTrYr) # change the fid field 

shp.YR0010 <- readOGR("M://Millennial_panel/15_GIS/CTUA", layer = "2000CTUA")
shp.YR0010@data <- shp.YR0010@data["StCntTr"]
shp.YR0010@data$StCntTrYr <- paste0(shp.YR0010@data$StCntTr, "yr0010")
shp.YR0010 <- spChFIDs(shp.YR0010, shp.YR0010@data$StCntTrYr) # change the fid field 

head(shp.YR8090@data)
head(shp.YR9000@data)
head(shp.YR0010@data)



#2.3. generate contiguity matrices for each UA and each decade 

geodata8090 <- merge(shp.YR8090, subset(migrdata, yr8090==1), by="StCntTrYr")#, duplicateGeoms = TRUE) <- inner join
#head(geodata8090@data)
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
#head(geodata9000@data)
geodata9000 <- subset(geodata9000, is.na(geodata9000@data[, 8])==0)
geodata9000@data <- geodata9000@data[order(geodata9000@data$UA, geodata9000@data$StCntTrYr), ] # sorting 
#head(geodata9000@data)

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
#head(geodata0010@data)
geodata0010 <- subset(geodata0010, is.na(geodata0010@data[, 8])==0)
geodata0010@data <- geodata0010@data[order(geodata0010@data$UA, geodata0010@data$StCntTrYr), ] # sorting 
#head(geodata0010@data)

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



# 2.8. run quantile regression by decade  


# 2.8.0. build block-diagonal matrices & model specifications 

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

form8090 <- Migr_young ~  
  cc8090_5 + lnpden8090 +  One_Transit8090 + 
  lnpop100 + pctyoungbg + pctnhw + pctforeign + pctmulti + #pctcoll + lnrent + elem +  
  UA01 + UA02 + UA03 + UA04 + UA05 + UA06 + UA07 + UA08 + UA09 + UA10 + 
  UA11 + UA12 + UA13 + UA14 + UA15 + UA16 + UA17 + UA18 + UA19

form9000 <-  Migr_young ~   
  cc9000_5 + lnpden9000 + One_Transit9000 + 
  lnpop100 + pctyoungbg + pctnhw + pctforeign + pctmulti + #pctcoll + lnrent + elem +  
  UA01 + UA02 + UA03 + UA04 + UA05 + UA06 + UA07 + UA08 + UA09 + UA10 + 
  UA11 + UA12 + UA13 + UA14 + UA15 + UA16 + UA17 + UA18 + UA19

form0010 <-  Migr_young ~  
  cc0010_5 + lnpden0010 + One_Transit0010 + 
  lnpop100 + pctyoungbg + pctnhw + pctforeign + pctmulti + #pctcoll + lnrent + elem +  
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

q25 <- myfunction(qregspiv(form8090,  wmat=wmat8090, tau=0.25, data=geodata8090@data, silent=TRUE), "25", "sig25")
q50 <- myfunction(qregspiv(form8090,  wmat=wmat8090, tau=0.50, data=geodata8090@data, silent=TRUE), "50", "sig50")
q75 <- myfunction(qregspiv(form8090,  wmat=wmat8090, tau=0.75, data=geodata8090@data, silent=TRUE), "75", "sig75")

q25$rn <- rownames(q25)
q50$rn <- rownames(q50)
q75$rn <- rownames(q75)

#write.csv(q25, file="./key_output/by_decade/qregspiv_young_1980s_q25.csv")
#write.csv(q50, file="./key_output/by_decade/qregspiv_young_1980s_q50.csv")
#write.csv(q75, file="./key_output/by_decade/qregspiv_young_1980s_q75.csv")

df8090 <- join_all(list(#q05, q10, q15, q20, 
  q25, #q30, q35, q40, q45, 
  q50, #q55, q60, q65, q70, 
  q75), by='rn', type='full') #, q80, q85, q90, q95), by='rn', type='full') 
row.names(df8090) <- df8090$rn
df8090$rn <- NULL
df8090


# 2.8.2. the 1990s 

q25b <- myfunction(qregspiv(form9000,  wmat=wmat9000, tau=0.25, data=geodata9000@data, silent=TRUE), "25", "sig25")
q50b <- myfunction(qregspiv(form9000,  wmat=wmat9000, tau=0.50, data=geodata9000@data, silent=TRUE), "50", "sig50")
q75b <- myfunction(qregspiv(form9000,  wmat=wmat9000, tau=0.75, data=geodata9000@data, silent=TRUE), "75", "sig75")

q25b$rn <- rownames(q25b)
q50b$rn <- rownames(q50b)
q75b$rn <- rownames(q75b)

#write.csv(q25b, file="./key_output/by_decade/qregspiv_young_1990s_q25b.csv")
#write.csv(q50b, file="./key_output/by_decade/qregspiv_young_1990s_q50b.csv")
#write.csv(q75b, file="./key_output/by_decade/qregspiv_young_1990s_q75b.csv")

df9000 <- join_all(list(#q05b, q10b, q15b, q20b, 
  q25b, #q30b, q35b, q40b, q45b, 
  q50b, #q55b, q60b, q65b, q70b, 
  q75b), by='rn', type='full') #q80b, q85b, q90b, q95b), by='rn', type='full') 
row.names(df9000) <- df9000$rn
df9000$rn <- NULL
df9000

# 2.8.3. the 2000s 

q25c <- myfunction(qregspiv(form0010,  wmat=wmat0010, tau=0.25, data=geodata0010@data, silent=TRUE), "25", "sig25")
q50c <- myfunction(qregspiv(form0010,  wmat=wmat0010, tau=0.50, data=geodata0010@data, silent=TRUE), "50", "sig50")
q75c <- myfunction(qregspiv(form0010,  wmat=wmat0010, tau=0.75, data=geodata0010@data, silent=TRUE), "75", "sig75")

q25c$rn <- rownames(q25c)
q50c$rn <- rownames(q50c)
q75c$rn <- rownames(q75c)

#write.csv(q25c, file="./key_output/by_decade/qregspiv_young_2000s_q25c.csv")
#write.csv(q50c, file="./key_output/by_decade/qregspiv_young_2000s_q50c.csv")
#write.csv(q75c, file="./key_output/by_decade/qregspiv_young_2000s_q75c.csv")

df0010 <- join_all(list(#q05c, q10c, q15c, q20c, 
  q25c, #q30c, q35c, q40c, q45c, 
  q50c, #q55c, q60c, q65c, q70c, 
  q75c), by='rn', type='full')  #, q80c, q85c, q90c, q95c), by='rn', type='full') 
row.names(df0010) <- df0010$rn
df0010$rn <- NULL
df0010 


# 2.8.4. merge & export 


