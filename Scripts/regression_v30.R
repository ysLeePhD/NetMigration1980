
#http://www.econ.uiuc.edu/~lab/workshop/Spatial_in_R.html
install.packages("maptools", dependencies = TRUE)
install.packages("spdep", dependencies = TRUE)
install.packages("leaflet", dependencies = TRUE)
install.packages("RColorBrewer", dependencies = TRUE)
install.packages("McSpatial", dependencies = TRUE)
install.packages('spDataLarge',
                 repos='https://nowosad.github.io/drat/', type='source')

library(spdep)
library(spDataLarge)
library(maptools)
library(leaflet)
library(RColorBrewer)

#library(foreign)
library(sas7bdat)
library(rgdal)
library(McSpatial)

setwd("M:/Millennial_panel/09_NetMigration1980/Scrach")

#1. import the main dataset 

temp  <- read.sas7bdat("M:/Millennial_panel/05_Scratch/pool3.sas7bdat")
colnames(temp)
nrow(temp)



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
#head(migrdata)
#colnames(migrdata)


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
wmat20a <- makew(shpfile=subset(geodata8090, geodata8090@data$UA20==1), method="queen")$wmat # <- do not run

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
wmat20b <- makew(shpfile=subset(geodata9000, geodata9000@data$UA20==1), method="queen")$wmat # <- do not run

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
wmat20c <- makew(shpfile=subset(geodata0010, geodata0010@data$UA20==1), method="queen")$wmat # <- do not run



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


#2.4.1. Estimate OLS 

decade80s.ols <- lm(
  Migr_young ~  
  cc8090_5 + lnpden8090 +  One_Transit8090 + 
  lnpop100 + pctyoungbg + pctnhw + pctforeign + pctmulti + #pctcoll + lnrent + elem +  
  UA01 + UA02 + UA03 + UA04 + UA05 + UA06 + UA07 + UA08 + UA09 + UA10 + 
  UA11 + UA12 + UA13 + UA14 + UA15 + UA16 + UA17 + UA18 + UA19, 
  data=geodata8090@data)

summary(decade80s.ols)


#2.4.2. Build the spatial weight matrices & test the presence of spatial autocorrelation 

#http://r.789695.n4.nabble.com/Spatial-Ananlysis-zero-policy-TRUE-doesn-t-work-for-no-neighbour-regions-td4664367.html
set.ZeroPolicyOption(TRUE)
list.queen <- poly2nb(geodata8090, queen=TRUE)
W <- nb2listw(list.queen, style="W", zero.policy=TRUE)
print(W, zero.policy=TRUE)
plot(W, coordinates(geodata8090))

moran.lm <- lm.morantest(decade80s.ols, W, alternative = "two.sided")
print(moran.lm)

LM <- lm.LMtests(decade80s.ols, W, test="all")
print(LM)


#2.4.3. Estimate spatial lag models 

sar2sls.80s<-stsls(
  Migr_young ~  
    cc8090_5 + lnpden8090 +  One_Transit8090 + 
    lnpop100 + pctyoungbg + pctnhw + pctforeign + pctmulti + #pctcoll + lnrent + elem +  
    UA01 + UA02 + UA03 + UA04 + UA05 + UA06 + UA07 + UA08 + UA09 + UA10 + 
    UA11 + UA12 + UA13 + UA14 + UA15 + UA16 + UA17 + UA18 + UA19, 
  data=geodata8090@data, W)

summary(sar2sls.80s)

#geodata8090@data$decade80s.ols.res <- resid(decade80s.ols) #residuals ols
#geodata8090@data$sar2sls.80s.res   <- resid(sar2sls.80s) #residual sar

#spplot(geodata8090,"decade80s.ols.res", 
#       at=seq(min(geodata8090@data$decade80s.ols.res, na.rm=TRUE),
#              max(geodata8090@data$decade80s.ols.res, na.rm=TRUE),
#              length=12),
#       col.regions=rev(brewer.pal(11,"RdBu")))

#spplot(geodata8090,"sar2sls.80s.res",
#       at=seq(min(geodata8090@data$sar2sls.80s.res,na.rm=TRUE),
#              max(geodata8090@data$sar2sls.80s.res,na.rm=TRUE), 
#              length=12), 
#       col.regions=rev(brewer.pal(11,"RdBu")))


#2.4.3. Estimate spatial error models

fgls.80s<-GMerrorsar(
  Migr_young ~  
    cc8090_5 + lnpden8090 +  One_Transit8090 + 
    lnpop100 + pctyoungbg + pctnhw + pctforeign + pctmulti + #pctcoll + lnrent + elem +  
    UA01 + UA02 + UA03 + UA04 + UA05 + UA06 + UA07 + UA08 + UA09 + UA10 + 
    UA11 + UA12 + UA13 + UA14 + UA15 + UA16 + UA17 + UA18 + UA19, 
  data=geodata8090@data, W)

summary(fgls.80s)
