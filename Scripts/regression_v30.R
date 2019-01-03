
#http://www.econ.uiuc.edu/~lab/workshop/Spatial_in_R.html
install.packages("maptools", dependencies = TRUE)
install.packages("spdep", dependencies = TRUE)
install.packages("leaflet", dependencies = TRUE)
install.packages("RColorBrewer", dependencies = TRUE)
install.packages("McSpatial", dependencies = TRUE)
install.packages('spDataLarge',
                 repos='https://nowosad.github.io/drat/', type='source')
install.packages("stargazer")

library(spdep)
library(spDataLarge)
library(maptools)
library(leaflet)
library(RColorBrewer)

#library(foreign)
library(sas7bdat)
library(rgdal)
library(McSpatial)
library(stargazer)

setwd("M:/Millennial_panel/09_NetMigration1980/Scrach")



#1. import the main dataset 

temp  <- read.sas7bdat("M:/Millennial_panel/05_Scratch/pool3.sas7bdat")
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


geodata9000 <- merge(shp.YR9000, subset(migrdata, yr9000==1), by="StCntTrYr")#, duplicateGeoms = TRUE)
#head(geodata9000@data)
geodata9000 <- subset(geodata9000, is.na(geodata9000@data[, 8])==0)
geodata9000@data <- geodata9000@data[order(geodata9000@data$UA, geodata9000@data$StCntTrYr), ] # sorting 
#head(geodata9000@data)


geodata0010 <- merge(shp.YR0010, subset(migrdata, yr0010==1), by="StCntTrYr")#, duplicateGeoms = TRUE)
#head(geodata0010@data)
geodata0010 <- subset(geodata0010, is.na(geodata0010@data[, 8])==0)
geodata0010@data <- geodata0010@data[order(geodata0010@data$UA, geodata0010@data$StCntTrYr), ] # sorting 
#head(geodata0010@data)



#2.4. Estimate spatial lag & error models for 80s.

list.queen.80s <- poly2nb(geodata8090, queen=TRUE)
W.80s <- nb2listw(list.queen.80s, style="W", zero.policy=TRUE)

formula.80s.youngest <- Migr_youngest ~  
  cc8090_5 + lnpden8090 +  One_Transit8090 + 
  lnpop100 + pctyoungestbg + pctnhw + pctforeign + pctmulti + #pctcoll + lnrent + elem +  
  UA01 + UA02 + UA03 + UA04 + UA05 + UA06 + UA07 + UA08 + UA09 + UA10 + 
  UA11 + UA12 + UA13 + UA14 + UA15 + UA16 + UA17 + UA18 + UA19
formula.80s.young <- Migr_young ~  
  cc8090_5 + lnpden8090 +  One_Transit8090 + 
  lnpop100 + pctyoungbg + pctnhw + pctforeign + pctmulti + #pctcoll + lnrent + elem +  
  UA01 + UA02 + UA03 + UA04 + UA05 + UA06 + UA07 + UA08 + UA09 + UA10 + 
  UA11 + UA12 + UA13 + UA14 + UA15 + UA16 + UA17 + UA18 + UA19
formula.80s.midlife <- Migr_midlife ~  
  cc8090_5 + lnpden8090 +  One_Transit8090 + 
  lnpop100 + pctmidlifebg + pctnhw + pctforeign + pctmulti + #pctcoll + lnrent + elem +  
  UA01 + UA02 + UA03 + UA04 + UA05 + UA06 + UA07 + UA08 + UA09 + UA10 + 
  UA11 + UA12 + UA13 + UA14 + UA15 + UA16 + UA17 + UA18 + UA19
formula.80s.middle <- Migr_middle ~  
  cc8090_5 + lnpden8090 +  One_Transit8090 + 
  lnpop100 + pctmiddlebg + pctnhw + pctforeign + pctmulti + #pctcoll + lnrent + elem +  
  UA01 + UA02 + UA03 + UA04 + UA05 + UA06 + UA07 + UA08 + UA09 + UA10 + 
  UA11 + UA12 + UA13 + UA14 + UA15 + UA16 + UA17 + UA18 + UA19
  
sar2sls.80s.youngest <- stsls(formula.80s.youngest,      data=geodata8090@data, W.80s)
sar2sls.80s.young    <- stsls(formula.80s.young,         data=geodata8090@data, W.80s)
sar2sls.80s.midlife  <- stsls(formula.80s.midlife,       data=geodata8090@data, W.80s)
sar2sls.80s.middle   <- stsls(formula.80s.middle,        data=geodata8090@data, W.80s)

fgls.80s.youngest    <- GMerrorsar(formula.80s.youngest, data=geodata8090@data, W.80s)
fgls.80s.young       <- GMerrorsar(formula.80s.young,    data=geodata8090@data, W.80s)
fgls.80s.midlife     <- GMerrorsar(formula.80s.midlife,  data=geodata8090@data, W.80s)
fgls.80s.middle      <- GMerrorsar(formula.80s.middle,   data=geodata8090@data, W.80s)

df.lag.80s <- data.frame(coef = double(), 
                     p.value= double())
df.lag.80s <- as.data.frame(summary(sar2sls.80s.youngest)$Coef)[, c(1, 4)]
row.names(df.lag.80s)[7] <- "pct.age.bg"
df.lag.80s[, 3:4] <- as.data.frame(summary(sar2sls.80s.young)$Coef)[, c(1, 4)]
df.lag.80s[, 5:6] <- as.data.frame(summary(sar2sls.80s.midlife)$Coef)[, c(1, 4)] 
df.lag.80s[, 7:8] <- as.data.frame(summary(sar2sls.80s.middle)$Coef)[, c(1, 4)] 
colnames(df.lag.80s) <- c("coef.youngest", "p.youngest",
                      "coef.young", "p.young",
                      "coef.midlife", "p.midlife",
                      "coef.middle", "p.middle")

#round(df.lag.80s, 3)

df.err.80s <- data.frame(coef = double(), 
                         p.value= double())
df.err.80s <- as.data.frame(summary(fgls.80s.youngest)$Coef)[, c(1, 4)]
row.names(df.err.80s)[7] <- "pct.age.bg"
df.err.80s[, 3:4] <- as.data.frame(summary(fgls.80s.young)$Coef)[, c(1, 4)]
df.err.80s[, 5:6] <- as.data.frame(summary(fgls.80s.midlife)$Coef)[, c(1, 4)] 
df.err.80s[, 7:8] <- as.data.frame(summary(fgls.80s.middle)$Coef)[, c(1, 4)] 
colnames(df.err.80s) <- c("coef.youngest", "p.youngest",
                          "coef.young", "p.young",
                          "coef.midlife", "p.midlife",
                          "coef.middle", "p.middle")

#round(df.err.80s, 3)



#2.5. Estimate spatial lag & error models for 90s. 

list.queen.90s <- poly2nb(geodata9000, queen=TRUE)
W.90s <- nb2listw(list.queen.90s, style="W", zero.policy=TRUE)

formula.90s.youngest <- Migr_youngest ~  
  cc9000_5 + lnpden9000 +  One_Transit9000 + 
  lnpop100 + pctyoungestbg + pctnhw + pctforeign + pctmulti + #pctcoll + lnrent + elem +  
  UA01 + UA02 + UA03 + UA04 + UA05 + UA06 + UA07 + UA08 + UA09 + UA10 + 
  UA11 + UA12 + UA13 + UA14 + UA15 + UA16 + UA17 + UA18 + UA19
formula.90s.young <- Migr_young ~  
  cc9000_5 + lnpden9000 +  One_Transit9000 + 
  lnpop100 + pctyoungbg + pctnhw + pctforeign + pctmulti + #pctcoll + lnrent + elem +  
  UA01 + UA02 + UA03 + UA04 + UA05 + UA06 + UA07 + UA08 + UA09 + UA10 + 
  UA11 + UA12 + UA13 + UA14 + UA15 + UA16 + UA17 + UA18 + UA19
formula.90s.midlife <- Migr_midlife ~  
  cc9000_5 + lnpden9000 +  One_Transit9000 + 
  lnpop100 + pctmidlifebg + pctnhw + pctforeign + pctmulti + #pctcoll + lnrent + elem +  
  UA01 + UA02 + UA03 + UA04 + UA05 + UA06 + UA07 + UA08 + UA09 + UA10 + 
  UA11 + UA12 + UA13 + UA14 + UA15 + UA16 + UA17 + UA18 + UA19
formula.90s.middle <- Migr_middle ~  
  cc9000_5 + lnpden9000 +  One_Transit9000 + 
  lnpop100 + pctmiddlebg + pctnhw + pctforeign + pctmulti + #pctcoll + lnrent + elem +  
  UA01 + UA02 + UA03 + UA04 + UA05 + UA06 + UA07 + UA08 + UA09 + UA10 + 
  UA11 + UA12 + UA13 + UA14 + UA15 + UA16 + UA17 + UA18 + UA19

sar2sls.90s.youngest <- stsls(formula.90s.youngest,      data=geodata9000@data, W.90s)
sar2sls.90s.young    <- stsls(formula.90s.young,         data=geodata9000@data, W.90s)
sar2sls.90s.midlife  <- stsls(formula.90s.midlife,       data=geodata9000@data, W.90s)
sar2sls.90s.middle   <- stsls(formula.90s.middle,        data=geodata9000@data, W.90s)

fgls.90s.youngest    <- GMerrorsar(formula.90s.youngest, data=geodata9000@data, W.90s)
fgls.90s.young       <- GMerrorsar(formula.90s.young,    data=geodata9000@data, W.90s)
fgls.90s.midlife     <- GMerrorsar(formula.90s.midlife,  data=geodata9000@data, W.90s)
fgls.90s.middle      <- GMerrorsar(formula.90s.middle,   data=geodata9000@data, W.90s)

df.lag.90s <- data.frame(coef = double(), 
                         p.value= double())
df.lag.90s <- as.data.frame(summary(sar2sls.90s.youngest)$Coef)[, c(1, 4)]
row.names(df.lag.90s)[7] <- "pct.age.bg"
df.lag.90s[, 3:4] <- as.data.frame(summary(sar2sls.90s.young)$Coef)[, c(1, 4)]
df.lag.90s[, 5:6] <- as.data.frame(summary(sar2sls.90s.midlife)$Coef)[, c(1, 4)] 
df.lag.90s[, 7:8] <- as.data.frame(summary(sar2sls.90s.middle)$Coef)[, c(1, 4)] 
colnames(df.lag.90s) <- c("coef.youngest", "p.youngest",
                          "coef.young", "p.young",
                          "coef.midlife", "p.midlife",
                          "coef.middle", "p.middle")

#round(df.lag.90s, 3)

df.err.90s <- data.frame(coef = double(), 
                         p.value= double())
df.err.90s <- as.data.frame(summary(fgls.90s.youngest)$Coef)[, c(1, 4)]
row.names(df.err.90s)[7] <- "pct.age.bg"
df.err.90s[, 3:4] <- as.data.frame(summary(fgls.90s.young)$Coef)[, c(1, 4)]
df.err.90s[, 5:6] <- as.data.frame(summary(fgls.90s.midlife)$Coef)[, c(1, 4)] 
df.err.90s[, 7:8] <- as.data.frame(summary(fgls.90s.middle)$Coef)[, c(1, 4)] 
colnames(df.err.90s) <- c("coef.youngest", "p.youngest",
                          "coef.young", "p.young",
                          "coef.midlife", "p.midlife",
                          "coef.middle", "p.middle")

#round(df.err.90s, 3)



#2.6. Estimate spatial lag & error models for 00s. 

list.queen.00s <- poly2nb(geodata0010, queen=TRUE)
W.00s <- nb2listw(list.queen.00s, style="W", zero.policy=TRUE)

formula.00s.youngest <- Migr_youngest ~  
  cc0010_5 + lnpden0010 +  One_Transit0010 + 
  lnpop100 + pctyoungestbg + pctnhw + pctforeign + pctmulti + #pctcoll + lnrent + elem +  
  UA01 + UA02 + UA03 + UA04 + UA05 + UA06 + UA07 + UA08 + UA09 + UA10 + 
  UA11 + UA12 + UA13 + UA14 + UA15 + UA16 + UA17 + UA18 + UA19
formula.00s.young <- Migr_young ~  
  cc0010_5 + lnpden0010 +  One_Transit0010 + 
  lnpop100 + pctyoungbg + pctnhw + pctforeign + pctmulti + #pctcoll + lnrent + elem +  
  UA01 + UA02 + UA03 + UA04 + UA05 + UA06 + UA07 + UA08 + UA09 + UA10 + 
  UA11 + UA12 + UA13 + UA14 + UA15 + UA16 + UA17 + UA18 + UA19
formula.00s.midlife <- Migr_midlife ~  
  cc0010_5 + lnpden0010 +  One_Transit0010 + 
  lnpop100 + pctmidlifebg + pctnhw + pctforeign + pctmulti + #pctcoll + lnrent + elem +  
  UA01 + UA02 + UA03 + UA04 + UA05 + UA06 + UA07 + UA08 + UA09 + UA10 + 
  UA11 + UA12 + UA13 + UA14 + UA15 + UA16 + UA17 + UA18 + UA19
formula.00s.middle <- Migr_middle ~  
  cc0010_5 + lnpden0010 +  One_Transit0010 + 
  lnpop100 + pctmiddlebg + pctnhw + pctforeign + pctmulti + #pctcoll + lnrent + elem +  
  UA01 + UA02 + UA03 + UA04 + UA05 + UA06 + UA07 + UA08 + UA09 + UA10 + 
  UA11 + UA12 + UA13 + UA14 + UA15 + UA16 + UA17 + UA18 + UA19

sar2sls.00s.youngest <- stsls(formula.00s.youngest,      data=geodata0010@data, W.00s)
sar2sls.00s.young    <- stsls(formula.00s.young,         data=geodata0010@data, W.00s)
sar2sls.00s.midlife  <- stsls(formula.00s.midlife,       data=geodata0010@data, W.00s)
sar2sls.00s.middle   <- stsls(formula.00s.middle,        data=geodata0010@data, W.00s)

fgls.00s.youngest    <- GMerrorsar(formula.00s.youngest, data=geodata0010@data, W.00s)
fgls.00s.young       <- GMerrorsar(formula.00s.young,    data=geodata0010@data, W.00s)
fgls.00s.midlife     <- GMerrorsar(formula.00s.midlife,  data=geodata0010@data, W.00s)
fgls.00s.middle      <- GMerrorsar(formula.00s.middle,   data=geodata0010@data, W.00s)

df.lag.00s <- data.frame(coef = double(), 
                         p.value= double())
df.lag.00s <- as.data.frame(summary(sar2sls.00s.youngest)$Coef)[, c(1, 4)]
row.names(df.lag.00s)[7] <- "pct.age.bg"
df.lag.00s[, 3:4] <- as.data.frame(summary(sar2sls.00s.young)$Coef)[, c(1, 4)]
df.lag.00s[, 5:6] <- as.data.frame(summary(sar2sls.00s.midlife)$Coef)[, c(1, 4)] 
df.lag.00s[, 7:8] <- as.data.frame(summary(sar2sls.00s.middle)$Coef)[, c(1, 4)] 
colnames(df.lag.00s) <- c("coef.youngest", "p.youngest",
                          "coef.young", "p.young",
                          "coef.midlife", "p.midlife",
                          "coef.middle", "p.middle")

#round(df.lag.00s, 3)

df.err.00s <- data.frame(coef = double(), 
                         p.value= double())
df.err.00s <- as.data.frame(summary(fgls.00s.youngest)$Coef)[, c(1, 4)]
row.names(df.err.00s)[7] <- "pct.age.bg"
df.err.00s[, 3:4] <- as.data.frame(summary(fgls.00s.young)$Coef)[, c(1, 4)]
df.err.00s[, 5:6] <- as.data.frame(summary(fgls.00s.midlife)$Coef)[, c(1, 4)] 
df.err.00s[, 7:8] <- as.data.frame(summary(fgls.00s.middle)$Coef)[, c(1, 4)] 
colnames(df.err.00s) <- c("coef.youngest", "p.youngest",
                          "coef.young", "p.young",
                          "coef.midlife", "p.midlife",
                          "coef.middle", "p.middle")

#round(df.err.00s, 3)



#2.7. Combine results for the 3-decade models

df.lag.8010 <- rbind(df.lag.80s[3:5, ], df.lag.90s[3:5, ])
df.lag.8010 <- rbind(df.lag.8010 , df.lag.00s[3:5, ])
df.lag.8010 <- df.lag.8010[c(1, 4, 7, 2, 5, 8, 3, 6, 9), ] 

round(df.lag.8010, 3)

df.err.8010 <- rbind(df.err.80s[2:4, ], df.err.90s[2:4, ])
df.err.8010 <- rbind(df.err.8010 , df.err.00s[2:4, ])
df.err.8010 <- df.err.8010[c(1, 4, 7, 2, 5, 8, 3, 6, 9), ] 

round(df.err.8010, 3)




#3.1. two-decade models: Estimate spatial lag & error models for 90s.

#list.queen.90s <- poly2nb(geodata9000, queen=TRUE)
#W.90s <- nb2listw(list.queen.90s, style="W", zero.policy=TRUE)

formula2.90s.youngest <- Migr_youngest ~  
  f_ccity9000b + f_pden9000b +  f_pt9000b + lncden9000b + 
  lnpop100 + pctyoungestbg + pctnhw + pctforeign + pctmulti + #pctcoll + lnrent + elem +  
  UA01 + UA02 + UA03 + UA04 + UA05 + UA06 + UA07 + UA08 + UA09 + UA10 + 
  UA11 + UA12 + UA13 + UA14 + UA15 + UA16 + UA17 + UA18 + UA19
formula2.90s.young <- Migr_young ~  
  f_ccity9000b + f_pden9000b +  f_pt9000b + lncden9000b + 
  lnpop100 + pctyoungbg + pctnhw + pctforeign + pctmulti + #pctcoll + lnrent + elem +  
  UA01 + UA02 + UA03 + UA04 + UA05 + UA06 + UA07 + UA08 + UA09 + UA10 + 
  UA11 + UA12 + UA13 + UA14 + UA15 + UA16 + UA17 + UA18 + UA19
formula2.90s.midlife <- Migr_midlife ~  
  f_ccity9000b + f_pden9000b +  f_pt9000b + lncden9000b + 
  lnpop100 + pctmidlifebg + pctnhw + pctforeign + pctmulti + #pctcoll + lnrent + elem +  
  UA01 + UA02 + UA03 + UA04 + UA05 + UA06 + UA07 + UA08 + UA09 + UA10 + 
  UA11 + UA12 + UA13 + UA14 + UA15 + UA16 + UA17 + UA18 + UA19
formula2.90s.middle <- Migr_middle ~  
  f_ccity9000b + f_pden9000b +  f_pt9000b + lncden9000b + 
  lnpop100 + pctmiddlebg + pctnhw + pctforeign + pctmulti + #pctcoll + lnrent + elem +  
  UA01 + UA02 + UA03 + UA04 + UA05 + UA06 + UA07 + UA08 + UA09 + UA10 + 
  UA11 + UA12 + UA13 + UA14 + UA15 + UA16 + UA17 + UA18 + UA19

sar2sls2.90s.youngest <- stsls(formula2.90s.youngest,      data=geodata9000@data, W.90s)
sar2sls2.90s.young    <- stsls(formula2.90s.young,         data=geodata9000@data, W.90s)
sar2sls2.90s.midlife  <- stsls(formula2.90s.midlife,       data=geodata9000@data, W.90s)
sar2sls2.90s.middle   <- stsls(formula2.90s.middle,        data=geodata9000@data, W.90s)

fgls2.90s.youngest    <- GMerrorsar(formula2.90s.youngest, data=geodata9000@data, W.90s)
fgls2.90s.young       <- GMerrorsar(formula2.90s.young,    data=geodata9000@data, W.90s)
fgls2.90s.midlife     <- GMerrorsar(formula2.90s.midlife,  data=geodata9000@data, W.90s)
fgls2.90s.middle      <- GMerrorsar(formula2.90s.middle,   data=geodata9000@data, W.90s)

df2.lag.90s <- data.frame(coef = double(), 
                         p.value= double())
df2.lag.90s <- as.data.frame(summary(sar2sls2.90s.youngest)$Coef)[, c(1, 4)]
row.names(df2.lag.90s)[7] <- "pct.age.bg"
df2.lag.90s[, 3:4] <- as.data.frame(summary(sar2sls2.90s.young)$Coef)[, c(1, 4)]
df2.lag.90s[, 5:6] <- as.data.frame(summary(sar2sls2.90s.midlife)$Coef)[, c(1, 4)] 
df2.lag.90s[, 7:8] <- as.data.frame(summary(sar2sls2.90s.middle)$Coef)[, c(1, 4)] 
colnames(df2.lag.90s) <- c("coef.youngest", "p.youngest",
                          "coef.young", "p.young",
                          "coef.midlife", "p.midlife",
                          "coef.middle", "p.middle")

#round(df.lag.90s, 3)

df2.err.90s <- data.frame(coef = double(), 
                         p.value= double())
df2.err.90s <- as.data.frame(summary(fgls2.90s.youngest)$Coef)[, c(1, 4)]
row.names(df2.err.90s)[7] <- "pct.age.bg"
df2.err.90s[, 3:4] <- as.data.frame(summary(fgls2.90s.young)$Coef)[, c(1, 4)]
df2.err.90s[, 5:6] <- as.data.frame(summary(fgls2.90s.midlife)$Coef)[, c(1, 4)] 
df2.err.90s[, 7:8] <- as.data.frame(summary(fgls2.90s.middle)$Coef)[, c(1, 4)] 
colnames(df2.err.90s) <- c("coef.youngest", "p.youngest",
                          "coef.young", "p.young",
                          "coef.midlife", "p.midlife",
                          "coef.middle", "p.middle")

#round(df.err.90s, 3)



#3.1. two-decade models: Estimate spatial lag & error models for 00s.

#list.queen.00s <- poly2nb(geodata0010, queen=TRUE)
#W.00s <- nb2listw(list.queen.00s, style="W", zero.policy=TRUE)

formula2.00s.youngest <- Migr_youngest ~  
  f_ccity0010b + f_pden0010b +  f_pt0010b + lncden0010b + 
  lnpop100 + pctyoungestbg + pctnhw + pctforeign + pctmulti + #pctcoll + lnrent + elem +  
  UA01 + UA02 + UA03 + UA04 + UA05 + UA06 + UA07 + UA08 + UA09 + UA10 + 
  UA11 + UA12 + UA13 + UA14 + UA15 + UA16 + UA17 + UA18 + UA19
formula2.00s.young <- Migr_young ~  
  f_ccity0010b + f_pden0010b +  f_pt0010b + lncden0010b + 
  lnpop100 + pctyoungbg + pctnhw + pctforeign + pctmulti + #pctcoll + lnrent + elem +  
  UA01 + UA02 + UA03 + UA04 + UA05 + UA06 + UA07 + UA08 + UA09 + UA10 + 
  UA11 + UA12 + UA13 + UA14 + UA15 + UA16 + UA17 + UA18 + UA19
formula2.00s.midlife <- Migr_midlife ~  
  f_ccity0010b + f_pden0010b +  f_pt0010b + lncden0010b + 
  lnpop100 + pctmidlifebg + pctnhw + pctforeign + pctmulti + #pctcoll + lnrent + elem +  
  UA01 + UA02 + UA03 + UA04 + UA05 + UA06 + UA07 + UA08 + UA09 + UA10 + 
  UA11 + UA12 + UA13 + UA14 + UA15 + UA16 + UA17 + UA18 + UA19
formula2.00s.middle <- Migr_middle ~  
  f_ccity0010b + f_pden0010b +  f_pt0010b + lncden0010b + 
  lnpop100 + pctmiddlebg + pctnhw + pctforeign + pctmulti + #pctcoll + lnrent + elem +  
  UA01 + UA02 + UA03 + UA04 + UA05 + UA06 + UA07 + UA08 + UA09 + UA10 + 
  UA11 + UA12 + UA13 + UA14 + UA15 + UA16 + UA17 + UA18 + UA19

sar2sls2.00s.youngest <- stsls(formula2.00s.youngest,      data=geodata0010@data, W.00s)
sar2sls2.00s.young    <- stsls(formula2.00s.young,         data=geodata0010@data, W.00s)
sar2sls2.00s.midlife  <- stsls(formula2.00s.midlife,       data=geodata0010@data, W.00s)
sar2sls2.00s.middle   <- stsls(formula2.00s.middle,        data=geodata0010@data, W.00s)

fgls2.00s.youngest    <- GMerrorsar(formula2.00s.youngest, data=geodata0010@data, W.00s)
fgls2.00s.young       <- GMerrorsar(formula2.00s.young,    data=geodata0010@data, W.00s)
fgls2.00s.midlife     <- GMerrorsar(formula2.00s.midlife,  data=geodata0010@data, W.00s)
fgls2.00s.middle      <- GMerrorsar(formula2.00s.middle,   data=geodata0010@data, W.00s)

df2.lag.00s <- data.frame(coef = double(), 
                         p.value= double())
df2.lag.00s <- as.data.frame(summary(sar2sls2.00s.youngest)$Coef)[, c(1, 4)]
row.names(df2.lag.00s)[7] <- "pct.age.bg"
df2.lag.00s[, 3:4] <- as.data.frame(summary(sar2sls2.00s.young)$Coef)[, c(1, 4)]
df2.lag.00s[, 5:6] <- as.data.frame(summary(sar2sls2.00s.midlife)$Coef)[, c(1, 4)] 
df2.lag.00s[, 7:8] <- as.data.frame(summary(sar2sls2.00s.middle)$Coef)[, c(1, 4)] 
colnames(df2.lag.00s) <- c("coef.youngest", "p.youngest",
                          "coef.young", "p.young",
                          "coef.midlife", "p.midlife",
                          "coef.middle", "p.middle")

#round(df.lag.00s, 3)

df2.err.00s <- data.frame(coef = double(), 
                         p.value= double())
df2.err.00s <- as.data.frame(summary(fgls2.00s.youngest)$Coef)[, c(1, 4)]
row.names(df2.err.00s)[7] <- "pct.age.bg"
df2.err.00s[, 3:4] <- as.data.frame(summary(fgls2.00s.young)$Coef)[, c(1, 4)]
df2.err.00s[, 5:6] <- as.data.frame(summary(fgls2.00s.midlife)$Coef)[, c(1, 4)] 
df2.err.00s[, 7:8] <- as.data.frame(summary(fgls2.00s.middle)$Coef)[, c(1, 4)] 
colnames(df2.err.00s) <- c("coef.youngest", "p.youngest",
                          "coef.young", "p.young",
                          "coef.midlife", "p.midlife",
                          "coef.middle", "p.middle")

#round(df.err.00s, 3)



#3.3. Combine results for the 2-decade models

df.lag.9010 <- rbind(df2.lag.90s[3:6, ], df2.lag.00s[3:6, ])
df.lag.9010 <- df.lag.9010[c(1, 5, 2, 6, 3, 7, 4, 8), ] 

round(df.lag.9010, 3)

df.err.9010 <- rbind(df2.err.90s[2:5, ], df2.err.00s[2:5, ])
df.err.9010 <- df.err.9010[c(1, 5, 2, 6, 3, 7, 4, 8), ] 

round(df.err.9010, 3)


