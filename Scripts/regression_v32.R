
install.packages("sas7bdat", dependencies = TRUE)
install.packages("tidyverse", dependencies = TRUE)
install.packages("stargazer", dependencies = TRUE)
install.packages("car", dependencies = TRUE)
install.packages("quantreg", dependencies = TRUE)

library(sas7bdat)
library(tidyverse)
library(stargazer)
library(car)
library(quantreg)

setwd("C:/Users/ylee366/Dropbox (GaTech)/3b_ResearchPersonal/11_BSL/Millennial_panel/09_NetMigration1980/Scratch")

#1. import the main dataset 

temp  <- read.sas7bdat("C:/Users/ylee366/Dropbox (GaTech)/3b_ResearchPersonal/11_BSL/Millennial_panel/05_Scratch/pool3.sas7bdat")
colnames(temp)
nrow(temp)

#2. process the main dataset 

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
             "pctnhw", "pctforeign", "pctpro", "pctcoll", 
             "lnmedhhinc", "lnrent", "lnhvalue", "pctunemp", "pctpoor",        
             "pctowner", "pctmulti", "yr8090", "yr9000", "yr0010", 
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

#migrdata <- migrdata[, c(1:4, 67:68, 5:42, 66, 43:65)]
head(migrdata)
#colnames(migrdata)


#3. 3-decade models  

lm.youngest1 <- lm(Migr_youngest ~ #ln_dist_cbd8090 + ln_dist_cbd9000 + ln_dist_cbd0010 + 
                     cc8090_5 + cc9000_5 + cc0010_5 +  
                     One_Transit8090 + One_Transit9000 + One_Transit0010 + 
                     yr9000 + yr0010 +  
                     pctyoungestbg + pctnhw + pctforeign + pctunemp +  
                     UA01 + UA02 + UA03 + UA04 + UA05 + UA06 + UA07 + UA08 + UA09 + UA10 +
                     UA11 + UA12 + UA13 + UA14 + UA15 + UA16 + UA17 + UA18 + UA19, 
                     data=migrdata[migrdata$Migr_youngest > quantile(migrdata$Migr_youngest, 0.01) & 
                                   migrdata$Migr_youngest < quantile(migrdata$Migr_youngest, 0.99), ])

lm.youngest2 <- lm(Migr_youngest ~ #ln_dist_cbd8090 + ln_dist_cbd9000 + ln_dist_cbd0010 + 
                     lnpden8090 + lnpden9000 + lnpden0010 + 
                     One_Transit8090 + One_Transit9000 + One_Transit0010 + 
                     yr9000 + yr0010 +  
                     pctyoungestbg + pctnhw + pctforeign + pctunemp +  
                     UA01 + UA02 + UA03 + UA04 + UA05 + UA06 + UA07 + UA08 + UA09 + UA10 +
                     UA11 + UA12 + UA13 + UA14 + UA15 + UA16 + UA17 + UA18 + UA19, 
                   data=migrdata[migrdata$Migr_youngest > quantile(migrdata$Migr_youngest, 0.01) & 
                                   migrdata$Migr_youngest < quantile(migrdata$Migr_youngest, 0.99), ])

lm.youngest3 <- lm(Migr_youngest ~ #ln_dist_cbd8090 + ln_dist_cbd9000 + ln_dist_cbd0010 + 
                     cc8090_5 + cc9000_5 + cc0010_5 + lnpden8090 + lnpden9000 + lnpden0010 + 
                     One_Transit8090 + One_Transit9000 + One_Transit0010 + 
                     yr9000 + yr0010 +  
                     pctyoungestbg + pctnhw + pctforeign + pctunemp +  
                     UA01 + UA02 + UA03 + UA04 + UA05 + UA06 + UA07 + UA08 + UA09 + UA10 +
                     UA11 + UA12 + UA13 + UA14 + UA15 + UA16 + UA17 + UA18 + UA19, 
                   data=migrdata[migrdata$Migr_youngest > quantile(migrdata$Migr_youngest, 0.01) & 
                                   migrdata$Migr_youngest < quantile(migrdata$Migr_youngest, 0.99), ])

lm.youngest4 <- lm(Migr_youngest ~ #ln_dist_cbd8090 + ln_dist_cbd9000 + ln_dist_cbd0010 + 
                     cc8090_5 + cc9000_5 + cc0010_5 + lnpden8090 + lnpden9000 + lnpden0010 + 
                     One_Transit8090 + One_Transit9000 + One_Transit0010 + 
                     yr9000 + yr0010 +  
                     lnpop100 + pctyoungestbg + pctnhw + pctforeign + pctunemp + 
                     UA01 + UA02 + UA03 + UA04 + UA05 + UA06 + UA07 + UA08 + UA09 + UA10 +
                     UA11 + UA12 + UA13 + UA14 + UA15 + UA16 + UA17 + UA18 + UA19, 
                   data=migrdata[migrdata$Migr_youngest > quantile(migrdata$Migr_youngest, 0.01) & 
                                   migrdata$Migr_youngest < quantile(migrdata$Migr_youngest, 0.99), ])

lm.youngest5 <- lm(Migr_youngest ~ #ln_dist_cbd8090 + ln_dist_cbd9000 + ln_dist_cbd0010 + 
                     cc8090_5 + cc9000_5 + cc0010_5 + lnpden8090 + lnpden9000 + lnpden0010 + 
                     One_Transit8090 + One_Transit9000 + One_Transit0010 + 
                     yr9000 + yr0010 +  
                     lnpop100 + pctyoungestbg + pctnhw + pctforeign + pctunemp + pctmulti +  
                     UA01 + UA02 + UA03 + UA04 + UA05 + UA06 + UA07 + UA08 + UA09 + UA10 +
                     UA11 + UA12 + UA13 + UA14 + UA15 + UA16 + UA17 + UA18 + UA19, 
                   data=migrdata[migrdata$Migr_youngest > quantile(migrdata$Migr_youngest, 0.01) & 
                                   migrdata$Migr_youngest < quantile(migrdata$Migr_youngest, 0.99), ])

lm.youngest6 <- lm(Migr_youngest ~ #ln_dist_cbd8090 + ln_dist_cbd9000 + ln_dist_cbd0010 + 
                     cc8090_5 + cc9000_5 + cc0010_5 + lnpden8090 + lnpden9000 + lnpden0010 + 
                     One_Transit8090 + One_Transit9000 + One_Transit0010 + 
                     yr9000 + yr0010 +  
                     lnpop100 + pctyoungestbg + pctnhw + pctforeign + lnmedhhinc + pctunemp + pctmulti +  
                     UA01 + UA02 + UA03 + UA04 + UA05 + UA06 + UA07 + UA08 + UA09 + UA10 +
                     UA11 + UA12 + UA13 + UA14 + UA15 + UA16 + UA17 + UA18 + UA19, 
                   data=migrdata[migrdata$Migr_youngest > quantile(migrdata$Migr_youngest, 0.01) & 
                                   migrdata$Migr_youngest < quantile(migrdata$Migr_youngest, 0.99), ])

lm.youngest7 <- lm(Migr_youngest ~ #ln_dist_cbd8090 + ln_dist_cbd9000 + ln_dist_cbd0010 + 
                     cc8090_5 + cc9000_5 + cc0010_5 + lnpden8090 + lnpden9000 + lnpden0010 + 
                     One_Transit8090 + One_Transit9000 + One_Transit0010 + 
                     yr9000 + yr0010 +  
                     lnpop100 + pctyoungestbg + pctnhw + pctforeign + pctmulti +  elem + 
                     UA01 + UA02 + UA03 + UA04 + UA05 + UA06 + UA07 + UA08 + UA09 + UA10 +
                     UA11 + UA12 + UA13 + UA14 + UA15 + UA16 + UA17 + UA18 + UA19, 
                   data=migrdata[migrdata$Migr_youngest > quantile(migrdata$Migr_youngest, 0.01) & 
                                   migrdata$Migr_youngest < quantile(migrdata$Migr_youngest, 0.99), ])

lm.youngest8 <- lm(Migr_youngest ~ #ln_dist_cbd8090 + ln_dist_cbd9000 + ln_dist_cbd0010 + 
                     cc8090_5 + cc9000_5 + cc0010_5 + lnpden8090 + lnpden9000 + lnpden0010 + 
                     One_Transit8090 + One_Transit9000 + One_Transit0010 + 
                     yr9000 + yr0010 +  
                     lnpop100 + pctyoungestbg + pctnhw + pctforeign + lnhvalue + pctunemp + pctmulti +  
                     UA01 + UA02 + UA03 + UA04 + UA05 + UA06 + UA07 + UA08 + UA09 + UA10 +
                     UA11 + UA12 + UA13 + UA14 + UA15 + UA16 + UA17 + UA18 + UA19, 
                   data=migrdata[migrdata$Migr_youngest > quantile(migrdata$Migr_youngest, 0.01) & 
                                   migrdata$Migr_youngest < quantile(migrdata$Migr_youngest, 0.99), ])

lm.youngest9 <- lm(Migr_youngest ~ #ln_dist_cbd8090 + ln_dist_cbd9000 + ln_dist_cbd0010 + 
                     cc8090_5 + cc9000_5 + cc0010_5 + lnpden8090 + lnpden9000 + lnpden0010 + 
                     One_Transit8090 + One_Transit9000 + One_Transit0010 + 
                     yr9000 + yr0010 +  
                     lnpop100 + pctyoungestbg + pctnhw + pctforeign + lnrent + pctunemp + pctmulti +  
                     UA01 + UA02 + UA03 + UA04 + UA05 + UA06 + UA07 + UA08 + UA09 + UA10 +
                     UA11 + UA12 + UA13 + UA14 + UA15 + UA16 + UA17 + UA18 + UA19, 
                   data=migrdata[migrdata$Migr_youngest > quantile(migrdata$Migr_youngest, 0.01) & 
                                   migrdata$Migr_youngest < quantile(migrdata$Migr_youngest, 0.99), ])

lm.youngest10 <- lm(Migr_youngest ~ #ln_dist_cbd8090 + ln_dist_cbd9000 + ln_dist_cbd0010 + 
                     cc8090_5 + cc9000_5 + cc0010_5 + lnpden8090 + lnpden9000 + lnpden0010 + 
                     One_Transit8090 + One_Transit9000 + One_Transit0010 + 
                     yr9000 + yr0010 +  
                     lnpop100 + pctyoungestbg + pctnhw + pctforeign + pctcoll +  
                     lnrent + pctunemp + pctmulti +  
                     UA01 + UA02 + UA03 + UA04 + UA05 + UA06 + UA07 + UA08 + UA09 + UA10 +
                     UA11 + UA12 + UA13 + UA14 + UA15 + UA16 + UA17 + UA18 + UA19, 
                   data=migrdata[migrdata$Migr_youngest > quantile(migrdata$Migr_youngest, 0.01) & 
                                   migrdata$Migr_youngest < quantile(migrdata$Migr_youngest, 0.99), ])

lm.youngest11 <- lm(Migr_youngest ~ #ln_dist_cbd8090 + ln_dist_cbd9000 + ln_dist_cbd0010 + 
                      cc8090_5 + cc9000_5 + cc0010_5 + lnpden8090 + lnpden9000 + lnpden0010 + 
                      One_Transit8090 + One_Transit9000 + One_Transit0010 + 
                      yr9000 + yr0010 +  
                      lnpop100 + pctyoungestbg + pctnhw + pctforeign + pctpro +  
                      lnrent + pctunemp + pctmulti +  
                      UA01 + UA02 + UA03 + UA04 + UA05 + UA06 + UA07 + UA08 + UA09 + UA10 +
                      UA11 + UA12 + UA13 + UA14 + UA15 + UA16 + UA17 + UA18 + UA19, 
                    data=migrdata[migrdata$Migr_youngest > quantile(migrdata$Migr_youngest, 0.01) & 
                                    migrdata$Migr_youngest < quantile(migrdata$Migr_youngest, 0.99), ]) #(don't run)

lm.youngest <- lm(Migr_youngest ~ #ln_dist_cbd8090 + ln_dist_cbd9000 + ln_dist_cbd0010 + 
                      cc8090_5 + cc9000_5 + cc0010_5 + lnpden8090 + lnpden9000 + lnpden0010 + 
                      One_Transit8090 + One_Transit9000 + One_Transit0010 + 
                      yr9000 + yr0010 +  
                      lnpop100 + pctyoungestbg + pctnhw + pctforeign + pctpro +  
                      lnmedhhinc + pctunemp + pctmulti +  
                      UA01 + UA02 + UA03 + UA04 + UA05 + UA06 + UA07 + UA08 + UA09 + UA10 +
                      UA11 + UA12 + UA13 + UA14 + UA15 + UA16 + UA17 + UA18 + UA19, 
                    data=migrdata[migrdata$Migr_youngest > quantile(migrdata$Migr_youngest, 0.01) & 
                                    migrdata$Migr_youngest < quantile(migrdata$Migr_youngest, 0.99), ])

vif(lm.youngest1)[c("lnpop100", "pctyoungestbg", "pctnhw", "pctforeign", "pctcoll", "pctpro", "lnmedhhinc", "lnhvalue", "lnrent", "pctunemp", "pctmulti", "elem")]
vif(lm.youngest2)[c("lnpop100", "pctyoungestbg", "pctnhw", "pctforeign", "pctcoll", "pctpro", "lnmedhhinc", "lnhvalue", "lnrent", "pctunemp", "pctmulti", "elem")]
vif(lm.youngest3)[c("lnpop100", "pctyoungestbg", "pctnhw", "pctforeign", "pctcoll", "pctpro", "lnmedhhinc", "lnhvalue", "lnrent", "pctunemp", "pctmulti", "elem")]
vif(lm.youngest4)[c("lnpop100", "pctyoungestbg", "pctnhw", "pctforeign", "pctcoll", "pctpro", "lnmedhhinc", "lnhvalue", "lnrent", "pctunemp", "pctmulti", "elem")]
vif(lm.youngest5)[c("lnpop100", "pctyoungestbg", "pctnhw", "pctforeign", "pctcoll", "pctpro", "lnmedhhinc", "lnhvalue", "lnrent", "pctunemp", "pctmulti", "elem")]
vif(lm.youngest6)[c("lnpop100", "pctyoungestbg", "pctnhw", "pctforeign", "pctcoll", "pctpro", "lnmedhhinc", "lnhvalue", "lnrent", "pctunemp", "pctmulti", "elem")]
vif(lm.youngest7)[c("lnpop100", "pctyoungestbg", "pctnhw", "pctforeign", "pctcoll", "pctpro", "lnmedhhinc", "lnhvalue", "lnrent", "pctunemp", "pctmulti", "elem")]
vif(lm.youngest8)[c("lnpop100", "pctyoungestbg", "pctnhw", "pctforeign", "pctcoll", "pctpro", "lnmedhhinc", "lnhvalue", "lnrent", "pctunemp", "pctmulti", "elem")]
vif(lm.youngest9)[c("lnpop100", "pctyoungestbg", "pctnhw", "pctforeign", "pctcoll", "pctpro", "lnmedhhinc", "lnhvalue", "lnrent", "pctunemp", "pctmulti", "elem")]
vif(lm.youngest10)[c("lnpop100", "pctyoungestbg", "pctnhw", "pctforeign", "pctcoll", "pctpro", "lnmedhhinc", "lnhvalue", "lnrent", "pctunemp", "pctmulti", "elem")]
vif(lm.youngest11)[c("lnpop100", "pctyoungestbg", "pctnhw", "pctforeign", "pctcoll", "pctpro", "lnmedhhinc", "lnhvalue", "lnrent", "pctunemp", "pctmulti", "elem")]
vif(lm.youngest12)[c("lnpop100", "pctyoungestbg", "pctnhw", "pctforeign", "pctcoll", "pctpro", "lnmedhhinc", "lnhvalue", "lnrent", "pctunemp", "pctmulti", "elem")]

stargazer(#lm.youngest1, lm.youngest2, lm.youngest3, lm.youngest4, 
          lm.youngest5, lm.youngest6, lm.youngest7, lm.youngest8, 
          lm.youngest9, lm.youngest10, lm.youngest11, lm.youngest12,type="text",  
          title="Net Migration of 20-24 in 1980-2010", omit=c("UA01", "UA02", "UA03", "UA04", "UA05", 
                                                              "UA06", "UA07", "UA08", "UA09", "UA10", 
                                                              "UA11", "UA12", "UA13", "UA14", "UA15", 
                                                              "UA16", "UA17", "UA18", "UA19", "Constant"), 
          ci.level=0.9, report="vc*", omit.stat=c("f", "ser")) #(don't run)

lm.young <- lm(Migr_young ~ #ln_dist_cbd8090 + ln_dist_cbd9000 + ln_dist_cbd0010 + 
                    cc8090_5 + cc9000_5 + cc0010_5 + lnpden8090 + lnpden9000 + lnpden0010 + 
                    One_Transit8090 + One_Transit9000 + One_Transit0010 + 
                    yr9000 + yr0010 +  
                    lnpop100 + pctyoungbg + pctnhw + pctforeign + pctpro +  
                    lnmedhhinc + pctunemp + pctmulti +  
                    UA01 + UA02 + UA03 + UA04 + UA05 + UA06 + UA07 + UA08 + UA09 + UA10 +
                    UA11 + UA12 + UA13 + UA14 + UA15 + UA16 + UA17 + UA18 + UA19, 
                  data=migrdata[migrdata$Migr_young > quantile(migrdata$Migr_young, 0.01) & 
                                  migrdata$Migr_young < quantile(migrdata$Migr_young, 0.99), ])

lm.midlife <- lm(Migr_midlife ~ #ln_dist_cbd8090 + ln_dist_cbd9000 + ln_dist_cbd0010 + 
                    cc8090_5 + cc9000_5 + cc0010_5 + lnpden8090 + lnpden9000 + lnpden0010 + 
                    One_Transit8090 + One_Transit9000 + One_Transit0010 + 
                    yr9000 + yr0010 +  
                    lnpop100 + pctmidlifebg + pctnhw + pctforeign + pctpro +  
                    lnmedhhinc + pctunemp + pctmulti +  
                    UA01 + UA02 + UA03 + UA04 + UA05 + UA06 + UA07 + UA08 + UA09 + UA10 +
                    UA11 + UA12 + UA13 + UA14 + UA15 + UA16 + UA17 + UA18 + UA19, 
                  data=migrdata[migrdata$Migr_midlife > quantile(migrdata$Migr_midlife, 0.01) & 
                                  migrdata$Migr_midlife < quantile(migrdata$Migr_midlife, 0.99), ])

lm.middle <- lm(Migr_middle ~ #ln_dist_cbd8090 + ln_dist_cbd9000 + ln_dist_cbd0010 + 
                    cc8090_5 + cc9000_5 + cc0010_5 + lnpden8090 + lnpden9000 + lnpden0010 + 
                    One_Transit8090 + One_Transit9000 + One_Transit0010 + 
                    yr9000 + yr0010 +  
                    lnpop100 + pctmiddlebg + pctnhw + pctforeign + pctpro +  
                    lnmedhhinc + pctunemp + pctmulti +  
                    UA01 + UA02 + UA03 + UA04 + UA05 + UA06 + UA07 + UA08 + UA09 + UA10 +
                    UA11 + UA12 + UA13 + UA14 + UA15 + UA16 + UA17 + UA18 + UA19, 
                  data=migrdata[migrdata$Migr_middle > quantile(migrdata$Migr_middle, 0.01) & 
                                  migrdata$Migr_middle < quantile(migrdata$Migr_middle, 0.99), ])

stargazer(lm.youngest, lm.young, lm.midlife, lm.middle, type="text",  
          title="Net Migration in 1980-2010", omit=c("UA01", "UA02", "UA03", "UA04", "UA05", 
                                                     "UA06", "UA07", "UA08", "UA09", "UA10", 
                                                     "UA11", "UA12", "UA13", "UA14", "UA15", 
                                                     "UA16", "UA17", "UA18", "UA19", "Constant"), 
          ci.level=0.9, report="vc*", omit.stat=c("f", "ser"))


#4. 2-decade models  

lm2.youngest <- lm(Migr_youngest ~ #ln_dist_cbd8090 + ln_dist_cbd9000 + ln_dist_cbd0010 + 
                     f_ccity9000b + f_ccity0010b + f_pden9000b + f_pden0010b +  
                     f_pt9000b + f_pt0010b + lncden9000b + lncden0010b +  
                    yr0010 +  
                    lnpop100 + pctyoungestbg + pctnhw + pctforeign + pctpro +  
                    lnmedhhinc + pctunemp + pctmulti +  
                    UA01 + UA02 + UA03 + UA04 + UA05 + UA06 + UA07 + UA08 + UA09 + UA10 +
                    UA11 + UA12 + UA13 + UA14 + UA15 + UA16 + UA17 + UA18 + UA19, 
                  data=migrdata[migrdata$Migr_youngest > quantile(migrdata$Migr_youngest, 0.01) & 
                                  migrdata$Migr_youngest < quantile(migrdata$Migr_youngest, 0.99) &
                                  migrdata$yr8090 != 1, ])

lm2.young <- lm(Migr_young ~ #ln_dist_cbd8090 + ln_dist_cbd9000 + ln_dist_cbd0010 + 
                  f_ccity9000b + f_ccity0010b + f_pden9000b + f_pden0010b +  
                  f_pt9000b + f_pt0010b + lncden9000b + lncden0010b + 
                 yr0010 +  
                 lnpop100 + pctyoungbg + pctnhw + pctforeign + pctpro +  
                 lnmedhhinc + pctunemp + pctmulti +  
                 UA01 + UA02 + UA03 + UA04 + UA05 + UA06 + UA07 + UA08 + UA09 + UA10 +
                 UA11 + UA12 + UA13 + UA14 + UA15 + UA16 + UA17 + UA18 + UA19, 
               data=migrdata[migrdata$Migr_young > quantile(migrdata$Migr_young, 0.01) & 
                               migrdata$Migr_young < quantile(migrdata$Migr_young, 0.99)&
                               migrdata$yr8090 != 1, ])

lm2.midlife <- lm(Migr_midlife ~ #ln_dist_cbd8090 + ln_dist_cbd9000 + ln_dist_cbd0010 + 
                    f_ccity9000b + f_ccity0010b + f_pden9000b + f_pden0010b +  
                    f_pt9000b + f_pt0010b + lncden9000b + lncden0010b + 
                   yr0010 +  
                   lnpop100 + pctmidlifebg + pctnhw + pctforeign + pctpro +  
                   lnmedhhinc + pctunemp + pctmulti +  
                   UA01 + UA02 + UA03 + UA04 + UA05 + UA06 + UA07 + UA08 + UA09 + UA10 +
                   UA11 + UA12 + UA13 + UA14 + UA15 + UA16 + UA17 + UA18 + UA19, 
                 data=migrdata[migrdata$Migr_midlife > quantile(migrdata$Migr_midlife, 0.01) & 
                                 migrdata$Migr_midlife < quantile(migrdata$Migr_midlife, 0.99)&
                                 migrdata$yr8090 != 1, ])

lm2.middle <- lm(Migr_middle ~ #ln_dist_cbd8090 + ln_dist_cbd9000 + ln_dist_cbd0010 + 
                   f_ccity9000b + f_ccity0010b + f_pden9000b + f_pden0010b +  
                   f_pt9000b + f_pt0010b + lncden9000b + lncden0010b + 
                  yr0010 +  
                  lnpop100 + pctmiddlebg + pctnhw + pctforeign + pctpro +  
                  lnmedhhinc + pctunemp + pctmulti +  
                  UA01 + UA02 + UA03 + UA04 + UA05 + UA06 + UA07 + UA08 + UA09 + UA10 +
                  UA11 + UA12 + UA13 + UA14 + UA15 + UA16 + UA17 + UA18 + UA19, 
                data=migrdata[migrdata$Migr_middle > quantile(migrdata$Migr_middle, 0.01) & 
                                migrdata$Migr_middle < quantile(migrdata$Migr_middle, 0.99)&
                                migrdata$yr8090 != 1, ])


vif(lm2.youngest)[c("lnpop100", "pctnhw", "pctforeign", "pctpro", "lnmedhhinc", "pctunemp", "pctmulti")]
vif(lm2.young)[c("lnpop100", "pctnhw", "pctforeign", "pctpro", "lnmedhhinc", "pctunemp", "pctmulti")]
vif(lm2.midlife)[c("lnpop100", "pctnhw", "pctforeign", "pctpro", "lnmedhhinc", "pctunemp", "pctmulti")]
vif(lm2.middle)[c("lnpop100", "pctnhw", "pctforeign", "pctpro", "lnmedhhinc", "pctunemp", "pctmulti")]

stargazer(lm2.youngest, lm2.young, lm2.midlife, lm2.middle, type="text",  
          title="Net Migration in 1990-2010", omit=c("UA01", "UA02", "UA03", "UA04", "UA05", 
                                                     "UA06", "UA07", "UA08", "UA09", "UA10", 
                                                     "UA11", "UA12", "UA13", "UA14", "UA15", 
                                                     "UA16", "UA17", "UA18", "UA19", "Constant"), 
          ci.level=0.9, report="vc*", omit.stat=c("f", "ser"))

stargazer(lm.youngest, lm.young, lm.midlife, lm.middle, 
          lm2.youngest, lm2.young, lm2.midlife, lm2.middle, type="text",  
          title="Net Migration", 
          keep = c("cc8090_5", "cc9000_5", "c0010_5", "lnpden8090", "lnpden9000", "lnpden0010", 
                   "One_Transit8090", "One_Transit9000", "One_Transit0010", 
                   "f_ccity9000b", "f_ccity0010b", "f_pden9000b", "f_pden0010b", 
                   "f_pt9000b", "f_pt0010b", "lncden9000b", "lncden0010b", "yr9000", "yr0010"),
          order=c(1, 2, 3, 11, 12, 4, 5, 6, 13, 14, 7, 8, 9, 15, 16, 17, 18, 10, 19), 
          #omit=c("UA01", "UA02", "UA03", "UA04", "UA05", 
          #"UA06", "UA07", "UA08", "UA09", "UA10", 
          #"UA11", "UA12", "UA13", "UA14", "UA15", 
          #"UA16", "UA17", "UA18", "UA19", "Constant"), 
          ci.level=0.9, report="vc*", omit.stat=c("f", "ser"))


#5. Quantile at the 50th percentile: 3-decade models 

rq.youngest <- rq(Migr_youngest ~ #ln_dist_cbd8090 + ln_dist_cbd9000 + ln_dist_cbd0010 + 
                    cc8090_5 + cc9000_5 + cc0010_5 + lnpden8090 + lnpden9000 + lnpden0010 + 
                    One_Transit8090 + One_Transit9000 + One_Transit0010 + 
                    yr9000 + yr0010 +  
                    lnpop100 + pctyoungestbg + pctnhw + pctforeign + pctpro +  
                    lnmedhhinc + pctunemp + pctmulti +  
                    UA01 + UA02 + UA03 + UA04 + UA05 + UA06 + UA07 + UA08 + UA09 + UA10 +
                    UA11 + UA12 + UA13 + UA14 + UA15 + UA16 + UA17 + UA18 + UA19, tau=0.5, data=migrdata)

rq.young <- rq(Migr_young ~ #ln_dist_cbd8090 + ln_dist_cbd9000 + ln_dist_cbd0010 + 
                 cc8090_5 + cc9000_5 + cc0010_5 + lnpden8090 + lnpden9000 + lnpden0010 + 
                 One_Transit8090 + One_Transit9000 + One_Transit0010 + 
                 yr9000 + yr0010 +  
                 lnpop100 + pctyoungbg + pctnhw + pctforeign + pctpro +  
                 lnmedhhinc + pctunemp + pctmulti +  
                 UA01 + UA02 + UA03 + UA04 + UA05 + UA06 + UA07 + UA08 + UA09 + UA10 +
                 UA11 + UA12 + UA13 + UA14 + UA15 + UA16 + UA17 + UA18 + UA19, tau=0.5, data=migrdata)

rq.midlife <- rq(Migr_midlife ~ #ln_dist_cbd8090 + ln_dist_cbd9000 + ln_dist_cbd0010 + 
                   cc8090_5 + cc9000_5 + cc0010_5 + lnpden8090 + lnpden9000 + lnpden0010 + 
                   One_Transit8090 + One_Transit9000 + One_Transit0010 + 
                   yr9000 + yr0010 +  
                   lnpop100 + pctmidlifebg + pctnhw + pctforeign + pctpro +  
                   lnmedhhinc + pctunemp + pctmulti +  
                   UA01 + UA02 + UA03 + UA04 + UA05 + UA06 + UA07 + UA08 + UA09 + UA10 +
                   UA11 + UA12 + UA13 + UA14 + UA15 + UA16 + UA17 + UA18 + UA19, tau=0.5, data=migrdata)

rq.middle <- rq(Migr_middle ~ #ln_dist_cbd8090 + ln_dist_cbd9000 + ln_dist_cbd0010 + 
                  cc8090_5 + cc9000_5 + cc0010_5 + lnpden8090 + lnpden9000 + lnpden0010 + 
                  One_Transit8090 + One_Transit9000 + One_Transit0010 + 
                  yr9000 + yr0010 +  
                  lnpop100 + pctmiddlebg + pctnhw + pctforeign + pctpro +  
                  lnmedhhinc + pctunemp + pctmulti +  
                  UA01 + UA02 + UA03 + UA04 + UA05 + UA06 + UA07 + UA08 + UA09 + UA10 +
                  UA11 + UA12 + UA13 + UA14 + UA15 + UA16 + UA17 + UA18 + UA19, tau=0.5, data=migrdata)

stargazer(rq.youngest, rq.young, rq.midlife, rq.middle, type="text", 
          title="Net Migration in 1980-2010 (quantile regression)", 
          omit=c("UA01", "UA02", "UA03", "UA04", "UA05", "UA06", "UA07", "UA08", "UA09", "UA10", 
                 "UA11", "UA12", "UA13", "UA14", "UA15", "UA16", "UA17", "UA18", "UA19", "Constant"), 
          ci.level=0.9, report="vc*", omit.stat=c("f", "ser"))#, 
          #order=c(1, 2, 3, 11, 12, 4, 5, 6, 13, 14, 7, 8, 9, 17, 18, 15, 16))  


#6. Quantile at the 50th percentile: 2-decade models 

rq2.youngest <- rq(Migr_youngest ~ #ln_dist_cbd8090 + ln_dist_cbd9000 + ln_dist_cbd0010 + 
                     f_ccity9000b + f_ccity0010b + f_pden9000b + f_pden0010b +  
                     f_pt9000b + f_pt0010b + lncden9000b + lncden0010b +  
                     yr0010 +  
                     lnpop100 + pctyoungestbg + pctnhw + pctforeign + pctpro +  
                     lnmedhhinc + pctunemp + pctmulti +  
                     UA01 + UA02 + UA03 + UA04 + UA05 + UA06 + UA07 + UA08 + UA09 + UA10 +
                     UA11 + UA12 + UA13 + UA14 + UA15 + UA16 + UA17 + UA18 + UA19, tau=0.5, 
                     data=migrdata[migrdata$yr8090 != 1, ])

rq2.young <- rq(Migr_young ~ #ln_dist_cbd8090 + ln_dist_cbd9000 + ln_dist_cbd0010 + 
                  f_ccity9000b + f_ccity0010b + f_pden9000b + f_pden0010b +  
                  f_pt9000b + f_pt0010b + lncden9000b + lncden0010b + 
                  yr0010 +  
                  lnpop100 + pctyoungbg + pctnhw + pctforeign + pctpro +  
                  lnmedhhinc + pctunemp + pctmulti +  
                  UA01 + UA02 + UA03 + UA04 + UA05 + UA06 + UA07 + UA08 + UA09 + UA10 +
                  UA11 + UA12 + UA13 + UA14 + UA15 + UA16 + UA17 + UA18 + UA19, tau=0.5,  
                data=migrdata[migrdata$yr8090 != 1, ])

rq2.midlife <- rq(Migr_midlife ~ #ln_dist_cbd8090 + ln_dist_cbd9000 + ln_dist_cbd0010 + 
                    f_ccity9000b + f_ccity0010b + f_pden9000b + f_pden0010b +  
                    f_pt9000b + f_pt0010b + lncden9000b + lncden0010b + 
                    yr0010 +  
                    lnpop100 + pctmidlifebg + pctnhw + pctforeign + pctpro +  
                    lnmedhhinc + pctunemp + pctmulti +  
                    UA01 + UA02 + UA03 + UA04 + UA05 + UA06 + UA07 + UA08 + UA09 + UA10 +
                    UA11 + UA12 + UA13 + UA14 + UA15 + UA16 + UA17 + UA18 + UA19, tau=0.5,  
                  data=migrdata[migrdata$yr8090 != 1, ])

rq2.middle <- rq(Migr_middle ~ #ln_dist_cbd8090 + ln_dist_cbd9000 + ln_dist_cbd0010 + 
                   f_ccity9000b + f_ccity0010b + f_pden9000b + f_pden0010b +  
                   f_pt9000b + f_pt0010b + lncden9000b + lncden0010b + 
                   yr0010 +  
                   lnpop100 + pctmiddlebg + pctnhw + pctforeign + pctpro +  
                   lnmedhhinc + pctunemp + pctmulti +  
                   UA01 + UA02 + UA03 + UA04 + UA05 + UA06 + UA07 + UA08 + UA09 + UA10 +
                   UA11 + UA12 + UA13 + UA14 + UA15 + UA16 + UA17 + UA18 + UA19, tau=0.5, 
                 data=migrdata[migrdata$yr8090 != 1, ])


stargazer(rq.youngest, rq.young, rq.midlife, rq.middle, 
          rq2.youngest, rq2.young, rq2.midlife, rq2.middle, type="text", 
          title="Net Migration (quantile regression)", 
          keep = c("cc8090_5", "cc9000_5", "c0010_5", "lnpden8090", "lnpden9000", "lnpden0010", 
                   "One_Transit8090", "One_Transit9000", "One_Transit0010", 
                   "f_ccity9000b", "f_ccity0010b", "f_pden9000b", "f_pden0010b", 
                   "f_pt9000b", "f_pt0010b", "lncden9000b", "lncden0010b", "yr9000", "yr0010"),
          order=c(1, 2, 3, 11, 12, 4, 5, 6, 13, 14, 7, 8, 9, 15, 16, 17, 18), 
          #omit=c("UA01", "UA02", "UA03", "UA04", "UA05", 
          #"UA06", "UA07", "UA08", "UA09", "UA10", 
          #"UA11", "UA12", "UA13", "UA14", "UA15", 
          #"UA16", "UA17", "UA18", "UA19", "Constant"), 
          ci.level=0.9, report="vc*", omit.stat=c("f", "ser"))#, 
#order=c(1, 2, 3, 11, 12, 4, 5, 6, 13, 14, 7, 8, 9, 17, 18, 15, 16))  




