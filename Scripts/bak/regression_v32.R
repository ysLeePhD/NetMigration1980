
install.packages("sas7bdat", dependencies = TRUE)
install.packages("tidyverse", dependencies = TRUE)
install.packages("stargazer", dependencies = TRUE)
install.packages("car", dependencies = TRUE) # for vif test 
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


#2. prepare the main dataset 

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

## search for the best specification (w/wo school quality)

lm.youngest.s1 <- lm(Migr_youngest ~ #ln_dist_cbd8090 + ln_dist_cbd9000 + ln_dist_cbd0010 + 
                     cc8090_5 + cc9000_5 + cc0010_5 +  
                     One_Transit8090 + One_Transit9000 + One_Transit0010 + 
                     yr9000 + yr0010 +  
                     pctyoungestbg + pctnhw + pctforeign + pctunemp +  
                     UA01 + UA02 + UA03 + UA04 + UA05 + UA06 + UA07 + UA08 + UA09 + UA10 +
                     UA11 + UA12 + UA13 + UA14 + UA15 + UA16 + UA17 + UA18 + UA19, 
                     data=migrdata[migrdata$Migr_youngest > quantile(migrdata$Migr_youngest, 0.01) & 
                                   migrdata$Migr_youngest < quantile(migrdata$Migr_youngest, 0.99), ])

lm.youngest.s2 <- lm(Migr_youngest ~ #ln_dist_cbd8090 + ln_dist_cbd9000 + ln_dist_cbd0010 + 
                     lnpden8090 + lnpden9000 + lnpden0010 + 
                     One_Transit8090 + One_Transit9000 + One_Transit0010 + 
                     yr9000 + yr0010 +  
                     pctyoungestbg + pctnhw + pctforeign + pctunemp +  
                     UA01 + UA02 + UA03 + UA04 + UA05 + UA06 + UA07 + UA08 + UA09 + UA10 +
                     UA11 + UA12 + UA13 + UA14 + UA15 + UA16 + UA17 + UA18 + UA19, 
                   data=migrdata[migrdata$Migr_youngest > quantile(migrdata$Migr_youngest, 0.01) & 
                                   migrdata$Migr_youngest < quantile(migrdata$Migr_youngest, 0.99), ])

lm.youngest.s3 <- lm(Migr_youngest ~ #ln_dist_cbd8090 + ln_dist_cbd9000 + ln_dist_cbd0010 + 
                     cc8090_5 + cc9000_5 + cc0010_5 + lnpden8090 + lnpden9000 + lnpden0010 + 
                     One_Transit8090 + One_Transit9000 + One_Transit0010 + 
                     yr9000 + yr0010 +  
                     pctyoungestbg + pctnhw + pctforeign + pctunemp +  
                     UA01 + UA02 + UA03 + UA04 + UA05 + UA06 + UA07 + UA08 + UA09 + UA10 +
                     UA11 + UA12 + UA13 + UA14 + UA15 + UA16 + UA17 + UA18 + UA19, 
                   data=migrdata[migrdata$Migr_youngest > quantile(migrdata$Migr_youngest, 0.01) & 
                                   migrdata$Migr_youngest < quantile(migrdata$Migr_youngest, 0.99), ])

lm.youngest.s4 <- lm(Migr_youngest ~ #ln_dist_cbd8090 + ln_dist_cbd9000 + ln_dist_cbd0010 + 
                     cc8090_5 + cc9000_5 + cc0010_5 + lnpden8090 + lnpden9000 + lnpden0010 + 
                     One_Transit8090 + One_Transit9000 + One_Transit0010 + 
                     yr9000 + yr0010 +  
                     lnpop100 + pctyoungestbg + pctnhw + pctforeign + pctunemp + 
                     UA01 + UA02 + UA03 + UA04 + UA05 + UA06 + UA07 + UA08 + UA09 + UA10 +
                     UA11 + UA12 + UA13 + UA14 + UA15 + UA16 + UA17 + UA18 + UA19, 
                   data=migrdata[migrdata$Migr_youngest > quantile(migrdata$Migr_youngest, 0.01) & 
                                   migrdata$Migr_youngest < quantile(migrdata$Migr_youngest, 0.99), ])

lm.youngest.s5 <- lm(Migr_youngest ~ #ln_dist_cbd8090 + ln_dist_cbd9000 + ln_dist_cbd0010 + 
                     cc8090_5 + cc9000_5 + cc0010_5 + lnpden8090 + lnpden9000 + lnpden0010 + 
                     One_Transit8090 + One_Transit9000 + One_Transit0010 + 
                     yr9000 + yr0010 +  
                     lnpop100 + pctyoungestbg + pctnhw + pctforeign + pctunemp + pctmulti +  
                     UA01 + UA02 + UA03 + UA04 + UA05 + UA06 + UA07 + UA08 + UA09 + UA10 +
                     UA11 + UA12 + UA13 + UA14 + UA15 + UA16 + UA17 + UA18 + UA19, 
                   data=migrdata[migrdata$Migr_youngest > quantile(migrdata$Migr_youngest, 0.01) & 
                                   migrdata$Migr_youngest < quantile(migrdata$Migr_youngest, 0.99), ])

lm.youngest.s6 <- lm(Migr_youngest ~ #ln_dist_cbd8090 + ln_dist_cbd9000 + ln_dist_cbd0010 + 
                     cc8090_5 + cc9000_5 + cc0010_5 + lnpden8090 + lnpden9000 + lnpden0010 + 
                     One_Transit8090 + One_Transit9000 + One_Transit0010 + 
                     yr9000 + yr0010 +  
                     lnpop100 + pctyoungestbg + pctnhw + pctforeign + pctunemp + lnmedhhinc + pctmulti +  
                     UA01 + UA02 + UA03 + UA04 + UA05 + UA06 + UA07 + UA08 + UA09 + UA10 +
                     UA11 + UA12 + UA13 + UA14 + UA15 + UA16 + UA17 + UA18 + UA19, 
                   data=migrdata[migrdata$Migr_youngest > quantile(migrdata$Migr_youngest, 0.01) & 
                                   migrdata$Migr_youngest < quantile(migrdata$Migr_youngest, 0.99), ])

lm.youngest.s7 <- lm(Migr_youngest ~ #ln_dist_cbd8090 + ln_dist_cbd9000 + ln_dist_cbd0010 + 
                     cc8090_5 + cc9000_5 + cc0010_5 + lnpden8090 + lnpden9000 + lnpden0010 + 
                     One_Transit8090 + One_Transit9000 + One_Transit0010 + 
                     yr9000 + yr0010 +  
                     lnpop100 + pctyoungestbg + pctnhw + pctforeign + pctunemp + lnhvalue + pctmulti +  
                     UA01 + UA02 + UA03 + UA04 + UA05 + UA06 + UA07 + UA08 + UA09 + UA10 +
                     UA11 + UA12 + UA13 + UA14 + UA15 + UA16 + UA17 + UA18 + UA19, 
                   data=migrdata[migrdata$Migr_youngest > quantile(migrdata$Migr_youngest, 0.01) & 
                                   migrdata$Migr_youngest < quantile(migrdata$Migr_youngest, 0.99), ])

lm.youngest.s8 <- lm(Migr_youngest ~ #ln_dist_cbd8090 + ln_dist_cbd9000 + ln_dist_cbd0010 + 
                     cc8090_5 + cc9000_5 + cc0010_5 + lnpden8090 + lnpden9000 + lnpden0010 + 
                     One_Transit8090 + One_Transit9000 + One_Transit0010 + 
                     yr9000 + yr0010 +  
                     lnpop100 + pctyoungestbg + pctnhw + pctforeign + pctunemp + lnrent + pctmulti + 
                     UA01 + UA02 + UA03 + UA04 + UA05 + UA06 + UA07 + UA08 + UA09 + UA10 +
                     UA11 + UA12 + UA13 + UA14 + UA15 + UA16 + UA17 + UA18 + UA19, 
                   data=migrdata[migrdata$Migr_youngest > quantile(migrdata$Migr_youngest, 0.01) & 
                                   migrdata$Migr_youngest < quantile(migrdata$Migr_youngest, 0.99), ])

lm.youngest.s9 <- lm(Migr_youngest ~ #ln_dist_cbd8090 + ln_dist_cbd9000 + ln_dist_cbd0010 + 
                      cc8090_5 + cc9000_5 + cc0010_5 + lnpden8090 + lnpden9000 + lnpden0010 + 
                      One_Transit8090 + One_Transit9000 + One_Transit0010 + 
                      yr9000 + yr0010 +  
                      lnpop100 + pctyoungestbg + pctnhw + pctforeign + pctunemp + pctcoll + pctmulti +  
                      UA01 + UA02 + UA03 + UA04 + UA05 + UA06 + UA07 + UA08 + UA09 + UA10 +
                      UA11 + UA12 + UA13 + UA14 + UA15 + UA16 + UA17 + UA18 + UA19, 
                    data=migrdata[migrdata$Migr_youngest > quantile(migrdata$Migr_youngest, 0.01) & 
                                    migrdata$Migr_youngest < quantile(migrdata$Migr_youngest, 0.99), ])

lm.youngest.s10 <- lm(Migr_youngest ~ #ln_dist_cbd8090 + ln_dist_cbd9000 + ln_dist_cbd0010 + 
                      cc8090_5 + cc9000_5 + cc0010_5 + lnpden8090 + lnpden9000 + lnpden0010 + 
                      One_Transit8090 + One_Transit9000 + One_Transit0010 + 
                      yr9000 + yr0010 +  
                      lnpop100 + pctyoungestbg + pctnhw + pctforeign + pctunemp + pctpro + pctmulti +  
                      UA01 + UA02 + UA03 + UA04 + UA05 + UA06 + UA07 + UA08 + UA09 + UA10 +
                      UA11 + UA12 + UA13 + UA14 + UA15 + UA16 + UA17 + UA18 + UA19, 
                    data=migrdata[migrdata$Migr_youngest > quantile(migrdata$Migr_youngest, 0.01) & 
                                    migrdata$Migr_youngest < quantile(migrdata$Migr_youngest, 0.99), ])

lm.youngest.s11 <- lm(Migr_youngest ~ #ln_dist_cbd8090 + ln_dist_cbd9000 + ln_dist_cbd0010 + 
                      cc8090_5 + cc9000_5 + cc0010_5 + lnpden8090 + lnpden9000 + lnpden0010 + 
                      One_Transit8090 + One_Transit9000 + One_Transit0010 + 
                      yr9000 + yr0010 +  
                      lnpop100 + pctyoungestbg + pctnhw + pctforeign + pctunemp + pctpro + lnmedhhinc + pctmulti + 
                      UA01 + UA02 + UA03 + UA04 + UA05 + UA06 + UA07 + UA08 + UA09 + UA10 +
                      UA11 + UA12 + UA13 + UA14 + UA15 + UA16 + UA17 + UA18 + UA19, 
                    data=migrdata[migrdata$Migr_youngest > quantile(migrdata$Migr_youngest, 0.01) & 
                                    migrdata$Migr_youngest < quantile(migrdata$Migr_youngest, 0.99), ]) 

lm.youngest.s12 <- lm(Migr_youngest ~ #ln_dist_cbd8090 + ln_dist_cbd9000 + ln_dist_cbd0010 + 
                      cc8090_5 + cc9000_5 + cc0010_5 + lnpden8090 + lnpden9000 + lnpden0010 + 
                      One_Transit8090 + One_Transit9000 + One_Transit0010 + 
                      yr9000 + yr0010 +  
                      lnpop100 + pctyoungestbg + pctnhw + pctforeign + pctunemp + pctpro + lnmedhhinc + pctmulti + elem + 
                      UA01 + UA02 + UA03 + UA04 + UA05 + UA06 + UA07 + UA08 + UA09 + UA10 +
                      UA11 + UA12 + UA13 + UA14 + UA15 + UA16 + UA17 + UA18 + UA19, 
                    data=migrdata[migrdata$Migr_youngest > quantile(migrdata$Migr_youngest, 0.01) & 
                                    migrdata$Migr_youngest < quantile(migrdata$Migr_youngest, 0.99), ])

#vif(lm.youngest.s1)[c("lnpop100", "pctyoungestbg", "pctnhw", "pctforeign", "pctcoll", "pctpro", "lnmedhhinc", "lnhvalue", "lnrent", "pctunemp", "pctmulti", "elem")]
#vif(lm.youngest.s2)[c("lnpop100", "pctyoungestbg", "pctnhw", "pctforeign", "pctcoll", "pctpro", "lnmedhhinc", "lnhvalue", "lnrent", "pctunemp", "pctmulti", "elem")]
#vif(lm.youngest.s3)[c("lnpop100", "pctyoungestbg", "pctnhw", "pctforeign", "pctcoll", "pctpro", "lnmedhhinc", "lnhvalue", "lnrent", "pctunemp", "pctmulti", "elem")]
#vif(lm.youngest.s4)[c("lnpop100", "pctyoungestbg", "pctnhw", "pctforeign", "pctcoll", "pctpro", "lnmedhhinc", "lnhvalue", "lnrent", "pctunemp", "pctmulti", "elem")]
#vif(lm.youngest.s5)[c("lnpop100", "pctyoungestbg", "pctnhw", "pctforeign", "pctcoll", "pctpro", "lnmedhhinc", "lnhvalue", "lnrent", "pctunemp", "pctmulti", "elem")]
#vif(lm.youngest.s6)[c("lnpop100", "pctyoungestbg", "pctnhw", "pctforeign", "pctcoll", "pctpro", "lnmedhhinc", "lnhvalue", "lnrent", "pctunemp", "pctmulti", "elem")]
#vif(lm.youngest.s7)[c("lnpop100", "pctyoungestbg", "pctnhw", "pctforeign", "pctcoll", "pctpro", "lnmedhhinc", "lnhvalue", "lnrent", "pctunemp", "pctmulti", "elem")]
#vif(lm.youngest.s8)[c("lnpop100", "pctyoungestbg", "pctnhw", "pctforeign", "pctcoll", "pctpro", "lnmedhhinc", "lnhvalue", "lnrent", "pctunemp", "pctmulti", "elem")]
#vif(lm.youngest.s9)[c("lnpop100", "pctyoungestbg", "pctnhw", "pctforeign", "pctcoll", "pctpro", "lnmedhhinc", "lnhvalue", "lnrent", "pctunemp", "pctmulti", "elem")]
#vif(lm.youngest.s10)[c("lnpop100", "pctyoungestbg", "pctnhw", "pctforeign", "pctcoll", "pctpro", "lnmedhhinc", "lnhvalue", "lnrent", "pctunemp", "pctmulti", "elem")]

vif(lm.youngest.s11)[c("lnpop100", "pctyoungestbg", "pctnhw", "pctforeign", "pctcoll", "pctpro", "lnmedhhinc", "lnhvalue", "lnrent", "pctunemp", "pctmulti", "elem")]
vif(lm.youngest.s12)[c("lnpop100", "pctyoungestbg", "pctnhw", "pctforeign", "pctcoll", "pctpro", "lnmedhhinc", "lnhvalue", "lnrent", "pctunemp", "pctmulti", "elem")]

stargazer(#lm.youngest.s1, #lm.youngest.s2, lm.youngest.s3, lm.youngest.s4, 
  lm.youngest.s5, lm.youngest.s6, lm.youngest.s7, lm.youngest.s8, 
  lm.youngest.s9, lm.youngest.s10, lm.youngest.s11, lm.youngest.s12, type="text",  
  title="Net Migration of 20-24 in 1980-2010", omit=c("UA01", "UA02", "UA03", "UA04", "UA05", 
                                                      "UA06", "UA07", "UA08", "UA09", "UA10", 
                                                      "UA11", "UA12", "UA13", "UA14", "UA15", 
                                                      "UA16", "UA17", "UA18", "UA19", "Constant"), 
  ci.level=0.9, report="vc*", omit.stat=c("f", "ser")) #(don't run)


## compare results from different central city definitions 

cc.cutoff <- 1:450/10

migrdata$cc8090.contd <- 0 
migrdata$cc9000.contd <- 0 
migrdata$cc0010.contd <- 0 

youngest.cc.coeff.contd <- data.frame(
  cc8090.contd=numeric(), 
  cc9000.contd=numeric(),
  cc0010.contd=numeric())

young.cc.coeff.contd <- data.frame(
  cc8090.contd=numeric(), 
  cc9000.contd=numeric(),
  cc0010.contd=numeric())

midlife.cc.coeff.contd <- data.frame(
  cc8090.contd=numeric(), 
  cc9000.contd=numeric(),
  cc0010.contd=numeric())

middle.cc.coeff.contd <- data.frame(
  cc8090.contd=numeric(), 
  cc9000.contd=numeric(),
  cc0010.contd=numeric())


for (i in 1:450){
  migrdata$cc8090.contd <- ifelse(migrdata$yr8090==1 & exp(migrdata$ln_dist_cbd8090)-1 < cc.cutoff[i], 1, migrdata$cc8090.contd)
  migrdata$cc9000.contd <- ifelse(migrdata$yr9000==1 & exp(migrdata$ln_dist_cbd9000)-1 < cc.cutoff[i], 1, migrdata$cc9000.contd)
  migrdata$cc0010.contd <- ifelse(migrdata$yr0010==1 & exp(migrdata$ln_dist_cbd0010)-1 < cc.cutoff[i], 1, migrdata$cc0010.contd)
  
  lm.youngest.contd <- lm(Migr_youngest ~ #ln_dist_cbd8090 + ln_dist_cbd9000 + ln_dist_cbd0010 + 
                            cc8090.contd + cc9000.contd + cc0010.contd + lnpden8090 + lnpden9000 + lnpden0010 + 
                            One_Transit8090 + One_Transit9000 + One_Transit0010 + yr9000 + yr0010 +  
                            lnpop100 + pctyoungestbg + pctnhw + pctforeign + pctpro + lnmedhhinc + pctunemp + pctmulti +  
                            UA01 + UA02 + UA03 + UA04 + UA05 + UA06 + UA07 + UA08 + UA09 + UA10 +
                            UA11 + UA12 + UA13 + UA14 + UA15 + UA16 + UA17 + UA18 + UA19, 
                          data=migrdata[migrdata$Migr_youngest > quantile(migrdata$Migr_youngest, 0.01) & 
                                          migrdata$Migr_youngest < quantile(migrdata$Migr_youngest, 0.99), ])
  
  youngest.results <- summary(lm.youngest.contd)$coefficients %>% 
    as.data.frame() 
  youngest.cc.coeff.contd[i, ]<- youngest.results[2:4, 1]
  
  lm.young.contd <- lm(Migr_young ~ #ln_dist_cbd8090 + ln_dist_cbd9000 + ln_dist_cbd0010 + 
                         cc8090.contd + cc9000.contd + cc0010.contd + lnpden9000 + lnpden0010 + 
                         One_Transit8090 + One_Transit9000 + One_Transit0010 + yr9000 + yr0010 +  
                         lnpop100 + pctyoungbg + pctnhw + pctforeign + pctpro + lnmedhhinc + pctunemp + pctmulti +  
                         UA01 + UA02 + UA03 + UA04 + UA05 + UA06 + UA07 + UA08 + UA09 + UA10 +
                         UA11 + UA12 + UA13 + UA14 + UA15 + UA16 + UA17 + UA18 + UA19, 
                       data=migrdata[migrdata$Migr_young > quantile(migrdata$Migr_young, 0.01) & 
                                       migrdata$Migr_young < quantile(migrdata$Migr_young, 0.99), ])
  
  young.results <- summary(lm.young.contd)$coefficients %>% 
    as.data.frame() 
  
  young.cc.coeff.contd[i, ]<- young.results[2:4, 1]
  
  lm.midlife.contd <- lm(Migr_midlife ~ #ln_dist_cbd8090 + ln_dist_cbd9000 + ln_dist_cbd0010 + 
                           cc8090.contd + cc9000.contd + cc0010.contd + lnpden9000 + lnpden0010 + 
                           One_Transit8090 + One_Transit9000 + One_Transit0010 + yr9000 + yr0010 +  
                           lnpop100 + pctmidlifebg + pctnhw + pctforeign + pctpro + lnmedhhinc + pctunemp + pctmulti +  
                           UA01 + UA02 + UA03 + UA04 + UA05 + UA06 + UA07 + UA08 + UA09 + UA10 +
                           UA11 + UA12 + UA13 + UA14 + UA15 + UA16 + UA17 + UA18 + UA19, 
                         data=migrdata[migrdata$Migr_midlife > quantile(migrdata$Migr_midlife, 0.01) & 
                                         migrdata$Migr_midlife < quantile(migrdata$Migr_midlife, 0.99), ])
  
  midlife.results <- summary(lm.midlife.contd)$coefficients %>% 
    as.data.frame() 
  midlife.cc.coeff.contd[i, ]<- midlife.results[2:4, 1]
  
  lm.middle.contd <- lm(Migr_middle ~ #ln_dist_cbd8090 + ln_dist_cbd9000 + ln_dist_cbd0010 + 
                          cc8090.contd + cc9000.contd + cc0010.contd + lnpden9000 + lnpden0010 + 
                          One_Transit8090 + One_Transit9000 + One_Transit0010 + yr9000 + yr0010 +  
                          lnpop100 + pctmiddlebg + pctnhw + pctforeign + pctpro +  lnmedhhinc + pctunemp + pctmulti +  
                          UA01 + UA02 + UA03 + UA04 + UA05 + UA06 + UA07 + UA08 + UA09 + UA10 +
                          UA11 + UA12 + UA13 + UA14 + UA15 + UA16 + UA17 + UA18 + UA19, 
                        data=migrdata[migrdata$Migr_middle > quantile(migrdata$Migr_middle, 0.01) & 
                                        migrdata$Migr_middle < quantile(migrdata$Migr_middle, 0.99), ])
  
  middle.results <- summary(lm.middle.contd)$coefficients %>% 
    as.data.frame() 
  middle.cc.coeff.contd[i, ]<- middle.results[2:4, 1]  
}

youngest.cc.coeff.contd$miles <- 1:450/10
youngest.cc.coeff.contd <- youngest.cc.coeff.contd %>% 
  gather(`cc8090.contd`, `cc9000.contd`, `cc0010.contd`, key = "decade", value = "coefficients") 
youngest.cc.coeff.contd$decade <- factor(youngest.cc.coeff.contd$decade, 
                                         levels = c("cc8090.contd", "cc9000.contd", "cc0010.contd"))
young.cc.coeff.contd$miles <- 1:450/10
young.cc.coeff.contd <- young.cc.coeff.contd %>% 
  gather(`cc8090.contd`, `cc9000.contd`, `cc0010.contd`, key = "decade", value = "coefficients") 
young.cc.coeff.contd$decade <- factor(young.cc.coeff.contd$decade, 
                                         levels = c("cc8090.contd", "cc9000.contd", "cc0010.contd"))
midlife.cc.coeff.contd$miles <- 1:450/10
midlife.cc.coeff.contd <- midlife.cc.coeff.contd %>% 
  gather(`cc8090.contd`, `cc9000.contd`, `cc0010.contd`, key = "decade", value = "coefficients") 
midlife.cc.coeff.contd$decade <- factor(midlife.cc.coeff.contd$decade, 
                                         levels = c("cc8090.contd", "cc9000.contd", "cc0010.contd"))
middle.cc.coeff.contd$miles <- 1:450/10
middle.cc.coeff.contd <- middle.cc.coeff.contd %>% 
  gather(`cc8090.contd`, `cc9000.contd`, `cc0010.contd`, key = "decade", value = "coefficients") 
middle.cc.coeff.contd$decade <- factor(middle.cc.coeff.contd$decade, 
                                         levels = c("cc8090.contd", "cc9000.contd", "cc0010.contd"))

ggplot(data = youngest.cc.coeff.contd) + 
  geom_smooth(mapping = aes(x = miles, y = coefficients, color = decade))+ 
  geom_hline(yintercept=0, color="grey") + 
  geom_vline(xintercept=3, color="skyblue") + 
  geom_vline(xintercept=5, color="skyblue3") + 
  coord_cartesian(ylim = c(-0.4, 0.4)) + 
  ggtitle("The Coefficients of the Central City Dummy (from 0.1 to 45.0 by 0.1 interval) for the Youngest Cohort")

ggplot(data = young.cc.coeff.contd) + 
  geom_smooth(mapping = aes(x = miles, y = coefficients, color = decade))+ 
  geom_hline(yintercept=0, color="grey") + 
  geom_vline(xintercept=3, color="skyblue") + 
  geom_vline(xintercept=5, color="skyblue3") + 
  coord_cartesian(ylim = c(-0.4, 0.4)) + 
  ggtitle("The Coefficients of the Central City Dummy (from 0.1 to 45.0 by 0.1 interval) for the Young Cohort")

ggplot(data = midlife.cc.coeff.contd) + 
  geom_smooth(mapping = aes(x = miles, y = coefficients, color = decade))+ 
  geom_hline(yintercept=0, color="grey") + 
  geom_vline(xintercept=3, color="skyblue") + 
  geom_vline(xintercept=5, color="skyblue3") + 
  coord_cartesian(ylim = c(-0.4, 0.4)) + 
  ggtitle("The Coefficients of the Central City Dummy (from 0.1 to 45.0 by 0.1 interval) for the Midlife Cohort")

ggplot(data = middle.cc.coeff.contd) + 
  geom_smooth(mapping = aes(x = miles, y = coefficients, color = decade))+ 
  geom_hline(yintercept=0, color="grey") + 
  geom_vline(xintercept=3, color="skyblue") + 
  geom_vline(xintercept=5, color="skyblue3") + 
  coord_cartesian(ylim = c(-0.4, 0.4)) + 
  ggtitle("The Coefficients of the Central City Dummy (from 0.1 to 45.0 by 0.1 interval) for the Middle Cohort")


lm.youngest <- lm(Migr_youngest ~ #ln_dist_cbd8090 + ln_dist_cbd9000 + ln_dist_cbd0010 + 
                      cc8090 + cc9000 + cc0010 + lnpden8090 + lnpden9000 + lnpden0010 + 
                      One_Transit8090 + One_Transit9000 + One_Transit0010 + 
                      yr9000 + yr0010 +  
                      lnpop100 + pctyoungestbg + pctnhw + pctforeign + pctpro +  
                      lnmedhhinc + pctunemp + pctmulti +  
                      UA01 + UA02 + UA03 + UA04 + UA05 + UA06 + UA07 + UA08 + UA09 + UA10 +
                      UA11 + UA12 + UA13 + UA14 + UA15 + UA16 + UA17 + UA18 + UA19, 
                    data=migrdata[migrdata$Migr_youngest > quantile(migrdata$Migr_youngest, 0.01) & 
                                    migrdata$Migr_youngest < quantile(migrdata$Migr_youngest, 0.99), ])

lm.young <- lm(Migr_young ~ #ln_dist_cbd8090 + ln_dist_cbd9000 + ln_dist_cbd0010 + 
                 cc8090 + cc9000 + cc0010 + lnpden8090 + lnpden9000 + lnpden0010 + 
                    One_Transit8090 + One_Transit9000 + One_Transit0010 + 
                    yr9000 + yr0010 +  
                    lnpop100 + pctyoungbg + pctnhw + pctforeign + pctpro +  
                    lnmedhhinc + pctunemp + pctmulti +  
                    UA01 + UA02 + UA03 + UA04 + UA05 + UA06 + UA07 + UA08 + UA09 + UA10 +
                    UA11 + UA12 + UA13 + UA14 + UA15 + UA16 + UA17 + UA18 + UA19, 
                  data=migrdata[migrdata$Migr_young > quantile(migrdata$Migr_young, 0.01) & 
                                  migrdata$Migr_young < quantile(migrdata$Migr_young, 0.99), ])

lm.midlife <- lm(Migr_midlife ~ #ln_dist_cbd8090 + ln_dist_cbd9000 + ln_dist_cbd0010 + 
                   cc8090 + cc9000 + cc0010 + lnpden8090 + lnpden9000 + lnpden0010 + 
                    One_Transit8090 + One_Transit9000 + One_Transit0010 + 
                    yr9000 + yr0010 +  
                    lnpop100 + pctmidlifebg + pctnhw + pctforeign + pctpro +  
                    lnmedhhinc + pctunemp + pctmulti +  
                    UA01 + UA02 + UA03 + UA04 + UA05 + UA06 + UA07 + UA08 + UA09 + UA10 +
                    UA11 + UA12 + UA13 + UA14 + UA15 + UA16 + UA17 + UA18 + UA19, 
                  data=migrdata[migrdata$Migr_midlife > quantile(migrdata$Migr_midlife, 0.01) & 
                                  migrdata$Migr_midlife < quantile(migrdata$Migr_midlife, 0.99), ])

lm.middle <- lm(Migr_middle ~ #ln_dist_cbd8090 + ln_dist_cbd9000 + ln_dist_cbd0010 + 
                  cc8090 + cc9000 + cc0010 + lnpden8090 + lnpden9000 + lnpden0010 + 
                    One_Transit8090 + One_Transit9000 + One_Transit0010 + 
                    yr9000 + yr0010 +  
                    lnpop100 + pctmiddlebg + pctnhw + pctforeign + pctpro +  
                    lnmedhhinc + pctunemp + pctmulti +  
                    UA01 + UA02 + UA03 + UA04 + UA05 + UA06 + UA07 + UA08 + UA09 + UA10 +
                    UA11 + UA12 + UA13 + UA14 + UA15 + UA16 + UA17 + UA18 + UA19, 
                  data=migrdata[migrdata$Migr_middle > quantile(migrdata$Migr_middle, 0.01) & 
                                  migrdata$Migr_middle < quantile(migrdata$Migr_middle, 0.99), ])

lm.youngest3 <- lm(Migr_youngest ~ #ln_dist_cbd8090 + ln_dist_cbd9000 + ln_dist_cbd0010 + 
                    cc8090_3 + cc9000_3 + cc0010_3 + lnpden8090 + lnpden9000 + lnpden0010 + 
                    One_Transit8090 + One_Transit9000 + One_Transit0010 + 
                    yr9000 + yr0010 +  
                    lnpop100 + pctyoungestbg + pctnhw + pctforeign + pctpro +  
                    lnmedhhinc + pctunemp + pctmulti +  
                    UA01 + UA02 + UA03 + UA04 + UA05 + UA06 + UA07 + UA08 + UA09 + UA10 +
                    UA11 + UA12 + UA13 + UA14 + UA15 + UA16 + UA17 + UA18 + UA19, 
                  data=migrdata[migrdata$Migr_youngest > quantile(migrdata$Migr_youngest, 0.01) & 
                                  migrdata$Migr_youngest < quantile(migrdata$Migr_youngest, 0.99), ])

lm.young3 <- lm(Migr_young ~ #ln_dist_cbd8090 + ln_dist_cbd9000 + ln_dist_cbd0010 + 
                  cc8090_3 + cc9000_3 + cc0010_3 + lnpden8090 + lnpden9000 + lnpden0010 + 
                 One_Transit8090 + One_Transit9000 + One_Transit0010 + 
                 yr9000 + yr0010 +  
                 lnpop100 + pctyoungbg + pctnhw + pctforeign + pctpro +  
                 lnmedhhinc + pctunemp + pctmulti +  
                 UA01 + UA02 + UA03 + UA04 + UA05 + UA06 + UA07 + UA08 + UA09 + UA10 +
                 UA11 + UA12 + UA13 + UA14 + UA15 + UA16 + UA17 + UA18 + UA19, 
               data=migrdata[migrdata$Migr_young > quantile(migrdata$Migr_young, 0.01) & 
                               migrdata$Migr_young < quantile(migrdata$Migr_young, 0.99), ])

lm.midlife3 <- lm(Migr_midlife ~ #ln_dist_cbd8090 + ln_dist_cbd9000 + ln_dist_cbd0010 + 
                    cc8090_3 + cc9000_3 + cc0010_3 + lnpden8090 + lnpden9000 + lnpden0010 + 
                   One_Transit8090 + One_Transit9000 + One_Transit0010 + 
                   yr9000 + yr0010 +  
                   lnpop100 + pctmidlifebg + pctnhw + pctforeign + pctpro +  
                   lnmedhhinc + pctunemp + pctmulti +  
                   UA01 + UA02 + UA03 + UA04 + UA05 + UA06 + UA07 + UA08 + UA09 + UA10 +
                   UA11 + UA12 + UA13 + UA14 + UA15 + UA16 + UA17 + UA18 + UA19, 
                 data=migrdata[migrdata$Migr_midlife > quantile(migrdata$Migr_midlife, 0.01) & 
                                 migrdata$Migr_midlife < quantile(migrdata$Migr_midlife, 0.99), ])

lm.middle3 <- lm(Migr_middle ~ #ln_dist_cbd8090 + ln_dist_cbd9000 + ln_dist_cbd0010 + 
                   cc8090_3 + cc9000_3 + cc0010_3 + lnpden8090 + lnpden9000 + lnpden0010 + 
                  One_Transit8090 + One_Transit9000 + One_Transit0010 + 
                  yr9000 + yr0010 +  
                  lnpop100 + pctmiddlebg + pctnhw + pctforeign + pctpro +  
                  lnmedhhinc + pctunemp + pctmulti +  
                  UA01 + UA02 + UA03 + UA04 + UA05 + UA06 + UA07 + UA08 + UA09 + UA10 +
                  UA11 + UA12 + UA13 + UA14 + UA15 + UA16 + UA17 + UA18 + UA19, 
                data=migrdata[migrdata$Migr_middle > quantile(migrdata$Migr_middle, 0.01) & 
                                migrdata$Migr_middle < quantile(migrdata$Migr_middle, 0.99), ])

lm.youngest5 <- lm(Migr_youngest ~ #ln_dist_cbd8090 + ln_dist_cbd9000 + ln_dist_cbd0010 + 
                     cc8090_5 + cc9000_5 + cc0010_5 + lnpden8090 + lnpden9000 + lnpden0010 + 
                     One_Transit8090 + One_Transit9000 + One_Transit0010 + 
                     yr9000 + yr0010 +  
                     lnpop100 + pctyoungestbg + pctnhw + pctforeign + pctpro +  
                     lnmedhhinc + pctunemp + pctmulti +  
                     UA01 + UA02 + UA03 + UA04 + UA05 + UA06 + UA07 + UA08 + UA09 + UA10 +
                     UA11 + UA12 + UA13 + UA14 + UA15 + UA16 + UA17 + UA18 + UA19, 
                   data=migrdata[migrdata$Migr_youngest > quantile(migrdata$Migr_youngest, 0.01) & 
                                   migrdata$Migr_youngest < quantile(migrdata$Migr_youngest, 0.99), ])

lm.young5 <- lm(Migr_young ~ #ln_dist_cbd8090 + ln_dist_cbd9000 + ln_dist_cbd0010 + 
                  cc8090_5 + cc9000_5 + cc0010_5 + lnpden8090 + lnpden9000 + lnpden0010 + 
                  One_Transit8090 + One_Transit9000 + One_Transit0010 + 
                  yr9000 + yr0010 +  
                  lnpop100 + pctyoungbg + pctnhw + pctforeign + pctpro +  
                  lnmedhhinc + pctunemp + pctmulti +  
                  UA01 + UA02 + UA03 + UA04 + UA05 + UA06 + UA07 + UA08 + UA09 + UA10 +
                  UA11 + UA12 + UA13 + UA14 + UA15 + UA16 + UA17 + UA18 + UA19, 
                data=migrdata[migrdata$Migr_young > quantile(migrdata$Migr_young, 0.01) & 
                                migrdata$Migr_young < quantile(migrdata$Migr_young, 0.99), ])

lm.midlife5 <- lm(Migr_midlife ~ #ln_dist_cbd8090 + ln_dist_cbd9000 + ln_dist_cbd0010 + 
                    cc8090_5 + cc9000_5 + cc0010_5 + lnpden8090 + lnpden9000 + lnpden0010 + 
                    One_Transit8090 + One_Transit9000 + One_Transit0010 + 
                    yr9000 + yr0010 +  
                    lnpop100 + pctmidlifebg + pctnhw + pctforeign + pctpro +  
                    lnmedhhinc + pctunemp + pctmulti +  
                    UA01 + UA02 + UA03 + UA04 + UA05 + UA06 + UA07 + UA08 + UA09 + UA10 +
                    UA11 + UA12 + UA13 + UA14 + UA15 + UA16 + UA17 + UA18 + UA19, 
                  data=migrdata[migrdata$Migr_midlife > quantile(migrdata$Migr_midlife, 0.01) & 
                                  migrdata$Migr_midlife < quantile(migrdata$Migr_midlife, 0.99), ])

lm.middle5 <- lm(Migr_middle ~ #ln_dist_cbd8090 + ln_dist_cbd9000 + ln_dist_cbd0010 + 
                   cc8090_5 + cc9000_5 + cc0010_5 + lnpden8090 + lnpden9000 + lnpden0010 + 
                   One_Transit8090 + One_Transit9000 + One_Transit0010 + 
                   yr9000 + yr0010 +  
                   lnpop100 + pctmiddlebg + pctnhw + pctforeign + pctpro +  
                   lnmedhhinc + pctunemp + pctmulti +  
                   UA01 + UA02 + UA03 + UA04 + UA05 + UA06 + UA07 + UA08 + UA09 + UA10 +
                   UA11 + UA12 + UA13 + UA14 + UA15 + UA16 + UA17 + UA18 + UA19, 
                 data=migrdata[migrdata$Migr_middle > quantile(migrdata$Migr_middle, 0.01) & 
                                 migrdata$Migr_middle < quantile(migrdata$Migr_middle, 0.99), ])

stargazer(lm.youngest, lm.youngest3, lm.youngest5, 
          lm.young, lm.young3, lm.young5, 
          lm.midlife, lm.midlife3, lm.midlife5, 
          lm.middle,lm.middle3, lm.middle5, type="text",  
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

stargazer(#lm.youngest , lm.young , lm.midlife , lm.middle , 
          lm.youngest3, lm.young3, lm.midlife3, lm.middle3, 
          #lm.youngest5, lm.young5, lm.midlife5, lm.middle5, 
          lm2.youngest, lm2.young, lm2.midlife, lm2.middle, type="text",  
          title="Net Migration", 
          keep = c("cc8090", "cc9000", "c0010", 
                   "cc8090_3", "cc9000_3", "c0010_3", 
                   "cc8090_5", "cc9000_5", "c0010_5", 
                   "lnpden8090", "lnpden9000", "lnpden0010", 
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
                    cc8090_3 + cc9000_3 + cc0010_3 + lnpden8090 + lnpden9000 + lnpden0010 + 
                    One_Transit8090 + One_Transit9000 + One_Transit0010 + 
                    yr9000 + yr0010 +  
                    lnpop100 + pctyoungestbg + pctnhw + pctforeign + pctpro +  
                    lnmedhhinc + pctunemp + pctmulti +  
                    UA01 + UA02 + UA03 + UA04 + UA05 + UA06 + UA07 + UA08 + UA09 + UA10 +
                    UA11 + UA12 + UA13 + UA14 + UA15 + UA16 + UA17 + UA18 + UA19, tau=0.5, data=migrdata)

rq.young <- rq(Migr_young ~ #ln_dist_cbd8090 + ln_dist_cbd9000 + ln_dist_cbd0010 + 
                 cc8090_3 + cc9000_3 + cc0010_3 + lnpden8090 + lnpden9000 + lnpden0010 + 
                 One_Transit8090 + One_Transit9000 + One_Transit0010 + 
                 yr9000 + yr0010 +  
                 lnpop100 + pctyoungbg + pctnhw + pctforeign + pctpro +  
                 lnmedhhinc + pctunemp + pctmulti +  
                 UA01 + UA02 + UA03 + UA04 + UA05 + UA06 + UA07 + UA08 + UA09 + UA10 +
                 UA11 + UA12 + UA13 + UA14 + UA15 + UA16 + UA17 + UA18 + UA19, tau=0.5, data=migrdata)

rq.midlife <- rq(Migr_midlife ~ #ln_dist_cbd8090 + ln_dist_cbd9000 + ln_dist_cbd0010 + 
                   cc8090_3 + cc9000_3 + cc0010_3 + lnpden8090 + lnpden9000 + lnpden0010 + 
                   One_Transit8090 + One_Transit9000 + One_Transit0010 + 
                   yr9000 + yr0010 +  
                   lnpop100 + pctmidlifebg + pctnhw + pctforeign + pctpro +  
                   lnmedhhinc + pctunemp + pctmulti +  
                   UA01 + UA02 + UA03 + UA04 + UA05 + UA06 + UA07 + UA08 + UA09 + UA10 +
                   UA11 + UA12 + UA13 + UA14 + UA15 + UA16 + UA17 + UA18 + UA19, tau=0.5, data=migrdata)

rq.middle <- rq(Migr_middle ~ #ln_dist_cbd8090 + ln_dist_cbd9000 + ln_dist_cbd0010 + 
                  cc8090_3 + cc9000_3 + cc0010_3 + lnpden8090 + lnpden9000 + lnpden0010 + 
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
          keep = c("cc8090_3", "cc9000_3", "c0010_3", "cc8090_5", "cc9000_5", "c0010_5", 
                   "lnpden8090", "lnpden9000", "lnpden0010", 
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



#7. Quantile at the 50th percentile: UA-specific 2-decade models 

#as.character(as.data.frame(table(migrdata$UAname))[, 1])

migrdataUA <- list()
for (i in 1:20){
  k <- 53+i
  migrdataUA[[i]] <- migrdata[migrdata$yr8090==0 & migrdata[, k]>0, ] 
} 


quantUA25 <- data.frame(coeff.90s = numeric(), p.value.90s = numeric(), 
                        coeff.00s = numeric(), p.value.00s = numeric())
for (i in 1:20){
    temp01 <- summary(rq(Migr_young ~ f_ccity9000b + f_ccity0010b + f_pden9000b + f_pden0010b +
           f_pt9000b + f_pt0010b + lncden9000b + lncden0010b + yr0010 +
           lnpop100 + pctyoungbg + pctnhw + pctforeign + pctpro + lnmedhhinc + pctunemp + pctmulti,
         tau = 0.50, data=migrdataUA[[i]]), se = "nid")
    quantUA25[i, 1:2] <- temp01$coefficients[8, c(1, 4)]
    quantUA25[i, 3:4] <- temp01$coefficients[9, c(1, 4)]
}

#Warning message: non-positive fis
#https://stat.ethz.ch/pipermail/r-help/2009-June/393112.html
#http://www.econ.uiuc.edu/~roger/research/rq/FAQ


quantUA50 <- data.frame(coeff.90s = numeric(), p.value.90s = numeric(), 
                        coeff.00s = numeric(), p.value.00s = numeric())
for (i in 1:20){
  temp01 <- summary(rq(Migr_young ~ f_ccity9000b + f_ccity0010b + f_pden9000b + f_pden0010b +
                         f_pt9000b + f_pt0010b + lncden9000b + lncden0010b + yr0010 +
                         lnpop100 + pctyoungbg + pctnhw + pctforeign + pctpro + lnmedhhinc + pctunemp + pctmulti,
                       tau = 0.50, data=migrdataUA[[i]]), se = "nid")
  quantUA50[i, 1:2] <- temp01$coefficients[8, c(1, 4)]
  quantUA50[i, 3:4] <- temp01$coefficients[9, c(1, 4)]
}


quantUA75 <- data.frame(coeff.90s = numeric(), p.value.90s = numeric(), 
                        coeff.00s = numeric(), p.value.00s = numeric())
for (i in 1:20){
  temp01 <- summary(rq(Migr_young ~ f_ccity9000b + f_ccity0010b + f_pden9000b + f_pden0010b +
                         f_pt9000b + f_pt0010b + lncden9000b + lncden0010b + yr0010 +
                         lnpop100 + pctyoungbg + pctnhw + pctforeign + pctpro + lnmedhhinc + pctunemp + pctmulti,
                       tau = 0.50, data=migrdataUA[[i]]), se = "nid")
  quantUA75[i, 1:2] <- temp01$coefficients[8, c(1, 4)]
  quantUA75[i, 3:4] <- temp01$coefficients[9, c(1, 4)]
}

quantUA25
quantUA50
quantUA75


stargazer(rq2.young.UA01, rq2.young.UA02, rq2.young.UA03, rq2.young.UA04, rq2.young.UA05, 
          rq2.young.UA06, rq2.young.UA07, rq2.young.UA08, rq2.young.UA09, rq2.young.UA10, type="text", 
          title="Net Migration by UA (quantile regression)", 
          keep = c("f_ccity9000b", "f_ccity0010b", "f_pden9000b", "f_pden0010b", 
                   "f_pt9000b", "f_pt0010b", "lncden9000b", "lncden0010b", "yr0010"),
          column.labels = c("Atlanta", "Baltimore", "Boston", "Chicago", "Cleveland", 
                            "Dallas", "Detroit", "Houston", "Los Angeles", "Miami"), 
          #order=c(1, 2, 3, 11, 12, 4, 5, 6, 13, 14, 7, 8, 9, 15, 16, 17, 18), 
          #omit=c("UA01", "UA02", "UA03", "UA04", "UA05", 
          #"UA06", "UA07", "UA08", "UA09", "UA10", 
          #"UA11", "UA12", "UA13", "UA14", "UA15", 
          #"UA16", "UA17", "UA18", "UA19", "Constant"), 
          ci.level=0.9, report="vc*", omit.stat=c("f", "ser"))#), 

stargazer(rq2.young.UA11, rq2.young.UA12, rq2.young.UA13, rq2.young.UA14, rq2.young.UA15, 
          rq2.young.UA16, rq2.young.UA17, rq2.young.UA18, rq2.young.UA19, rq2.young.UA20, type="text", 
          title="Net Migration by UA (quantile regression)", 
          keep = c("f_ccity9000b", "f_ccity0010b", "f_pden9000b", "f_pden0010b", 
                   "f_pt9000b", "f_pt0010b", "lncden9000b", "lncden0010b", "yr0010"),
          column.labels = c("Minneapolis", "New York","Philadelphia", "Phoenix",  "St. Louis", 
                            "San Diego", "San Francisco", "Seattle", "Tampa", "Washington, DC"), 
          #order=c(1, 2, 3, 11, 12, 4, 5, 6, 13, 14, 7, 8, 9, 15, 16, 17, 18), 
          #omit=c("UA01", "UA02", "UA03", "UA04", "UA05", 
          #"UA06", "UA07", "UA08", "UA09", "UA10", 
          #"UA11", "UA12", "UA13", "UA14", "UA15", 
          #"UA16", "UA17", "UA18", "UA19", "Constant"), 
          ci.level=0.9, report="vc*", omit.stat=c("f", "ser"))#), 

