###########################################
############------kattegatmidt------############
###########################################
library(mgcv)
library(gratia)
library(readr)
library(tidyverse)
library(dplyr)
library(gdata)
library(lubridate)
library(rstudioapi)

# Getting the path of your current open file
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))

#--------------------------------------------#
source("funcs.R")
periods <- seq(2000,2024,6)

#------------Make as factors----------------_#
dat_cod <- read_csv("../data_cleaning/dat_cod.csv")
dat_cod <- check.data(dat_cod, periods = periods)

dat_flounder <- read_csv("../data_cleaning/dat_flounder.csv")
dat_flounder <- check.data(dat_flounder, periods = periods)

dat_plaice <- read_csv("../data_cleaning/dat_plaice.csv")
dat_plaice <- check.data(dat_plaice, periods = periods)

dat_herring<- read_csv("../data_cleaning/dat_herring.csv")
dat_herring <- check.data(dat_herring, periods = periods)

dat_sprat<- read_csv("../data_cleaning/dat_sprat.csv")
dat_sprat <- check.data(dat_sprat, periods = periods)

dat_whiting<- read_csv("../data_cleaning/dat_whiting.csv")
dat_whiting <- check.data(dat_whiting, periods = periods)
##########################################
################ MODELS ##################
##########################################

#--------COD -------#
mod.codJ <- gam(cpue ~
                  period+
                  s(depth,k=15) + 
                  s(Gear,bs="re")+ 
                  Quarter+ 
                  SedimentDK,
                data = subset(dat_cod, loc == "kattegatmidt_codJ"), 
                family=tw)
mod.codJ <- get.mini.model(mod.codJ, check.terms = c("period","depth",
                                                     "Quarter","SedimentDK"))

#--------FLOUNDER -------#
mod.flounderJ <- gam(cpue ~ 
                       period+
                  s(depth,k=5) + 
                  s(Gear,bs="re")+ 
                  Quarter+ 
                  SedimentDK, 
                data = subset(dat_flounder, loc == "kattegatmidt_flounderJ"),
                family=tw) 
mod.flounderJ <- get.mini.model(mod.flounderJ, check.terms = c("period","depth",
                                                     "Quarter","SedimentDK"))
#--------PLAICE -------#
mod.plaiceJ <- gam(cpue ~
                     period+
                  s(depth,k=15) + 
                  s(Gear,bs="re")+ 
                  Quarter+ 
                  SedimentDK, 
                data = subset(dat_plaice, loc == "kattegatmidt_plaiceJ"), 
                family=tw) 
mod.plaiceJ <- get.mini.model(mod.plaiceJ, check.terms = c("period","depth",
                                                     "Quarter","SedimentDK"))
#--------HERRING -------#
mod.herringJ <- gam(cpue ~ #response
                      period+
                  s(depth,k=15) +
                  s(Gear,bs="re")+
                  Quarter+ 
                  SedimentDK, 
                data = subset(dat_herring, loc == "kattegatmidt_herringJ"), 
                family=tw) 
mod.herringJ <- get.mini.model(mod.herringJ, check.terms = c("period","depth",
                                                     "Quarter","SedimentDK"))
#--------SPRAT -------#
mod.spratJ <- gam(cpue ~ #response
                    period+
                  s(depth,k=15) +
                  s(Gear,bs="re")+ 
                  Quarter+ 
                  SedimentDK, 
                data = subset(dat_sprat, loc == "kattegatmidt_spratJ"), 
                family=tw) 
mod.spratJ <- get.mini.model(mod.spratJ, check.terms = c("period","depth",
                                                     "Quarter","SedimentDK"))
#--------WHITING -------#
mod.whitingJ <- gam(cpue ~ #response
                      period+
                  s(depth,k=5) + 
                  s(Gear,bs="re")+
                  Quarter+ 
                  SedimentDK, 
                data = subset(dat_whiting, loc == "kattegatmidt_whitingJ"), 
                family=tw) 
mod.whitingJ <- get.mini.model(mod.whitingJ, check.terms = c("period","depth",
                                                     "Quarter","SedimentDK"))

############################### Predict CPUE ############################### 
#depth.by.loc <- by(dat_cod$depth, dat_cod$loc, median)
## Assuming one overall depth for all locations is one approach, alternatively, one could use the median depth for each location

newdat <- expand.grid(period = levels(dat_cod$period),
                      loc = c("kattegatmidt_codJ","kattegatmidt_flounderJ",
                              "kattegatmidt_plaiceJ","kattegatmidt_herringJ",
                              "kattegatmidt_spratJ","kattegatmidt_whitingJ"),
                      depth = 19, #kattegatmidt is 19
                      Gear=c("TVS"),
                      Quarter=c("4"),
                      SedimentDK=c("5")
)
#to check the depth of each location: 
#newdat$depth <- depth.by.loc[match(newdat$loc,names(depth.by.loc))] #overwrite for each location
## individual models
tmp1 <- predict(mod.codJ, newdata = newdat, type = "response", se.fit = TRUE)
tmp2 <- predict(mod.flounderJ, newdata = newdat, type = "response", se.fit = TRUE)
tmp3 <- predict(mod.plaiceJ, newdata = newdat, type = "response", se.fit = TRUE)
tmp4 <- predict(mod.herringJ, newdata = newdat, type = "response", se.fit = TRUE)
tmp5 <- predict(mod.spratJ, newdata = newdat, type = "response", se.fit = TRUE)
tmp6 <- predict(mod.whitingJ, newdata = newdat, type = "response", se.fit = TRUE)

#make new data predictions
newdat$cpue_pred <- NA
newdat$cpue_pred[newdat$loc == "kattegatmidt_codJ"] <- tmp1$fit[newdat$loc == "kattegatmidt_codJ"]
newdat$cpue_pred[newdat$loc == "kattegatmidt_flounderJ"] <- tmp2$fit[newdat$loc == "kattegatmidt_flounderJ"]
newdat$cpue_pred[newdat$loc == "kattegatmidt_plaiceJ"] <- tmp3$fit[newdat$loc == "kattegatmidt_plaiceJ"]
newdat$cpue_pred[newdat$loc == "kattegatmidt_herringJ"] <- tmp4$fit[newdat$loc == "kattegatmidt_herringJ"]
newdat$cpue_pred[newdat$loc == "kattegatmidt_spratJ"] <- tmp5$fit[newdat$loc == "kattegatmidt_spratJ"]
newdat$cpue_pred[newdat$loc == "kattegatmidt_whitingJ"] <- tmp6$fit[newdat$loc == "kattegatmidt_whitingJ"]

newdat$cpue_sd <- NA
newdat$cpue_sd[newdat$loc == "kattegatmidt_codJ"] <- tmp1$se.fit[newdat$loc == "kattegatmidt_codJ"]
newdat$cpue_sd[newdat$loc == "kattegatmidt_flounderJ"] <- tmp2$se.fit[newdat$loc == "kattegatmidt_flounderJ"]
newdat$cpue_sd[newdat$loc == "kattegatmidt_plaiceJ"] <- tmp3$se.fit[newdat$loc == "kattegatmidt_plaiceJ"]
newdat$cpue_sd[newdat$loc == "kattegatmidt_herringJ"] <- tmp4$se.fit[newdat$loc == "kattegatmidt_herringJ"]
newdat$cpue_sd[newdat$loc == "kattegatmidt_spratJ"] <- tmp5$se.fit[newdat$loc == "kattegatmidt_spratJ"]
newdat$cpue_sd[newdat$loc == "kattegatmidt_whitingJ"] <- tmp6$se.fit[newdat$loc == "kattegatmidt_whitingJ"]


newdat$cpue_lo <- newdat$cpue_pred - 1.96 * newdat$cpue_sd
newdat$cpue_up <- newdat$cpue_pred + 1.96 * newdat$cpue_sd

## individual models
tmp1.log <- predict(mod.codJ, newdata = newdat, type = "link", se.fit = TRUE)
tmp2.log <- predict(mod.flounderJ, newdata = newdat, type = "link", se.fit = TRUE)
tmp3.log <- predict(mod.plaiceJ, newdata = newdat, type = "link", se.fit = TRUE)
tmp4.log <- predict(mod.herringJ, newdata = newdat, type = "link", se.fit = TRUE)
tmp5.log <- predict(mod.spratJ, newdata = newdat, type = "link", se.fit = TRUE)
tmp6.log <- predict(mod.whitingJ, newdata = newdat, type = "link", se.fit = TRUE)

## for error propagations
newdat$cpue_pred.log <- NA
newdat$cpue_pred.log[newdat$loc == "kattegatmidt_codJ"] <- tmp1.log$fit[newdat$loc == "kattegatmidt_codJ"]
newdat$cpue_pred.log[newdat$loc == "kattegatmidt_flounderJ"] <- tmp2.log$fit[newdat$loc == "kattegatmidt_flounderJ"]
newdat$cpue_pred.log[newdat$loc == "kattegatmidt_plaiceJ"] <- tmp3.log$fit[newdat$loc == "kattegatmidt_plaiceJ"]
newdat$cpue_pred.log[newdat$loc == "kattegatmidt_herringJ"] <- tmp4.log$fit[newdat$loc == "kattegatmidt_herringJ"]
newdat$cpue_pred.log[newdat$loc == "kattegatmidt_spratJ"] <- tmp5.log$fit[newdat$loc == "kattegatmidt_spratJ"]
newdat$cpue_pred.log[newdat$loc == "kattegatmidt_whitingJ"] <- tmp6.log$fit[newdat$loc == "kattegatmidt_whitingJ"]
newdat$cpue_sd.log <- NA
newdat$cpue_sd.log[newdat$loc == "kattegatmidt_codJ"] <- tmp1.log$se.fit[newdat$loc == "kattegatmidt_codJ"]
newdat$cpue_sd.log[newdat$loc == "kattegatmidt_flounderJ"] <- tmp2.log$se.fit[newdat$loc == "kattegatmidt_flounderJ"]
newdat$cpue_sd.log[newdat$loc == "kattegatmidt_plaiceJ"] <- tmp3.log$se.fit[newdat$loc == "kattegatmidt_plaiceJ"]
newdat$cpue_sd.log[newdat$loc == "kattegatmidt_herringJ"] <- tmp4.log$se.fit[newdat$loc == "kattegatmidt_herringJ"]
newdat$cpue_sd.log[newdat$loc == "kattegatmidt_spratJ"] <- tmp5.log$se.fit[newdat$loc == "kattegatmidt_spratJ"]
newdat$cpue_sd.log[newdat$loc == "kattegatmidt_whitingJ"] <- tmp6.log$se.fit[newdat$loc == "kattegatmidt_whitingJ"]

  write_rds (newdat, "newdat/newdat_kattegatmidtJ.rds")

