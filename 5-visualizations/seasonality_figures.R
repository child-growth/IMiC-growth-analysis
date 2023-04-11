
rm(list=ls())
source(paste0(here::here(), "/0-config.R"))
library(lubridate)
#load the combined raw data with dates merged in

dfull <- readRDS("/data/imic/data/combined_raw_data.rds")

# start with elicit seasonality
# but eventually repeat for all studies
elicit <- dfull %>% 
  filter(studyid=="ELICIT") %>% 
  select("subjido", "dob", "age (days)", "bmc_collection_date", "anthro date") %>%
  rename(anthro_date = `anthro date`)
colnames(elicit)
head(elicit)

#1) Make histogram of number of measurements by month by study
<<<<<<< HEAD
p_hist <- ggplot(dfull, aes(x=month)) + facet_wrap(~studyid)
=======

elicit <- elicit %>%
  select(bmc_collection_date, anthro_date) %>%
  mutate(bmc_month = month(bmc_collection_date),
         anthro_month = month(anthro_date)) %>%
  gather(key = "column", value = "date_month", bmc_month, anthro_month)

elicit_hist <- ggplot(elicit, aes(x = date_month)) +
  geom_histogram(binwidth = 0.5) +
  facet_wrap(column ~ ., labeller = labeller(column = c("bmc_month" = "Breastmilk collection", "anthro_month" = "Anthropometry visit"))) +
  xlab("month") +
  scale_x_continuous(breaks = seq(0, 12, by = 1)) +
  ylab("number of visits") +
  ggtitle("ELICIT") +
  scale_y_continuous(breaks = seq(0, 800, by = 100)) +
  theme(plot.title = element_text(hjust = 0.5),
        strip.text = element_text(size = 10, face = "bold"))

elicit_hist
>>>>>>> eb4cf5c944bec8c54a40932ed888d206ba3fa6b4

#2) Make spline plots of child growth by day of the year
#https://github.com/child-growth/ki-longitudinal-manuscripts/blob/master/5-visualizations/wasting/fig-wasting-seasonality.R


<<<<<<< HEAD

=======
>>>>>>> eb4cf5c944bec8c54a40932ed888d206ba3fa6b4
# 2a) Make spline plot of mean whz by day of the year


# estimate a pooled fit, over all studies
# need to adapt below code to variable names of IMIC data.
# Need child age, growth and day and month of the year variables (calculated from measurement date)
# try lubridate package to work with dates

plotdf <- NULL
for(i in 1:length(levels(d$studyid))){
  cat=levels(d$studyid)[i]
  di <- filter(d, studyid==cat)
  fiti <- mgcv::gam(whz~s(studyday,bs="cr"),data=di)
  #fiti <- mgcv::gam(whz~s(studyday,bs="cr", k=10),data=di)
  range=min(di$studyday):max(di$studyday)
  agedays=1:(diff(range(range))+1)
  newd <- data.frame(studyday=range)
  fitci <- gamCI(m=fiti,newdata=newd,nreps=1000)
  dfit <- data.frame(studyid=cat,studyday=range, agedays=agedays,
                     fit=fitci$fit,fit_se=fitci$se.fit,
                     fit_lb=fitci$lwrS,fit_ub=fitci$uprS)
  plotdf<-rbind(plotdf,dfit)
}




#Add points at 6, 12, and 18 months
plotdf$xpos <- plotdf$agem <- NA
plotdf$xpos[plotdf$agedays %in% c(182, 365,548)] <- plotdf$studyday[plotdf$agedays %in% c(182, 365,548)]
plotdf$agem[plotdf$agedays==182] <- "6 months"
plotdf$agem[plotdf$agedays==365] <- "12 months"
plotdf$agem[plotdf$agedays==548] <- "18 months"
table(plotdf$xpos)

#Add monsoon indicator to plot dataframe
plotdf$month <- floor(plotdf$studyday/30.417) + 1
plotdf$month[plotdf$month>12] <- plotdf$month[plotdf$month>12] - 12
plotdf$month[plotdf$month>12] <- plotdf$month[plotdf$month>12] -12
table(plotdf$month)

#Monsoon is assumed to be May-October 
plotdf$monsoon <- factor(ifelse(plotdf$month > 5 & plotdf$month < 10, "Monsoon", "Not monsoon"))
plotdf$studyid <- factor(plotdf$studyid, levels=c("Born Jan-Mar",  "Born Apr-June", "Born Jul-Sept", "Born Oct-Dec" ))


shade="grey80"

rectd=data.frame(x1=30.4617*c(5,17,29), x2=30.4617*c(10,22,34), y1=rep(-1.25, 3), y2=rep(0, 3))

p <- ggplot() +
  geom_rect(data=rectd, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill=shade, color=shade, alpha=1) +
  geom_line(data=plotdf, aes(x=studyday, y=fit, group=studyid, color=studyid,  fill=studyid), size=2) +
  geom_ribbon(data=plotdf, aes(x=studyday, y=fit, ymin=fit_lb, ymax=fit_ub, group=studyid, color=studyid,  fill=studyid), alpha=0.3, color=NA) +
  scale_shape_manual(values=c(0,1,2), na.translate = F) +
  scale_color_manual(values=tableau10[c(5,7,9,10)], na.translate = F) + 
  scale_fill_manual(values=tableau10[c(5,7,9,10)], na.translate = F) + 
  ylab("Mean WLZ") + xlab("Month of the year") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(limits=c(1,1086), expand = c(0, 0),
                     breaks = 1:18*30.41*2-50, labels = rep(c("Jan.", "Mar.", "May", "Jul.", "Sep.", "Nov."),3)) +
  coord_cartesian(ylim=c(-1.25, 0)) +
  guides(color=guide_legend(ncol=2)) + 
  theme(legend.position = c(.78,.9),
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        legend.text=element_text(size=rel(1)))
p


# 2b) Make spline plot of mean haz by day of the year
   # Best done by making the gam fit and plot code above into functions

# 2c) Make spline plot of mean waz by day of the year


# 2d) Make spline plot of birthweight by day of the year


#3)

# 3a) Get GPS data for each study and merge into dataset


# 3b) Download and merge precipitation data from:
# https://www.r-bloggers.com/2014/06/updates-to-r-package-raincpc-global-daily-rainfall-for-over-35-years/