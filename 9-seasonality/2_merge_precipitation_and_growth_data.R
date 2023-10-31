
rm(list = ls())
source(paste0(here::here(), "/0-config.R"))

whz <- readRDS(paste0(ghapdata_dir,"wasting_data.rds")) %>%
            filter(!is.na(anthro_date)) %>% #check why this is missing
            mutate(    month=month(anthro_date),
                       year=year(anthro_date))
rain <- read.csv(paste0(here(),"/data/all_precipitation.csv")) %>%
  rename(studyid=study) %>%
  mutate(studyid = case_when(
    studyid=="MISAME" ~ "MISAME-3",
    studyid=="VITAL" ~ "VITAL-Lactation",
    studyid=="ELICIT" ~ "ELICIT"),
    month=month(datetime),
    year=year(datetime))

head(whz)
head(rain)

table(whz$studyid)
table(rain$study)

#rain_monthly <- rain %>% group_by(studyid,month, year) %>% summarize(rain=mean(rainfall)) #Should I use the average monthly rain over the study period? Or get rain prior to measurement?
rain_monthly <- rain %>% group_by(studyid,month) %>% summarize(rain=mean(rainfall)) #Should I use the average monthly rain over the study period? Or get rain prior to measurement?

#temp lag month
rain_monthly <- rain_monthly %>% filter()


#d <- left_join(whz, rain_monthly, by=c("studyid","month", "year"))
d <- left_join(whz, rain_monthly, by=c("studyid","month"))
head(d)

ggplot(d, aes(x=month ,y=whz)) + geom_smooth() + facet_wrap(~studyid)
ggplot(d, aes(x=month ,y=rain)) + geom_jitter() + facet_wrap(~studyid)


ggplot(d, aes(x=rain, y=whz)) + geom_point(alpha=0.1) + geom_smooth(method="lm") + facet_wrap(~studyid)


#to do: add age adjustment to spline curves, adapt KI code
   #

#Try forrier terms
head(d)
# Fourier terms are often used for daily data where you might not want to use 364 dummies. They can help in capturing seasonality.

df <- d %>% filter(studyid=="MISAME-3")



# Add Fourier terms for daily seasonality
df$sin365 <- sin(2 * pi * as.numeric(df$anthro_date) / 365)
df$cos365 <- cos(2 * pi * as.numeric(df$anthro_date) / 365)

library(splines)
library(lmtest)
model1 <- glm(whz ~ bs(agedays), data = df)
model2 <- glm(whz ~ bs(agedays) + sin365 + cos365, data = df)
summary(model2)
lrtest_res <- lrtest(model1, model2)


library(SuperLearner)
library(hal9001)
listWrappers()
res_full <- CV.SuperLearner(Y=df$whz, X=df %>% subset(., select=c(sex, agedays,sin365,cos365)), SL.library = "SL.hal9001")


Fourier Terms:
  Fourier terms are often used for daily data where you might not want to use 364 dummies. They can help in capturing seasonality.

R
Copy code
# Let's say we have daily data
df <- data.frame(date = seq.Date(from = as.Date("2020-01-01"), by = "day", length.out = 365),
                 y = rnorm(365))

# Add Fourier terms for daily seasonality
df$sin365 <- sin(2 * pi * as.numeric(df$date) / 365)
df$cos365 <- cos(2 * pi * as.numeric(df$date) / 365)

model <- lm(y ~ sin365 + cos365, data = df)
summary(model)
