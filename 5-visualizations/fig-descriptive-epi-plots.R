
rm(list=ls())
source(paste0(here::here(), "/0-config.R"))

#Load data
d <- readRDS(paste0(BV_dir,"/results/desc_data_cleaned.rds"))

#Subset to primary analysis
d <- d %>% mutate(pooling=ifelse(cohort=="pooled" & is.na(pooling),cohort,pooling)) %>%
  filter(analysis=="Primary", (pooling!="country" | is.na(pooling)))

#convert cohort specific estimates to percents
d$est[(is.na(d$pooling) | d$pooling=="no pooling") & d$measure %in%
        c("Prevalence","Cumulative incidence","Incidence proportion","Persistent wasting", "Recovery" )] <-
  d$est[(is.na(d$pooling) | d$pooling=="no pooling") & d$measure %in%
          c("Prevalence","Cumulative incidence","Incidence proportion","Persistent wasting", "Recovery" )] * 100
d$lb[(is.na(d$pooling) | d$pooling=="no pooling") & d$measure %in%
       c("Prevalence","Cumulative incidence","Incidence proportion","Persistent wasting", "Recovery" )] <-
  d$lb[(is.na(d$pooling) | d$pooling=="no pooling") & d$measure %in%
         c("Prevalence","Cumulative incidence","Incidence proportion","Persistent wasting", "Recovery" )] * 100
d$ub[(is.na(d$pooling) | d$pooling=="no pooling") & d$measure %in%
       c("Prevalence","Cumulative incidence","Incidence proportion","Persistent wasting", "Recovery" )] <-
  d$ub[(is.na(d$pooling) | d$pooling=="no pooling") & d$measure %in%
         c("Prevalence","Cumulative incidence","Incidence proportion","Persistent wasting", "Recovery" )] * 100

d$nmeas.f <- clean_nmeans(d$nmeas)
d$nstudy.f <- gsub("N=","",d$nstudy.f)
d$nmeas.f <- gsub("N=","",d$nmeas.f)
d$nstudy.f <- gsub(" studies","",d$nstudy.f)
d$nmeas.f <- gsub(" children","",d$nmeas.f)

# scale cohort-specific estimates
scale_estimates <- function(d) {
  d = d %>% mutate(
    est = ifelse(cohort!="pooled", est*100, est),
    lb = ifelse(cohort!="pooled", lb*100, lb),
    ub = ifelse(cohort!="pooled", ub*100, ub)
  )
  return(d)
}




#-------------------------------------------------------------------------------------------
# To do
#-------------------------------------------------------------------------------------------

# make a figure for each of these combinations
table(d$disease, d$measure)

#Hold: will also make growth velocity figure seperately. 


#-------------------------------------------------------------------------------------------
# Mean WLZ by month  -NEED TO ADD
#-------------------------------------------------------------------------------------------



# df <- d %>% filter(
#   disease == "Wasting" &
#     measure == "Mean WLZ" & 
#     birth == "yes" &
#     severe == "no" &
#     age_range == "30 days" )
# df <- droplevels(df)
# 
# df <- df %>% 
#   arrange(agecat) %>%
#   filter(!is.na(agecat)) %>%
#   filter(!is.na(cohort)) %>%
#   mutate(agecat = as.character(agecat)) %>%
#   mutate(agecat = ifelse(agecat == "Two weeks", ".5", agecat)) %>%
#   mutate(agecat = gsub(" month", "", agecat)) %>%
#   mutate(agecat = gsub(" months", "", agecat)) %>%
#   mutate(agecat = gsub("s", "", agecat)) %>%
#   mutate(agecat = ifelse(agecat == "One", "1", agecat)) %>%
#   mutate(agecat = as.numeric(agecat)) %>% 
#   arrange(agecat) 
# 
# 
# p <- ggplot(df,aes(y=est,x=agecat, group=cohort)) +
#   stat_smooth(aes(fill=cohort, color=cohort), se=F, span = 1) +
#   geom_hline(yintercept = 0, colour = "black") +
#   scale_y_continuous(breaks = scales::pretty_breaks(n = 10),
#                      limits = c(-1, 0.5)) +
#   scale_x_continuous(limits = c(0,24), breaks = seq(0,24,2), labels = seq(0,24,2)) +
#   scale_fill_manual(values=tableau11, drop=TRUE, limits = levels(df$measure),
#                     name = 'cohort') +
#   scale_color_manual(values=tableau11, drop=TRUE, limits = levels(df$measure),
#                      name = 'cohort') +
#   xlab("Child age, months")+
#   ylab("Weight-for-length Z-score") +
#   ggtitle("") +
#   theme(legend.position="right")
# 
# ggsave(p, file=paste0(fig_dir,"wasting/WLZ_by_cohort.png"), width=10, height=4)


#-------------------------------------------------------------------------------------------
# Mean WLZ by 3 month interval
#-------------------------------------------------------------------------------------------



df <- d %>% filter(
  disease == "Wasting" &
    measure == "Mean WLZ" & 
    birth == "yes" &
    severe == "no" &
    age_range == "3 months" )
df <- droplevels(df)


  p <- ggplot(df,aes(y=est,x=agecat)) +
      geom_point(aes(shape=measure, size=measure, fill=cohort, color=cohort), size = 2, stroke = 0, data = df ) +
    geom_errorbar(aes(color=cohort, group=interaction(measure, cohort),ymin=lb, ymax=ub), width = 0,data = df ) +
    geom_text(data=df , aes(x = agecat, y = est, label = round(est,2)),hjust = 1.5, vjust = 0.5) + 
    scale_color_manual(values=tableau11, drop=TRUE, limits = levels(df$measure),guide = FALSE) +
    scale_size_manual(values = c(2, 1.5), guide = FALSE) +
    scale_shape_manual(values = c(16, 17),name = 'Measure')+
    scale_fill_manual(values=tableau11, guide = FALSE) +
    xlab("")+
    ylab("") +
    scale_x_discrete(expand = expand_scale(add = 1)) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10))  +
    theme( axis.text.x = element_text(margin =margin(t = 0, r = 0, b = 0, l = 0),size = 14)) +
    theme(axis.title.y = element_text(size = 14)) +
    ggtitle("") + facet_wrap(~cohort) +
      theme(strip.text = element_text(size=12, margin = margin(t = 0)))
p
  
#-------------------------------------------------------------------------------------------
# Wasting prevalence by 3 month interval
#-------------------------------------------------------------------------------------------



df <- d %>% filter(
  disease == "Wasting" &
    measure == "Prevalence" & 
    birth == "yes" &
    severe == "no" &
    age_range == "3 months" )
df <- droplevels(df)
table(df$agecat)


p2 <- ggplot(df,aes(y=est,x=agecat)) +
  geom_point(aes(shape=measure, size=measure, fill=cohort, color=cohort), size = 2, stroke = 0, data = df ) +
  geom_errorbar(aes(color=cohort, group=interaction(measure, cohort),ymin=lb, ymax=ub), width = 0,data = df ) +
  geom_text(data=df , aes(x = agecat, y = est, label = round(est,2)),hjust = 1.5, vjust = 0.5) + 
  scale_color_manual(values=tableau11, drop=TRUE, limits = levels(df$measure),guide = FALSE) +
  scale_size_manual(values = c(2, 1.5), guide = FALSE) +
  scale_shape_manual(values = c(16, 17),name = 'Measure')+
  scale_fill_manual(values=tableau11, guide = FALSE) +
  xlab("")+
  ylab("") +
  scale_x_discrete(expand = expand_scale(add = 1)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))  +
  theme( axis.text.x = element_text(margin =margin(t = 0, r = 0, b = 0, l = 0),size = 14)) +
  theme(axis.title.y = element_text(size = 14)) +
  ggtitle("") + facet_wrap(~cohort) +
  theme(strip.text = element_text(size=12, margin = margin(t = 0)))
p2

  
  
