
rm(list=ls())
source(paste0(here::here(), "/0-config.R"))

#Load data
d <- readRDS(paste0(BV_dir,"/results/desc_data_cleaned.rds"))

#Subset to primary analysis
d <- d %>% mutate(pooling=ifelse(cohort=="pooled" & is.na(pooling),
                                 cohort,pooling)) %>%
  filter(analysis=="Primary", (pooling!="country" | is.na(pooling)))

#convert cohort specific estimates to percents
d$est[(is.na(d$pooling) | d$pooling=="no pooling") & d$measure %in%
        c("Prevalence","Cumulative incidence","Incidence proportion",
          "Persistent wasting", "Recovery" )] <-
  d$est[(is.na(d$pooling) | d$pooling=="no pooling") & d$measure %in%
          c("Prevalence","Cumulative incidence","Incidence proportion",
            "Persistent wasting", "Recovery" )] * 100
d$lb[(is.na(d$pooling) | d$pooling=="no pooling") & d$measure %in%
       c("Prevalence","Cumulative incidence","Incidence proportion",
         "Persistent wasting", "Recovery" )] <-
  d$lb[(is.na(d$pooling) | d$pooling=="no pooling") & d$measure %in%
         c("Prevalence","Cumulative incidence","Incidence proportion",
           "Persistent wasting", "Recovery" )] * 100
d$ub[(is.na(d$pooling) | d$pooling=="no pooling") & d$measure %in%
       c("Prevalence","Cumulative incidence","Incidence proportion",
         "Persistent wasting", "Recovery" )] <-
  d$ub[(is.na(d$pooling) | d$pooling=="no pooling") & d$measure %in%
         c("Prevalence","Cumulative incidence","Incidence proportion",
           "Persistent wasting", "Recovery" )] * 100

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

# Change the name of country
d $ cohort <- case_when(d $ cohort == "ELICIT-TANZANIA, UNITED REPUBLIC OF" ~
  "ELICIT", d $ cohort == "VITAL-Lactation-PAKISTAN" ~ "VITAL")

# Get rid of the word "months" from agecat for visualizations
d $ agecat <- gsub(" months", "m", d $ agecat)

# Relevel the agecat variable
d $ agecat <- factor(d $ agecat, 
                     level = c("Birth", "3m", "6m", "9m", "12m", "15m",
                               "18m", "0-3m", "3-6m", "6-9m",
                               "9-12m", "15-18m", "8 days-3m", "0-6m", 
                               "6-12m", "12-18m"))

#-------------------------------------------------------------------------------
# To do
#-------------------------------------------------------------------------------

#Hold: will also make growth velocity figure separately. 

#-------------------------------------------------------------------------------
# Mean WLZ by month  -NEED TO ADD
#-------------------------------------------------------------------------------

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

#-------------------------------------------------------------------------------
# Make a plot function
#-------------------------------------------------------------------------------
plot <- function (d, Disease, Measure, ageRange) {
  df <- d %>% filter(
    disease == Disease &
      measure == Measure & 
      birth == "yes" &
      severe == "no" &
      age_range == ageRange)
  df <- droplevels(df)
  table(df$agecat)
  p <- ggplot(df,aes(y=est,x=agecat)) +
    geom_point(aes(shape=measure, size=measure, fill=cohort, color=cohort), 
               size = 2, stroke = 0, data = df) +
    geom_errorbar(aes(color=cohort, group=interaction(measure, cohort),ymin=lb,
                      ymax=ub), width = 0,data = df ) +
    geom_text(data=df , aes(x = agecat, y = est, label = round(est,2)),
              hjust = 1.2, vjust = 0.5) + 
    scale_color_manual(values=tableau11, drop=TRUE, limits = levels(df$measure),
                       guide = FALSE) +
    scale_size_manual(values = c(2, 1.5), guide = FALSE) +
    scale_shape_manual(values = c(16, 17),name = 'Measure') +
    scale_fill_manual(values=tableau11, guide = FALSE) +
    xlab("") +
    ylab("") +
    scale_x_discrete(expand = expansion(add = 1)) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
    theme(axis.text.x = element_text(margin =margin(t = 0, r = 0, b = 0, l = 0),
                                     size = 14)) +
    theme(axis.title.y = element_text(size = 14)) +
    ggtitle("") + facet_wrap(~cohort) +
    theme(strip.text = element_text(size=12, margin = margin(t = 0)))
  return(p)
}

#-------------------------------------------------------------------------------
# Wasting
#-------------------------------------------------------------------------------
# Cumulative incidence by 3 month interval
CI <- plot(d, Disease = "Wasting", Measure = "Cumulative incidence", ageRange = "3 months") #+
  #ggtitle("Wasting Cumulative Incidence: 3 Months Interval")
ggsave(CI, filename = paste0(BV_dir, "/results/figures/wasting/CI3.png"))

# Incidence proportion by 3 month interval
IP <- plot(d, Disease = "Wasting", Measure = "Incidence proportion", ageRange = "3 months") #+
  #ggtitle("Wasting Incidence Proportion: 3 Months Interval")
ggsave(IP, filename = paste0(BV_dir, "/results/figures/wasting/IP3.png"))

# Incidence rate by 3 month interval
IR <- plot(d, Disease = "Wasting", Measure = "Incidence rate", ageRange = "3 months") #+
  #ggtitle("Wasting Incidence Rate: 3 Months Interval")
ggsave(IR, filename = paste0(BV_dir, "/results/figures/wasting/IR3.png"))

# Incidence rate by 6 month interval
IR <- plot(d, Disease = "Wasting", Measure = "Incidence rate", ageRange = "6 months") #+
  #ggtitle("Wasting Incidence Rate: 6 Months Interval")
ggsave(IR, filename = paste0(BV_dir, "/results/figures/wasting/IR6.png"))

# Mean WLZ by 3 month interval
WLZ <- plot(d, Disease = "Wasting", Measure = "Mean WLZ", ageRange = "3 months") #+
  #ggtitle("Mean WLZ: 3 Months Interval")
ggsave(WLZ, filename = paste0(BV_dir, "/results/figures/wasting/WLZ3.png"))

# Persistent wasting by 6 month interval
PW <- plot(d, Disease = "Wasting", Measure = "Persistent wasting", ageRange = "6 months") #+
  #ggtitle("Persistent Wasting: 6 Months Interval")
ggsave(PW, filename = paste0(BV_dir, "/results/figures/wasting/PW6.png"))

# Prevalence by 3 month interval
P <- plot(d, Disease = "Wasting", Measure = "Prevalence", ageRange = "3 months") #+
  #ggtitle("Wasting Prevalence: 3 Months Interval")
ggsave(P, filename = paste0(BV_dir, "/results/figures/wasting/prevalence3.png"))

# Recovery by 30-day intervals
R <- plot(d, Disease = "Wasting", Measure = "Recovery", 
     ageRange = c("30 days", "60 days", "90 days")) #+
  #ggtitle("Wasting Recovery: 6 Months Interval")
ggsave(R, filename = paste0(BV_dir, "/results/figures/wasting/recovery6.png"))

#-------------------------------------------------------------------------------
# Stunting
#-------------------------------------------------------------------------------
# Cumulative incidence by 3 month interval
CI <- plot(d, Disease = "Stunting", Measure = "Cumulative incidence", ageRange = "3 months")# +
  #ggtitle("Stunting Cumulative Incidence: 3 Months Interval")
ggsave(CI, filename = paste0(BV_dir, "/results/figures/stunting/CI3.png"))

# Incidence proportion by 3 month interval
IP <- plot(d, Disease = "Stunting", Measure = "Incidence proportion", ageRange = "3 months")# +
  #ggtitle("Stunting Incidence Proportion: 3 Months Interval")
ggsave(IP, filename = paste0(BV_dir, "/results/figures/stunting/IP3.png"))

# Mean LAZ by 3 month interval
LAZ <- plot(d, Disease = "Stunting", Measure = "Mean LAZ", ageRange = "3 months") #+
  #ggtitle("Mean LAZ: 3 Months Interval")
ggsave(LAZ, filename = paste0(BV_dir, "/results/figures/stunting/LAZ3.png"))

# Prevalence by 3 month interval
P <- plot(d, Disease = "Stunting", Measure = "Prevalence", ageRange = "3 months") #+
  #ggtitle("Stunting Prevalence: 3 Months Interval")
ggsave(P, filename = paste0(BV_dir, "/results/figures/stunting/prevalence3.png"))

#-------------------------------------------------------------------------------
#  Co-Occurrence
#-------------------------------------------------------------------------------
# Incidence proportion by 3 month interval
IP <- plot(d, Disease = "co-occurrence", Measure = "Incidence proportion", ageRange = "3 months") #+
  #ggtitle("Co-Occurrence Incidence Proportion: 3 Months Interval")
ggsave(IP, filename = paste0(BV_dir, "/results/figures/co-oc/IP3.png"))

# Prevalence by 3 month interval
P <- plot(d, Disease = "co-occurrence", Measure = "Prevalence", ageRange = "3 months") #+
  #ggtitle("Co-Occurrence Prevalence: 3 Months Interval")
ggsave(P, filename = paste0(BV_dir, "/results/figures/co-oc/prevalence3.png"))

#-------------------------------------------------------------------------------
#  Underweight
#-------------------------------------------------------------------------------
# Mean WAZ by 3 month interval
WAZ <- plot(d, Disease = "Underweight", Measure = "Mean WAZ", ageRange = "3 months") #+
  #ggtitle("Mean WAZ: 3 Months Interval")
ggsave(WAZ, filename = paste0(BV_dir, "/results/figures/underweight/WAZ3.png"))

# Prevalence by 3 month interval
P <- plot(d, Disease = "Underweight", Measure = "Prevalence", ageRange = "3 months") #+
  #ggtitle("Underweight Prevalence: 3 Months Interval")
ggsave(P, filename = paste0(BV_dir, "/results/figures/underweight/prevalence3.png"))
  
