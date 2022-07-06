################################################################################
# IMiC longitudinal manuscripts
# Stunting analysis
#
# Plots of linear growth velocity

# Inputs: pool_vel.RData, 
# WHO_linear_growth_velocity_standard.RDS

# Outputs: 
# Plots: 
#   lenVelE.png
#   lenVelV.png
#   lazVelE.png
#   lazVelV.png

# data for each plot is saved as an RDS file
# with the same file name and the prefix "figdata"
################################################################################

rm(list=ls())
source(paste0(here::here(), "/0-config.R"))
library(gridExtra)

# Load data
vel <- readRDS(paste0(BV_dir,"/results/stunting/pool_vel.RDS"))

# Load who standard
who_cm = readRDS(paste0(res_dir, "/WHO_linear_growth_velocity_standard.RDS"))

# Remove mo from age label
vel <- vel %>% 
  mutate(strata = gsub(" months", "", strata)) %>%
  mutate(strata = factor(strata, 
                         levels = c("0-3", "3-6", "6-9",
                                    "9-12", "12-15", "15-18")))

# Merge vel with who_cm
vel <- merge(vel, who_cm, by=c('sex', 'strata'))

# WHY?
vel <- vel %>% mutate(pct_50 = pct_50 / 3)
vel <- vel %>% mutate(pct_25 = pct_25 / 3)
vel <- vel %>% mutate(pct_15 = pct_15 / 3)
vel <- vel %>% mutate(pct_50 = ifelse(ycat == 'haz', NA, pct_50))
vel <- vel %>% mutate(pct_25 = ifelse(ycat == 'haz', NA, pct_25))
vel <- vel %>% mutate(pct_15 = ifelse(ycat == 'haz', NA, pct_15))

# clean up y label
vel$ycat <- gsub('haz', 'LAZ change (Z-score per month)', vel$ycat)
vel$ycat <- gsub('lencm', 'Length velocity (cm per month)', vel$ycat)

# Separate datasets
elicit <- vel %>%
  filter(country_cohort == "Elicit Tanzania, United Republic Of")

vital <- vel %>%
  filter(country_cohort == "Vital-lactation Pakistan")

# define color palette
mypalette = c("#D87A16", "#0EA76A")


# get N's for figure caption
vel_cohorts = vel %>% filter(pooled==0) %>%
  mutate(unique_cohorts = length(unique(country_cohort))) %>%
  summarize(unique_cohorts=unique_cohorts[1])
vel_cohorts

#Our transformation function
scaleFUN <- function(x) sprintf("%0.1f", x)

## Absolute length plot --------------------------------------------------------

# Make a plot function

velplot <- function (data) {
  velplot_cm = data %>% 
    filter(ycat == "Length velocity (cm per month)") %>%
    dplyr::select(country_cohort, pooled, Mean, `Lower.95.CI`, `Upper.95.CI`, 
                  strata, sex, pct_50, pct_25) %>%
    mutate(sex = as.factor(sex)) %>%
    gather(`pct_25`, `pct_50`, `Mean`, key = "msmt_type", value = "length_cm") %>%
    mutate(msmt_type = factor(msmt_type, levels = c("pct_50", "pct_25", "Mean"))) %>% 
    mutate(linecol = ifelse(msmt_type != "Mean", "black", 
                            ifelse(sex == "Male", "male_color", "female_color")), 
           sexcol = ifelse(sex == "Male", "male_color2", "female_color2"))
  
  # Plot
  plot_cm <- ggplot(velplot_cm, aes(y = length_cm, x = strata))+
    
    facet_grid(~ sex) +
    
    # cohort-specific lines
    geom_line(aes(group = country_cohort),
              alpha=0.18) +
    
    # WHO standard lines
    geom_line(aes(y = length_cm, group = msmt_type, color = linecol,
                  linetype = msmt_type),
              data = velplot_cm %>%  filter(
                msmt_type=="pct_25"|
                  msmt_type=="pct_50"),
              size = 0.4) +
    
    # IMic pooled lines
    geom_line(aes(y = length_cm, group = msmt_type, color = linecol),
              data = velplot_cm %>%  filter( msmt_type=="Mean"),
              size = 0.8) +
    
    # confidence intervals
    geom_errorbar(aes(ymin = Lower.95.CI, ymax = Upper.95.CI, color = sexcol),
                  alpha=0.5, size=0.8, width=0.15) +
    
    scale_color_manual("WHO Growth\nVelocity Standards", 
                       values = c("black" = "black",
                                  "male_color" = mypalette[2],
                                  "female_color" = mypalette[1],
                                  "male_color" = "male_color", 
                                  "female_color2" = mypalette[1],
                                  "male_color2" = mypalette[2],
                                  "female_color2" = mypalette[1],
                                  "male_color2" = mypalette[2])) +
    scale_y_continuous(limits=c(0.25,4), breaks=seq(0.4,4,0.2),
                       labels=scaleFUN) +
    xlab("Child age, months") +  
    ylab("Difference in length (cm) per month") +
    #ggtitle("a") +
    theme(plot.title = element_text(hjust=0, size = 20, face = "bold"),
          strip.text.x = element_text(size=20, face="bold"),
          strip.text.y = element_text(size=20),
          axis.title.x = element_text(size=20),
          axis.title.y = element_text(size=20))
  
  return(plot_cm)
}

# Plot for ELICIT
plot_cm_e <- velplot(elicit)

ggsave(plot_cm_e, filename = paste0(BV_dir, "/results/figures/lenVelE.png"), 
       width=10, height=8)

# Plot for VITAL
plot_cm_v <- velplot(vital)

ggsave(plot_cm_v, filename = paste0(BV_dir, "/results/figures/lenVelV.png"), 
       width=10, height=8)


# Figure 5b: LAZ velocity plots ------------------------------------------------

## LAZ plot --------------------------------------------------------------------

plot <- function (data) {
  velplot_laz = data %>% 
    filter(ycat == "LAZ change (Z-score per month)") %>%
    mutate(sex = factor(sex))
  
  #saveRDS(velplot_laz, file=paste0(BV_dir, "/results/figdata-lazVelE.RDS"))
  #saveRDS(velplot_laz, file=paste0(BV_dir, "/results/figdata-lazVelV.RDS"))
  
  plot_laz <- ggplot(velplot_laz, aes(y=Mean,x=strata))+
    
    # cohort point estimates
    geom_point(data = velplot_laz  %>% filter(pooled==0),
               aes(fill=sex, color=sex), size = 3,             
               position = position_jitterdodge(dodge.width = 0.75), alpha =0.2) +
    # CIs
    geom_errorbar(data = velplot_laz  %>% filter(pooled==0),
                  aes(ymin=Lower.95.CI, ymax=Upper.95.CI, color=sex),
                  position = position_dodge(width=0.75), size=1, width = 0.15) +
    
    scale_color_manual(values=mypalette)+  
    scale_y_continuous(limits=c(-0.45,0.3), breaks=seq(-0.4,0.3,0.1), 
                       labels=seq(-0.4,0.3,0.1)) +
    xlab("Child age, months") +  
    ylab("Difference in length-for-age\nZ-score per month")+
    geom_hline(yintercept = -0) +
    #ggtitle("b") +
    theme(plot.title = element_text(hjust=0, size = 20, face = "bold"),
          strip.text.x = element_text(size=20, face="bold"),
          axis.title.x = element_text(size=20),
          axis.title.y = element_text(size=20),
          legend.position = "bottom",
          legend.text = element_text(size=16),
          legend.title = element_blank())
  
  return(plot_laz)
}

# Plot for ELICIT
plot_e <- plot(elicit)

ggsave(plot_e, filename = paste0(BV_dir, "/results/figures/lazVelE.png"), 
       width=10, height=8)

# Plot for VITAL
plot_v <- plot(vital)

ggsave(plot_v, filename = paste0(BV_dir, "/results/figures/lazVelV.png"), 
       width=10, height=8)





