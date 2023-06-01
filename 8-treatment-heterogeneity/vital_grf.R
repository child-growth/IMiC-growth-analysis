
#-------------------------------------------------------------------------------
# GRF causal forest treatment heterogeneity
# Inputs:
#   Wide dataset + biomarker data for VITAL trial
# Outputs:
#   A dataframe of results
#-------------------------------------------------------------------------------

rm(list=ls())
source(paste0(here::here(), "/0-config.R"))
library(caret)
library(grf)
library(DiagrammeRsvg)
set.seed(63971)

#load dataset of HMO biomarkers and baseline variables
dfull <- readRDS("/data/imic/data/raw_lab_data/vital/merged_vital/hmoClean.RDS")
#dfull <- readRDS("C:/Users/andre/Downloads/hmoClean.RDS")


head(dfull)

table(dfull$arm_base)

#drop control arm and just contrast nutritional supplementation vs  nutritional supplementation + AZT
df <- dfull %>% filter(arm_base!="Control") %>%
                mutate(tr=ifelse(arm_base=="Nutrient supplement+Ex.BreastFeed+AZT",1,0))

#Set outcome, treatment, and biomarkers

Y = "haz_6m_simulated" #scrambles outcome assignment
# Y = "haz_m6" #outcome assignment
W = "tr" #treatment assignment (called W in grf), 0= Control arm

#baseline variables:
base_vars <- c("bmid_base", "subjid", "country", "studyid", "subjido", 
        "sex_base", "brthyr_base", "brthweek_base", "mage_base", 
        "parity_base", "nlchild_base", "nperson_base", "nrooms_base", 
        "meducyrs_base", "h2osrcp_base", "agedays_base", "epochn_base", 
        "epoch_base", "mhtcm_base", "mwtkg_base", "mbmi_base", "dlvloc_base", 
        "wtkg_base", "lencm_base", "bmi_base", "muaccm_base", "waz_base", 
        "haz_base", "whz_base", "baz_base", "feeding_base", "dur_bf_base", 
        "dur_ebf_base", "visit_r_fl_base", "dur_r_base", "fever_r_base", 
        "cough_r_base", "anti_r_base", "citytown_base", "gagebrth_base", 
        "gagecm_base", "birthwt_base", "birthlen_base", "birthord_base", 
        "gravida_base", "nlivbrth_base", "floor_base", "gagedays_base", 
        "postbmi_base", "mmuaccm_base", "delivery_base", "bfinittm_base", 
        "cmfdint_base", "formlkfl_base", "fever_base", "diarr_base", 
        "physican_base", "hosp_base", "antibiot_base", "haz_m6", "anti_inj_base",
        "anti_oral_base")


base_vars <- c("country", "studyid", 
               "sex_base", "brthyr_base", "brthweek_base", "mage_base", 
               "parity_base", "nlchild_base", "nperson_base", "nrooms_base", 
               "meducyrs_base", "h2osrcp_base", "agedays_base", "epochn_base", 
               "epoch_base", "mhtcm_base", "mwtkg_base", "mbmi_base", "dlvloc_base", 
               "wtkg_base", "lencm_base", "bmi_base", "muaccm_base", "waz_base", 
               "haz_base", "whz_base", "baz_base", "feeding_base", "dur_bf_base", 
               "dur_ebf_base", "visit_r_fl_base", "dur_r_base", "fever_r_base", 
               "cough_r_base", "anti_r_base", "citytown_base", "gagebrth_base", 
               "gagecm_base", "birthwt_base", "birthlen_base", "birthord_base", 
               "gravida_base", "nlivbrth_base", "floor_base", "gagedays_base", 
               "postbmi_base", "mmuaccm_base", "delivery_base", "bfinittm_base", 
               "cmfdint_base", "formlkfl_base", "fever_base", "diarr_base", 
               "physican_base", "hosp_base", "antibiot_base", "anti_inj_base",
               "anti_oral_base")

biomarkers <- c("X2.FL_pct",       "X3FL_pct",        "DFLac_pct",       "X3.SL_pct",       "X6.SL_pct",       "LNT_pct",        
                "LNnT_pct",        "LNFP.I_pct",      "LNFP.II_pct",     "LNFP.III_pct",    "LSTb_pct",        "LSTc_pct",        "DFLNT_pct",       "LNH_pct",        
                "DSLNT_pct",       "FLNH_pct",        "DFLNH_pct",       "FDSLNH_pct",      "DSLNH_pct")


base_vars[!(base_vars %in% colnames(df))]

#process predictors
X_df <- df %>% subset(., select = c(base_vars,biomarkers))

#remove near zero variance predictors
X_df = X_df[, -nzv(X_df)]

#Convert factors to indicators:
X_df = model.matrix(~. , data=X_df)

grf_model <- causal_forest(X=X_df, 
                           Y=df$haz_6m_simulated, 
                           W=df$tr,
                           min.node.size=1)

saveRDS(grf_model, file=paste0(here::here(),"/results/vital_grf_model.RDS"))

# Plot an example tree from the GRF
plot(tree <- get_tree(grf_model, 36))
tree.plot = plot(tree)
cat(DiagrammeRsvg::export_svg(tree.plot), file = 'plot.svg')

# Plot histogram of out-of-box predictions
grf_model.oob <- predict(grf_model)

## Variable Importance
var_imp <- variable_importance(grf_model)
# add var names
names(var_imp) <- colnames(X_df)
# sort by importance
sorted_var_imp <- sort(var_imp, decreasing = T)
print(sorted_var_imp)
variables <- sorted_var_imp

topten.val <- (sorted_var_imp[1:10])
topten.var <- data.frame(var = names(topten.val)) %>%
  distinct()

topten.all <- data.frame(var = names(sorted_var_imp), value = sorted_var_imp, original_order=match(names(sorted_var_imp), names(var_imp))) %>%
  arrange(-value) %>%
  mutate(order = row_number()) %>%
  filter(var %in% topten.var$var)

plot.varimp <- data.frame(cbind(
                          variable = topten.all$var, 
                          value = topten.all$value,
                          order = topten.all$order))
plot.varimp %>%
  mutate(value = as.numeric(value)) %>%
  ggplot() + 
  geom_col(aes(x = reorder(variable, value), y = value), position = "dodge") + 
  geom_text(aes(x = reorder(variable, value), y = value, label = order), hjust = -1) +
  coord_flip() +
  labs(y = paste0("Importance (out of ", length(sorted_var_imp), " variables)"), x = "") +
  theme_minimal() +
  theme(legend.position = "none",
        panel.spacing = unit(1.25, "lines"),
        strip.text = element_text(size = 12),
        axis.title.x = element_text(size = 14),
        axis.text.y = element_text(size = 8))

#save image
ggsave(height = 12, width = 10,
       filename = "8-treatment-heterogeneity/varimp_total.jpg")

# Plot data with variable of most importance
sorted_most_imp_var <- data.frame(value = X_df[, topten.all[1,3]], predictions = grf_model$predictions) %>%
  arrange(-value)

png("8-treatment-heterogeneity/tau_most_imp_var.png")
plot(sorted_most_imp_var$value, sorted_most_imp_var$predictions, xlab = "x", ylab = "tau", type = "l")
dev.off()

## Best Linear Projection
blp <- broom::tidy(best_linear_projection(grf_model, X_df[, topten.all$original_order]))

blp %>%
  filter(term != "(Intercept)") %>%
  mutate(conf.low = estimate - 1.96*std.error,
         conf.high = estimate + 1.96*std.error,
         sig = as.factor(as.numeric(p.value<0.2))) %>%
  ggplot() +
  aes(x = reorder(term, estimate), y = estimate) + 
  geom_point() +
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high), width=.2, position=position_dodge(0.2)) +
  geom_hline(aes(yintercept = 0), color = "red", linetype = "dashed") +
  theme_minimal() +
  theme(legend.position = "none") +
  coord_flip() +
  #scale_shape_manual(values = c("circle", "star")) + 
  labs(#caption = "Starred estimate are significant at p<0.2",
    y = "Estimate", x = "")

#save image
ggsave(height = 12, width = 10,
       filename = "8-treatment-heterogeneity/BLP.jpg")

## Rank Average Treatment Effect
#Define training dataset
train <- sample(1:nrow(X_df), round(nrow(X_df) * .5))

# Train on half of the data
grf_train <- causal_forest(X=X_df[train,], 
                           Y=df[train,]$haz_6m_simulated, 
                           W=df[train,]$tr,
                           min.node.size=1)

# Compute prioritization based on estimated treatment effects
grf_priority <- predict(grf_train, X_df[-train,])$predictions

# Predict on test data
grf_model.test <- predict(grf_model, X_df[-train,])
hist(grf_model.test$predictions)

# Evaluate model on held out data
grf_eval <- causal_forest(X=X_df[-train,], 
                          Y=df[-train,]$haz_6m_simulated, 
                          W=df[-train,]$tr,
                          min.node.size=1)

# Calculate rank average treatment effects
rate <- rank_average_treatment_effect(
  grf_eval,
  grf_priority,
  target = "AUTOC"
)

png("8-treatment-heterogeneity/grf_rate.png")
plot(rate, main = "TOC: By decreasing estimated CATE")
dev.off()

# By most important variable
rate.most_imp_var <- rank_average_treatment_effect(
  grf_model,
  -X_df[, topten.all[1,3]],
  target = "AUTOC"
)

png("8-treatment-heterogeneity/grf_rate_most_imp_var.png")
plot(rate.most_imp_var, main = paste0("TOC: By increasing ", topten.all[1,1]))
dev.off()
