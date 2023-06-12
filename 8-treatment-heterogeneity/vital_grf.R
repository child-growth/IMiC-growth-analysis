
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
set.seed(98348)


generate_grf_model <- function(dataset_filepath, outcome_column, 
                               treatment_arm, predictors,
                               remove_arms=c(), save_prefix=NULL) {
  # Read in dataset
  dfull <- readRDS(dataset_filepath)
  
  # Remove arms not needed for comparison; set treatment arm
  df <- dfull %>%
    filter(!arm_base %in% remove_arms) %>%
    mutate(tr=ifelse(arm_base==treatment_arm,1,0))
  
  # Process predictors
  X_df <- df %>% subset(., select = predictors)
  
  # Remove near zero variance predictors
  X_df <- X_df[, -nzv(X_df)]
  
  # Convert factors to indicators
  X <- model.matrix(~. , data=X_df)
  
  # Create GRF model
  grf_model <- causal_forest(X=X,
                             Y=as.numeric(unlist(df[outcome_column])), 
                             W=df$tr,
                             min.node.size=1)
  
  # Save GRF if save_prefix is defined
  if(!is.null(save_prefix)) {
    saveRDS(grf_model, file=paste0(save_prefix, "_grf_model.RDS"))
  }
  
  return(list("grf_model"=grf_model, "X"=X, "df"=df))
}


plot_example_tree <- function(grf_model, tree_number=1, save_prefix=NULL) {
  tree <- get_tree(grf_model, tree_number)
  tree.plot = plot(tree)
  if (!is.null(save_prefix)) {
    cat(DiagrammeRsvg::export_svg(tree.plot), file = paste0(save_prefix, '_tree_', 
                                                            tree_number ,'.svg'))
  }
}


plot_oob_hist <- function (grf_model, save_prefix=NULL) {
  grf_model.oob <- predict(grf_model)
  if (!is.null(save_prefix)) {
    png(paste0(save_prefix,'_oob_hist.png'))
  }
  hist(grf_model$predictions)
  if (!is.null(save_prefix)) {
    dev.off()
  }
}


plot_var_imp <- function (grf_model, X, save_prefix=NULL) {
  ## Variable Importance
  var_imp <- variable_importance(grf_model)
  # add var names
  names(var_imp) <- colnames(X)
  # sort by importance
  sorted_var_imp <- sort(var_imp, decreasing = T)
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
  if (!is.null(save_prefix)) {
    ggsave(height = 12, width = 10,
           filename = paste0(save_prefix, "_varimp.jpg"))
  }
  return(topten.all)
}

plot_most_imp_var <- function (grf_model, X, topten, save_prefix=NULL) {
  sorted_most_imp_var <- data.frame(value = X[, topten[1,3]], predictions = grf_model$predictions) %>%
    arrange(-value)
  if (!is.null(save_prefix)) {
    png(paste0(save_prefix, "_tau_most_imp_var.png"))
  }
  plot(sorted_most_imp_var$value, sorted_most_imp_var$predictions, xlab = "x", ylab = "tau", type = "l")
  if (!is.null(save_prefix)) {
    dev.off()
  }
}

plot_blp <- function(grf_model, X, topten, save_prefix=NULL) {
  blp <- broom::tidy(best_linear_projection(grf_model, X[, topten$original_order]))
  
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
  if (!is.null(save_prefix)) {
    ggsave(height = 12, width = 10,
           filename = paste0(save_prefix, "_BLP.jpg"))
  }
}

## Rank Average Treatment Effect
plot_rate <- function (grf_model, X, df, outcome_column, save_prefix=NULL) {
  #Define training data set
  train <- sample(1:nrow(X), round(nrow(X) * .5))
  
  # Train on half of the data
  grf_train <- causal_forest(X=X[train,], 
                             Y=as.numeric(unlist(df[outcome_column]))[train], 
                             W=df[train,]$tr,
                             min.node.size=1)
  
  # Compute prioritization based on estimated treatment effects
  grf_priority <- predict(grf_train, X[-train,])$predictions
  
  # Evaluate model on held out data
  grf_eval <- causal_forest(X=X[-train,], 
                            Y=as.numeric(unlist(df[outcome_column]))[-train], 
                            W=df[-train,]$tr,
                            min.node.size=1)
  
  # Calculate rank average treatment effects
  rate <- rank_average_treatment_effect(
    grf_eval,
    grf_priority,
    target = "AUTOC"
  )
  
  if (!is.null(save_prefix)) {
    png(paste0(save_prefix, "_rate.png"))
  }
  plot(rate, main = "TOC: By decreasing estimated CATE")
  if (!is.null(save_prefix)) {
    dev.off()
  }
  
  # By most important variable
  rate.most_imp_var <- rank_average_treatment_effect(
    grf_model,
    -X[, topten[1,3]],
    target = "AUTOC"
  )
  if (!is.null(save_prefix)) {
    png(paste0(save_prefix, "_rate_most_imp_var.png"))
  }
  plot(rate, main = paste0("TOC: By increasing ", topten[1,1]))
  if (!is.null(save_prefix)) {
    dev.off()
  }
}


dataset_filepath <-"/data/imic/data/raw_lab_data/vital/merged_vital/hmoClean.RDS"
outcome_column <- "haz_6m_simulated"
# outcome_column <- "haz_m6" #outcome assignment
treatment_arm <- "Nutrient supplement+Ex.BreastFeed+AZT"
# Set file path to save plots to
save_prefix <- paste0(here::here(),"/8-treatment-heterogeneity/results/vital_scrambled")

#baseline variables:
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



grf_output <- generate_grf_model(dataset_filepath, outcome_column, treatment_arm,
                                 c(base_vars, biomarkers), remove_arms=c("Control"),
                                 save_prefix=save_prefix)

plot_example_tree(grf_output$grf_model, tree_number=1, save_prefix=save_prefix)
plot_oob_hist(grf_output$grf_model, save_prefix=save_prefix)
topten <- plot_var_imp(grf_output$grf_model, grf_output$X, save_prefix=save_prefix)
plot_most_imp_var(grf_output$grf_model, grf_output$X, topten, save_prefix=save_prefix)
plot_blp(grf_output$grf_model, grf_output$X, topten, save_prefix=save_prefix)
plot_rate(grf_output$grf_model, grf_output$X, grf_output$df, outcome_column, save_prefix=save_prefix)
