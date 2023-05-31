

rm(list=ls())
source(paste0(here::here(), "/0-config.R"))

hmo_res <- readRDS(paste0(here::here(),"/results/tmle_res_HMO.RDS"))

head(hmo_res)

hmo_res <- hmo_res %>% arrange(tmle_est) %>%
  mutate(A=factor(A, levels=unique(A)))



p_ATE <- ggplot(hmo_res, aes(x=(A))) + 
  geom_point(aes(y= tmle_est), size = 4) +
  geom_errorbar(aes(ymin=lower, ymax= upper)) +
  coord_flip(ylim=range(-1.7, 1.1)) +
  ylab("Average Treatment Effect (above vs below median)") +
  xlab("HMO") +
  geom_hline(yintercept = 0) +
  theme(strip.background = element_blank(),
        legend.position="none",
        strip.text.x = element_text(size=12),
        axis.text.x = element_text(size=12),
        panel.grid.minor=element_blank()) +
  ggtitle("Ranked ATEs") +guides(shape=FALSE)
p_ATE



hmo_results <- hmo_fit$results
hmo_results$predictors <- c("nlchild_base", "nperson_base", "nrooms_base", 
      "meducyrs_base", "h2osrcp_base", "cookplac_base", "inctot_base")

hmo_results$estDiff[hmo_results$predictors %in% c("inctot_base","meducyrs_base")] <- -hmo_results$estDiff[hmo_results$predictors %in% c("inctot_base","meducyrs_base")]
hmo_results$CI.l[hmo_results$predictors %in% c("inctot_base","meducyrs_base")] <- -hmo_results$CI.l[hmo_results$predictors %in% c("inctot_base","meducyrs_base")]
hmo_results$CI.h[hmo_results$predictors %in% c("inctot_base","meducyrs_base")] <- -hmo_results$CI.h[hmo_results$predictors %in% c("inctot_base","meducyrs_base")]
                                                                                                                                                                                                                    
                                                                                                          
hmo_results <- hmo_results %>% arrange(estDiff) %>%
  mutate(predictors=factor(predictors, levels=unique(predictors)))


p_var_importance <- ggplot(hmo_results, aes(x=(predictors))) + 
  geom_point(aes(y=estDiff), size = 4) +
  geom_errorbar(aes(ymin=CI.l, ymax=CI.h)) +
  coord_flip(ylim=range(-0.02,0.05)) +
  ylab("R2 difference") +
  xlab("predictor") +
  geom_hline(yintercept = 0) +
  theme(strip.background = element_blank(),
        legend.position="none",
        strip.text.x = element_text(size=12),
        axis.text.x = element_text(size=12),
        panel.grid.minor=element_blank()) +
  ggtitle("Variable importance of baseline variables\nin predicting combined outcome") +guides(shape=FALSE)
p_var_importance
