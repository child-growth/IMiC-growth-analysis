#--------------------------------------------------------------#
# Title: WASH-Benefits Analysis of Heterogeneous Treatment Effects
#        Outcome: 7-day prevalence of diarrhea, 
#                 Length-for-age Z-score,
#                 and combined EASQ Z-score
# Author: Caitlin Hemlock
# Date: March 17, 2023
#--------------------------------------------------------------#

# install packages
# devtools::install_github('susanathey/causalTree')

#load packages
library(causalTree)
library(grf)
library(glmnet)
library(splines)
library(MASS)
library(lmtest)
library(sandwich)
library(ggplot2)
library(stringr)
library(tidyverse)
library(haven)
library(flextable)

#read in data
if(here::here()=="C:/Users/andre/Documents/chemlock_dissertation"){
  #main <- 
  #diar <-
  #tr <- 
}else{
  main <- readRDS("~/Documents/GitHub/chemlock_dissertation/HTE/final HTE data.RDS")
  diar <- read.csv(file = "~/Documents/WASH Benefits/Data/Bangladesh/washb-bangladesh-diar-public.csv")
  anthro <- read.csv(file = "~/Documents/WASH Benefits/Data/Bangladesh/washb-bangladesh-anthro-public.csv")
  dev <- read_dta(file = "/Users/caitlinhemlock/Library/CloudStorage/Box-Box/WASHB Child Development/wash-b_easq_std_5mar2021.dta")
  publicids <- read.csv("~/Documents/GitHub/chemlock_dissertation/HTE/public-ids.csv")
  tr <- read.csv(file = "~/Documents/WASH Benefits/Data/Bangladesh/washb-bangladesh-tr-public.csv")
}

#merge in treatment data
main <- merge(tr, main, by = c("clusterid", "block"))  %>%
  #replace clusterid with sequential group ID
  group_by(clusterid) %>%
  mutate(group_id = cur_group_id()) %>%
  ungroup()  %>%
  relocate(group_id, .after = "dataid")

# Treatment: 
treatment <- "tr"
arms <- c("Sanitation", "Handwashing", "Water", "WSH", "Nutrition", "Nutrition + WSH")           

#create diarrhea dataset - 1 obs for every child in compound at year 2
diar <- diar %>%
  filter(svy == 2) %>%
  dplyr::select(dataid, childid, clusterid, block, diar7d)

main.diar <- left_join(diar, main, by = c("dataid", "clusterid", "block")) %>%
  #remove households without baseline survey
  filter(!is.na(shoes)) %>%
  filter(!is.na(diar7d))

#create scaled database for BLP
main.diar.scale <- main.diar

continuous <- c("n_hh", "n_com","hh_sizeavg", "n_u3_com",               
                "n_o40_hh","n_rooms","landacre","momage",
                "n_chickens_com","n_chickens_hh","n_cows_com", "n_cows_hh",
                "n_goats_com","n_goats_hh","distmarket","distHC","cluster_popdens",        
                "hh_density", "Nhh", "mins_to_city","mins_to_dhaka")

for(i in continuous){
  scaling <- abs(quantile(main.diar.scale[[i]], probs = 0.75) - quantile(main.diar.scale[[i]], probs = 0.25))
  main.diar.scale[[i]] <- main.diar.scale[[i]]/scaling
}

#create anthro dataset - 1 obs for target children at year 2
anthro <- anthro %>%
  filter(svy == 2) %>%
  dplyr::select(dataid, childid, clusterid, block, laz)

main.anthro <- merge(main, anthro, by = c("dataid", "clusterid", "block"), all = T) %>%
  relocate(c(childid, laz), .after = "dataid") %>%
  filter(!is.na(laz)) %>%
  #remove households without baseline survey
  filter(!is.na(shoes))

#create scaled database for BLP
main.anthro.scale <- main.anthro
for(i in continuous){
  scaling <- abs(quantile(main.anthro.scale[[i]], probs = 0.75) - quantile(main.anthro.scale[[i]], probs = 0.25))
  main.anthro.scale[[i]] <- main.anthro.scale[[i]]/scaling
}

#create dev dataset - 1 obs for target children at year 2
dev <- dev %>%
  dplyr::select(dataid, childid, clusterid, block, z_combined) %>%
  mutate(across(c("dataid", "clusterid", "block"), as.integer))

#merge in public IDs to match other datasets
dev <- left_join(dev, publicids, by = c("dataid", "clusterid", "block")) %>%
  select(-clusterid, -dataid, -block) %>%
  rename(clusterid = clusterid_r,
         dataid = dataid_r,
         block = block_r)

main.dev <- merge(main, dev,  by = c("dataid", "clusterid", "block"), all = T) %>%
  relocate(c(childid, z_combined), .after = "dataid") %>%
  filter(!is.na(z_combined)) %>%
  #remove households without baseline survey
  filter(!is.na(shoes))

#created scaled dataset for BLP
main.dev.scale <- main.dev
for(i in continuous){
  scaling <- abs(quantile(main.dev.scale[[i]], probs = 0.75) - quantile(main.dev.scale[[i]], probs = 0.25))
  main.dev.scale[[i]] <- main.dev.scale[[i]]/scaling
}

outcomes <- c("diar7d", "laz", "z_combined")

### Generate causal forests for each arm and outcome
set.seed(12345)
trees <- list()
for(j in outcomes){
  for(i in arms){
    
    if(j == "diar7d"){
      data <- main.diar %>%
        #select treatment and control only
        filter(tr == i | tr == "Control") %>%
        #make treatment an indicator
        mutate(tr = ifelse(tr == i, 1, 0)) 
    } else if(j=="laz"){
      data <- main.anthro %>%
        #select treatment and control only
        filter(tr == i | tr == "Control") %>%
        #make treatment an indicator
        mutate(tr = ifelse(tr == i, 1, 0))
    } else{
      data <- main.dev %>%
        #select treatment and control only
        filter(tr == i | tr == "Control") %>%
        #make treatment an indicator
        mutate(tr = ifelse(tr == i, 1, 0))
    }
    
    X <- as.matrix(data[,8:ncol(data)])
    W <- unlist(as.vector(data[,treatment]))
    Y <- unlist(as.vector(data[,j]))
    
    forest.tau <- causal_forest(
      X = X,                       #covariates
      Y = Y,                       #outcomes  
      W = W,                       #treatment
      W.hat = .33,                 #treatment propensity (known)
      clusters = data$group_id     #cluster ID
    )
    
    trees[[paste(i, "-", j)]] <- forest.tau 
    rm(forest.tau)
    print(i)
  }
  print(j)
}

### 2. Variable importance
#create list for each treatment causal forest
variables <- list()
for(i in 1:length(trees)){
  #extract variable importance
  var_imp <- c(variable_importance(trees[[i]]))
  #add var names
  names(var_imp) <- colnames(X)
  #sort by importance
  sorted_var_imp <- sort(var_imp, decreasing = T)
  variables[[i]] <- sorted_var_imp
  names(variables)[[i]] <- names(trees)[[i]]
}

#plot variable importance
#1. extract top 10 important variables for all arms
all.var <- NULL
for(i in 1:length(variables)){
  temp <- (variables[[i]][1:5])
  all.var <- rbind(all.var, data.frame(var = names(temp))) %>%
    distinct()
}

plot.varimp <- NULL
for(i in 1:length(variables)){
  temp <- data.frame(var = names(variables[[i]]), value = variables[[i]]) %>%
    arrange(-value) %>%
    mutate(order = row_number()) %>%
    filter(var %in% all.var$var)
  
  temp2 <- data.frame(cbind(tr = names(variables)[[i]], 
                            variable = temp$var, 
                            value = temp$value,
                            order = temp$order))
  
  plot.varimp <- rbind(plot.varimp, temp2)
}

total <- plot.varimp %>%
  separate(tr, c("tr", "outcome"), sep = " - ") %>%
  mutate(name = case_when(variable == "mins_to_city"	~	"Travel time to city"	,
                          variable == "chick_inside_Always" ~ "Chickens always go inside house",
                          variable == "anfeces_child" ~ "Animal feces observed in child play area",
                          variable == "mins_to_dhaka"	~	"Travel time to Dhaka"	,
                          variable == "landacre"	~	"Amount of land owned"	,
                          variable == "n_com"	~	"Number of individuals in compound"	,
                          variable == "n_chickens_com"	~	"Number of chickens in compound"	,
                          variable == "n_chickens_hh"	~	"Number of chickens in household"	,
                          variable == "n_cows_com"	~	"Number of cows in compound"	,
                          variable == "distmarket"	~	"Walking distance to market"	,
                          variable == "distHC"	~	"Walking distance to healthcare facility"	,
                          variable == "hh_density"	~	"Household density"	,
                          variable == "hh_sizeavg"	~	"Average household size in compound"	,
                          variable == "cluster_popdens"	~	"Cluster-level population density"	,
                          variable == "momage" ~ "Maternal age",
                          variable == "HHwealth_4" ~ "Household wealth in 4th quartile"),
         value = as.numeric(value),
         topten = ifelse(as.numeric(order)<=5, tr, NA)) %>%
  group_by(variable) %>%
  mutate(n = sum(value))

total %>%
  mutate(outcome = case_when(outcome == "diar7d" ~ "7-day diarrhea prevalence",
                             outcome == "laz" ~ "Length-for-age Z-score",
                             outcome == "z_combined" ~ "Combined EASQ Z-score"),
         outcome = factor(outcome, levels = c("7-day diarrhea prevalence",
                                              "Length-for-age Z-score",
                                              "Combined EASQ Z-score")),
         tr = factor(tr, levels = c("Sanitation", "Handwashing", "Water",
                                    "WSH", "Nutrition", "Nutrition + WSH"))) %>%
  ggplot() + 
  geom_col(aes(x = reorder(name, n), y = value, group = tr, fill = topten), position = "dodge") + 
  geom_text(aes(x = reorder(name, n), y = value, label = order), hjust = -1) +
  facet_grid(tr~outcome) + 
  coord_flip() +
  labs(y = "Importance (out of 82 total variables)", x = "", fill = "Treatment Arm") +
  theme_minimal() +
  theme(legend.position = "none",
        panel.spacing = unit(1.25, "lines"),
        strip.text = element_text(size = 12),
        axis.title.x = element_text(size = 14),
        axis.text.y = element_text(size = 8))

#save image
ggsave(height = 12, width = 10,
       filename = "~/Documents/GitHub/chemlock_dissertation/HTE/figs/varimp_total.jpg")

### 2. Best Linear Projection
blp <- NULL
for(j in outcomes){
  for(i in arms){
    
    if(j == "diar7d"){
      data <- main.diar.scale %>%
        #select treatment and control only
        filter(tr == i | tr == "Control") %>%
        #make treatment an indicator
        mutate(tr = ifelse(tr == i, 1, 0)) 
      
      
    } else if(j=="laz"){
      data <- main.anthro.scale %>%
        #select treatment and control only
        filter(tr == i | tr == "Control") %>%
        #make treatment an indicator
        mutate(tr = ifelse(tr == i, 1, 0))
    } else{
      data <- main.dev.scale %>%
        #select treatment and control only
        filter(tr == i | tr == "Control") %>%
        #make treatment an indicator
        mutate(tr = ifelse(tr == i, 1, 0))
    }
    
    X <- as.matrix(data[,8:ncol(data)])
    colnames(X)[colnames(X) %in% as.vector(all.var$var)]
    A <- X[,which(colnames(X) %in% as.vector(all.var$var))]
    
    temp <- cbind(tr = i, outcome = j, broom::tidy(best_linear_projection(trees[[paste(i, "-", j)]], A)))
    blp <- rbind(blp, temp)
  }
}

#plot BLP results
blp <- left_join(blp, total %>% rename(term = variable), by = c("tr", "term", "outcome"))

blp %>%
  mutate(outcome = case_when(outcome == "diar7d" ~ "7-day diarrhea prevalence",
                             outcome == "laz" ~ "Length-for-age Z-score",
                             outcome == "z_combined" ~ "Combined EASQ Z-score"),
         outcome = factor(outcome, levels = c("7-day diarrhea prevalence",
                                              "Length-for-age Z-score",
                                              "Combined EASQ Z-score")),
         tr = factor(tr, levels = c("Sanitation", "Handwashing", "Water",
                                    "WSH", "Nutrition", "Nutrition + WSH"))) %>%
  filter(term != "(Intercept)") %>%
  mutate(name = ifelse(term %in% continuous, paste(name, "*", sep = ""), name)) %>%
  mutate(conf.low = estimate - 1.96*std.error,
         conf.high = estimate + 1.96*std.error,
         sig = as.factor(as.numeric(p.value<0.2))) %>%
  ggplot() +
  aes(x = reorder(name, n), y = estimate, group=tr, color=topten) + 
  geom_point() +
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high), width=.2, position=position_dodge(0.2)) +
  geom_hline(aes(yintercept = 0), color = "red", linetype = "dashed") +
  theme_minimal() +
  theme(legend.position = "none") +
  facet_grid(tr~outcome, scales = "free_x") + 
  coord_flip() +
  #scale_shape_manual(values = c("circle", "star")) + 
  labs(#caption = "Starred estimate are significant at p<0.2",
    y = "Estimate", x = "")

#save image
ggsave(height = 12, width = 10,
       filename = "~/Documents/GitHub/chemlock_dissertation/HTE/figs/BLP_total.jpg")


### 3. Ranked Average Treatment Effect to Visualize CATEs over population
set.seed(12345)
ratedata <- NULL
for(j in outcomes){
  for(i in arms){
    
    if(j == "diar7d"){
      data <- main.diar %>%
        #select treatment and control only
        filter(tr == i | tr == "Control") %>%
        #make treatment an indicator
        mutate(tr = ifelse(tr == i, 1, 0)) 
    } else if(j=="laz"){
      data <- main.anthro %>%
        #select treatment and control only
        filter(tr == i | tr == "Control") %>%
        #make treatment an indicator
        mutate(tr = ifelse(tr == i, 1, 0))
    } else{
      data <- main.dev %>%
        #select treatment and control only
        filter(tr == i | tr == "Control") %>%
        #make treatment an indicator
        mutate(tr = ifelse(tr == i, 1, 0))
    }
    
    X <- as.matrix(data[,8:ncol(data)])
    W <- unlist(as.vector(data[,treatment]))
    Y <- unlist(as.vector(data[,outcome]))
    clusters <- unique(data$group_id)
    n <- length(clusters)
    
    #split data in half
    sample <- sample(clusters, n / 2)
    
    train <- which(data$group_id %in% sample)
    
    #train on one half
    cf.priority <- causal_forest(X[train, ], Y[train], W[train], clusters = data$group_id[train])
    
    # Compute a prioritization based on estimated treatment effects.
    if(j == "diar7d"){
      # -1: in this example the treatment should reduce the risk of an event occurring.
      priority.cate <- -1 * predict(cf.priority, X[-train, ])$predictions
    } else{
      priority.cate <- predict(cf.priority, X[-train, ])$predictions
    }
    
    # Estimate AUTOC on held out data.
    cf.eval <- causal_forest(X[-train, ], Y[-train], W[-train], clusters = data$group_id[-train])
    rate <- rank_average_treatment_effect(cf.eval, priority.cate)
    
    temp <- cbind(rate[["TOC"]], tr = i, outcome = j, 
                  AUTOC = paste(round(rate$estimate, 2), "+/", round(1.96 * rate$std.err, 2)))
    ratedata <- rbind(ratedata, temp)
    print(i)
  }
  print(j)
}

#plot
ratedata %>%
  group_by(tr, outcome) %>%
  mutate(AUTOC = ifelse(row_number() != 1, NA, paste("AUTOC:\n", AUTOC))) %>%
  ungroup() %>%
  mutate(conf.low = estimate - 1.96*std.err,
         conf.high = estimate + 1.96*std.err,
         outcome = case_when(outcome == "diar7d" ~ "7-day diarrhea prevalence",
                             outcome == "laz" ~ "Length-for-age Z-score",
                             outcome == "z_combined" ~ "Combined EASQ Z-score"),
         outcome = factor(outcome, levels = c("7-day diarrhea prevalence",
                                              "Length-for-age Z-score",
                                              "Combined EASQ Z-score")),
         tr = factor(tr, levels = c("Sanitation", "Handwashing", "Water",
                                    "WSH", "Nutrition", "Nutrition + WSH"))) %>%
  ggplot(aes(color = tr)) +
  geom_line(aes(x = q, y = estimate)) +
  geom_line(aes(x = q, y = conf.low), linetype = "dashed") +
  geom_line(aes(x = q, y = conf.high), linetype = "dashed") +
  geom_hline(aes(yintercept = 0), linetype = "dotted") +
  geom_text(aes(y = -0.1, x = 0.8, label = AUTOC), color = "black") +
  theme_minimal() +
  facet_grid(outcome ~ tr, scales = "free") +
  theme(legend.position = "none") +
  labs(x = "Treated Fraction of Population", y = "Estimated Treatment Effect")

#save image
ggsave(height = 8, width = 15,
       filename = "~/Documents/GitHub/chemlock_dissertation/HTE/figs/RATE TOC_total.jpg")

#### 4. Predict treatment effects in quintiles

## load functions
#1. FWER correction
romano_wolf_correction <- function(t.orig, t.boot) {
  abs.t.orig <- abs(t.orig)
  abs.t.boot <- abs(t.boot)
  abs.t.sorted <- sort(abs.t.orig, decreasing = TRUE)
  
  max.order <- order(abs.t.orig, decreasing = TRUE)
  rev.order <- order(max.order)
  
  M <- nrow(t.boot)
  S <- ncol(t.boot)
  
  p.adj <- rep(0, S)
  p.adj[1] <- mean(apply(abs.t.boot, 1, max) > abs.t.sorted[1])
  for (s in seq(2, S)) {
    cur.index <- max.order[s:S]
    p.init <- mean(apply(abs.t.boot[, cur.index, drop=FALSE], 1, max) > abs.t.sorted[s])
    p.adj[s] <- max(p.init, p.adj[s-1])
  }
  p.adj[rev.order]
}

#2. test for significant differences between quartiles
summary_rw_lm <- function(model, indices=NULL, cov.type="HC2", num.boot=10000, seed=2020) {
  if (is.null(indices)) {
    indices <- 1:nrow(coef(summary(model)))
  }
  # Grab the original t values.
  summary <- coef(summary(model))[indices,,drop=FALSE]
  t.orig <- summary[, "t value"]
  
  # Null resampling.
  # This is a bit of trick to speed up bootstrapping linear models.
  # Here, we don't really need to re-fit linear regressions, which would be a bit slow.
  # We know that betahat ~ N(beta, Sigma), and we have an estimate Sigmahat.
  # So we can approximate "null t-values" by
  #  - Draw beta.boot ~ N(0, Sigma-hat)     note the 0 here, this is what makes it a *null* t-value.
  #  - Compute t.boot = beta.boot / sqrt(diag(Sigma.hat))
  Sigma.hat <- sandwich::vcovHC(model, type=cov.type)[indices, indices]
  se.orig <- sqrt(diag(Sigma.hat))
  num.coef <- length(se.orig)
  beta.boot <- MASS::mvrnorm(n=num.boot, mu=rep(0, num.coef), Sigma=Sigma.hat)
  t.boot <- sweep(beta.boot, 2, se.orig, "/")
  p.adj <- romano_wolf_correction(t.orig, t.boot)
  
  result <- cbind(summary[,c(1,2,4),drop=F], p.adj)
  colnames(result) <- c('Estimate', 'Std. Error', 'Orig. p-value', 'Adj. p-value')
  result
}

# create quartiles and heatmaps for descriptive differences between quartiles
set.seed(12345)
quartile.eff <- NULL
heatmaps <- NULL
for(j in outcomes){
  for(i in arms){
    
    if(j == "diar7d"){
      data <- main.diar %>%
        #select treatment and control only
        filter(tr == i | tr == "Control") %>%
        #make treatment an indicator
        mutate(tr = ifelse(tr == i, 1, 0)) 
    } else if(j=="laz"){
      data <- main.anthro %>%
        #select treatment and control only
        filter(tr == i | tr == "Control") %>%
        #make treatment an indicator
        mutate(tr = ifelse(tr == i, 1, 0))
    } else{
      data <- main.dev %>%
        #select treatment and control only
        filter(tr == i | tr == "Control") %>%
        #make treatment an indicator
        mutate(tr = ifelse(tr == i, 1, 0))
    }
    
    n <- nrow(data)
    X <- as.matrix(data[,8:ncol(data)])
    W <- unlist(as.vector(data[,treatment]))
    Y <- unlist(as.vector(data[,j]))
    covariates <- colnames(X)
    fmla <- formula(paste0("~ 0 + ", paste0(covariates, collapse="+")))
    
    # Prepare for data.splitting
    # Assign a fold number to each observation.
    # The argument 'clusters' in the next step will mimic K-fold cross-fitting.
    # Following code from https://bookdown.org/stanfordgsbsilab/ml-ci-tutorial/hte-i-binary-treatment.html#data-driven-hypotheses
    num.folds <- 10
    folds <- sort(seq(n) %% num.folds) + 1
    
    # take clustering into account <- CURRENTLY NOT WORKING
    # data <- data %>%
    #   group_by(group_id) %>%
    #   mutate(fold = sample(1:10, 1, replace = T)) %>% ungroup()
    # folds <- data$fold
    
    # Randomized settings with fixed and known probabilities (here: 0.33 to account for double sized control arm).
    forest <- causal_forest(X, Y, W, W.hat=.33, clusters = folds)
    
    # Retrieve out-of-bag predictions.
    # Predictions for observation in fold k will be computed using trees that were not trained using observations for that fold.
    tau.hat <- predict(forest)$predictions
    
    # Rank observations *within each fold* into quintiles according to their CATE predictions.
    num.rankings <- 4
    ranking <- rep(NA, n)
    for (fold in seq(num.folds)) {
      tau.hat.quantiles <- quantile(tau.hat[folds == fold], probs = seq(0, 1, by=1/num.rankings))
      if(j == "diar7d"){
        ranking[folds == fold] <- cut(tau.hat[folds == fold], tau.hat.quantiles, include.lowest=TRUE,labels=c(4,3,2,1))
      } else{
        ranking[folds == fold] <- cut(tau.hat[folds == fold], tau.hat.quantiles, include.lowest=TRUE,labels=seq(num.rankings))  
      }
    }
    
    #obtain margins - interaction coefficient is treatment effect within each quartile
    fmla <- paste0(j, " ~ 0 + factor(ranking) + factor(ranking):", treatment)
    ols.ate <- lm(fmla, data=data) 
    ols.ate <- coeftest(ols.ate, vcov=vcovHC(ols.ate, type='HC2'))
    interact <- which(grepl(":", rownames(ols.ate)))
    ols.ate <- data.frame("ols", paste0("Q", seq(num.rankings)), ols.ate[interact, 1:2])
    rownames(ols.ate) <- NULL # just for display
    colnames(ols.ate) <- c("method", "ranking", "estimate", "std.err")
    
    ols.ate <- cbind(ols.ate, tr = i, outcome = j)
    
    #testing differences between groups
    fmla <- paste0(j, "~ ranking + ", treatment, " + ranking:", treatment)
    ols <- lm(fmla, data=transform(data, ranking=factor(ranking)))
    interact <- which(sapply(names(coef(ols)), function(x) grepl(":", x)))
    
    res <- summary_rw_lm(ols, indices=interact)
    res <- data.frame(ranking = c("Q2", "Q3", "Q4"), adj.p = res[,4])
    rownames(res) <-  1:nrow(res)
    
    ols.ate <- left_join(ols.ate, res, by = "ranking")
    quartile.eff <- rbind(quartile.eff, ols.ate)
    
    # # Computing AIPW scores.
    # tau.hat <- predict(trees[[i]])$predictions
    # e.hat <- trees[[i]]$W.hat # P[W=1|X]
    # m.hat <- trees[[i]]$Y.hat # E[Y|X]
    # 
    # # Estimating mu.hat(X, 1) and mu.hat(X, 0) for obs in held-out sample
    # # Note: to understand this, read equations 6-8 in this vignette:
    # # https://grf-labs.github.io/grf/articles/muhats.html
    # mu.hat.0 <- m.hat - e.hat * tau.hat        # E[Y|X,W=0] = E[Y|X] - e(X)*tau(X)
    # mu.hat.1 <- m.hat + (1 - e.hat) * tau.hat  # E[Y|X,W=1] = E[Y|X] + (1 - e(X))*tau(X)
    # 
    # # AIPW scores
    # aipw.scores <- tau.hat + W / e.hat * (Y -  mu.hat.1) - (1 - W) / (1 - e.hat) * (Y -  mu.hat.0)
    # ols <- lm(aipw.scores ~ 0 + factor(ranking))
    # forest.ate <- data.frame("aipw", paste0("Q", seq(num.rankings)), coeftest(ols, vcov=vcovHC(ols, "HC2"))[,1:2])
    # colnames(forest.ate) <- c("method", "ranking", "estimate", "std.err")
    # rownames(forest.ate) <- NULL # just for display
    # forest.ate
    # 
    # res <- rbind(forest.ate, ols.ate)
    
    ### heatmap
    df <- mapply(function(covariate) {
      # Looping over covariate names
      # Compute average covariate value per ranking (with correct standard errors)
      fmla <- formula(paste0(covariate, "~ 0 + ranking"))
      ols <- lm(fmla, data=transform(data, ranking=factor(ranking)))
      ols.res <- coeftest(ols, vcov=vcovHC(ols, "HC2"))
      
      # Retrieve results
      avg <- ols.res[,1]
      stderr <- ols.res[,2]
      sig = case_when(exp(-0.717*(avg/stderr) - 0.416*(avg/stderr)^2) < 0.001 ~ "***",
                      exp(-0.717*(avg/stderr) - 0.416*(avg/stderr)^2) < 0.05 ~ "**",
                      exp(-0.717*(avg/stderr) - 0.416*(avg/stderr)^2) < 0.1 ~ "*")
      
      # Tally up results
      data.frame(covariate, avg, stderr, ranking=paste0("Q", seq(num.rankings)),
                 # Used for coloring
                 scaling=pnorm((avg - mean(avg))/sd(avg)),
                 # We will order based on how much variation is 'explain' by the averages
                 # relative to the total variation of the covariate in the data
                 variation=sd(avg) / sd(data[,covariate]),
                 sig = sig,
                 # String to print in each cell in heatmap below
                 labels=paste0(signif(avg, 3), "\n", "(", signif(stderr, 3), ")", sig))
    }, covariates[covariates %in% plot.varimp$variable], SIMPLIFY = FALSE)
    df <- do.call(rbind, df)
    
    # a small optional trick to ensure heatmap will be in decreasing order of 'variation'
    #df$covariate <- reorder(df$covariate, order(df$variation))
    
    #join ordering and names from variable importance plots
    df <- left_join(df,
                    total %>% 
                      filter(tr == i & outcome == j) %>%
                      select(variable, name, n, order) %>%
                      rename(covariate = variable),
                    by = "covariate") %>%
      mutate(newname = paste0(name, " (", order, ")"))
    
    # plot and save heatmap
    p <- ggplot(df) +
      aes(ranking, reorder(newname, n)) +
      geom_tile(aes(fill = scaling)) + 
      geom_text(aes(label = labels)) +
      scale_fill_gradient(low = "#E1BE6A", high = "#40B0A6") +
      theme_minimal() + 
      labs(y = "", x = "CATE estimate ranking", fill = "Z-score") +
      theme(plot.title = element_text(size = 11, face = "bold"),
            axis.text=element_text(size=11)) 
    
    heatmaps[[paste(i, "-", j)]] <- p 
    print(i)
  }
  print(j)
}

# Plotting the point estimate of average treatment effect 
# and 95% confidence intervals around it.
quartile.eff %>%
  #filter(tr == "Sanitation" & outcome == "z_combined") %>%
  mutate(conf.low = estimate - 1.96*std.err,
         conf.high = estimate + 1.96*std.err,
         sig = case_when(adj.p < 0.05 ~ 1,
                         TRUE ~ 0),
         outcome = case_when(outcome == "diar7d" ~ "7-day diarrhea prevalence",
                             outcome == "laz" ~ "Length-for-age Z-score",
                             outcome == "z_combined" ~ "Combined EASQ Z-score"),
         outcome = factor(outcome, levels = c("7-day diarrhea prevalence",
                                              "Length-for-age Z-score",
                                              "Combined EASQ Z-score")),
         tr = factor(tr, levels = c("Sanitation", "Handwashing", "Water",
                                    "WSH", "Nutrition", "Nutrition + WSH"))) %>%
  ggplot(aes(color=tr)) +
  geom_point(aes(x = ranking, y = estimate, shape = factor(sig)), size = 3, position=position_dodge(0.2)) +
  geom_errorbar(aes(x = ranking, ymin=conf.low, ymax=conf.high), position=position_dodge(0.2)) +
  geom_hline(aes(yintercept = 0), linetype = "dotted") +
  theme_minimal() +
  theme(legend.position="none") +
  labs(x = "Quartile", y = "Predicted Treatment Effect") +
  facet_grid(outcome~tr, scales = "free") +
  scale_shape_manual(values = c("circle", "star"))

#save image
ggsave(height = 8, width = 15,
       filename = "~/Documents/GitHub/chemlock_dissertation/HTE/figs/SGATE_total.jpg")

### 5. Classification Analysis 

## Option A. Heatmaps (from SGATE code)

#across outcomes within Sanitation arm
heatmaps[["Sanitation - diar7d"]] + labs(title = "Sanitation intervention, 7-day prevalence of diarrhea")
heatmaps[["Sanitation - z_combined"]]+ labs(title = "Sanitation intervention, Combined EASQ score")
heatmaps[["Sanitation - laz"]] + labs(title = "Sanitation intervention, LAZ")

#across arms within diarrhea outcome
heatmaps[["Sanitation - diar7d"]] + labs(title = "Sanitation intervention, 7-day prevalence of diarrhea")
heatmaps[["Handwashing - diar7d"]] + labs(title = "Handwashing intervention, 7-day prevalence of diarrhea")
heatmaps[["Water - diar7d"]] + labs(title = "Water intervention, 7-day prevalence of diarrhea")
heatmaps[["WSH - diar7d"]] + labs(title = "WSH intervention, 7-day prevalence of diarrhea")
heatmaps[["Nutrition - diar7d"]] + labs(title = "Nutrition intervention, 7-day prevalence of diarrhea")
heatmaps[["Nutrition + WSH - diar7d"]] + labs(title = "Nutrition + WSH intervention, 7-day prevalence of diarrhea")


## Option B. table without coloring (need to move pvalues from table to heatmaps)
#add labels for CLAN table
attr(main$mins_to_city, "label") <- "Travel time to city"
attr(main$mins_to_dhaka, "label") <- "Travel time to Dhaka"
attr(main$landacre, "label") <- "Amount of land owned"
attr(main$n_com, "label") <- "Number of individuals in compound"
attr(main$n_chickens_com, "label") <- "Number of chickens in compound"
attr(main$n_chickens_hh, "label") <- "Number of chickens in household"
attr(main$n_cows_com, "label") <- "Number of cows in compound"
attr(main$n_cows_hh, "label") <- "Number of cows in household"
attr(main$distmarket, "label") <- "Walking distance to market"
attr(main$distHC, "label") <- "Walking distance to healthcare facility"
attr(main$hh_density, "label") <- "Household density"
attr(main$hh_sizeavg, "label") <- "Average household size in compound"
attr(main$cluster_popdens, "label") <- "Cluster-level population density"
attr(main$monsoon, "label") <- "Intervention implemented in monsoon season"
attr(main$Nhh, "label") <- "Individuals in household"
attr(main$momage, "label") <- "Maternal age"
attr(main$hw_nearcook, "label") <- "Handwashing station near kitchen"

clan.all <- NULL
for(i in 1:length(trees)){
  tau.hat <- predict(trees[[i]])$predictions
  clusters <- trees[[i]]$clusters
  
  data <- main %>%
    filter(tr == names(trees)[[i]] | tr == "Control") %>%
    mutate(tr = ifelse(tr == names(trees)[[i]], 1, 0)) %>%
    filter(!is.na(diar7d))
  
  n <- nrow(data)
  ranking <- rep(NA, n)
  tau.hat.quantiles <- quantile(tau.hat, 
                                probs = seq(0, 1, by = 1/num.rankings))
  ranking <- cut(tau.hat, 
                 tau.hat.quantiles,
                 include.lowest=TRUE,
                 labels=seq(num.rankings))
  
  data$ranking <- ranking
  
  clan <- data %>%
    ungroup() %>%
    select(ranking, names(variables[[i]][1:5])) %>%
    filter(ranking %in% c(1, num.rankings)) %>%
    mutate(ranking = as.character(ranking)) %>%
    tbl_summary(by = "ranking") %>%
    add_p() %>%
    add_q(method = "bonferroni") %>%
    as_tibble()
  
  colnames(clan) <- 1:ncol(clan)
  
  clan <- rbind(c(names(trees)[[i]], NA, NA, NA, NA), clan)
  
  clan.all <- rbind(clan.all, clan)
}

table <- clan.all %>% 
  flextable() %>%
  align(j = 2:5, part = "all", align = "center") %>%
  bold(i = c(1,7,13,19,25,31)) %>%
  width(j = 1, width = 2) %>%
  width(j = 2:3, width = 1.5) %>%
  padding(padding.left = 15, i = c(2:6, 8:12, 14:18, 20:24, 26:30, 32:36)) %>%
  set_header_labels(`1` = "", `2` = "Quartile 1", `3` = "Quartile 4", `4` = "p-value", `5` = "q-value")

#save table
save_as_docx(table, path = "~/Documents/GitHub/chemlock_dissertation/HTE/tables/CLAN table.docx")


###### 7. Sensitivity analyses 

### A. using diarrhea outcome for all children at year 1 and year 2 measurements
### B. dropping population density
# Generate causal forests for each arm
main <- main %>% dplyr::select(-pop_density)
covariates <- names(main[,8:ncol(main)])

trees.sens <- list()
for(i in c(unique(tr$tr)[which(unique(tr$tr) != "Control")])){
  data <- main %>%
    filter(tr == i | tr == "Control") %>%
    mutate(tr = ifelse(tr == i, 1, 0)) %>%
    filter(!is.na(diar7d))
  
  n <- nrow(data)
  
  X <- as.matrix(data[,8:ncol(data)])
  nrow(X)
  W <- unlist(as.vector(data[,treatment]))
  Y <- unlist(as.vector(data[,outcome]))
  
  forest.tau <- causal_forest(
    X = X, 
    Y = Y, 
    W = W, 
    W.hat = .33,
    clusters = data$group_id)
  
  trees.sens[[i]] <- forest.tau 
  rm(forest.tau)
}

### Variable importance
variables <- NULL

for(i in 1:length(trees.sens)){
  # hist(tau.hat, main="CATE estimates", freq=F, las=1)
  var_imp <- c(variable_importance(trees.sens[[i]]))
  names(var_imp) <- covariates
  sorted_var_imp <- sort(var_imp, decreasing = T)
  
  temp <- data.frame(tr = names(trees.sens)[[i]], value = sorted_var_imp[1:10])
  temp <- data.frame(tr = temp$tr, variable = rownames(temp), value = temp$value)
  
  variables <- rbind(variables, temp)
}

#plot
variables %>%
  mutate(name = case_when(variable == "mins_to_city"	~	"Travel time to city"	,
                          variable == "mins_to_dhaka"	~	"Travel time to Dhaka"	,
                          variable == "landacre"	~	"Amount of land owned"	,
                          variable == "n_hh"	~	"Number of households in compound"	,
                          variable == "n_com"	~	"Number of individuals in compound"	,
                          variable == "n_chickens_com"	~	"Number of chickens in compound"	,
                          variable == "n_chickens_hh"	~	"Number of chickens in household"	,
                          variable == "n_cows_com"	~	"Number of cows in compound"	,
                          variable == "n_cows_hh"	~	"Number of cows in household"	,
                          variable == "distmarket"	~	"Walking distance to market"	,
                          variable == "distHC"	~	"Walking distance to healthcare facility"	,
                          variable == "hh_density"	~	"Household density"	,
                          variable == "hh_sizeavg"	~	"Average household size in compound"	,
                          variable == "n_goats_com"	~	"Number of goats in compound"	,
                          variable == "rainy"	~	"Intervention implemented in rainy season"	,
                          variable == "Nhh"	~	"Individuals in household",
                          variable == "momage" ~ "Maternal age",
                          variable == "hw_nearcook" ~ "Handwashing station near kitchen")) %>%
  group_by(variable) %>%
  mutate(n = sum(value)) %>%
  ggplot() + 
  geom_col(aes(x = reorder(name, n), y = value, fill = tr)) + 
  facet_wrap(~tr, nrow = 1) + 
  coord_flip() +
  labs(y = "Importance", x = "", fill = "Treatment Arm")




###### Extra code
#plot a particular tree
plot(tree <- get_tree(forest.tau, 50))
