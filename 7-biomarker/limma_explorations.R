
library(tidyverse)
library(broom)
library(limma)
library(EnhancedVolcano)
library(ggplot2)
#install.packages("fdrci")
library(fdrci)


# 1) Make Limma function that returns results from eBayes. 
# Note that this is not FDR adjusted but for volcaplots, the unadjusted p-values are preferred.
# 2) Make a topTable function that takes in the limma function and outputs FDR adjusted table.
# 3) Make a volcanoplot function that takes in the limma function and outputs volcano plots.
# 1) Limma function
limmaFunc <- function(data, outcome, matrix){
  
  # Make design matrix that will be fed into limma - Must be nxp.
  design <- model.matrix(~ 0 + outcome, data = data)
  
  # Take the log
  biomarkerLogMatrix <- log(matrix)
  
  # Transpose the matrix to be able to fit linear model
  biomarkerLogMatrix <- t(biomarkerLogMatrix)
  
  ## Does the limmat fit - experHS is a matrix of n (subject) rows and p (biomarker) columns.
  fit <- lmFit(biomarkerLogMatrix, design)
  
  ## Derives the empirical Bayes SE's for more robust inference
  ebayes <- eBayes(fit)
  
  return(ebayes)
}

# 2) FDR adjusted table
fdrTable <- function(data, outcome, matrix){
  ebayes <- limmaFunc(data, outcome, matrix)
  
  # Take the log
  biomarkerLogMatrix <- log(matrix)
  
  # Transpose the matrix to be able to fit linear model
  biomarkerLogMatrix <- t(biomarkerLogMatrix)
  
  # Number of biomarkers (p)
  nprobes = dim(biomarkerLogMatrix)[1]
  
  tt1 = topTable(ebayes, adjust.method = "fdr",
                 number = nprobes, genelist = rownames(biomarkerLogMatrix))
  return(tt1)
}

# 3) Volcano plot function
volcanoPlot <- function(data, outcome, matrix){
  ebayes <- limmaFunc(data, outcome, matrix)
  volcanoPlot <- EnhancedVolcano(ebayes,
                                 lab = rownames(ebayes),
                                 x = 'coefficients',
                                 y = 'p.value',
                                 xlim = c(-5, 5),
                                 pCutoff = 10e-16,
                                 pointSize = 1.5,
                                 FCcutoff = 0.5,
                                 labSize = 4,
                                 subtitle = "",
                                 title = "",
                                 caption = "")
  
  return(volcanoPlot)
}


# Limma Elicit: loop limmaFunc over every biomarker family and save the plots and FDR-adjusted
# tables.
elicit <- read.csv("/data/imic/data/raw_lab_data/elicit/merged_elicit/wideDataset.csv")

id <- c("X", "bmid_base", "country", "studyid", "subjid", "subjido", 
        "arm_base", "sex_base", "brthyr_base", "brthweek_base", "mage_base", 
        "parity_base", "nlchild_base", "nperson_base", "nrooms_base", 
        "meducyrs_base", "h2osrcp_base", "cookplac_base", "epoch_base", 
        "mhtcm_base", "mwtkg_base", "mbmi_base", "dlvloc_base", "wtkg_base", 
        "lencm_base", "bmi_base", "waz_base", "haz_base", "whz_base", 
        "baz_base", "agedays_base", "dur_bf_base", "dur_ebf_base", "haz_6m", 
        "haz_6m_simulated", "family", "site")

# Write a for loop over each family
for (i in levels(as.factor(elicit $ family))) {
  family_data <- elicit[elicit $ family == i, ]
  
  # Make matrix of log-transformed biomarkers (rows are the observations)
  biomarkerMatrix <- family_data %>%
    select(c(!id)) %>%
    as.matrix()
  
  # Save FDR adjusted tables
  table <- fdrTable(data = family_data, 
                    outcome = family_data $ haz_6m, matrix = biomarkerMatrix)
  
  # Drop all NA
  table <- na.omit(table)
  
  # Remove row names
  row.names(table) <- NULL
  
  # Save the table
  file_name <- paste("/data/imic/results/limma/fdr_elicit_", i, ".RDS", sep = "")
  saveRDS(table, file = file_name)
  
  # Make volcano plots and save them
  plot <- volcanoPlot(data = family_data, 
                      outcome = family_data $ haz_6m, matrix = biomarkerMatrix)
  # Save the plot
  file_name <- paste("/data/imic/results/figures/limma/volcano_elicit_", i, ".png", sep = "")
  ggsave(plot, filename = file_name)
}


# Limma Vital
vital <- read.csv("/data/imic/data/raw_lab_data/vital/merged_vital/wideDataset.csv")

id <- c("X", "bmid_base", "subjid", "country", "studyid", "subjido", 
        "arm_base", "sex_base", "brthyr_base", "brthweek_base", "mage_base", 
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
        "anti_oral_base", "haz_6m_simulated", "family", "site")


#temp checl
d <- vital %>% filter(family=="pbl")

cor.test(d$haz_m6, d$ADHGAEGAFVFGGPFLMDESSR_3)

res <- glm(haz_m6~ADHGAEGAFVFGGPFLMDESSR_3, data=d)
summary(res)

res <- glm(haz_m6~ADHGAEGAFVFGGPFLMDESSR_3, data=d)
summary(res)


# Write a for loop over each family
for (i in levels(as.factor(vital $ family))) {
  family_data <- vital[vital $ family == i, ]
  
  # Make matrix of log-transformed biomarkers (rows are the observations)
  biomarkerMatrix <- family_data %>%
    select(c(!id)) %>%
    as.matrix()
  
  # Save FDR adjusted tables
  table <- fdrTable(data = family_data, 
                    outcome = family_data $ haz_m6, matrix = biomarkerMatrix)
  
  # Drop all NA
  table <- na.omit(table)
  
  # Remove row names
  row.names(table) <- NULL
  
  # Save the table
  file_name <- paste("/data/imic/results/limma/fdr_vital_", i, ".RDS", sep = "")
  saveRDS(table, file = file_name)
  
  # Make volcano plots and save them
  plot <- volcanoPlot(data = family_data, 
                      outcome = family_data $ haz_m6, matrix = biomarkerMatrix)
  # Save the plot
  file_name <- paste("/data/imic/results/figures/limma/volcano_vital_", i, ".png", sep = "")
  ggsave(plot, filename = file_name)
}


# TO DO: Meta-analysis
# Load datasets
hmoE <- readRDS("/data/imic/results/limma/fdr_elicit_hmo.RDS")
hmoV <- readRDS("/data/imic/results/limma/fdr_vital_hmo.RDS")

# Check if both datasets have the same biomarkers
table(hmoE $ ID %in% c(hmoV $ ID))



# Unadjusted analyses: Elicit
data <- read.csv("/data/imic/data/raw_lab_data/elicit/merged_elicit/longDataset.csv")

names(data)

df <- data %>% 
  select(haz_6m, biomarker, value) %>%
  filter(complete.cases(.)) %>%
  droplevels()

# With a log-transformed outcome: Elicit
df $ value[df $ value == 0] <- df $ value[df $ value == 0] + 0.01 
df $ log_value <- log(df $ value)
summary(df $ log_value)

# Group data by biomarker and then apply linear regression, predicting the growth from the biomarker
res <- df %>% 
  group_nest(biomarker ) %>% 
  mutate(fit = map(data, ~ lm(haz_6m ~ log_value, data = .)),
         results = map(fit, tidy)) %>%
  unnest(results) %>%
  filter(term != "(Intercept)") #drop intercept term and just keep the coefficient on the biomarker concentration

# Sort results
res <- res[order(res $ p.value, decreasing = FALSE), ]

# Adjusted p-value
res $ BH <- p.adjust(res $ p.value, method = "BH", n = length(res $ p.value))

#dput(names(res2))

res <- res %>%
  select(c("biomarker", "estimate", "std.error", "statistic", "p.value", "BH"))

write.csv(res, file = "/data/imic/results/unadjustedElicit.csv")

# Vital
data <- read.csv("/data/imic/data/raw_lab_data/vital/merged_vital/longDataset.csv")

df <- data %>% 
  select(haz_m6, biomarker, value) %>%
  filter(complete.cases(.)) %>%
  droplevels()

# With a log-transformed outcome: Elicit
df $ value[df $ value == 0] <- df $ value[df $ value == 0] + 0.01 
df $ log_value <- log(df $ value)
summary(df $ log_value)


head(df)
#temp: subset to pbl

#Group data by biomarker and then apply linear regression, predicting the growth from the biomarker
res <- df %>% 
  group_nest(biomarker ) %>% 
  mutate(fit = map(data, ~ lm(haz_m6 ~ log_value, data = .)),
         results = map(fit, tidy)) %>%
  unnest(results) %>%
  filter(term != "(Intercept)") #drop intercept term and just keep the coefficient on the biomarker concentration


# Sort results
#res <- res[order(res $ p.value, decreasing = FALSE), ]

# Adjusted p-value
res $ BH <- p.adjust(res $ p.value, method = "BH", n = length(res $ p.value))


ggplot(aes(x=estimate, y=BH), data=res) + geom_point(alpha=0.5)

#dput(names(res2))

res <- res %>%
  select(c("biomarker", "estimate", "std.error", "statistic", "p.value", "BH"))








