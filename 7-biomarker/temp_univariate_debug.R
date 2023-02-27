
library(tidyverse)
library(broom)

data <- read.csv("/data/imic/data/raw_lab_data/elicit/merged_elicit/longDataset.csv")

table(is.na(data$value))

df <- data %>% select(haz_6m, biomarker, value) %>% filter(complete.cases(.)) %>% droplevels()
summary(df$haz_6m)
summary(df$value)
summary(log(df$value[df$value!=0]))


df$value[df$value==0] <- df$value[df$value==0] + 0.01 
df$log_value <- log(df$value)


res <- df %>% 
  group_nest(biomarker ) %>% 
  mutate(fit = map(data, ~ lm(haz_6m ~ value, data = .)),
         results = map(fit, augment)) %>% 
  unnest(results)


res2 <- df %>% 
  group_nest(-biomarker ) %>% 
  mutate(fit = map(data, ~ lm(haz_6m ~ log_value, data = .)),
         results = map(fit, augment)) %>% 
  unnest(results)

res3 <- df %>% 
  group_nest(biomarker ) %>% 
  mutate(fit = map(data, ~ lm(haz_6m ~ value, data = .)),
         results = map(fit, tidy)) %>% 
  unnest(results) %>%
  filter(term!="(Intercept)")
res3



# wide <- read.csv("/data/imic/data/raw_lab_data/elicit/merged_elicit/wideDataset.csv")
# 
# # Define the predictor
# predictor <- "haz_6m"
# 
# # Run the linear regressions for each biomarker
# results <- data.frame()
# for (i in 2:length(wide)) {
#   # Get the biomarker name
#   biomarker <- colnames(wide)[i]
#   # Run the regression
#   regression <- lm(wide[[i]] ~ wide[[predictor]])
#   # Extract the regression estimate and p-value
#   estimate <- regression$coefficients[2]
#   pvalue <- summary(regression)$coefficients[2, 4]
#   # Calculate the Bonferroni adjusted p-value
#   bonferroni_pvalue <- p.adjust(pvalue, method = "bonferroni", n = length(wide)-1)
#   # Add the results to the table
#   results <- rbind(results, data.frame(biomarker = biomarker, 
#                                        outcome = "haz_6m", 
#                                        estimate = estimate, 
#                                        pvalue = pvalue, 
#                                        bonferroni_pvalue = bonferroni_pvalue))
# }
# 
# # Print the results
# print(results)
# 
# # Sort results
# results <- results[order(results$pvalue, decreasing=FALSE), ]
