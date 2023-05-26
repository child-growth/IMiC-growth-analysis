

rm(list=ls())
source(paste0(here::here(), "/0-config.R"))

#data/results and named unadjustedElicit and unadjustedVital

res <- read.csv("/data/imic/results/unadjustedElicit.csv")
res2 <- read.csv("/data/imic/results/unadjustedVital.csv")

res$BH.pvalue <- p.adjust(res $ p.value, method = "BH")
res$BH.pvalue_log <- p.adjust(res $ p.value_log, method = "BH")

res2$BH.pvalue <- p.adjust(res2 $ p.value, method = "BH")
res2$BH.pvalue_log <- p.adjust(res2 $ p.value_log, method = "BH")

(71/nrow(res))*0.05

prop.table(table(res$p.value < 0.05)) * 100
prop.table(table(res$p.value_log < 0.05)) * 100

prop.table(table(res2$p.value < 0.05)) * 100
prop.table(table(res2$p.value_log < 0.05)) * 100



length(unique(res$biomarker))

min(res$p.value)


p.adjust.M <- p.adjust.methods[p.adjust.methods != "fdr"]
p.adj    <- sapply(p.adjust.M, function(meth) p.adjust(res$p.value, meth))