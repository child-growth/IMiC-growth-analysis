# 
# install.packages("partykit")
# install.packages("pre")
# #CVtreeMLE uses the sl3 package to build ensemble machine learners for each nuisance parameter. We have to install off the development branch, first download these two packages for sl3
# 
# install.packages(c("ranger", "arm", "xgboost", "nnls"))
# #Now install sl3 on devel:
#   
# remotes::install_github("tlverse/sl3@devel")
# #Make sure sl3 installs correctly then install CVtreeMLE
# 
#remotes::install_github("blind-contours/CVtreeMLE@main")
# #CVtreeMLE has some other miscellaneous dependencies that are used in the examples as well as in the plotting functions.
# 
# install.packages(c("kableExtra", "hrbrthemes", "viridis"))

library(parallel)
library(doParallel)
registerDoParallel(cores=100)


library(CVtreeMLE)
library(sl3)
library(pre)
library(partykit)
library(kableExtra)
library(ggplot2)

#set.seed(429153)

sim_data <- simulate_mixture_cube(
  n_obs = 800,
  splits = c(0.99, 2.0, 2.5),
  mins = c(0, 0, 0),
  maxs = c(3, 4, 5),
  subspace_assoc_strength_betas = c(
    0, 0, 0, 0,
    0, 0, 6, 0
  )
)
head(sim_data) %>%
  kbl(caption = "Simulated Data") %>%
  kable_classic(full_width = FALSE, html_font = "Cambria")


ptm <- proc.time()

sim_results <- CVtreeMLE(
  data = sim_data,
  w = c("age", "sex", "bmi"),
  a = c(paste("M", seq(3), sep = "")),
  y = "y",
  n_folds = 5,
  parallel_cv = FALSE,
  family="continuous",
  seed = 2333,
  parallel_type = "multi_session",
  num_cores = 100
)

proc.time() - ptm