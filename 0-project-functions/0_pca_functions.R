# Make a function to clean the data.
cleanFunc <- function(data) {
  data %>%
    select(-bmid_base)
  
  # Change <LOD to NA.
  for (i in 1:ncol(data)) {
    data[, i] = ifelse(data[, i] == "< LOD", NA, data[, i])
  }
  
  # Make all variables numeric.
  #str(data)
  data <- as.data.frame(sapply(data, as.numeric))
  
  # Prepare for test beforehand
  #min(data $C5.OH..C3.DC.M., na.rm = T) / 2 #0.040535 #Correct
  
  # Change these values to min/2.
  data <- data %>% 
    mutate_if(is.numeric, function(x) ifelse(is.na(x), min(x, na.rm = T) / 2, x))
  
  # Change Inf values to 0.
  for (i in 1:ncol(data)) {
    data[, i] = ifelse(data[, i] == "Inf", 0, data[, i])
  }
  
  # Check for missingness.
  table(is.na(data))
  
  # Remove columns with no variance.
  data <- data[ - as.numeric(which(apply(data, 2, var) == 0))]
  #str(data)
  
  return(data)
}

# Make a function to plot the scree plot.
screePlot <- function(data) {
  pca = prcomp(data, center = TRUE, scale = TRUE)
  
  # Variability of each principal component: pr.var
  pr.var <- pca $ sdev ^ 2
  
  # Variance explained by each principal component: pve
  pve <- pr.var / sum(pr.var)
  
  # # Plot variance explained for each principal component
  # plot(pve, xlab = "Principal Component",
  #      ylab = "Proportion of Variance Explained",
  #      ylim = c(0, 1), type = "b")
  
  # # Plot cumulative proportion of variance explained
  # plot(cumsum(pve), xlab = "Principal Component",
  #      ylab = "Cumulative Proportion of Variance Explained",
  #      ylim = c(0, 1), type = "b")
  
  # Let's zoom in a bit - nope, a lot.
  plot <- plot(pve, xlab = "Principal Component",
       ylab = "Proportion of Variance Explained",
       ylim = c(0, 1), xlim = c(0, 25), type = "b") # 6-10 components = optimal.
  
  # # Let's zoom in by a lot in here as well
  # plot(cumsum(pve), xlab = "Principal Component",
  #      ylab = "Cumulative Proportion of Variance Explained",
  #      ylim = c(0, 1), xlim = c(0, 25), type = "b")
  return(plot)
}

# Make a function to plot the scree plot.
screePlot2 <- function(data) {
  pca = prcomp(data, center = TRUE, scale = TRUE)
  
  # Variability of each principal component: pr.var
  pr.var <- pca $ sdev ^ 2
  
  # Variance explained by each principal component: pve
  pve <- pr.var / sum(pr.var)
  
  # Plot cumulative proportion of variance explained
  plot <- plot(cumsum(pve), xlab = "Principal Component",
       ylab = "Cumulative Proportion of Variance Explained",
       ylim = c(0, 1), xlim = c(0, 50), type = "b")
  return(plot)
}

# Make a function to extract data needed for merging.
prepData <- function(data, numC) {
  # Create a recipe
  char_recipe <- recipe(~ ., data = data) %>%
    # Tell PCA that these are the identification variables and to not use them.
    #update_role(bmid_base, new_role = "id") %>%
    # Scale so the range of variables don't affect our analysis.
    step_normalize(all_predictors()) %>%
    step_pca(all_numeric(), num_comp = numC) # Useful for juice().
  
  # Prep the recipe and take a look at it. 
  pca_estimates_prep <- prep(char_recipe)
  return(pca_estimates_prep)
}

# Make a function to save PCA estimates by importance and value.
pcaEstimates <- function(pca_estimates_prep) {
  # Extract pca results: loadings
  pca_estimates <- tidy(pca_estimates_prep, 2)
  return(pca_estimates)
}

# Make a function to save PCA estimates formatted for plotting.
pcaEstimatesPlot <- function(pcaEstimates, PC, n) {
  # Get data in shape to plot
  pca_estimates2 <- pcaEstimates %>%
    filter(component %in% paste0("PC", PC)) %>%
    mutate(importance = value * value,
           component = fct_inorder(component),
           terms = reorder_within(terms, # order within component.
                                  by = importance,
                                  within = component)) %>%
    # Get the highest 10 contributors by importance for each component
    group_by(component) %>%
    slice_max(importance, n = n)
  return(pca_estimates2)
}

# Plot top 10 contributors to each component based on importance.
top10cI <- function(estimates, data) {
  num = 1/ncol(data)
  plot <- estimates %>%
    ggplot() +
    geom_col(aes(y = importance,
                 x = terms, 
                 fill = importance < num)) +
    geom_hline(yintercept = num) +
    # facet_wrap(~ component,
    #            nrow = 2,
    #            scales = "free") +
    facet_grid(~ component, scales = "free") +#, space = 'free') +
    guides(fill = "none") +
    scale_x_reordered() +
    xlab("") + ylab("") +
    coord_flip() +
    #ggtitle("10 top contributors to each component by importance") +
    theme(strip.background = element_blank(),
          axis.text.y = element_text(size = 5),
          axis.text.x = element_blank(),
          axis.ticks = element_blank(),
        strip.text = element_text(size = rel(0.5), margin = margin()),
        panel.spacing = unit(2, "pt"))
  return(plot)
}

# Plot top 10 contributors based on value.
top10cV <- function(estimates, data) {
  num = 1/ncol(data)
  estimates %>%
  ggplot() +
  geom_col(aes(y = value, 
               x = terms, 
               fill = importance < num)) +
  geom_hline(yintercept = num) +
  # facet_wrap(~ component, 
  #            nrow = 2,
  #            scales = "free") + 
  facet_grid(~ component, scales = "free") +#, space = 'free') +
  guides(fill = "none") +
  scale_x_reordered() +
  xlab("") + ylab("") +
  coord_flip() +
  #ggtitle("10 top contributors to each component by value") +
    theme(strip.background = element_blank(),
          axis.text.y = element_text(size = 5),
          axis.text.x = element_blank(),
          axis.ticks = element_blank(),
          strip.text = element_text(size = rel(0.5), margin = margin()),
          panel.spacing = unit(2, "pt"))
}