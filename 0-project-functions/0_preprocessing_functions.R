
# Mode function
Mode <- function(x, na.rm = TRUE){
  if(na.rm){
    x = x[!is.na(x)]
  }
  
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}


type = "standard"
add_indicators = TRUE
prefix = "miss_"
skip_vars = NULL
all_vars = FALSE
remove_constant = TRUE 
remove_collinear = TRUE
values = NULL
h2o_glrm = NULL 
glrm_k = 10L
verbose = FALSE
impute_missing_values <- function (data, type = "standard", add_indicators = TRUE, prefix = "miss_", 
                                   skip_vars = NULL, all_vars = FALSE, remove_constant = TRUE, 
                                   remove_collinear = TRUE, values = NULL, h2o_glrm = NULL, 
                                   glrm_k = 10L, verbose = FALSE){
  missing_indicators = NULL
  new_data = data
  non_skipped_vars = !colnames(data) %in% skip_vars
  results = list(type = type, add_indicators = add_indicators, 
                 skip_vars = skip_vars, prefix = prefix)
  any_nas = which(sapply(colnames(data), function(col) !col %in% 
                           skip_vars && anyNA(data[[col]])))
  if (verbose) {
    cat("Found", length(any_nas), "variables with NAs.\n")
  }
  if (type == "standard") {
    if (verbose) {
      cat("Running standard imputation.\n")
    }
    impute_values = vector("list", sum(non_skipped_vars))
    names(impute_values) = colnames(data)[non_skipped_vars]
    if (all_vars) {
      loop_over = which(non_skipped_vars)
      names(loop_over) = colnames(data)[non_skipped_vars]
    }
    else {
      loop_over = any_nas
    }
    sum_nas = sapply(loop_over, function(col_i) sum(is.na(data[[col_i]])))
    col_classes = sapply(loop_over, function(col_i) class(data[[col_i]]))
    for (i in loop_over) {
      colname = names(loop_over)[loop_over == i]
      nas = sum_nas[colname]
      col_class = col_classes[colname]
      if (verbose) {
        cat("Imputing", colname, paste0("(", i, " ", 
                                        col_class, ")"), "with", prettyNum(nas, big.mark = ","), 
            "NAs.")
      }
      if (colname %in% names(values)) {
        impute_value = values[[colname]]
        if (verbose) {
          cat(" Pre-filled.")
        }
      }
      else if (col_class %in% c("factor")) {
        impute_value = Mode(data[[i]], exclude_na = TRUE)[1]
      }
      else if (col_class %in% c("integer", "numeric", "logical", 
                                "labelled", "integer64")) {
        impute_value = median(data[[i]], na.rm = TRUE)
      }
      else {
        warning(paste(colname, "should be numeric or factor type. But its class is", 
                      col_class))
      }
      if (verbose) {
        cat(" Impute value:", impute_value, "\n")
      }
      impute_values[[colname]] = impute_value
      if (nas == nrow(data)) {
        if (verbose) {
          cat("Note: cannot impute", colname, "because all values are NA.\n")
        }
        next
      }
      else if (nas == 0) {
        next
      }
      else {
        new_data[is.na(data[[i]]), i] = impute_value
      }
    }
    if (!all_vars) {
      impute_values = impute_values[names(any_nas)]
    }
    results$impute_values = impute_values
  }
  else if (type == "knn") {
    if (verbose) {
      cat("Running knn imputation. NOTE: this will standardize your data!\n")
    }
    if (!"RANN" %in% installed.packages()) {
      stop("knn imputation requires the RANN package. Please run install.packages(\"RANN\")")
    }
    impute_info = caret::preProcess(new_data, method = c("knnImpute"))
    new_data = predict(impute_info, new_data)
    results$impute_info = impute_info
  }
  else if (type == "glrm") {
    if (verbose) {
      cat("Running glrm imputation via h2o.\n")
    }
    capture.output({
      h2o::h2o.init(nthreads = -1)
    }, split = verbose)
    capture.output({
      df_h2o = h2o::as.h2o(new_data[, !names(new_data) %in% 
                                      skip_vars])
    }, split = verbose)
    if (is.null(h2o_glrm)) {
      capture.output({
        model_glrm = h2o::h2o.glrm(training_frame = df_h2o, 
                                   k = min(ncol(df_h2o), glrm_k), loss = "Quadratic", 
                                   init = "SVD", svd_method = "GramSVD", regularization_x = "None", 
                                   regularization_y = "None", min_step_size = 1e-06, 
                                   max_iterations = 1000)
      }, split = verbose)
    }
    else {
      model_glrm = h2o_glrm
    }
    capture.output({
      imp_h2o = predict(model_glrm, df_h2o)
    }, split = verbose)
    results$h2o_glrm = model_glrm
    capture.output({
      glrm_data = as.data.frame(imp_h2o)
    }, split = verbose)
    names(glrm_data) = setdiff(names(data), skip_vars)
    for (colname_i in names(data)[any_nas]) {
      missing_val = is.na(new_data[, colname_i])
      new_data[missing_val, colname_i] = glrm_data[missing_val, 
                                                   colname_i]
    }
  }
  if (add_indicators) {
    if (length(any_nas) > 0L) {
      if (verbose) {
        cat("Generating missingness indicators.\n")
      }
      missing_indicators = missingness_indicators(#data[, any_nas, drop = FALSE], 
                                                  data %>% select(all_of(any_nas)), 
                                                  prefix = prefix, remove_constant = remove_constant, 
                                                  remove_collinear = remove_collinear, verbose = verbose)
      if (verbose) {
        cat(paste0("Indicators added (", ncol(missing_indicators), 
                   "):"), paste(colnames(missing_indicators), 
                                collapse = ", "), "\n")
      }
      results$indicators_added = colnames(missing_indicators)
      new_data = cbind(new_data, missing_indicators)
    }
  }
  results$data = new_data
  results
}


missingness_indicators<-function (data, prefix = "miss_", remove_constant = TRUE, 
                                  remove_collinear = TRUE, skip_vars = c(), verbose = FALSE) 
{
  any_nas = which(sapply(data[, !colnames(data) %in% skip_vars, 
                              drop = FALSE], function(col) anyNA(col)))
  if (verbose) {
    cat("Generating", length(any_nas), "missingness indicators.\n")
  }
  indicators = 1L * is.na(data[, names(any_nas), drop = FALSE])
  if (length(any_nas) > 0) {
    colnames(indicators) = paste0(prefix, names(any_nas))
  }
  if (remove_constant) {
    col_means = colMeans(indicators)
    if (verbose) {
      num_removed = sum(col_means %in% c(0, 1))
      if (num_removed > 0) {
        cat("Removing", num_removed, "indicators that are constant.\n")
      }
    }
    indicators = indicators[, !col_means %in% c(0, 1), drop = FALSE]
  }
  if (remove_collinear) {
    if (verbose) {
      cat("Checking for collinearity of indicators.\n")
    }
    linear_combos = caret::findLinearCombos(indicators)
    remove_columns = linear_combos$remove
    if (length(linear_combos$remove) > 0L) {
      if (verbose) {
        cat("Removing", length(linear_combos$remove), 
            "indicators due to collinearity:\n")
        cat(paste0(colnames(indicators)[linear_combos$remove], 
                   collapse = ", "), "\n")
      }
      indicators = indicators[, -linear_combos$remove, 
                              drop = FALSE]
    }
  }
  if (verbose) {
    cat("Final number of indicators:", ncol(indicators), 
        "\n")
  }
  return(indicators)
}
