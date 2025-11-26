# setup ----------------------------------------------------------------------------------
#require(data.table)
#require(magrittr)
# require(cbcTools)
#require(bayesm)
#require(mlogit)
#require(ggplot2)

# DO NOT TOUCH ANYTHING!!! :-)

# fns ------------------------------------------------------------------------------------

D_eff <- function(X) {
  n <- nrow(X)
  k <- ncol(X)

  deff <- (100 / n) * det(crossprod(X))^(1 / k)

  return(deff)
}

build_profiles <- function(attr_lvls) {
  # Check if attr_lvls is provided but not a list
  if (!is.null(attr_lvls) && !is.list(attr_lvls)) {
    stop("Error: attr_lvls must be a named list of attribute levels.")
  }

  # Check if attr_lvls doesn't have names when provided
  if (!is.null(attr_lvls) && is.null(names(attr_lvls))) {
    stop("Error: attr_lvls must be a named list with attribute names.")
  }

  # Force attribute names to be lowercase
  original_names <- names(attr_lvls)
  lower_names <- tolower(original_names)
  if (!identical(original_names, lower_names)) {
    warning("Warning: Attribute names in attr_lvls were converted to lowercase.")
    names(attr_lvls) <- lower_names
  }

  # # Force attributes to be lowercase
  # original_att <- unlist(attr_lvls)
  # lower_att <- tolower(original_att)
  # if (!identical(original_att, lower_att)) {
  #   warning("Warning: Attributes in attr_lvls were converted to lowercase.")
  #   attr_lvls <- lapply(attr_lvls, tolower)
  # }

  # Check if price variable exists and can be converted to numeric
  if (any(names(attr_lvls) == "price")) {
    price_converted <- suppressWarnings(as.numeric(as.character(attr_lvls$price)))
    if (any(is.na(price_converted))) {
      stop("Error: Some price values could not be converted to numeric.")
    }
  } else {
    stop("Error: 'price' variable is missing.")
  }

  profiles <- expand.grid(attr_lvls) |>
    setDT()
  profiles <- data.table(profile = seq.int(nrow(profiles)), profiles)
  return(profiles)
}


# NOTE: updtae function!!!
# - remove dependency on cbcTools
# - provide only random method as option
# - afterwards, update the tutorial accordingly incl results
# create_cbc_design <- function(attr_lvls,
#                               n_alt = 3,
#                               n_cs = 16,
#                               seed = 666,
#                               report = FALSE,
#                               method = "random",
#                               profiles = NULL) {
#   if (is.null(profiles)) {
#     # Check if attr_lvls is provided but not a list
#     if (!is.null(attr_lvls) && !is.list(attr_lvls)) {
#       stop("Error: attr_lvls must be a named list of attribute levels.")
#     }
#
#     # Check if attr_lvls doesn't have names when provided
#     if (!is.null(attr_lvls) && is.null(names(attr_lvls))) {
#       stop("Error: attr_lvls must be a named list with attribute names.")
#     }
#
#     # Force attribute names to be lowercase
#     original_names <- names(attr_lvls)
#     lower_names <- tolower(original_names)
#     if (!identical(original_names, lower_names)) {
#       warning("Warning: Attribute names in attr_lvls were converted to lowercase.")
#       names(attr_lvls) <- lower_names
#     }
#
#     profiles <- build_profiles(attr_lvls)
#     class(profiles) <- c("cbc_profiles", class(profiles))
#     features <- names(attr_lvls)
#   } else {
#     features <- setdiff(names(profiles), "profileID")
#   }
#   if (!is.null(seed)) set.seed(seed)
#   design <- suppressMessages(cbc_design(
#     profiles = profiles,
#     n_resp = 1,
#     n_alts = n_alt,
#     n_q = n_cs,
#     method = method
#   ))
#   if (report) {
#     print(cbc_balance(design))
#     print(cbc_overlap(design))
#   }
#
#   design <- data.table(design)
#   design[, text := do.call(paste, c(Map(function(x) paste0(x, ": ", get(x)), features), sep = ", ")), by = qID]
#   design <- design[, c("qID", "altID", features, "text"), with = FALSE]
#   setnames(design, c("qID", "altID"), c("cs", "alt"))
#
#   if (any(names(design) == "price")) {
#     # Check if price variable exists and can be converted to numeric
#     price_converted <- suppressWarnings(as.numeric(as.character(design$price)))
#     if (any(is.na(price_converted))) {
#       stop("Error: Some price values could not be converted to numeric.")
#     }
#     design[, price := as.numeric(as.character(price))]
#   } else {
#     stop("Error: 'price' variable is missing.")
#   }
#
#   return(design)
# }

create_cbc_design <- function(attr_lvls,
                              n_alt = 3,
                              n_cs = 16,
                              seed = 666,
                              profiles = NULL) {
  if (is.null(profiles)) {
    profiles <- build_profiles(attr_lvls)
    features <- names(attr_lvls)
  } else {
    features <- setdiff(names(profiles), "profile")
  }
  if (!is.null(seed)) {
    set.seed(seed)
  }
  sampled_profiles <- sample(profiles$profile, n_alt * n_cs, replace = TRUE)
  design <- merge(
    x = data.table(
      profile = sampled_profiles,
      cs = rep(seq.int(n_cs), each = n_alt),
      alt = rep(seq.int(n_alt), n_cs)
    ),
    y = profiles, by = "profile", all.x = TRUE
  )
  setkey(design, cs, alt)
  design[, id := .I]
  design[, text := do.call(paste, c(Map(function(x) paste0(x, ": ", get(x)), features), sep = ", ")), by = id]
  design[, c("profile", "id") := NULL]

  if (any(names(design) == "price")) {
    # Check if price variable exists and can be converted to numeric
    price_converted <- suppressWarnings(as.numeric(as.character(design$price)))
    if (any(is.na(price_converted))) {
      stop("Error: Some price values could not be converted to numeric.")
    }
    design[, price := as.numeric(as.character(price))]
  } else {
    stop("Error: 'price' variable is missing.")
  }

  return(design)
}



# reshape data from google forms, we assume a very specific structure => see tutorial
prep_google_forms_data <- function(data, design,
                                   choice_text = "Choice",
                                   purchase_text = "Purchase") {
  # Check inputs
  if (missing(data) || missing(design)) {
    stop("Error: Both data and design arguments must be provided.")
  }

  if (!is.data.frame(data) || !is.data.frame(design)) {
    stop("Error: Both data and design must be data frames or data tables.")
  }

  # Force the data input to be a data.table object if not already
  if (!is.data.table(data)) {
    warning("Warning: The data input was converted to a data.table object.")
    data <- as.data.table(data)
  }

  # Check if the data has the expected columns
  choice_cols <- names(data)[names(data) %like% choice_text]
  purchase_cols <- names(data)[names(data) %like% purchase_text]

  if (length(choice_cols) == 0) {
    stop(paste0("Error: No columns matching '", choice_text, "' found in the data."))
  }

  if (length(purchase_cols) == 0) {
    stop(paste0("Error: No columns matching '", purchase_text, "' found in the data."))
  }

  # Check if choice and purchase columns have the same number
  if (length(choice_cols) != length(purchase_cols)) {
    stop("Error: Number of choice columns does not match number of purchase columns.")
  }

  # Check if design has the expected columns
  required_design_cols <- c("cs", "alt", "text")
  if (!all(required_design_cols %in% names(design))) {
    missing_cols <- required_design_cols[!required_design_cols %in% names(design)]
    stop(paste0(
      "Error: The following required columns are missing from the design: ",
      paste(missing_cols, collapse = ", ")
    ))
  }

  # Check if price column exists in design
  if (!"price" %in% names(design)) {
    stop("Error: The design must include a 'price' column.")
  }

  # Original function code continues here
  # resp id
  data[, id := .I]

  # design id for merging
  design[, design_id := .I]

  # dv: choice
  choice <- tryCatch(
    {
      data[, c("id", names(data)[names(data) %like% choice_text]), with = FALSE] %>%
        melt.data.table(id.vars = "id", value.name = "text", variable.name = "cs")
    },
    error = function(e) {
      stop(paste0("Error in melting choice data: ", e$message))
    }
  )

  # Extract cs number from column names
  tryCatch(
    {
      choice[, cs := as.integer(gsub(choice_text, "", cs))]
    },
    error = function(e) {
      stop("Error: Failed to extract choice set numbers from column names. Make sure column names follow the expected pattern (e.g., 'Choice1', 'Choice2', etc.).")
    }
  )

  choice[, choice := 1]

  design_long <- tryCatch(
    {
      merge(CJ(design_id = design$design_id, id = seq.int(data$id)),
        design,
        by = c("design_id")
      )
    },
    error = function(e) {
      stop(paste0("Error in creating design_long: ", e$message))
    }
  )

  design_long[, design_id := NULL]

  choice_data <- tryCatch(
    {
      merge(design_long, choice, by = c("id", "cs", "text"), all = TRUE)
    },
    error = function(e) {
      stop(paste0(
        "Error in merging design with choices: ", e$message,
        "\nThis could be due to a mismatch between the text in the design and the responses."
      ))
    }
  )

  # Check if there are any matches in the merge
  if (all(is.na(choice_data$choice))) {
    stop("Error: No matches found when merging design with choices. Check that the text in the design exactly matches the choices in the survey responses.")
  }

  choice_data[is.na(choice), choice := 0]
  setkey(choice_data, id, cs, alt)

  # dv: purchase (outside option)
  purchase <- tryCatch(
    {
      data[, c("id", names(data)[names(data) %like% purchase_text]), with = FALSE] %>%
        melt.data.table(id.vars = "id", value.name = "purchase", variable.name = "cs")
    },
    error = function(e) {
      stop(paste0("Error in melting purchase data: ", e$message))
    }
  )

  tryCatch(
    {
      purchase[, cs := as.integer(gsub(purchase_text, "", cs))]
    },
    error = function(e) {
      stop("Error: Failed to extract choice set numbers from purchase column names. Make sure column names follow the expected pattern (e.g., 'Purchase1', 'Purchase2', etc.).")
    }
  )

  # Check if purchase values are as expected
  unique_purchase_values <- unique(purchase$purchase)
  if (!all(unique_purchase_values %in% c("Yes", "No"))) {
    unexpected_values <- unique_purchase_values[!unique_purchase_values %in% c("Yes", "No")]
    warning(paste0(
      "Warning: Unexpected purchase values found: ",
      paste(unexpected_values, collapse = ", "),
      ". Expected values are 'Yes' and 'No'."
    ))
  }

  purchase[, purchase := ifelse(purchase == "Yes", 1, 0)]

  # expand with outside option
  choice_data_0 <- copy(choice_data)
  choice_data_0[, choice := 0]
  choice_data_0[, text := "outside option"]
  choice_data_0[, alt := max(alt) + 1]
  choice_data_0[, price := 0] # assumption: price is always included
  clnms_0 <- setdiff(
    names(choice_data_0),
    c("id", "cs", "text", "alt", "choice", "price")
  )
  choice_data_0[, (clnms_0) := NA]

  choice_data <- rbind(choice_data, unique(choice_data_0))
  setkey(choice_data, id, cs, alt)

  # combine with choices
  choice_data <- tryCatch(
    {
      merge(choice_data, purchase, by = c("id", "cs"))
    },
    error = function(e) {
      stop(paste0("Error in merging with purchase data: ", e$message))
    }
  )

  # overwrite choices with purchase
  choice_data[text == "outside option" & purchase == 0, choice := 1]
  choice_data[text != "outside option", choice := purchase * choice]
  choice_data[, purchase := NULL]

  # Check if each choice set has exactly one choice
  choice_sums <- choice_data[, .(choice_sum = sum(choice)), by = .(id, cs)]
  if (any(choice_sums$choice_sum != 1)) {
    problem_cases <- choice_sums[choice_sum != 1]
    warning(paste0(
      "Warning: Some choice sets don't have exactly one choice. Check cases: ",
      paste(paste0("id=", problem_cases$id, ", cs=", problem_cases$cs), collapse = "; ")
    ))
  }

  # sort again
  setkey(choice_data, id, cs, alt)

  # return final dt
  return(choice_data)
}

# sim choice form MNL model with heterogeneity
sim_y <- function(x, beta) {
  vars <- setdiff(names(x), c("id", "cs", "alt"))

  data <- copy(x)

  for (i in data[, unique(id)]) {
    x_i <- as.matrix(data[id == i, vars, with = FALSE])
    data[id == i, u := c(x_i %*% beta[i, ])]
  }

  data[, prob := exp(u) / sum(exp(u)), by = .(id, cs)]
  data[, choice := rmultinom(1, 1, prob), by = .(id, cs)]

  data[, u := NULL]
  data[, prob := NULL]
  setkey(data, id, cs, alt)
  return(copy(data))
}

# creta fake data for MNL model with heterogeneity
prep_fake_data <- function(design, n_I = 100, seed = 666, beta = NULL) {
  choice_data <- copy(design)

  # expand with outside option
  choice_data_0 <- copy(design)
  choice_data_0[, text := "outside option"]
  choice_data_0[, alt := max(alt) + 1]
  choice_data_0[, price := 0] # assumption: price is always included
  clnms_0 <- setdiff(
    names(choice_data_0),
    c("id", "cs", "text", "alt", "choice", "price")
  )
  choice_data_0[, (clnms_0) := NA]

  choice_data <- rbind(choice_data, unique(choice_data_0))
  choice_data[, c_id := .I]

  if (!is.null(beta)) {
    n_I <- nrow(beta)
  }

  choice_data <- merge(CJ(c_id = choice_data$c_id, id = 1:n_I),
    choice_data,
    by = c("c_id")
  )
  setkey(choice_data, id, cs, alt)
  choice_data[, c_id := NULL]

  # fake choices
  if (!is.null(seed)) set.seed(seed)
  choice_data[, choice := rmultinom(1, 1, rep(1 / .N, .N)), by = .(id, cs)]

  if (!is.null(beta)) {
    choice_data_with_coding <- copy(build_coding(choice_data))
    choice_data_with_coding[, choice := NULL]
    choice_data[, choice := sim_y(choice_data_with_coding, beta)$choice]
    choice_data[, og := NULL]
  }

  return(copy(choice_data))
}

# create dummy coding for all categorical features in data
build_coding <- function(data) {
  data_no_og <- data[text != "outside option"]

  features <- setdiff(
    names(data_no_og),
    c("id", "cs", "text", "alt", "choice")
  )

  # force all categorical attributes to be a character-class before running model.matrix
  no_price_features <- setdiff(features, "price")
  data_no_og[, (no_price_features) := lapply(.SD, as.character), .SDcols = no_price_features]

  # build model matrix with dummy coding
  f <- as.formula(paste0("~ ", paste0(features, collapse = " + ")))
  mm <- model.matrix(f, data_no_og)[, -1] # no intercept
  colnames(mm) <- gsub(" ", "", colnames(mm), fixed = TRUE)

  data_no_og <- cbind(data_no_og[, .(id, cs, alt)], mm)

  # og dummy
  data_og <- copy(data)
  data_og[, og := ifelse(text == "outside option", 1, 0)]

  data_all <- merge(data_og[, .(id, cs, alt, choice, og)],
    data_no_og,
    by = c("id", "cs", "alt"), all = TRUE
  )
  setnafill(data_all, fill = 0)

  return(data_all)
}

# reshape data for bayesm package (list format)
build_bayesm_data <- function(data, formula,
                              id.var = "id", alt.var = "alt", choice.var = "choice") {
  dt <- data.table(data)
  x_mm <- data.table(
    dt[, id.var, with = FALSE],
    model.matrix(formula, data = dt)
  )

  X_l <- x_mm[,
    list(list(list(X = unname(as.matrix(.SD))))),
    by = id.var
  ]$V1
  y_l <- dt[dt[[choice.var]] == 1L,
    list(list(list("y" = get(alt.var)))),
    by = id.var
  ]$V1

  list(
    lgtdata = mapply(c, y_l, X_l, SIMPLIFY = FALSE),
    p = uniqueN(dt[[alt.var]]),
    coef_names = colnames(x_mm[, -1]),
    ids = unique(x_mm[, get(id.var)])
  )
}

# reshape the posterior draws created by the bayesm package
reshape_bayesm_draws <- function(out, post_index = NULL, coef_names = NULL, ids = NULL) {
  C <- dim(out$nmix$probdraw)[2] # number of components
  I <- dim(out$betadraw)[1] # number of individuals
  K <- dim(out$betadraw)[2] # number of parameters
  R <- dim(out$betadraw)[3] # number of draws

  if (is.null(post_index)) post_index <- 1:R
  if (is.null(coef_names)) coef_names <- paste0("par_", 1:K)

  # pop draws
  sdMat <- bMat <- array(NA, dim = c(R, K))
  corArray <- sigmaArray <- array(NA, dim = c(K, K, R))
  muArray <- array(NA, dim = c(R, K, C))
  sigArray <- array(NA, dim = c(K, K, R, C))
  for (r in 1:R) {
    moms <- momMix(out$nmix$probdraw[r, , drop = FALSE], out$nmix$compdraw[r])
    sigmaArray[, , r] <- moms$sigma
    corArray[, , r] <- moms$corr
    sdMat[r, ] <- moms$sd
    bMat[r, ] <- moms$mu
    for (c in 1:C) {
      rTemp <- out$nmix$compdraw[[r]][[c]]$rooti
      sigArray[, , r, c] <- crossprod(chol(chol2inv(chol(tcrossprod(rTemp)))))
      # sigArray[,, r, c] <- solve(tcrossprod(rTemp))
      muArray[r, , c] <- out$nmix$compdraw[[r]][[c]]$mu
    }
  }
  colnames(sigArray) <- coef_names
  rownames(sigArray) <- coef_names
  colnames(sigmaArray) <- coef_names
  rownames(sigmaArray) <- coef_names
  colnames(corArray) <- abbreviate(coef_names, 8)
  rownames(corArray) <- coef_names
  colnames(sdMat) <- colnames(bMat) <- colnames(muArray) <- coef_names
  # ind draws
  betaMat <- out$betadraw
  colnames(betaMat) <- coef_names

  return(list(
    b = bMat[post_index, ], sd = sdMat[post_index, ],
    Sigma = sigmaArray[, , post_index],
    Cor = corArray[, , post_index],
    beta = betaMat[, , post_index],
    loglike = out$loglike[post_index],
    muArray = muArray[post_index, , ],
    sigArray = sigArray[, , post_index, ],
    probdraw = out$nmix$probdraw[post_index, ],
    coef_names = coef_names, ids = ids
  ))
}


# function to estimate an MNL model
mnl <- function(formula, data) {
  # Check if required arguments are provided
  if (missing(formula) || missing(data)) {
    stop("Both formula and data arguments must be provided. All variables, except for the indices, created by the build_coding() function should be included in the formula. The data argument should be the output of the build_coding() function.")
  }

  # Check if data is a data.table object
  if (!is.data.table(data)) {
    warning("Warning: The data input was converted to a data.table object.")
    data <- as.data.table(data)
  }

  # Extract all RHS variables from formula
  formula_vars <- all.vars(formula[[3]])

  # Get all variables in data except indices
  data_vars <- setdiff(names(data), c("id", "cs", "alt", "choice"))

  # Check if all data variables (except indices) are included in formula
  missing_vars <- data_vars[!data_vars %in% formula_vars]
  if (length(missing_vars) > 0) {
    stop(paste(
      "The following variables from data are not included in the formula:",
      paste(missing_vars, collapse = ", "),
      "\nConsider including all variables created by build_coding() in your model."
    ))
  }

  # copy data to prevent side-efects
  data_ml <- copy(data)

  # add chid var for mlogit
  data_ml[, chid := .GRP, keyby = .(id, cs)]

  # build data in mlogit format
  data_mlogit <- mlogit.data(as.data.frame(data_ml),
    choice = "choice",
    shape = "long",
    id.var = "id",
    alt.var = "alt",
    chid.var = "chid"
  )

  # run model
  mnl_res <- mlogit(
    formula = formula,
    data = data_mlogit
  )

  # return the model list
  return(mnl_res)
}

# function to estimate an HBMNL model
hbmnl <- function(formula, data, R = 200000, keep = 100, nprint = NULL, seed = 666,
                  id.var = "id", alt.var = "alt", choice.var = "choice") {
  # Check if required arguments are provided
  if (missing(formula) || missing(data)) {
    stop("Both formula and data arguments must be provided. All variables, except for the indices, created by the build_coding() function should be included in the formula. The data argument should be the output of the build_coding() function.")
  }

  # Check if data is a data.table object
  if (!is.data.table(data)) {
    warning("Warning: The data input was converted to a data.table object.")
    data <- as.data.table(data)
  }

  # Extract all RHS variables from formula
  formula_vars <- all.vars(formula[[3]])

  # Get all variables in data except indices
  data_vars <- setdiff(names(data), c("id", "cs", "alt", "choice"))

  # Check if all data variables (except indices) are included in formula
  missing_vars <- data_vars[!data_vars %in% formula_vars]
  if (length(missing_vars) > 0) {
    stop(paste(
      "The following variables from data are not included in the formula:",
      paste(missing_vars, collapse = ", "),
      "\nConsider including all variables created by build_coding() in your model."
    ))
  }

  # Original code continues here
  if (is.null(nprint)) nprint <- R / 100
  if (!is.null(seed)) set.seed(seed)

  # build data in bayesm format
  data_list <- build_bayesm_data(data, formula, id.var, alt.var, choice.var)

  # run hierarchical Bayes model
  out_list <- rhierMnlRwMixture(
    Data = data_list,
    Prior = list(ncomp = 1), # normal
    Mcmc = list(R = R, keep = keep, nprint = nprint)
  )

  # reshape the output of bayesm
  draws_list <- reshape_bayesm_draws(out_list,
    post_index = seq(1 + 0.5 * R / keep, R / keep),
    coef_names = data_list$coef_names,
    ids = data_list$ids
  )

  # return the draws
  return(draws_list)
}

# summarize the results
summarize_hbmnl_results <- function(draws, digits = 3) {
  b_table <- cbind(
    mean = apply(draws$b, 2, mean),
    sd = apply(draws$b, 2, sd),
    t(apply(draws$b, 2, quantile, prob = c(0.025, 0.975)))
  ) %>%
    data.table(par = "b", coef = rownames(.), .)
  sd_table <- cbind(
    mean = apply(draws$sd, 2, mean),
    sd = apply(draws$sd, 2, sd),
    t(apply(draws$sd, 2, quantile, prob = c(0.025, 0.975)))
  ) %>%
    data.table(par = "sigma", coef = rownames(.), .)

  ll_table <- cbind(
    mean = mean(draws$loglike),
    sd = sd(draws$loglike),
    t(quantile(draws$loglike, prob = c(0.025, 0.975)))
  ) %>%
    data.table(par = "log Lik.", coef = "log Lik.", .)

  b_sd_table <- rbind(b_table, sd_table, ll_table)[, lapply(.SD, round, digits), by = .(par, coef)]

  return(b_sd_table)
}

# summarize the results
summarize_mnl_results <- function(model, digits = 3) {
  print(round(cbind(summary(model)$CoefTable[, 1:2], confint(model)), digits))
  print(round(logLik(model), digits))
}

# compute LMD value with trimming (see, e.g., Dubé et al. 2010)
compute_log_marg_density <- function(draws, trim = 0.01) {
  # compute LMD using the approx of Newton und Raftery (1994) and trimmed draws
  # adapted from bayesm::logMargDenNR
  ll <- draws$loglike

  # trim draws for robustness
  if (trim != 0) {
    cutoff <- quantile(ll, c(trim, 1 - trim))
    ll <- ll[ll > cutoff[1] & ll < cutoff[2]]
  }

  med <- median(ll)
  lmd <- med - log(mean(exp(-ll + med)))
  return(lmd)
}

# trace plot of population means draws (b parameter) to check convergence
trace_plot_b <- function(draws) {
  b <- data.table(R = seq.int(nrow(draws$b)), draws$b)
  b_long <- melt.data.table(b, id.vars = "R", variable.name = "coef")
  b_mean <- b_long[, .(mean = mean(value)), by = coef]
  ggplot(b_long, aes(x = R, y = value)) +
    # geom_hline(yintercept = 0, linetype = "dashed") +
    geom_line(alpha = 0.5, color = "steelblue3") +
    geom_hline(data = b_mean, aes(yintercept = mean), color = "magenta2") +
    theme_bw() +
    scale_x_continuous(minor_breaks = NULL) +
    scale_y_continuous(minor_breaks = NULL) +
    labs(title = "Trace plots of b draws") +
    facet_wrap(~coef, scales = "free_y")
}

# convenience function to get posterior mean of beta (i.e., individual-level estimates)
get_posterior_mean_beta <- function(draws, out = "matrix") {
  # posterior mean of beta
  beta <- apply(draws$beta, c(1, 2), mean)
  if (out == "matrix") {
    rownames(beta) <- draws$id
  } else if (out == "data.table") {
    beta <- data.table(id = draws$id, beta)
  }
  return(beta)
}

# convenience function to get part worths
get_pw <- function(beta_i, features, add_price = FALSE) {
  non_price_features <- setdiff(features, "price")
  alpha_i_list <- NULL
  for (k in non_price_features) { # k = "bio"
    beta_ik <- beta_i[, colnames(beta_i) %like% k, drop = FALSE]
    names_beta_ik <- colnames(beta_ik)
    beta_ik <- cbind(beta_ik, 0)
    colnames(beta_ik) <- c(names_beta_ik, paste0(k, "Reference"))
    rmk <- rowMeans(beta_ik)
    alpha_i_list[[k]] <- beta_ik - rmk
  }
  alpha_i <- do.call(cbind, alpha_i_list)
  if (add_price) {
    alpha_i <- cbind(alpha_i, beta_i[, "price", drop = FALSE])
  }
  return(alpha_i)
}

# convenience function to get wtp values
get_wtp <- function(beta_i, drop_pos_price_coef = TRUE) {
  beta_price_i <- beta_i[, "price"]
  if (drop_pos_price_coef) {
    beta_price_ok <- beta_price_i < 0
    bad_i <- paste(unname(which(!beta_price_ok)), collapse = ", ")
    warning(paste0("Dropping results of i = ", bad_i, " becasue of a positive price coef."))
    beta_i <- beta_i[beta_price_ok, ]
  }

  wtp_i <- beta_i[, colnames(beta_i) != "price"] / -beta_i[, "price"]
  return(wtp_i)
}

# convenience function to get relative importance
get_ri <- function(beta_i, features, price_range = 1) {
  non_price_features <- setdiff(features, "price")
  ri_i_list <- NULL
  for (k in non_price_features) { # k = "bio"
    beta_ik <- cbind(beta_i[, colnames(beta_i) %like% k, drop = FALSE], 0)
    ri_i_list[[k]] <- apply(beta_ik, 1, function(x) diff(range(x)))
  }
  ri_i_list[["price"]] <- c(abs(beta_i[, "price", drop = FALSE]) * price_range)
  ri_i <- do.call(cbind, ri_i_list)
  ri_i <- ri_i / apply(ri_i, 1, sum)
  return(ri_i)
}

mnl_prob <- function(beta, x) {
  u <- x %*% beta
  eu <- exp(u)
  p <- eu / sum(eu)
  return(p)
}

predict_mnl <- function(beta_i, x_new) {
  I <- nrow(beta_i)
  prob_i <- matrix(0, nrow = I, ncol = nrow(x_new))
  x <- as.matrix(x_new)
  for (i in 1:I) {
    prob_i[i, ] <- mnl_prob(beta_i[i, ], x)
  }
  return(prob_i)
}
