# Bootstrap Objects
# Based on Version (2025/04/12)
# Edit: 2025/12/30 Modified into my own package

# pbapply is needed for progress bar in *apply function

#' Class: Bootstrap Base
#' @importFrom R6 R6Class
#'
#' @description
#' An R6 class providing the foundational logic for temperature-based bootstrapping.
#'
#' @field data The input dataframe.
#' @field y_var Character. The name of the dependent variable.
#' @field x_vars Character vector. The names of the independent variables.
#' @field boot_results List. Stores the results of each bootstrap round.
#' @field aggregated_data Dataframe. The processed results (medians, CIs).
#' @field x_range Numeric vector. The min and max range for simulation.
#'
#' @export
BaseBootstrap <- R6::R6Class(
    "BaseBootstrap",
    public = list(
        # Data and model parameters
        data = NULL,
        y_var = NULL,
        x_vars = NULL,

        # Results storage
        boot_results = NULL,
        aggregated_data = NULL,
        x_range = NULL,

        #' @description
        #' Create a new BaseBootstrap object.
        #' @param data A dataframe.
        #' @param y_var Name of the response variable.
        #' @param x_vars Vector of regressor names.
        initialize = function(data, y_var, x_vars){
            self$data <- data
            self$y_var <- y_var
            self$x_vars <- x_vars
        }
    ),

    private = list(
        create_sim_mx = function(x_max, x_min, tmax, tmin, order) {
            # order=c(p, q) the numbers of pairs and poly; ncol = 2q+p+1
            p <- order[1]; q <- order[2]
            r <- seq(x_min, x_max, 0.5)
            s <- (r - tmin)/(tmax - tmin)
            mx1 <- matrix(sapply(1:p, function(i) s^i), ncol=p, byrow=FALSE)
            mx2 <- matrix(sapply(1:q, function(i) c(sin(2*i*pi*s), cos(2*i*pi*s))), ncol=2*q, byrow=FALSE)
            ones <- matrix(1, nrow=length(s), ncol=1)
            cbind(ones, mx1, mx2)
        },

        fill_table = function(boot_results, sim_mx) {
            # Use stored x_range
            rounds <- length(boot_results)
            x_min <- self$x_range[1]
            x_max <- self$x_range[2]

            # Build an empty dataframe
            df <- data.frame(
                matrix(NA, nrow=nrow(sim_mx), ncol=(rounds+1))
            )
            colnames(df) <- c("temperature", paste0("round_", seq_len(rounds)))
            # Fill in temp
            df$temperature <- seq(x_min, x_max, 0.5)
            # Loop: every round has a set of fitted values
            for (rd in 1:rounds){
                coef_vec <- unname(boot_results[[rd]]) # strip names
                ghat_x <- sim_mx %*% coef_vec
                j <- rd + 1
                df[, j] <- ghat_x[, 1]
            }
            # Calculate quantiles
            df$CI_lb <- apply(df[, -1], 1, quantile, probs=0.025, na.rm=TRUE)
            df$fv_median <- apply(df[, -1], 1, median, na.rm=TRUE)
            df$CI_ub <- apply(df[, -1], 1, quantile, probs=0.975, na.rm=TRUE)

            return(df)
        }
    )
)

# TRF ---------------------------------------------
#' Class: TRF Model
#' @importFrom R6 R6Class
#'
#' @description
#' Inherits from [BaseBootstrap] to implement TRF-specific bootstrapping.
#'
#' @field sim_matrix The generated simulation matrix.
#' @field order Numeric vector. The order of the polynomial and trigonometric terms.
#' @field temp_vars Character vector. Automatically detected temperature-related variables.
#'
#' @export
TRF_Model <- R6::R6Class(
    "TRF_Model",
    inherit = BaseBootstrap,

    public = list(
        sim_matrix = NULL,
        order = NULL,
        temp_vars = NULL,

        #' @description
        #' Generates the simulation matrix for predictions.
        #' @param x_range Numeric vector c(min, max) for the temperature grid.
        #' @param t_range Numeric vector c(min, max) for scaling.
        #' @param order Numeric vector c(p, q) for model order.
        create_sim_matrix = function(x_range, t_range, order) {
            self$x_range <- x_range
            self$order <- order
            x_min <- x_range[1]; x_max <- x_range[2]
            t_min <- t_range[1]; t_max <- t_range[2]
            self$sim_matrix <- private$create_sim_mx(x_max, x_min, t_max, t_min, order)
            # Automatically detect temperature variables
            self$temp_vars <- grep("ts|sin|cos", self$x_vars, value = TRUE)
        },

        #' @description
        #' Runs the bootstrap resampling.
        #' @param rounds Integer. Number of bootstrap iterations.
        run_bootstrap = function(rounds) {
            y <- self$data[[self$y_var]]
            X <- model.matrix(~ ., data = self$data[self$x_vars])
            rgr <- lm.fit(X, y) # faster than lm()
            u <- residuals(rgr)

            # Bootstrap loop - TRF
            self$boot_results <- pbapply::pblapply(1:rounds, function(rd) {
                u_ <- sample(u, replace = TRUE)
                y_ <- X %*% rgr$coefficient + u_
                lm.fit(X, y_)$coefficients # faster
            })
        },

        #' @description
        #' Processes bootstrap results into an aggregate table.
        aggregate_results = function() {
            # Convert to plain numeric vectors
            boot_results_subset <- lapply(self$boot_results, function(coefs) {
                unname(unlist(c(coefs["(Intercept)"], coefs[self$temp_vars])))
            })
            # Verify dimensions
            expected_length <- ncol(self$sim_matrix)
            if (!all(sapply(boot_results_subset, length) == expected_length)) {
                stop(paste("Coefficient length mismatch. Expected ", expected_length,
                           " but got ", unique(sapply(boot_results_subset, length))))
            }
            self$aggregated_data <- private$fill_table(boot_results_subset, self$sim_matrix)
        },

        #' @description
        #' Plots the median curve and confidence bands.
        #' @param base_plot An ggplot2 object to layer the results onto.
        #' @param save_path Optional. File path to save the plot.
        plot = function(base_plot, save_path = NULL) {
            p <- base_plot+
                geom_line(data=self$aggregated_data, aes(x=temperature, y=fv_median))+
                geom_ribbon(data=self$aggregated_data,
                            aes(x=temperature, ymin = CI_lb, ymax = CI_ub),
                            fill="grey", alpha=0.5)
            if (!is.null(save_path)) ggsave(save_path, p)
            return(p)
        }
    )
)

# CTRF ----------------------------------------------
#' Class: CTRF Model
#' @importFrom R6 R6Class
#'
#' @description
#' Inherits from [BaseBootstrap] to handle Covariate-dependent TRF models.
#'
#' @field var_groups List of character vectors defining variable subsets.
#' @field sim_matrices List of simulation matrices for each group.
#' @field orders List of order vectors for each group.
#'
#' @export
CTRF_Model <- R6::R6Class(
    "CTRF_Model",
    inherit = BaseBootstrap,

    public = list(
        var_groups = NULL,  # List of variable subsets, list(vars1, vars2, ...)
        sim_matrices = NULL,  # store multiple simulation matrices
        orders = NULL,  # List of orders, list(c(2,2), c(1,1), ...)

        #' @description
        #' Initialize CTRF model.
        #' @param data Dataframe.
        #' @param y_var Response variable name.
        #' @param x_vars All covariate names.
        #' @param var_groups List of character vectors for variable grouping.
        #' @param orders List of numeric vectors c(p, q) for each group.
        initialize = function(data, y_var, x_vars, var_groups, orders) {
            super$initialize(data, y_var, x_vars)
            self$var_groups <- var_groups
            self$orders <- orders
        },

        #' @description
        #' Create simulation matrices for all variable groups.
        #' @param x_range Numeric vector for temperature grid.
        #' @param t_range Numeric vector for scaling.
        create_sim_matrices = function(x_range, t_range) {
            self$x_range <- x_range
            self$sim_matrices <- lapply(seq_along(self$var_groups), function(i) {
                order <- self$orders[[i]]
                private$create_sim_mx(x_range[2], x_range[1], t_range[2], t_range[1], order)
            })
        },

        #' @description
        #' Runs the bootstrap resampling for CTRF.
        #' @param rounds Integer. Number of iterations.
        run_bootstrap = function(rounds) {
            y <- self$data[[self$y_var]]
            X <- model.matrix(~ ., data = self$data[self$x_vars])
            rgr <- lm.fit(X, y)
            u <- residuals(rgr)

            # Bootstrap loop - CTRF
            self$boot_results <- pbapply::pblapply(1:rounds, function(rd) {
                u_ <- sample(u, replace = TRUE)
                y_ <- X %*% rgr$coefficient + u_
                coefs <- lm.fit(X, y_)$coefficients

                # Return a list of coefficients by var_groups
                lapply(seq_along(self$var_groups), function(i) {
                    vars <- self$var_groups[[i]]
                    if (i == 1) { # for base TRF
                        c(coefs["(Intercept)"], coefs[vars])
                    } else { # for covariate groups
                        coefs[vars]
                    }
                })
            })
        },

        #' @description
        #' Aggregates results for all groups.
        aggregate_results = function() {
            # Process each variable group separately
            self$aggregated_data <- lapply(seq_along(self$var_groups), function(i) {
                group_results <- lapply(self$boot_results, `[[`, i)
                ## extraction operator [[ to extract the i-th element from each list in boot_results
                private$fill_table(group_results, self$sim_matrices[[i]])
                ## use group-specific matrix
            })
        },

        #' @description
        #' Plots partial effects for each covariate group.
        #' @param save_dir Directory to save images.
        #' @param x_breaks Breaks for the x-axis.
        #' @param hrsuffix String suffix for file names.
        plot = function(save_dir, x_breaks=seq(-5, 35, 5), hrsuffix=NULL) {
            for (i in 2:length(self$aggregated_data)) {
                filename <- paste0("CTRF_", i, hrsuffix, ".png")
                p <- ggplot(self$aggregated_data[[i]], aes(temperature, fv_median)) +
                    geom_line() +
                    geom_hline(yintercept=0, color="plum", linetype="solid")+
                    geom_ribbon(aes(ymin=CI_lb, ymax=CI_ub), fill="grey", alpha=0.5)+
                    labs(x="Temperature (C)", y="Partial Effect")+
                    scale_x_continuous(breaks = x_breaks)
                ggsave(file.path(save_dir, filename), p, width=5, height=0.618*5)
            }
        }
    )
)

# END
