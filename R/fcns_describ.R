#' Print regression summaries into a text file
#' This function takes one or more fitted model objects.
#' @importFrom lmtest coeftest
#' @importFrom sandwich vcovHC
#' @importFrom broom glance
#'
#' @param ... One or more fitted model objects
#' (e.g., objects returned by \code{lm()}, \code{glm()}, or other regression functions).
#' These are collected into a list and processed sequentially.
#' @param filepath Character. The path where the summaries will be written.
#' @param text Character default \code{NULL} Text you want to put between models.
#'
#' @return Invisibly returns \code{NULL}. The function is called for its side effect of writing output to an file.
#' @export
print_output <- function(..., filepath, text=NULL){
    models <- list(...)
    sink(filepath)
    for (i in seq_along(models)) {
        cat("Model ", i, " summary:\n")
        print(lmtest::coeftest(models[[i]], vcov=sandwich::vcovHC(models[[i]], type="HC3")))
        print(broom::glance(models[[i]]))
        cat("\n")
    }
    cat(text)
    sink(file = NULL)
    invisible()
}

#' Draw and save the hourly plot as a PNG
#' @importFrom ggplot2 ggplot geom_line geom_ribbon geom_hline labs
#' @importFrom ggplot2 scale_x_continuous scale_y_continuous ggsave
#'
#' @param df Dataframe. Bootstrap results, usually Class method aggregate_results().
#' @param w_name Character. Weather variable name.
#' @param v_breaks Numeric vector.
#' See https://ggplot2.tidyverse.org/reference/scale_continuous.html#arg-breaks.
#' @param suffix Character. Suffix in the picture name.
#' @param save_dir Character. Directory where you save the plot.
#' @param y_lim Numeric vector.
#' See https://ggplot2.tidyverse.org/reference/scale_continuous.html#arg-limits.
#'
#' @return Invisibly returns \code{NULL}, saving the plot as an PNG.
#' @export
draw_save_CTRF <- function(df, w_name, v_breaks, suffix, save_dir, y_lim=NULL){
    filename <- paste0("CTRF_", suffix, ".png")
    plot_CTRF <- ggplot()+
        geom_line(data=df, aes(x=temperature, y=fv_median))+
        geom_ribbon(data=df, aes(x=temperature, ymin=CI_lb, ymax=CI_ub), fill="grey", alpha=0.5)+
        geom_hline(yintercept=0, color="plum", linetype="solid")+
        labs(x="Temperature (C)", y=paste0(w_name, " partial effect"))+
        scale_x_continuous(breaks = v_breaks)
    if (!is.null(y_lim)) {
        plot_CTRF + scale_y_continuous(limits=y_lim)
    }
    ggsave(file=file.path(save_dir, filename), width=5, height=0.618*5)
    invisible()
}

#' Draw hourly plot
#' This could be used as "base_plot" in TRF_Model environment, so it is not automatically saved.
#' @importFrom dplyr filter
#' @importFrom ggplot2 ggplot geom_point labs
#' @importFrom ggplot2 scale_x_continuous scale_y_continuous theme_classic
#' @param data Dataframe. Usually the main dataset, NOT bootstrap results.
#' @param hour Numeric. Hour.
#' @param y_var Character. Outcome variable name.
#' @param y_lab Character. See https://ggplot2.tidyverse.org/reference/labs.
#' @param color_hour Character. Color name specified for the selected Hour.
#' @param x_lim Numeric vector. See below
#' @param y_lim Numeric vector.
#' See https://ggplot2.tidyverse.org/reference/scale_continuous.html#arg-limits.
#' Fixed limits make it easier to compare hours.
#' @return An ggplot object.
#' @export
draw_tempplot <- function(data, hour, y_var, y_lab, color_hour, x_lim, y_lim){
    df <- dplyr::filter(data, Hour==hour)
    ## Drawing scatterplot doesn't need separate dt_hr beforehand
    plot <- ggplot()+
        geom_point(mapping=aes(x=df$temperature, y=df[[y_var]]), color=color_hour, alpha=0.3)+
        labs(x="Temperature (C)", y=y_lab)+
        scale_x_continuous(limits=x_lim)+
        scale_y_continuous(limits=y_lim)+
        theme_classic()
    return(plot)
}
