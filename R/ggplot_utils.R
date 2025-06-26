# ggplotUtils Package
# A utility package for enhanced ggplot2 functionality

# Declare global variables used in ggplot2 aes() calls
utils::globalVariables(c("x", "y", "lower", "upper", "mean"))

#' Prepare Data with Ordered X-axis Labels
#'
#' Helper function to reorder factor levels in data before plotting
#'
#' @param df Data frame
#' @param x_col Character. Name of the x column to reorder
#' @param xlabels_ordered Character vector. Desired order of factor levels
#'
#' @return Data frame with reordered factor levels
#' @export
prepare_data_with_ordered_x <- function(df, x_col = "x", xlabels_ordered = NULL) {
    
    if (!is.null(xlabels_ordered)) {
        if (!is.character(xlabels_ordered)) {
            warning("xlabels_ordered should be a character vector. Converting to character.")
            xlabels_ordered <- as.character(xlabels_ordered)
        }
        
        # Convert x column to ordered factor
        df[[x_col]] <- factor(df[[x_col]], levels = xlabels_ordered, ordered = TRUE)
    }
    
    return(df)
}

#' Compute Summary Statistics for Grouped Data
#'
#' Computes mean and error bounds for grouped data
#'
#' @param df Data frame
#' @param varname Character. Name of the variable to summarize
#' @param groupnames Character vector. Names of grouping variables
#' @param error_type Character. Type of error ("sd", "se", "ci")
#' @param ci_level Numeric. Confidence level for CI (default: 0.95)
#'
#' @return Data frame with summary statistics
#' @export
compute_summary_stats <- function(df, varname, groupnames, error_type = "sd", ci_level = 0.95) {
    
    # Group and summarize
    result <- df %>%
        dplyr::group_by(dplyr::across(dplyr::all_of(groupnames))) %>%
        dplyr::summarise(
            n = dplyr::n(),
            mean = mean(.data[[varname]], na.rm = TRUE),
            sd = stats::sd(.data[[varname]], na.rm = TRUE),
            se = sd / sqrt(n),
            .groups = "drop"
        )
    
    # Calculate error bounds based on type
    if (error_type == "sd") {
        result$lower <- result$mean - result$sd
        result$upper <- result$mean + result$sd
    } else if (error_type == "se") {
        result$lower <- result$mean - result$se
        result$upper <- result$mean + result$se
    } else if (error_type == "ci") {
        t_val <- stats::qt((1 + ci_level) / 2, result$n - 1)
        margin <- t_val * result$se
        result$lower <- result$mean - margin
        result$upper <- result$mean + margin
    }
    
    return(result)
}

#' Create Basic Bar Plot
#'
#' Creates a basic bar plot with mean values from grouped data
#'
#' @param df Data frame with columns 'x' and 'y'
#' @param summary_fun Function to compute summary statistics (default: compute_summary_stats)
#' @param xlabels_ordered Character vector. Ordered factor levels for x-axis (applied before plotting)
#' @param bar_width Numeric. Width of bars (default: 0.65)
#' @param bar_fill Character. Fill color for bars (default: "white")
#' @param bar_linewidth Numeric. Line width for bar outlines (default: 1.6)
#' @param show_points Logical. Whether to show individual data points (default: TRUE)
#' @param point_size Numeric. Size of individual points (default: 5)
#' @param point_stroke Numeric. Stroke width for points (default: 3)
#' @param point_shape Numeric. Shape of points (default: 1)
#' @param error_type Character. Type of error for summary calculation ("sd", "se", "ci") (default: "sd")
#'
#' @return ggplot object
#' @export
#' @import ggplot2
#'
#' @examples
#' df <- data.frame(x = rep(c("A", "B", "C"), each = 10), 
#'                  y = c(rnorm(10, 5), rnorm(10, 7), rnorm(10, 6)))
#' p <- create_barplot(df)
#' print(p)
create_barplot <- function(
    df,
    summary_fun = compute_summary_stats,
    xlabels_ordered = NULL,
    bar_width = 0.65,
    bar_fill = "white",
    bar_linewidth = 1.6,
    show_points = TRUE,
    point_size = 5,
    point_stroke = 3,
    point_shape = 1,
    error_type = "sd"
) {
    
    # Validate inputs
    if (!is.data.frame(df)) {
        stop("df must be a data frame")
    }
    
    required_cols <- c("x", "y")
    if (!all(required_cols %in% names(df))) {
        stop("Data frame must contain columns: ", paste(required_cols, collapse = ", "))
    }
    
    if (!error_type %in% c("sd", "se", "ci")) {
        stop("error_type must be one of: 'sd', 'se', 'ci'")
    }
    
    # IMPORTANT: Prepare data with ordered x-axis labels BEFORE any processing
    df <- prepare_data_with_ordered_x(df, x_col = "x", xlabels_ordered = xlabels_ordered)
    
    # Compute summary statistics (this will preserve the factor ordering)
    df_summary <- summary_fun(df, varname = "y", groupnames = "x", error_type = error_type)
    
    # Create base plot with bars
    p <- ggplot2::ggplot() +
        ggplot2::geom_bar(
            data = df_summary,
            ggplot2::aes(x = x, y = mean, color = x),
            fill = bar_fill,
            stat = "identity",
            width = bar_width,
            linewidth = bar_linewidth
        )
    
    # Add individual points if requested
    if (show_points) {
        p <- p + ggplot2::geom_point(
            data = df,
            ggplot2::aes(x = x, y = y, color = factor(x)),
            size = point_size,
            stroke = point_stroke,
            shape = point_shape
        )
    }
    
    # Store summary data as attribute for use by add_errorbars
    attr(p, "summary_data") <- df_summary
    attr(p, "original_data") <- df
    
    return(p)
}

#' Add Error Bars to Existing Plot
#'
#' Adds error bars to an existing ggplot object, typically created with create_barplot()
#'
#' @param plot ggplot object to add error bars to
#' @param df_summary Data frame with summary statistics (optional, will use plot's stored data if available)
#' @param error_bar_width Numeric. Width of error bar caps (default: 0.25)
#' @param error_bar_linewidth Numeric. Line width for error bars (default: 1)
#' @param error_bar_color Character. Color for error bars (default: "#404040ff")
#' @param error_type Character. Type of error bars ("sd", "se", "ci") (default: "sd")
#'
#' @return Modified ggplot object with error bars added
#' @export
#' @import ggplot2
#'
#' @examples
#' df <- data.frame(x = rep(c("A", "B", "C"), each = 10), 
#'                  y = c(rnorm(10, 5), rnorm(10, 7), rnorm(10, 6)))
#' p <- create_barplot(df)
#' p_with_errors <- add_errorbars(p)
#' print(p_with_errors)
add_errorbars <- function(
    plot,
    df_summary = NULL,
    error_bar_width = 0.25,
    error_bar_linewidth = 1,
    error_bar_color = "#404040ff",
    error_type = "sd"
) {
    
    # Validate inputs
    if (!inherits(plot, "ggplot")) {
        stop("plot must be a ggplot object")
    }
    
    # Try to get summary data from plot attributes if not provided
    if (is.null(df_summary)) {
        df_summary <- attr(plot, "summary_data")
        if (is.null(df_summary)) {
            stop("df_summary must be provided or plot must contain stored summary data from create_barplot()")
        }
    }
    
    # Check if summary data has required columns
    required_cols <- c("x", "mean", "lower", "upper")
    if (!all(required_cols %in% names(df_summary))) {
        stop("df_summary must contain columns: ", paste(required_cols, collapse = ", "))
    }
    
    # Add error bars to the plot
    plot <- plot + ggplot2::geom_errorbar(
        data = df_summary,
        ggplot2::aes(x = x, ymin = lower, ymax = upper),
        width = error_bar_width,
        linewidth = error_bar_linewidth,
        color = error_bar_color
    )
    
    return(plot)
}

#' Apply Minimal Aesthetics to ggplot Objects
#'
#' This function applies a consistent minimal theme and styling to ggplot objects
#' with customizable parameters for publication-ready plots.
#'
#' @param ggplt A ggplot object to modify
#' @param width Numeric. Plot width for display (sets repr.plot.width option)
#' @param height Numeric. Plot height for display (sets repr.plot.height option)
#' @param title Character. Plot title
#' @param xlabel Character. X-axis label
#' @param ylabel Character. Y-axis label
#' @param xlim Numeric vector of length 2. X-axis limits
#' @param ylim Numeric vector of length 2. Y-axis limits
#' @param x.text.angle Numeric. Angle for x-axis text rotation
#' @param expand Numeric vector. Expansion parameters for axes (default: c(0, 0))
#' @param plot.title element_text object. Title styling
#' @param axis.text element_text object. Axis text styling
#' @param axis.title element_text object. Axis title styling
#' @param legend.position Character. Legend position ("right", "left", "top", "bottom", "none")
#' @param legend.title element_text object. Legend title styling
#' @param legend.text element_text object. Legend text styling
#' @param legend.size Numeric. Legend key size
#' @param legend.size.unit Character. Unit for legend size (default: "in")
#' @param scale_color_manual.values Named vector. Custom color values
#' @param filename Character. Base filename for saving (without extension)
#' @param file_formats Character vector. File formats to save (default: c("png", "svg"))
#' @param remove_grid Logical. Whether to remove grid lines (default: TRUE)
#' @param axis_lines Logical. Whether to add axis lines (default: TRUE)
#'
#' @return Modified ggplot object
#' @export
#' @import ggplot2
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point()
#' apply_minimal_theme(p, title = "Car Weight vs MPG")
apply_minimal_theme <- function(
    ggplt, 
    width = NULL, 
    height = NULL,
    title = NULL,
    xlabel = NULL,
    ylabel = NULL,
    xlim = NULL,
    ylim = NULL, 
    x.text.angle = NULL,
    expand = c(0, 0),
    plot.title = ggplot2::element_text(size = 24),
    axis.text = ggplot2::element_text(size = 24),
    axis.title = ggplot2::element_text(size = 26),
    legend.position = "right",
    legend.title = ggplot2::element_text(size = 16),
    legend.text = ggplot2::element_text(size = 16),
    legend.size = NULL,
    legend.size.unit = "in",
    scale_color_manual.values = NULL,
    filename = NULL,
    file_formats = c("png", "svg"),
    remove_grid = TRUE,
    axis_lines = TRUE
) {
    
    # Validate inputs
    if (!inherits(ggplt, "ggplot")) {
        stop("ggplt must be a ggplot object")
    }
    
    # Set display dimensions
    if (!is.null(width)) {
        options(repr.plot.width = width)
    }
    if (!is.null(height)) {
        options(repr.plot.height = height)
    }
    
    # Apply color scale if provided
    if (!is.null(scale_color_manual.values)) {
        ggplt <- ggplt + ggplot2::scale_color_manual(values = scale_color_manual.values)
    }
    
    # Apply base theme and labels
    ggplt <- ggplt +
        ggplot2::theme_minimal() +
        ggplot2::labs(
            title = title,
            x = xlabel,
            y = ylabel
        ) +
        ggplot2::theme(
            plot.title = plot.title,
            axis.text = axis.text,
            axis.title = axis.title,
            legend.position = legend.position,
            legend.title = legend.title,
            legend.text = legend.text
        )
    
    # Conditionally remove grid and add axis lines
    if (remove_grid) {
        ggplt <- ggplt + ggplot2::theme(
            panel.grid.major = ggplot2::element_blank(),
            panel.grid.minor = ggplot2::element_blank()
        )
    }
    
    if (axis_lines) {
        ggplt <- ggplt + ggplot2::theme(
            axis.line.x = ggplot2::element_line(color = "black"),
            axis.line.y = ggplot2::element_line(color = "black")
        )
    }
    
    # Apply axis limits and expansion
    if (!is.null(xlim)) {
        ggplt <- ggplt + ggplot2::scale_x_continuous(limits = xlim)
    }
    
    if (!is.null(ylim)) {
        ggplt <- ggplt + ggplot2::scale_y_continuous(limits = ylim, expand = expand)
    } else {
        ggplt <- ggplt + ggplot2::scale_y_continuous(expand = expand)
    }
    
    # Handle x-axis text rotation
    if (!is.null(x.text.angle)) {      
        ggplt <- ggplt + ggplot2::theme(
            axis.text.x = ggplot2::element_text(
                angle = x.text.angle,
                vjust = 1,
                hjust = 1
            )
        )
    }
    
    # Adjust legend key size
    if (!is.null(legend.size)) {
        ggplt <- ggplt + ggplot2::theme(
            legend.key.size = ggplot2::unit(legend.size, legend.size.unit)
        )
    }
    
    # Save plot if filename provided
    if (!is.null(filename)) {
        save_plot(ggplt, filename, width, height, file_formats)
    }
    
    return(ggplt)
}

#' Create Complete Bar Plot with Error Bars
#'
#' Convenience function that combines create_barplot() and add_errorbars()
#' This is equivalent to the old create_barplot_with_errors function
#'
#' @param df Data frame with columns 'x' and 'y'
#' @param summary_fun Function to compute summary statistics (default: compute_summary_stats)
#' @param xlabels_ordered Character vector. Ordered factor levels for x-axis (applied before plotting)
#' @param title Character. Plot title
#' @param xlabel Character. X-axis label  
#' @param ylabel Character. Y-axis label
#' @param scale_color_manual.values Named vector. Custom color values
#' @param width Numeric. Plot width
#' @param height Numeric. Plot height
#' @param ylim Numeric vector. Y-axis limits
#' @param filename Character. Base filename for saving
#' @param file_formats Character vector. File formats to save
#' @param bar_width Numeric. Width of bars (default: 0.65)
#' @param bar_fill Character. Fill color for bars (default: "white")
#' @param bar_linewidth Numeric. Line width for bar outlines (default: 1.6)
#' @param show_points Logical. Whether to show individual data points (default: TRUE)
#' @param point_size Numeric. Size of individual points (default: 5)
#' @param point_stroke Numeric. Stroke width for points (default: 3)
#' @param point_shape Numeric. Shape of points (default: 1)
#' @param error_bar_width Numeric. Width of error bar caps (default: 0.25)
#' @param error_bar_linewidth Numeric. Line width for error bars (default: 1)
#' @param error_bar_color Character. Color for error bars (default: "#404040ff")
#' @param error_type Character. Type of error bars ("sd", "se", "ci") (default: "sd")
#'
#' @return ggplot object with bars, points, and error bars
#' @export
#' @import ggplot2
#'
#' @examples
#' df <- data.frame(x = rep(c("A", "B", "C"), each = 10), 
#'                  y = c(rnorm(10, 5), rnorm(10, 7), rnorm(10, 6)))
#' p <- create_complete_barplot(df, title = "Complete Bar Plot", xlabel = "Groups", ylabel = "Values")
#' print(p)
create_complete_barplot <- function(
    df,
    summary_fun = compute_summary_stats,
    xlabels_ordered = NULL,
    title = NULL,
    xlabel = NULL,
    ylabel = NULL,
    scale_color_manual.values = NULL,
    width = NULL,
    height = NULL,
    ylim = NULL,
    filename = NULL,
    file_formats = c("png", "svg"),
    bar_width = 0.65,
    bar_fill = "white",
    bar_linewidth = 1.6,
    show_points = TRUE,
    point_size = 5,
    point_stroke = 3,
    point_shape = 1,
    error_bar_width = 0.25,
    error_bar_linewidth = 1,
    error_bar_color = "#404040ff",
    error_type = "sd"
) {
    
    # Create base bar plot (xlabels_ordered is handled inside create_barplot)
    p <- create_barplot(
        df = df,
        summary_fun = summary_fun,
        xlabels_ordered = xlabels_ordered,
        bar_width = bar_width,
        bar_fill = bar_fill,
        bar_linewidth = bar_linewidth,
        show_points = show_points,
        point_size = point_size,
        point_stroke = point_stroke,
        point_shape = point_shape,
        error_type = error_type
    )
    
    # Add error bars
    p <- add_errorbars(
        plot = p,
        error_bar_width = error_bar_width,
        error_bar_linewidth = error_bar_linewidth,
        error_bar_color = error_bar_color,
        error_type = error_type
    )
    
    # Apply minimal theme and styling (xlabels_ordered parameter removed here)
    p <- apply_minimal_theme(
        ggplt = p,
        title = title,
        xlabel = xlabel,
        ylabel = ylabel,
        width = width,
        height = height,
        ylim = ylim,
        scale_color_manual.values = scale_color_manual.values,
        x.text.angle = 45,
        legend.position = "none",
        filename = filename,
        file_formats = file_formats
    )
    
    return(p)
}

#' Save ggplot to Multiple Formats
#'
#' Helper function to save ggplot objects to multiple file formats
#'
#' @param plot ggplot object to save
#' @param filename Character. Base filename (without extension)
#' @param width Numeric. Plot width (default: 3.7)
#' @param height Numeric. Plot height (default: 7.6)
#' @param file_formats Character vector. File formats to save
#'
#' @return Invisible NULL (called for side effects)
#' @export
save_plot <- function(plot, filename, width = NULL, height = NULL, 
                     file_formats = c("png", "svg")) {
    
    # Set default dimensions
    if (is.null(width)) width <- 3.7
    if (is.null(height)) height <- 7.6
    
    for (format in file_formats) {
        fileoutput <- paste0(filename, ".", format)
        ggplot2::ggsave(
            file = fileoutput,
            plot = plot,
            width = width,
            height = height,
            bg = "white"
        )
        message("Saved: ", fileoutput)
    }
}

#' Create Default Color Palette
#'
#' Returns a default color palette for consistent plotting
#'
#' @param n Integer. Number of colors needed
#' @param palette Character. Palette type ("viridis", "Set1", "Dark2", etc.)
#'
#' @return Named vector of colors
#' @export
get_default_colors <- function(n, palette = "viridis") {
    
    if (palette == "viridis") {
        return(viridisLite::viridis(n))
    } else {
        return(RColorBrewer::brewer.pal(min(n, 8), palette))
    }
}

# Package dependencies
#' @import ggplot2
#' @import dplyr
#' @importFrom stats sd qt
#' @importFrom viridisLite viridis
#' @importFrom RColorBrewer brewer.pal
NULL

#' Add Statistical Annotations to ggplot
#'
#' Adds statistical comparison annotations (p-values with brackets) to an existing ggplot object
#' and optionally adjusts the y-axis limits to accommodate the annotations.
#' Note: pwc must be preprocessed with rstatix::add_xy_position(pwc, x = "x") before using this function.
#'
#' @param plot ggplot object to add annotations to
#' @param pwc Data frame with pairwise comparison results that has been preprocessed with rstatix::add_xy_position(). Must contain columns 'group1', 'group2', 'xmin', 'xmax', 'y.position', and a p-value column (e.g., 'p.adj', 'p', or 'p.value')
#' @param label Character. Label format for p-values (default: "{ifelse(p.adj < 0.0001, 'p<0.0001', paste('p=', format.pval(round(p.adj, 4))))}")
#' @param step.increase Numeric. Step increase for automatic y.position adjustment (default: 0.05)
#' @param label.size Numeric. Size of annotation labels (default: 5)
#' @param bracket.size Numeric. Size of annotation brackets (default: 0.6)
#' @param tip.length Numeric. Length of bracket tips (default: 0)
#' @param ylim Numeric vector of length 2. Y-axis limits to apply after adding annotations (optional)
#' @param ... Additional parameters passed to ggpubr::stat_pvalue_manual()
#'
#' @return Modified ggplot object with statistical annotations
#' @export
#' @import ggplot2
#' @importFrom ggpubr stat_pvalue_manual
#' @importFrom rstatix add_xy_position
#'
#' @examples
#' \dontrun{
#' # Create sample data and plot
#' df <- data.frame(
#'   x = rep(c("A", "B", "C"), each = 10), 
#'   y = c(rnorm(10, 5, 1), rnorm(10, 7, 1), rnorm(10, 6, 1))
#' )
#' 
#' # Create base plot
#' p <- create_complete_barplot(df, title = "Statistical Comparisons", 
#'                              xlabel = "Groups", ylabel = "Values")
#' 
#' # Sample pairwise comparison data
#' pwc <- data.frame(
#'   group1 = c("A", "A", "B"),
#'   group2 = c("B", "C", "C"),
#'   p.adj = c(0.001, 0.05, 0.12)
#' )
#' 
#' # Preprocess pwc with rstatix
#' pwc <- rstatix::add_xy_position(pwc, x = "x")
#' 
#' # Add statistical annotations
#' p_with_stats <- add_stat_annotations(p, pwc, ylim = c(0, 10))
#' print(p_with_stats)
#' }
add_stat_annotations <- function(
    plot,
    pwc,
    label = "{ifelse(p.adj < 0.0001, 'p<0.0001', paste('p=', format.pval(round(p.adj, 4))))}",
    step.increase = 0.05,
    label.size = 5,
    bracket.size = 0.6,
    tip.length = 0,
    ylim = NULL,
    ...
) {
    
    # Validate inputs
    if (!inherits(plot, "ggplot")) {
        stop("plot must be a ggplot object")
    }
    
    if (!is.data.frame(pwc)) {
        stop("pwc must be a data frame")
    }
    
    # Check if ggpubr is available
    if (!requireNamespace("ggpubr", quietly = TRUE)) {
        stop("Package 'ggpubr' is required for this function. Please install it with: install.packages('ggpubr')")
    }
    
    # Check if rstatix is available
    if (!requireNamespace("rstatix", quietly = TRUE)) {
        stop("Package 'rstatix' is required for this function. Please install it with: install.packages('rstatix')")
    }
    
    # Check required columns (including those added by rstatix::add_xy_position)
    required_cols <- c("group1", "group2", "xmin", "xmax", "y.position")
    missing_cols <- required_cols[!required_cols %in% names(pwc)]
    if (length(missing_cols) > 0) {
        stop("pwc must contain columns: ", paste(missing_cols, collapse = ", "), 
             "\nMake sure to preprocess pwc with: pwc <- rstatix::add_xy_position(pwc, x = 'x')")
    }
    
    # Check for p-value column
    p_cols <- c("p.adj", "p", "p.value", "pval")
    p_col_exists <- any(p_cols %in% names(pwc))
    if (!p_col_exists) {
        stop("pwc must contain at least one p-value column: ", paste(p_cols, collapse = ", "))
    }
    
    # Add statistical annotations
    plot <- plot + ggpubr::stat_pvalue_manual(
        pwc,
        label = label,
        step.increase = step.increase,
        label.size = label.size,
        bracket.size = bracket.size,
        tip.length = tip.length,
        ...
    )
    
    # Apply y-axis limits if provided
    if (!is.null(ylim)) {
        if (!is.numeric(ylim) || length(ylim) != 2) {
            warning("ylim should be a numeric vector of length 2. Ignoring ylim parameter.")
        } else {
            plot <- plot + ggplot2::scale_y_continuous(
                limits = ylim,
                expand = ggplot2::expansion(mult = c(0, 0))
            )
        }
    }
    
    return(plot)
}


#' Add Statistical Annotations with Auto Y-limit Adjustment
#'
#' Convenience function that adds statistical annotations and automatically adjusts 
#' y-axis limits based on the maximum annotation position plus a buffer.
#' Note: pwc must be preprocessed with rstatix::add_xy_position(pwc, x = "x") before using this function.
#'
#' @param plot ggplot object to add annotations to
#' @param pwc Data frame with pairwise comparison results that has been preprocessed with rstatix::add_xy_position(). Must contain columns 'group1', 'group2', 'xmin', 'xmax', 'y.position', and a p-value column
#' @param label Character. Label format for p-values (default: "{ifelse(p.adj < 0.0001, 'p<0.0001', paste('p=', format.pval(round(p.adj, 4))))}")
#' @param step.increase Numeric. Step increase for automatic y.position adjustment (default: 0.05)
#' @param label.size Numeric. Size of annotation labels (default: 5)
#' @param bracket.size Numeric. Size of annotation brackets (default: 0.6)
#' @param tip.length Numeric. Length of bracket tips (default: 0)
#' @param y.buffer Numeric. Additional buffer to add above the highest annotation (default: 0.1)
#' @param min.y Numeric. Minimum y-axis value (default: 0)
#' @param ... Additional parameters passed to ggpubr::stat_pvalue_manual()
#'
#' @return Modified ggplot object with statistical annotations and adjusted y-limits
#' @export
#' @import ggplot2
#' @importFrom ggpubr stat_pvalue_manual
#' @importFrom rstatix add_xy_position
#'
#' @examples
#' \dontrun{
#' # Create sample data and plot
#' df <- data.frame(
#'   x = rep(c("A", "B", "C"), each = 10), 
#'   y = c(rnorm(10, 5, 1), rnorm(10, 7, 1), rnorm(10, 6, 1))
#' )
#' 
#' # Create base plot
#' p <- create_complete_barplot(df, title = "Auto Y-limit Adjustment", 
#'                              xlabel = "Groups", ylabel = "Values")
#' 
#' # Sample pairwise comparison data
#' pwc <- data.frame(
#'   group1 = c("A", "A", "B"),
#'   group2 = c("B", "C", "C"),
#'   p.adj = c(0.001, 0.05, 0.12)
#' )
#' 
#' # Preprocess pwc with rstatix
#' pwc <- rstatix::add_xy_position(pwc, x = "x")
#' 
#' # Add statistical annotations with auto y-limit adjustment
#' p_auto <- add_stat_annotations_auto(p, pwc)
#' print(p_auto)
#' }
add_stat_annotations_auto <- function(
    plot,
    pwc,
    label = "{ifelse(p.adj < 0.0001, 'p<0.0001', paste('p=', format.pval(round(p.adj, 4))))}",
    step.increase = 0.05,
    label.size = 5,
    bracket.size = 0.6,
    tip.length = 0,
    y.buffer = 0.1,
    min.y = 0,
    ...
) {
    
    # Validate inputs
    if (!inherits(plot, "ggplot")) {
        stop("plot must be a ggplot object")
    }
    
    if (!is.data.frame(pwc)) {
        stop("pwc must be a data frame")
    }
    
    # Check if ggpubr is available
    if (!requireNamespace("ggpubr", quietly = TRUE)) {
        stop("Package 'ggpubr' is required for this function. Please install it with: install.packages('ggpubr')")
    }
    
    # Check if rstatix is available
    if (!requireNamespace("rstatix", quietly = TRUE)) {
        stop("Package 'rstatix' is required for this function. Please install it with: install.packages('rstatix')")
    }
    
    # Check required columns (including those added by rstatix::add_xy_position)
    required_cols <- c("group1", "group2", "xmin", "xmax", "y.position")
    missing_cols <- required_cols[!required_cols %in% names(pwc)]
    if (length(missing_cols) > 0) {
        stop("pwc must contain columns: ", paste(missing_cols, collapse = ", "), 
             "\nMake sure to preprocess pwc with: pwc <- rstatix::add_xy_position(pwc, x = 'x')")
    }
    
    # Get the maximum y value from the plot data
    plot_data <- attr(plot, "summary_data")
    if (is.null(plot_data)) {
        # Try to extract from plot layers if summary data not available
        warning("Could not find summary data. Using basic estimation for y-limits.")
        max_y <- 10  # Default fallback
    } else {
        max_y <- max(plot_data$upper, na.rm = TRUE)
    }
    
    # Use the maximum y.position from the preprocessed pwc data
    max_annotation_y <- max(pwc$y.position, na.rm = TRUE)
    
    # Set y-limits with buffer
    ylim_auto <- c(min.y, max_annotation_y + (y.buffer * max_annotation_y))
    
    # Add annotations with auto-calculated y-limits
    plot <- add_stat_annotations(
        plot = plot,
        pwc = pwc,
        label = label,
        step.increase = step.increase,
        label.size = label.size,
        bracket.size = bracket.size,
        tip.length = tip.length,
        ylim = ylim_auto,
        ...
    )
    
    return(plot)
}