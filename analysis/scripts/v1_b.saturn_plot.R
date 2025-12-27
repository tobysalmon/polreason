################################################################################
# Saturn Plots: Visualizing Global Constraint via Mean Absolute Correlation
#
# Overview
# --------
# This module creates "Saturn plots" that visualize the overall constraint in
# belief systems by plotting 50% probability contours of bivariate normal
# distributions whose correlation equals the Mean Absolute Correlation (MAC).
#
# Following Tokuda et al. (2025), we represent constraint through bivariate
# normal contours: highly constrained systems (high MAC) produce tight ellipses,
# while weakly constrained systems (low MAC) produce more circular contours
# approaching a unit circle as MAC → 0.
#
# The "Saturn" metaphor: when LLM contours are overlaid on the GSS (human)
# contours, they appear as tighter "rings" around the broader, planet-like
# GSS contour, visually highlighting the hyper-constraint in LLM personas.
#
# Key Metric: Mean Absolute Correlation (MAC)
# --------------------------------------------
# For a p × p correlation matrix R:
#   MAC(R) = (2 / (p(p-1))) × Σ_{j<ℓ} |ρ_{jℓ}|
#
# MAC provides an intuitive summary of overall constraint, though it has
# limitations (see manuscript for discussion):
#   - Upward biased by noise (especially with large p)
#   - Sensitive to extreme values
#   - Ignores latent structure and sign patterns
#   - Affected by redundant items measuring the same construct
#
# Workflow
# --------
# 1. Load bootstrap correlation matrices for all raters (GSS + LLMs)
# 2. For each bootstrap draw, calculate MAC
# 3. Compute median MAC across bootstraps for each rater
# 4. For each rater's median MAC, plot the 50% probability contour of a
#    bivariate normal with correlation = median MAC
# 5. Overlay all contours on a single plot, with GSS prominently displayed
#
# Prerequisites
# -------------
# This script assumes:
#   - BASE_OUT_DIR, BASE_VIZ_DIR, YEAR are defined (from master.R)
#   - v.common_utils.R has been sourced (for available_raters, load_corr_for_rater)
#   - Correlation matrices exist in <rater>-<year>/polychor_bootstrap.rds
#
# Output
# ------
# Saves PDF: <BASE_VIZ_DIR>/saturn_plot_<YEAR>.pdf
# Optionally saves summary statistics: <BASE_OUT_DIR>/mac_summary_<YEAR>.csv
################################################################################

# Ensure required packages are loaded
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  stop("Package 'ggplot2' required for Saturn plots")
}
library(ggplot2)
library(data.table)

#' Compute Mean Absolute Correlation (MAC) from a correlation matrix
#'
#' @param R Square correlation matrix
#' @return Scalar MAC value, or NA if R is NULL or has < 2 variables
#'
#' @details
#' MAC = (2 / (p(p-1))) × Σ_{j<ℓ} |ρ_{jℓ}|
#' Only upper triangle is used to avoid double-counting.
compute_mac <- function(R) {
  if (is.null(R)) return(NA_real_)

  p <- ncol(R)
  if (p < 2) return(NA_real_)

  # Extract upper triangle (excluding diagonal)
  upper_tri <- R[upper.tri(R, diag = FALSE)]

  # Mean absolute correlation
  mac <- mean(abs(upper_tri), na.rm = TRUE)

  return(mac)
}


#' Compute MAC for a list of bootstrap correlation matrices
#'
#' @param corr_list List of correlation matrices (one per bootstrap)
#' @return Numeric vector of MAC values (one per bootstrap)
compute_mac_bootstraps <- function(corr_list) {
  vapply(corr_list, compute_mac, numeric(1L))
}


#' Extract all pairwise absolute correlations from a correlation matrix
#'
#' @param R Square correlation matrix
#' @return Numeric vector of absolute correlations (upper triangle only)
extract_all_abs_cors <- function(R) {
  if (is.null(R)) return(numeric(0))
  p <- ncol(R)
  if (p < 2) return(numeric(0))

  upper_tri <- R[upper.tri(R, diag = FALSE)]
  abs(upper_tri)
}


#' Compute correlation quantiles across bootstrap samples
#'
#' @param corr_list List of correlation matrices (one per bootstrap)
#' @param probs Quantile probabilities (default: 0.25, 0.5, 0.75, 0.9)
#' @return Data frame with columns: quantile, prob, median_rho, q025, q975
#'
#' @details
#' For each bootstrap, extracts ALL pairwise correlations, computes quantiles,
#' then summarizes across bootstraps with median and 95% CI
compute_correlation_quantiles <- function(corr_list, probs = c(0.25, 0.5, 0.75, 0.9)) {

  # For each bootstrap, get all |ρ| values and compute quantiles
  quantile_matrix <- matrix(NA_real_, nrow = length(corr_list), ncol = length(probs))
  colnames(quantile_matrix) <- paste0("q", probs * 100)

  for (i in seq_along(corr_list)) {
    abs_cors <- extract_all_abs_cors(corr_list[[i]])
    if (length(abs_cors) > 0) {
      quantile_matrix[i, ] <- quantile(abs_cors, probs = probs, na.rm = TRUE)
    }
  }

  # Summarize across bootstraps: median and 95% CI for each quantile
  results <- data.frame(
    quantile = paste0("Q", probs * 100),
    prob = probs,
    median_rho = apply(quantile_matrix, 2, median, na.rm = TRUE),
    q025 = apply(quantile_matrix, 2, quantile, 0.025, na.rm = TRUE),
    q975 = apply(quantile_matrix, 2, quantile, 0.975, na.rm = TRUE)
  )

  return(results)
}


#' Generate ellipse points for a bivariate normal with given correlation
#'
#' @param rho Correlation coefficient (-1 to 1)
#' @param prob Probability mass for HDR (default 0.5 for 50% contour)
#' @param n Number of points for ellipse smoothness
#' @return n × 2 matrix with columns "x" and "y"
#'
#' @details
#' Uses chi-square cutoff for 2D HDR: sqrt(qchisq(prob, df=2))
#' Ellipse is parameterized using eigendecomposition of Sigma = [[1, rho], [rho, 1]]
ellipse_from_rho <- function(rho, prob = 0.5, n = 361L) {
  # Clamp rho to avoid numerical issues
  rho <- pmax(pmin(rho, 0.9999), -0.9999)

  # Correlation matrix
  Sigma <- matrix(c(1, rho, rho, 1), 2, 2)

  # Chi-square radius for desired probability
  r <- sqrt(qchisq(prob, df = 2))

  # Parameterize circle
  theta <- seq(0, 2 * pi, length.out = n)
  U <- rbind(cos(theta), sin(theta))

  # Transform circle to ellipse via matrix square root
  e <- eigen(Sigma, symmetric = TRUE)
  A <- e$vectors %*% diag(sqrt(pmax(e$values, 0))) %*% t(e$vectors)

  XY <- t(A %*% (r * U))
  colnames(XY) <- c("x", "y")

  return(XY)
}


#' Create Saturn plot comparing all raters with quantile contours
#'
#' @param base_out_dir Base directory with rater subdirectories
#' @param year Survey year
#' @param prob Probability for HDR contour (default 0.5)
#' @param quantiles Quantile probabilities to plot (default: 0.25, 0.5, 0.75, 0.9)
#' @param highlight_gss Logical; emphasize GSS contour (default TRUE)
#' @param color_palette Named vector of colors for raters (optional)
#' @param save_pdf Logical; save plot to PDF (default TRUE)
#' @param output_file Output PDF filename (if save_pdf=TRUE)
#'
#' @return ggplot object (invisibly)
create_saturn_plot <- function(
    base_out_dir    = BASE_OUT_DIR,
    year            = YEAR,
    prob            = 0.5,
    quantiles       = c(0.25, 0.5, 0.75, 0.9),
    highlight_gss   = TRUE,
    color_palette   = NULL,
    save_pdf        = TRUE,
    output_file     = NULL
) {

  cat("\n=== Creating Saturn Plot (Quantile Contours) ===\n")
  cat("Probability contour:", prob, "\n")
  cat("Quantiles:", paste(quantiles, collapse = ", "), "\n")

  # Get all available raters
  raters <- available_raters(base_out_dir = base_out_dir, year = year)
  cat("Found", length(raters), "raters:", paste(raters, collapse = ", "), "\n")

  if (length(raters) == 0) {
    stop("No raters found in ", base_out_dir, "\n",
         "Expected directories like: <rater>-", year)
  }

  # Initialize storage
  mac_summary <- data.table(
    rater      = character(),
    median_mac = numeric(),
    mean_mac   = numeric(),
    sd_mac     = numeric(),
    q025_mac   = numeric(),
    q975_mac   = numeric()
  )

  quantile_summary <- list()
  ellipse_data <- list()

  # Process each rater
  for (rater in raters) {
    cat("Processing:", rater, "...")

    # Load correlation matrices
    corr_list <- tryCatch(
      load_corr_for_rater(
        rater        = rater,
        base_out_dir = base_out_dir,
        year         = year,
        strict       = FALSE
      ),
      error = function(e) {
        warning("Could not load ", rater, ": ", e$message)
        return(NULL)
      }
    )

    if (is.null(corr_list) || length(corr_list) == 0) {
      cat(" SKIP (no data)\n")
      next
    }

    # Compute MAC for each bootstrap
    mac_vec <- compute_mac_bootstraps(corr_list)
    mac_vec <- mac_vec[!is.na(mac_vec)]

    if (length(mac_vec) == 0) {
      cat(" SKIP (all NA)\n")
      next
    }

    # Summary statistics
    med_mac <- median(mac_vec, na.rm = TRUE)
    mean_mac <- mean(mac_vec, na.rm = TRUE)
    sd_mac <- sd(mac_vec, na.rm = TRUE)
    q025 <- quantile(mac_vec, 0.025, na.rm = TRUE)
    q975 <- quantile(mac_vec, 0.975, na.rm = TRUE)

    mac_summary <- rbindlist(list(
      mac_summary,
      data.table(
        rater      = rater,
        median_mac = med_mac,
        mean_mac   = mean_mac,
        sd_mac     = sd_mac,
        q025_mac   = q025,
        q975_mac   = q975
      )
    ))

    # Compute correlation quantiles across bootstrap distribution
    quant_stats <- compute_correlation_quantiles(corr_list, probs = quantiles)
    quant_stats$rater <- rater
    quantile_summary[[rater]] <- quant_stats

    # Generate ellipses for each quantile
    for (q in seq_len(nrow(quant_stats))) {
      rho_q <- quant_stats$median_rho[q]
      quantile_label <- quant_stats$quantile[q]

      ellipse_xy <- ellipse_from_rho(rho = rho_q, prob = prob, n = 361)
      ellipse_df <- data.frame(
        x        = ellipse_xy[, "x"],
        y        = ellipse_xy[, "y"],
        rater    = rater,
        quantile = quantile_label,
        prob_q   = quant_stats$prob[q],
        rho      = rho_q
      )

      ellipse_key <- paste(rater, quantile_label, sep = "_")
      ellipse_data[[ellipse_key]] <- ellipse_df
    }

    cat(sprintf(" MAC = %.3f [%.3f, %.3f]", med_mac, q025, q975))
    cat(sprintf(" | Q90 = %.3f, Q75 = %.3f, Q50 = %.3f, Q25 = %.3f\n",
                quant_stats$median_rho[quant_stats$prob == 0.9],
                quant_stats$median_rho[quant_stats$prob == 0.75],
                quant_stats$median_rho[quant_stats$prob == 0.5],
                quant_stats$median_rho[quant_stats$prob == 0.25]))
  }

  # Check if we got any data
  if (length(ellipse_data) == 0) {
    stop("No ellipse data created. All raters failed to load or had invalid data.\n",
         "Check that correlation matrices exist in: ", base_out_dir, "/<rater>-", year, "/polychor_bootstrap.rds")
  }

  # Combine all ellipse data
  ellipse_dt <- rbindlist(ellipse_data, use.names = TRUE)

  if (nrow(ellipse_dt) == 0) {
    stop("ellipse_dt has no rows. No valid data was created.")
  }

  # Order raters by median MAC (for legend)
  mac_summary <- mac_summary[order(-median_mac)]
  rater_order <- mac_summary$rater

  # Factor rater for consistent ordering
  ellipse_dt[, rater := factor(rater, levels = rater_order)]

  # Factor quantile for consistent ordering (Q90 > Q75 > Q50 > Q25)
  quantile_levels <- paste0("Q", sort(quantiles * 100, decreasing = TRUE))
  ellipse_dt[, quantile := factor(quantile, levels = quantile_levels)]

  # Identify GSS
  ellipse_dt[, is_gss := tolower(rater) == "gss"]

  # Set visual properties based on quantile
  # Q90 (outermost) = thickest/most opaque
  # Q25 (innermost) = thinnest/most transparent
  ellipse_dt[, lwd := fcase(
    quantile == "Q90", 1.2,
    quantile == "Q75", 0.9,
    quantile == "Q50", 0.7,
    quantile == "Q25", 0.5,
    default = 0.7
  )]

  # Base alpha values for LLMs (very transparent to highlight GSS)
  ellipse_dt[, alpha := fcase(
    quantile == "Q90", 0.4,
    quantile == "Q75", 0.3,
    quantile == "Q50", 0.2,
    quantile == "Q25", 0.15,
    default = 0.25
  )]

  # GSS gets full opacity regardless of quantile (stands out boldly)
  ellipse_dt[is_gss == TRUE, alpha := 1.0]
  ellipse_dt[is_gss == TRUE, lwd := lwd * 1.5]

  # Set up colors
  n_raters <- length(unique(ellipse_dt$rater))

  if (is.null(color_palette)) {
    # Default: rainbow colors, GSS in black
    base_colors <- rainbow(n_raters - 1, v = 0.7, s = 0.8)
    color_palette <- setNames(
      c("black", base_colors),
      c(rater_order[tolower(rater_order) == "gss"],
        rater_order[tolower(rater_order) != "gss"])
    )
  }

  # Create interaction variable for grouping (rater × quantile)
  ellipse_dt[, group_id := paste(rater, quantile, sep = "_")]

  # Create plot with multiple contours per rater
  p <- ggplot(ellipse_dt, aes(x = x, y = y, group = group_id, color = rater)) +
    geom_path(aes(size = lwd, alpha = alpha, linetype = quantile)) +
    scale_size_identity() +
    scale_alpha_identity() +
    scale_linetype_manual(
      values = c("Q90" = "solid", "Q75" = "solid", "Q50" = "dashed", "Q25" = "dotted"),
      name = "Quantile of |ρ|",
      labels = c("Q90" = "90th percentile",
                 "Q75" = "75th percentile",
                 "Q50" = "50th (median)",
                 "Q25" = "25th percentile")
    ) +
    scale_color_manual(
      values = color_palette,
      name = "Model"
    ) +
    coord_fixed(ratio = 1) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.3) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.3) +
    labs(
      title = sprintf("Saturn Plot: Constraint Distribution (%d%% Probability Contours)", prob * 100),
      subtitle = sprintf("Multiple Quantiles of |ρ| Across All Belief Pairs (%d)", year),
      x = "Latent Variable 1 (standardized)",
      y = "Latent Variable 2 (standardized)",
      caption = "Each model shows 4 contours: Q90 (outermost, tightest constraints), Q75, Q50 (median), Q25 (innermost, weakest constraints)"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "right",
      legend.box = "vertical",
      legend.text = element_text(size = 8),
      legend.title = element_text(size = 9, face = "bold"),
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 10, color = "gray30"),
      plot.caption = element_text(size = 8, color = "gray50", hjust = 0),
      panel.grid.minor = element_blank()
    ) +
    guides(
      color = guide_legend(order = 1, override.aes = list(linewidth = 1.5, alpha = 1)),
      linetype = guide_legend(order = 2, override.aes = list(linewidth = 1))
    )

  # Save plot
  if (save_pdf) {
    if (is.null(output_file)) {
      # Save in mvn_YEAR subdirectory to match other MVN visualizations
      mvn_dir <- file.path(BASE_VIZ_DIR, sprintf("mvn_%d", year))
      output_file <- file.path(mvn_dir, "saturn_plot.pdf")
    }

    dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)

    cat("\nSaving Saturn plot to:", output_file, "\n")
    ggsave(
      filename = output_file,
      plot     = p,
      width    = 12,
      height   = 9,
      device   = "pdf"
    )
  }

  # Save MAC summary
  mac_file <- file.path(base_out_dir, sprintf("mac_summary_%d.csv", year))
  fwrite(mac_summary, mac_file)
  cat("MAC summary saved to:", mac_file, "\n")

  # Save quantile summary
  quantile_dt <- rbindlist(quantile_summary)
  quant_file <- file.path(base_out_dir, sprintf("correlation_quantiles_%d.csv", year))
  fwrite(quantile_dt, quant_file)
  cat("Correlation quantiles saved to:", quant_file, "\n")

  # Print summary table
  cat("\n=== MAC Summary (sorted by constraint) ===\n")
  print(mac_summary)

  cat("\n=== Correlation Quantile Summary ===\n")
  print(quantile_dt[order(-median_rho)])

  cat("\n=== Saturn Plot Complete ===\n\n")

  invisible(p)
}


################################################################################
# MAIN EXECUTION
################################################################################

if (exists("BASE_OUT_DIR") && exists("YEAR")) {

  cat("\n")
  cat("=" , rep("=", 68), "\n", sep = "")
  cat("Saturn Plot: Visualizing Global Constraint via MAC\n")
  cat("=" , rep("=", 68), "\n", sep = "")

  # Create the Saturn plot
  saturn_plot <- create_saturn_plot(
    base_out_dir  = BASE_OUT_DIR,
    year          = YEAR,
    prob          = 0.5,
    highlight_gss = TRUE,
    save_pdf      = TRUE
  )

} else {
  message("BASE_OUT_DIR and YEAR must be defined to run Saturn plot")
}
