#' Compare the order of the items in two vectors
#'
#' Run a rolling window from the start of both vectors and compute how many
#' items in each window of `one` are in the same window of `two`, and so on.
#' For each individual "hit", increment a counter by one.
#' Then, divide by the total hits by the total length of all windows.
#'
#' This function is symmetrical: if you swap `one` and `two`, the result is the
#' same.
#'
#' It is designed to compare two lists that have the same elements but in
#' different order. However, it is not a requirement.
#'
#' `length(one) == length(two)` MUST be true.
#'
#' @param one First vector to compare
#' @param two Second vector to compare
#' @param binsize The size of the rolling window. Defaults to 1
#'
#' @returns a value between 0 and 1, where 0 is perfect difference and 1 is
#'          perfect equality.
#'
#' @examples
#' window_compare_rank(c("a", "b", "c", "d"), c("d", "a", "c", "b"), binsize = 2)
#'
#' @author Hedmad
#' @export
window_compare_rank <- function(one, two, binsize = 1) {
  if (length(one) != length(two)) {
    stop(paste0("The two lists differ in length: ", length(one), " vs ", length(two)))
  }

  windows <- if (binsize > length(one)) {
    seq_along(one)
  } else {
    window_start <- 1
    window_stop <- binsize
    i <- 1
    ret <- list()
    while (window_stop <= length(one)) {
      ret[[i]] <- seq(from = window_start, to=window_stop, by = 1)
      window_start <- window_start + 1
      window_stop <- window_stop + 1
      i <- i + 1
    }

    ret
  }

  total_presences <- 0
  for (i in seq_along(windows)) {
    one_chunk <- one[windows[[i]]]
    two_chunk <- two[windows[[i]]]

    total_presences <- total_presences + sum(as.numeric(purrr::map(one_chunk, \(x) {x %in% two_chunk})))
  }

  n <- length(one)
  k <- binsize
  return(total_presences / (k * (n - k + 1)))
}


#' Compute the continuous congruency of two vectors
#'
#' This function calls `window_compare_rank` over and over with progressively
#' larger windows, from `1` to `length(one)` (the two vectors must be of the
#' same length), and returns a list of all of these values.
#'
#' One `window_compare_rank` is not especially interesting, but the distribution
#' of these values as the window increases is.
#'
#' @param one The first vector to compare
#' @param two The second vector to compare
#'
#' @returns A data.frame with the following columns:
#'          - `steps`: the window size of the calculated rank;
#'          - `step_fraction`: `size` but divided by the length of the vectors;
#'          - `value`: the value of `window_compare_rank` with those windows.
#'
#' @examples
#' continuous_congruency(c("a", "b", "c", "d"), c("d", "a", "c", "b"))
#'
#' @author Hedmad
#' @export
continuous_congruency <- function(one, two) {
  steps = seq(from = 1, to = length(one), by = 1)

  data.frame(
    steps = steps,
    step_fraction = steps / length(one),
    value = purrr::map_vec(steps, \(x) {window_compare_rank(one, two, binsize = x)})
  )
}

#' Plot the results of one or more continuous congruency comparisons
#'
#' The plot can show how similar each comparison is by drawing a line with the
#' value of the congruency metric in function of the width size.
#'
#' @param comparisons A dataframe with at least these columns:
#'                    - `steps`: the window size of the calculated rank;
#'                    - `step_fraction`: `size` but divided by the length of the vectors;
#'                    Each additional column is treated as a comparison, and will
#'                    be plotted with a label equal to the column name.
#' @param legend_scale The scale of the legend. With many comparisons, it might
#'                     be a good idea to make the legend smaller by reducing
#'                     this value. Defaults to 10.
#'
#' @author Hedmad
#' @export
plot_continuous_congruency <- function(comparisons, legend_scale = 10) {
  data <- reshape2::melt(
    comparisons,
    id.vars = c("steps", "step_fraction"),
    variable.name = "comparisons"
  )

  # Stolen from stackoverflow - don't judge me
  # the smaller this value is, the smaller the legend
  width_scale <- legend_scale
  p <- ggplot2::ggplot(data, ggplot2::aes(x = step_fraction, y = value, color = comparisons)) +
    ggplot2::geom_line() +
    ggplot2::ylab("Congruency") +
    ggplot2::xlab("Bin size (fraction of total)") +
    ggplot2::ggtitle("Congruency curves") +
    ggplot2::theme_minimal() +
    ggplot2::scale_color_hue(c = 150) +
    ggplot2::theme(
      legend.text=ggplot2::element_text(size=width_scale),
      legend.box.margin =  ggplot2::margin(6, 6, 6, 6),
      legend.title=ggplot2::element_text(size=1.5*width_scale,face="bold"),
      legend.position="bottom",
      legend.key.size = grid::unit(width_scale/50, "inch"),
      legend.key.width = grid::unit(width_scale/50, "inch")
    )

  p
}

#' Compare all ranks in a list taken two at a time
#'
#' All ranks must be of the same length. Their names in the list are the names
#' used in the output, with the pattern `one_vs_two`.
#'
#' @param ranks A named list with a vector in each slot, with a specific order.
#'
#' @returns A data.frame with the following columns:
#'          - `steps`: the window size of the calculated rank;
#'          - `step_fraction`: `size` but divided by the length of the vectors;
#'          Each additional column is a comparison, with the name
#'          `<one>_vs_<two>`, where `<one>` and `<two>` are the names of the
#'          two considered vectors in the `ranks` list.
#'
#' @author Hedmad
#' @export
compare_two_way_ranks <- function(ranks) {
  items <- names(ranks)
  pairs <- t(utils::combn(items, 2))

  l <- length(ranks[[1]])
  purrr::walk(ranks, \(x) {
    if (length(x) != l) {stop("All input lists are not equal in length")}
  })

  results <- list()
  for (i in seq(from = 1, to = nrow(pairs))) {
    first_label <- pairs[i, 1]
    second_label <- pairs[i, 2]
    new_name <- paste0(first_label, "_vs_", second_label)
    print(paste0("Comparing ", new_name))
    values <- continuous_congruency(ranks[[first_label]], ranks[[second_label]])

    results$steps <- values$steps
    results$step_fraction <- values$step_fraction
    results[[new_name]] <- values$value
  }

  as.data.frame(results)
}

#' Compute scores for two-way comparisons
#'
#' It can be useful to summarize to a single score the comparisons.
#' This can be done by computing the area under the curve, akin to what is done
#' with receiver operating characteristics curves.
#'
#' @param comparisons A data.frame with at least these columns:
#'                    - `steps`: the window size of the calculated rank;
#'                    - `step_fraction`: `size` but divided by the length of the vectors;
#'                    Each additional column is treated as a comparison
#' @returns A list with one slot per additional column in the input with the value
#'          of the computed metric for that comparison.
#'
#' @author Hedmad
#'
#' @export
score_comparisons <- function(comparisons) {
  steps <- comparisons$step_fraction
  stripped <- comparisons[, ! names(comparisons) %in% c("steps", "step_fraction"), drop=FALSE]

  results <- list()
  for (col in names(stripped)) {
    results[[col]] <- (sum(steps * stripped[[col]]) / sum(steps))
  }

  results
}

#' Compute a p-value of the difference in ordering between two vectors a and b
#'
#' The test involves permuting `b` a number `perms` times, and then computing
#' the score for each comparison.
#' The final p-value is the fraction of permutation scores that are greater
#' than the true score divided by `perms`.
#'
#' This is incredibly expensive, so it will take a long, long time to compute
#' for long vectors and many permutations.
#'
#' The `a` and `b` vectors must have the same length.
#'
#' @param a A vector to compare.
#' @param b Another vector to compare with.
#' @param perms The number of permutations to compute. Defaults to 1000.
#'
#' @returns A p-value between 0 and 1.
#'
#' @author Hedmad
#' @export
test_comparisons <- function(a, b, perms = 1000) {
  shuffle <- function(x) {sample(x, length(x))}

  original <- unlist(score_comparisons(continuous_congruency(a, b)))
  permutations <- c()
  pb <- progress::progress_bar$new(total = perms)
  for (i in seq_len(perms)) {
    pb$tick()
    permutations <- c(
      permutations,
      unlist(score_comparisons(
        continuous_congruency(a, shuffle(b))
      )
      ))
  }

  sum(permutations > original) / length(permutations)
}
