#' Find the Optimal Hyperparameters for Recovering Neighbors Using Locality
#' Sensitive Hashing
#'
#' Runs a genetic algorithm to find the hyperparameters for LSH that minimize
#' runtime while coming close to achieving an (d1,d2,p1,p2)-sensitive hash. A
#' locality sensitive hash can be called (d1,d2,p1,p2)-sensitive iff two
#' vectors with distance greater than d1 have at most a p1 chance of being
#' compared, while two vectors with distance less than d2 have a greater than
#' p2 chance of being compared. As an example, a (5,1,.001,.999)-sensitive LSH
#' means that vectors with distance greater than 5 will have a .1% chance of
#' being compared, while vectors with distance less than 1 from each other have
#' a 99.9% chance of being compared.
#'
#' @param d1  the d1 parameter (the first distance).
#' @param d2  the d2 parameter (the second distance, must be less than than d1).
#' @param p1  the p1 parameter (the first probability).
#' @param p2  the p2 parameter (the second probability, must be less than than
#' p1).
#' @param pop_size  population size parameter passed to the `rgenoud` function
#' @param wait_generations  number of generations after which the search
#' algorithm should terminate if no improvement has been found. Passed to the
#' `genoud` function.
#'
#' @param print.level the degree to which optimization progress should be
#' reported. From the greound documentation: 'There are four possible levels: 0
#' (minimal printing), 1 (normal), 2 (detailed), and 3 (debug).' This argument
#' is passed directly to the `genoud` function.
#'
#' @return a named list with the hyperparameters that will meet the LSH
#' criteria, while reducing runtime.
#'
#' @example
#' euclidean_hyper_search(5,1,.001,.999)
euclidean_hyper_search <- function(d1, d2, p1, p2,
                                   pop_size = 10^3,
                                   wait_generations = 50,
                                   print.level = 0
                                   ) {
  stopifnot("probability 1 must be less than probability 2" = p1 < p2)
  stopifnot("d1 must be greater than d2" = d1 > d2)
  stopifnot("probability  must be a valid probability" = 0 < p1 & p1 < 1)
  stopifnot("probability  must be a valid probability" = 0 < p2 & p2 < 1)
  stopifnot("distances must be greater than zero" = d1 > 0 & d2 > 0)

  score_hyperparameter_config <- function(x) {
    n_bands <- x[1]
    band_width <- x[2]
    r <- x[3]

    p1_hat <- zoomerjoin::euclidean_probability(d1, n_bands, band_width, r)
    p2_hat <- zoomerjoin::euclidean_probability(d2, n_bands, band_width, r)

    time <- .0001 * n_bands * band_width

    constraints <- abs(pmin(p1 - p1_hat, 0)) + abs(pmin(p2_hat - p2, 0))
    constraints <- constraints * (-1) * (10^4)

    return(time - constraints)
  }

  x <- rgenoud::genoud(
    score_hyperparameter_config,
    nvars = 3,
    max = FALSE,
    Domains = matrix(
      c(1, 2000, 1, 500, 0, d1),
      ncol = 2,
      byrow = TRUE
    ),
    pop.size = pop_size,
    wait.generations = wait_generations,
    solution.tolerance = 1,
    print.level = print.level
  )

  x <- x$par
  x <- list(
    n_bands = round(x[[1]]),
    band_width = round(x[[2]]),
    r = x[[3]]
  )

  return(x)
}
