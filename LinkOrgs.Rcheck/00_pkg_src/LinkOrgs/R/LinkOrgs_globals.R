#' @importFrom stats na.omit
NULL

# Global variable declarations to avoid R CMD check NOTEs
# These are non-standard evaluation (NSE) variables used in data.table, dplyr, foreach, and reticulate

utils::globalVariables(c(
  # data.table NSE variables
  "the.row",
  "trigram",
  "phrase.no",
  "start_pos",
  "end_pos",
  "unique_x",

  # foreach NSE variable
  "outer_ix",

  # dplyr pipe operator
  "%>%",

  # Python objects from reticulate (defined at runtime)
  "jax",
  "jnp",
  "np",
  "eq",
  "opt_state",
  "my_data",

  # Functions defined dynamically in Python/network code
  "BuildTransfer",
  "GetAliasRep_BigBatch",
  "getRepresentation_transfer"
))
