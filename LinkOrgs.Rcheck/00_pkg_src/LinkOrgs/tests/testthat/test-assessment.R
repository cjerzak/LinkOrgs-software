# =============================================================================
# Performance Assessment Tests
# =============================================================================

test_that("Perfect matches return correct metrics", {
  df_x <- data.frame(
    id = 1:4,
    name = c("Apple", "Google", "Microsoft", "Amazon"),
    stringsAsFactors = FALSE
  )
  df_y <- data.frame(
    id = 1:4,
    org = c("Apple Inc", "Alphabet", "Microsoft Corp", "Amazon.com"),
    stringsAsFactors = FALSE
  )

  z_perfect <- data.frame(
    name = c("Apple", "Google", "Microsoft", "Amazon"),
    org = c("Apple Inc", "Alphabet", "Microsoft Corp", "Amazon.com"),
    stringsAsFactors = FALSE
  )
  z_true <- z_perfect

  result <- AssessMatchPerformance(
    x = df_x, y = df_y,
    z = z_perfect, z_true = z_true,
    by.x = "name", by.y = "org"
  )

  expect_equal(result["TruePositives"], c(TruePositives = 4))
  expect_equal(result["FalsePositives"], c(FalsePositives = 0))
  expect_equal(result["FalseNegatives"], c(FalseNegatives = 0))
})

test_that("Wrong matches return TP=0", {
  df_x <- data.frame(
    id = 1:4,
    name = c("Apple", "Google", "Microsoft", "Amazon"),
    stringsAsFactors = FALSE
  )
  df_y <- data.frame(
    id = 1:4,
    org = c("Apple Inc", "Alphabet", "Microsoft Corp", "Amazon.com"),
    stringsAsFactors = FALSE
  )

  z_perfect <- data.frame(
    name = c("Apple", "Google", "Microsoft", "Amazon"),
    org = c("Apple Inc", "Alphabet", "Microsoft Corp", "Amazon.com"),
    stringsAsFactors = FALSE
  )

  z_wrong <- data.frame(
    name = c("Apple", "Google"),
    org = c("Microsoft Corp", "Amazon.com"),  # Wrong pairings
    stringsAsFactors = FALSE
  )

  result <- AssessMatchPerformance(
    x = df_x, y = df_y,
    z = z_wrong, z_true = z_perfect,
    by.x = "name", by.y = "org"
  )

  expect_equal(result["TruePositives"], c(TruePositives = 0))
})

test_that("Partial matches return correct TP and FN", {
  df_x <- data.frame(
    id = 1:4,
    name = c("Apple", "Google", "Microsoft", "Amazon"),
    stringsAsFactors = FALSE
  )
  df_y <- data.frame(
    id = 1:4,
    org = c("Apple Inc", "Alphabet", "Microsoft Corp", "Amazon.com"),
    stringsAsFactors = FALSE
  )

  z_perfect <- data.frame(
    name = c("Apple", "Google", "Microsoft", "Amazon"),
    org = c("Apple Inc", "Alphabet", "Microsoft Corp", "Amazon.com"),
    stringsAsFactors = FALSE
  )

  z_partial <- data.frame(
    name = c("Apple", "Google"),
    org = c("Apple Inc", "Alphabet"),
    stringsAsFactors = FALSE
  )

  result <- AssessMatchPerformance(
    x = df_x, y = df_y,
    z = z_partial, z_true = z_perfect,
    by.x = "name", by.y = "org"
  )

  expect_equal(result["TruePositives"], c(TruePositives = 2))
  expect_equal(result["FalseNegatives"], c(FalseNegatives = 2))
})

test_that("TrueNegatives calculation is correct", {
  df_x <- data.frame(
    id = 1:4,
    name = c("Apple", "Google", "Microsoft", "Amazon"),
    stringsAsFactors = FALSE
  )
  df_y <- data.frame(
    id = 1:4,
    org = c("Apple Inc", "Alphabet", "Microsoft Corp", "Amazon.com"),
    stringsAsFactors = FALSE
  )

  z_perfect <- data.frame(
    name = c("Apple", "Google", "Microsoft", "Amazon"),
    org = c("Apple Inc", "Alphabet", "Microsoft Corp", "Amazon.com"),
    stringsAsFactors = FALSE
  )

  z_partial <- data.frame(
    name = c("Apple", "Google"),
    org = c("Apple Inc", "Alphabet"),
    stringsAsFactors = FALSE
  )

  result <- AssessMatchPerformance(
    x = df_x, y = df_y,
    z = z_partial, z_true = z_perfect,
    by.x = "name", by.y = "org"
  )

  total_pairs <- nrow(df_x) * nrow(df_y)
  expected_tn <- total_pairs - result["TruePositives"] - result["FalsePositives"] - result["FalseNegatives"]
  expect_equal(unname(result["TrueNegatives"]), unname(expected_tn))
})

test_that("Empty predicted matches return TP=0, FN=ground_truth_size", {
  df_x <- data.frame(
    id = 1:4,
    name = c("Apple", "Google", "Microsoft", "Amazon"),
    stringsAsFactors = FALSE
  )
  df_y <- data.frame(
    id = 1:4,
    org = c("Apple Inc", "Alphabet", "Microsoft Corp", "Amazon.com"),
    stringsAsFactors = FALSE
  )

  z_perfect <- data.frame(
    name = c("Apple", "Google", "Microsoft", "Amazon"),
    org = c("Apple Inc", "Alphabet", "Microsoft Corp", "Amazon.com"),
    stringsAsFactors = FALSE
  )

  z_empty <- data.frame(name = character(0), org = character(0), stringsAsFactors = FALSE)

  result <- AssessMatchPerformance(
    x = df_x, y = df_y,
    z = z_empty, z_true = z_perfect,
    by.x = "name", by.y = "org"
  )

  expect_equal(result["TruePositives"], c(TruePositives = 0))
  expect_equal(result["FalseNegatives"], c(FalseNegatives = 4))
})

test_that("Empty ground truth returns TP=0, FP=predicted_size", {
  df_x <- data.frame(
    id = 1:4,
    name = c("Apple", "Google", "Microsoft", "Amazon"),
    stringsAsFactors = FALSE
  )
  df_y <- data.frame(
    id = 1:4,
    org = c("Apple Inc", "Alphabet", "Microsoft Corp", "Amazon.com"),
    stringsAsFactors = FALSE
  )

  z_partial <- data.frame(
    name = c("Apple", "Google"),
    org = c("Apple Inc", "Alphabet"),
    stringsAsFactors = FALSE
  )

  z_true_empty <- data.frame(name = character(0), org = character(0), stringsAsFactors = FALSE)

  result <- AssessMatchPerformance(
    x = df_x, y = df_y,
    z = z_partial, z_true = z_true_empty,
    by.x = "name", by.y = "org"
  )

  expect_equal(result["TruePositives"], c(TruePositives = 0))
  expect_equal(result["FalsePositives"], c(FalsePositives = 2))
})
