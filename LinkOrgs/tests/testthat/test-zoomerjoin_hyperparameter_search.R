test_that("euclidean_hyper_search validates input", {

    # Tests that code ensures that distances and probabilities are given in
    # the correct order

    expect_error(euclidean_hyper_search(1,1,.9,.1), "must be less")
    expect_error(euclidean_hyper_search(1,1,.1,.9), "must be greater")

    # Tests to ensure code will raise an error if invalid probabilities are
    # given
    expect_error(euclidean_hyper_search(5,1,-.1,.9), "valid probability")
    expect_error(euclidean_hyper_search(5,1,.1,1.9), "valid probability")


    # Tests that distances are valid (greater than 0)
    expect_error(euclidean_hyper_search(-2,-5,.1,.9), "than zero")
    expect_error(euclidean_hyper_search(2,-2,.1,.9), "than zero")
})
