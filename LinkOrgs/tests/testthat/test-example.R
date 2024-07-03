test_that("fuzzy matching example works", {
    for (i in 1:5) {
    x <- data.frame("orgnames_x" = x_orgnames <- c("apple","oracle",
                                                 "enron inc.","mcdonalds corporation"))
    y <- data.frame("orgnames_y"= y_orgnames <- c("apple corp","oracle inc",
                                                "enron","mcdonalds co"))
    LinkedOrgs_fuzzy <- LinkOrgs(x = x, by.x = "orgnames_x",
                               y = y, by.y = "orgnames_y",
                               algorithm = "fuzzy")

    expect_equal(nrow(LinkedOrgs_fuzzy), 7)

    }

})
