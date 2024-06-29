test_that("pDistMatch_euclidean works", {
    x_orgnames <- c("apple","oracle","enron inc.","mcdonalds corporation")
    y_orgnames <- c("apple corp","oracle inc","enron","mcdonalds co")
    x <- data.frame("orgnames_x"=x_orgnames)

    for (i in 1:10) {
        set.seed(i)
        embed_x <- matrix(rnorm(4*100), 4,100)
        embed_y <- embed_x + matrix(rnorm(4*100, 0,.1), 4,100)

        y <- data.frame("orgnames_y"=y_orgnames)
        z <- data.frame("orgnames_x"=x_orgnames[1:2], "orgnames_y"=y_orgnames[1:2])
        z_true <- data.frame("orgname_x"=x_orgnames, "orgnames_y"=y_orgnames)

        # Test that we can succesfully compare every pair of records
        every_pair <- pDistMatch_euclidean(embed_x, embed_y, MaxDist = Inf, ReturnProgress=F)

        expect_equal(nrow(every_pair), 16)

        # Test that when the threshold is set at a reasonable level, we only
        # find matching records
        matching_pairs <- pDistMatch_euclidean(embed_x, embed_y, MaxDist = 5, ReturnProgress=F)
        expect_equal(matching_pairs[,1],matching_pairs[,2])
    }

})
