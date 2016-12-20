context("Test computation of group scores")


test_that("group_score gives expected errors", {
    skip_on_cran()

    w <- distcrete("gamma", 1L, w = 0, 10, 0.65)$d
    
    msg <- "All values in 'x' need to be finite, non-NA numbers."
    expect_error(group_score(NA, 2, 0, SI$d), msg)
    msg <- "'x' must contain at least one item."
    expect_error(group_score(integer(0), 2, 0, SI$d), msg)
    msg <- "'R' cannot be less than 0."
    expect_error(group_score(0, -1, 0, SI$d), msg)
    msg <- "'R' must be a finite number."
    expect_error(group_score(0, NA, 0, SI$d), msg)
    msg <- "'lambda' must be a finite number."
    expect_error(group_score(0, 2, -Inf, SI$d), msg)
    msg <- "'lambda' cannot be less than 0."
    expect_error(group_score(0, 2, -3, SI$d), msg)
    msg <- "'w' must be a function."
    expect_error(group_score(0, 2, 1.2, 1:10), msg)

})


test_that("group_score gives expected answers", {
    skip_on_cran()

    SI <- distcrete("gamma", 1L, w = 0, 10, 0.65)$d

    set.seed(1)
    x <- replicate(30, sample(0:30, sample(1:5), replace = TRUE))

    g <- group_score(x, 2, 3, SI)

    ## corner cases
    expect_equal(group_score(0, 0, 3, SI)(10), 0)


    ## same values whether t is atomic or not
    expect_equal(g(10), g(10:12)[1])


    ## group = sum of inviv
    dates <- 11:25
    list_f <- lapply(x, contact_score, 2, 3, SI)
    res_sum_indiv <- Reduce("+", lapply(list_f, function(f) f(dates)))
    expect_equal(g(dates), res_sum_indiv)
    
})



