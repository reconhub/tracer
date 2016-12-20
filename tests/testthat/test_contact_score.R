context("Test computation of contact scores")


test_that("contact_score gives expected errors", {
    skip_on_cran()

    w <- distcrete("gamma", 1L, w = 0, 10, 0.65)$d
    
    msg <- "All values in 'x' need to be finite, non-NA numbers."
    expect_error(contact_score(NA, 2, 0, SI$d), msg)
    msg <- "'x' must have at least one value."
    expect_error(contact_score(integer(0), 2, 0, SI$d), msg)
    msg <- "'R' cannot be less than 0."
    expect_error(contact_score(0, -1, 0, SI$d), msg)
    msg <- "'R' must be a finite number."
    expect_error(contact_score(0, NA, 0, SI$d), msg)
    msg <- "'lambda' must be a finite number."
    expect_error(contact_score(0, 2, -Inf, SI$d), msg)
    msg <- "'lambda' cannot be less than 0."
    expect_error(contact_score(0, 2, -3, SI$d), msg)
    msg <- "'w' must be a function."
    expect_error(contact_score(0, 2, 1.2, 1:10), msg)
})


test_that("contact_score gives expected answers", {
    skip_on_cran()

    SI <- distcrete("gamma", 1L, w = 0, 10, 0.65)$d

    f <- contact_score(0, 2, 3, SI)

    ## corner cases
    expect_equal(contact_score(0, 0, 3, SI)(10), 0)
})





