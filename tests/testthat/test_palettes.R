context("Test color palettes")


test_that("contact_score gives expected errors", {
    skip_on_cran()

    msg <- "'n' is not a number."
    expect_error(pal1("red"), msg)
    expect_equal_to_reference(pal1(100), file="rds/pal1.rds")
})
