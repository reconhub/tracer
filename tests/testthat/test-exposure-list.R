context("Test exposure_list")

describe("exposure_list()", {
  it("fails if the argument is not a single epicontacts object", {
    expect_error(exposure_list(c("wat", "wat")))
  })
  it("fails if no date_of_onset is present", {
    outbreak <- outbreaks::mers_korea_2015
    expect_error(exposure_list(outbreak, date_onset_column = "date_of_onset"))
  })
  it("can use a custom onset column", {
    outbreak <- outbreaks::mers_korea_2015
    expect_silent(exposure_list(outbreak, date_onset_column = "dt_onset"))
  })
  it("stops if date_of_onset does not exists", {
    outbreak <- outbreaks::mers_korea_2015
    expect_error(exposure_list(outbreak, date_onset_column = "not_there"))
  })
  it("returns a list of exposures for each individual", {
    outbreak <- outbreaks::mers_korea_2015
    result <- exposure_list(outbreak, date_onset_column = "dt_onset")
    expect_equal(nrow(outbreak$linelist), length(result))
    expect_equal(0, length(result[[1]]))
    expect_equal(1, length(result[[2]]))
  })
})
