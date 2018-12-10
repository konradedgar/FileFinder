context("Most Recent File")

test_that(
    desc = "Wrong path returns error",
    code = expect_error(object = most_recent_file(path = "wrong path"),
                        regexp = "Assertion on \\'path\\' failed")
)

test_that(desc = "vector is of correct length",
          code = expect_length(object = most_recent_file(
              path = Sys.getenv("R_DOC_DIR"),
              n = 1
          ),
          n = 1))
