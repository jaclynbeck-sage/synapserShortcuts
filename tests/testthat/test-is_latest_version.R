test_that("missing version in both `syn_id` and `version` throws an error", {
  expect_error(is_latest_version("syn123"),
               regexp = "A version must be specified")
})

test_that("non-numeric version in `syn_id` throws an error", {
  expect_error(is_latest_version("syn123.test"),
               regexp = "'test' is not a valid Synapse ID")

  expect_error(is_latest_version("syn123."),
               regexp = "'' is not a valid Synapse ID")
})

test_that("non-numeric `version` argument throws an error", {
  expect_error(is_latest_version("syn123", version = "test"),
               regexp = "`version` must be numeric")

  expect_error(is_latest_version("syn123", version = ""),
               regexp = "`version` must be numeric")
})

test_that("non-numeric version in `syn_id` throws an error even when `version` is specified", {
  expect_error(is_latest_version("syn123.test", version = 1),
               regexp = "'test' is not a valid Synapse ID")
})

test_that("non-numeric `version` argument throws an error even when a version is specified in `syn_id`", {
  expect_error(is_latest_version("syn123.1", version = "test"),
               regexp = "`version` must be numeric")
})

test_that("specifying conflicting versions in `syn_id` and `version` throws an error", {
  expect_error(is_latest_version("syn123.7", version = 3),
               regexp = "Conflicting values given for version")
})

# TODO any other tests require mocking synGet
#test_that("giving a larger version number than what exists on Synapse throws an error", {
#  expect_error(is_latest_version(??),
#               regexp = "'??' is not a valid version number")
#})
