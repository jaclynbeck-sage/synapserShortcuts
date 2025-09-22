test_that("using a non-existent file throws an error", {
  expect_error(safe_synStore("not_a_file.txt", parent_id = "syn123"),
               regexp = "'not_a_file.txt' does not exist")
})

test_that("NULL parent_id throws an error", {
  filename <- test_path("fixtures", "upload_test_file.txt")
  expect_error(safe_synStore(filename, parent_id = NULL),
               regexp = "`parent_id` can not be NULL")
})

test_that("Invalid or empty parent_id throws an error", {
  filename <- test_path("fixtures", "upload_test_file.txt")
  expect_error(safe_synStore(filename, parent_id = "bad_id"),
               regexp = "'bad_id' is not a valid Synapse folder ID")

  expect_error(safe_synStore(filename, parent_id = ""),
               regexp = "'' is not a valid Synapse folder ID")
})

# TODO other tests require mocking synStore, synFindEntityId, and synGet
