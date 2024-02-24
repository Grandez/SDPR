
test_that("Logger Default values", {
   logger = SDPLogger$new()
   expect_equal(logger$level,  0)
   expect_equal(logger$output, 0)
})
test_that("Logger Environment", {
   Sys.setenv(TEST_LOG_LEVEL=5)
   Sys.setenv(TEST_LOG_OUTPUT=3)
   logger = SDPLogger$new(envvars="TEST")
   expect_equal(logger$level,  5)
   expect_equal(logger$output, 3)
})
test_that("Logger Environment overrrided", {
   Sys.setenv(TEST_LOG_LEVEL=5)
   Sys.setenv(TEST_LOG_OUTPUT=3)
   logger = SDPLogger$new(envvars="TEST", level=1, output=2)
   expect_equal(logger$level,  1)
   expect_equal(logger$output, 2)
})
