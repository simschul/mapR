# load data
bin_mat_wide <- readRDS(system.file('testdata', 'CT_bin_example.RData', package = 'mapR'))
num_mat_wide <- readRDS(system.file('testdata', 'CT_num_example.RData', package = 'mapR'))

bin_dt_wide <- as.data.table(bin_mat_wide, keep.rownames = 'UNFCCC_code')
num_dt_wide <- as.data.table(num_mat_wide, keep.rownames = 'UNFCCC_code')


test_that("reshaping binary CT works", {
  expect_equal(
    CT_to_wide(CT_to_long(bin_mat_wide), return = 'matrix'),
    bin_mat_wide
    )

  expect_equal(
    CT_to_wide(CT_to_long(bin_dt_wide, target_name = 'NACE_code'),
               return = 'data.table'),
    bin_dt_wide
  )
})


test_that("reshaping numeric CT works", {

  expect_equal(
    CT_to_wide(CT_to_long(num_mat_wide), return = 'matrix'),
    num_mat_wide
  )

  expect_equal(
    CT_to_wide(CT_to_long(num_dt_wide, target_name = 'NACE_code'),
               return = 'data.table'),
    num_dt_wide
  )
})


num_long1 <- CT_to_long(num_dt_wide)
num_long2 <- copy(num_long1)
num_long2[, c('a','b') := list(runif(.N), sample(LETTERS, .N, TRUE))]

test_that('reshaping from long to wide works also for more than 3 columns CTs', {
  expect_equal(
    CT_to_wide(num_long1),
    CT_to_wide(num_long2)
  )
})

