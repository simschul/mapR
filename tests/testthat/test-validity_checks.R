# # load data
# bin_mat_wide <- readRDS(system.file('testdata', 'CT_bin_example.RData', package = 'mapR'))
# num_mat_wide <- readRDS(system.file('testdata', 'CT_num_example.RData', package = 'mapR'))
#
# bin_dt_wide <- as.data.table(bin_mat_wide, keep.rownames = 'UNFCCC_code')
# num_dt_wide <- as.data.table(num_mat_wide, keep.rownames = 'UNFCCC_code')
#
# bin_long <- CT_to_long(bin_mat_wide)
# num_long <- CT_to_long(num_mat_wide)
#
#
# test_that('regocinition works', {
#   expect_true(is_CT_wide(bin_mat_wide))
#   expect_true(is_CT_wide(num_mat_wide))
#   expect_true(is_CT_wide(bin_dt_wide))
#   expect_true(is_CT_wide(num_dt_wide))
# })
#
