# workflow for initiating new R package

# Install first necessary packages
install.packages(c('pak', 'job'))

# function to speed up package installation
install_packages <- function(pkgs) {
  job::job({
    pak::pkg_install(pkgs)
  })
}

install_packages(c('testthat',
                   'usethis'))
install_packages(c('data.table'))

library(usethis)
library(devtools)

# begin set up of package
use_git()
use_github()
use_readme_rmd()
use_testthat()
use_ccby_license()

use_test()

#
load_all()






