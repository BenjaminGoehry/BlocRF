# devtools history
usethis::use_build_ignore("devtools_history.R")

usethis::use_vignette('ranger')
devtools::build(pkg = '.', vignettes = T)