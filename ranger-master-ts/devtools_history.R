# devtools history
usethis::use_build_ignore("devtools_history.R")
usethis::use_build_ignore("appveyor.yml")

usethis::use_vignette('rangerts')
devtools::build(pkg = '.', vignettes = T)
