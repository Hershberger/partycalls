load("inst/extdata/houKHfiles001-111.rdata")
load("inst/extdata/senate93-112.RData")
h112 <- pscl::readKH("inst/extdata/hou112kh.ord")
devtools::use_data(
  h093, h094, h095, h096, h097, h098, h099, h100,
  h101, h102, h103, h104, h105, h106, h107, h108,
  h109, h110, h111, h112, sen93, sen94, sen95, sen96, sen97, sen98,
  sen99, sen100, sen101, sen102, sen103, sen104, sen105,
  sen106, sen107, sen108, sen109, sen110, sen111, sen112,
  overwrite = TRUE)
devtools::document()
devtools::build()
devtools::install()
