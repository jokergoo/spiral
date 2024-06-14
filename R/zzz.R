.onAttach = function(libname, pkgname) {
    version = utils::packageDescription(pkgname, fields = "Version")

  	msg = paste0("========================================
", pkgname, " version ", version, "
Github page: https://github.com/jokergoo/spiralize
CRAN page: https://CRAN.R-project.org/package=spiralize

If you use it in published research, please cite:
Gu, Z. spiralize: an R package for Visualizing Data on Spirals. 
  Bioinformatics 2021.

This message can be suppressed by:
  suppressPackageStartupMessages(library(spiralize))
========================================
")	

    packageStartupMessage(msg)
}
