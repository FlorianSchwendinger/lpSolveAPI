pkg <- "lpSolveAPI"
check_dir <- "Rcheck-all"
dir.create(check_dir)
file.copy(dir(pattern = sprintf("%s_.*.tar.gz", pkg)), check_dir)
result <- tools::check_packages_in_dir(check_dir, reverse = TRUE, clean = FALSE)
summarize_check_packages_in_dir_results(check_dir)

library("tools")

check_logs <- file.path(dir(check_dir, pattern = "Rcheck", full.names = TRUE), "00check.log")
check_logs <- lapply(check_logs, readLines)
xpkgs <- grep("but not available:", unlist(check_logs), value = TRUE)
xpkgs <- unlist(strsplit(gsub(".*:", "", xpkgs), ","))
install.packages(trimws(gsub("[‘’']", "", xpkgs)))



install.packages("ape")
install.packages("MLmetrics")
install.packages("BiocManager")
BiocManager::install("lpsymphony")
install.packages("MultiplierDEA")
install.packages("oppr")

reverse_dependencies <- devtools::revdep()
pkgs_avail <- rownames(installed.packages())
(inst_us <- setdiff(reverse_dependencies, pkgs_avail))
install.packages(inst_us)


args(tools::check_packages_in_dir)

