
build:
	R CMD build .

inst: build
	R CMD INSTALL lpSolveAPI*.tar.gz
	
check: build
	R CMD check lpSolveAPI*.tar.gz

manual: clean
	R CMD Rd2pdf --output=Manual.pdf .

clean:
	rm -f Manual.pdf README.knit.md README.html
	rm -rf .Rd2pdf*


check_mac_m1: build
	R -e "rhub::check(dir(pattern = 'lpSolveAPI_.*.tar.gz'), platform = 'macos-m1-bigsur-release')"

check_mac_old: build
	R -e "rhub::check(dir(pattern = 'lpSolveAPI_.*.tar.gz'), platform = 'macos-highsierra-release-cran')"

check_gcc_san: build
	R -e "rhub::check(dir(pattern = 'lpSolveAPI_.*.tar.gz'), platform = 'linux-x86_64-rocker-gcc-san')"

check_debian_clang: build
	R -e "rhub::check(dir(pattern = 'lpSolveAPI_.*.tar.gz'), platform = 'debian-clang-devel')"

check_fedora_gcc: build
	R -e "rhub::check(dir(pattern = 'lpSolveAPI_.*.tar.gz'), platform = 'fedora-gcc-devel')"

check_fedora_clang: build
	R -e "rhub::check(dir(pattern = 'lpSolveAPI_.*.tar.gz'), platform = 'fedora-clang-devel')"

devcheck_win_release: build
	R -e "devtools::check_win_release(email = 'FlorianSchwendinger@gmx.at')"

devcheck_win_devel: build
	R -e "devtools::check_win_devel(email = 'FlorianSchwendinger@gmx.at')"

