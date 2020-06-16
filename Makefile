test:
	- Rscript -e "devtools::test()"

coverage:
	- Rscript -e "covr::package_coverage()"
