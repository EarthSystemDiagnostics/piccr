test:
	- Rscript -e "library(testthat); test_dir('tests', reporter = 'summary')"
