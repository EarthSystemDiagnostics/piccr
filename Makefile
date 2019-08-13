test:
	- Rscript -e "devtools::load_all(); testthat::test_dir('tests', reporter = 'summary')"
