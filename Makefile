
local-install:
	rm -rf .local
	mkdir .local
	R CMD INSTALL --library=.local .

autotest: local-install

	R -q -e "library(FrasteR, lib.loc = '.local')" \
	   -e "library(testthat)" \
	   -e "auto_test('./R', './inst/tests', 'summary')"

clean:
	rm -rf .local
	find .   -name .Rhistory -exec rm {} \;
	find src -name *.o  -exec rm {} \;
	find src -name *.so -exec rm {} \;

## ----- eof ------------------------------------------------------------------

