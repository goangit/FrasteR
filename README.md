FrasteR
==========

Template for Fortran inter-operation from R, using raster data.

Simple cases of calling Fortran subroutines from R are provided in
Simple.R / Simple.f90 / test.Simple.R. Those not familiar with data
marshalling between the two languages may benefit from familiarity
with these simple cases. Examples of passing vector, matrix and array
data to and from Fortran subroutines are provided, as is an example
where a RasterLayer object is decomposed, passed and reconstructed
post-call.

Unit tests are provided at inst/tests using the testthat
framework. Automated test runs are available via the top-level
makefile. From FrasteR/:

make autotests builds a local copy of the package and runs the test suite

make clean restores the file tree to a state suitable for R CMD check.

Further examples using RasterBrick objects will be provided in
FrasteR.R / FrasteR.f90. The use-case envisioned is one where some
iterative calculation must be performed across all layers in each cell
of the RasterBrick. Each cell represents a vector case which may be
treated independently.
