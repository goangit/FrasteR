FrasteR
==========

Templates for Fortran inter-operation from R, using raster data.

Users new to R-Fortran inter-operation or those returning to it intermittently can easily forget details of indexing and data marshalling. This repository provides a set of working examples to lower the setup overhead of getting your R-Fortran project running. The aim is to provide boilerplate code for handling inter-operation details so that users can immediately focus on Fortran solutions. Fortran programmers wishing to take advantage of R as an interface to various data types may also benefit. The routines provided examine trivial cases but should be easy to extend. Automated unit tests are provided for all routines via a makefile. 

Simple cases of calling Fortran subroutines from R are provided in Simple.R/f90. These examples demonstrate vector, matrix and array
data passing to and from Fortran subroutines. The unit tests provide simple use cases which may be used as a tutorial. Once the basics of those operations are understood, working with Raster\* objects is easy. Final examples demonstrate cases where RasterLayer and RasterBrick objects are decomposed, passed to Fortran and reconstructed as Raster* objects on return.

Each of the examples mentioned to this point duplicate functionality already available in R or the raster package. Typically users will be interested in cases where Fortran calls provide functionality which is not otherwise available. Some suggestions for seeding such routines, with an emphasis on Raster\* objects, will be provided in FrasteR.R/f90.  

Unit tests are provided at inst/tests using the testthat framework. Testing can be automated using the makefile. 

* `make autotest` builds a local copy of the package and runs the test suite

* `make clean`: restores the file tree to a state suitable for `R CMD check`.

It is possible that the makefile will be extended to include a target which appropriately purge/rename the src tree to facilitate new project development. 

[Fortran Best Practices](www.fortran90.org/src/best-practices.html) is a useful resource.




