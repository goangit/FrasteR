FrasteR
==========

Template for Fortran inter-operation from R, emphasising raster data.

Simple cases of calling Fortran subroutines from R are provided in Simple.R / Simple.f90. These provide examples of passing vector, matrix and array data to and from Fortran subroutines. A RasterLayer object is decomposed using values() and reconstructed post-call. Those not familiar with data marshalling between the two languages may benefit from familiarity with these simple cases. 

A more realistic raster-based example will be provided in FrasteR.R / FrasteR.f90
