brick_add <- function(a,b) {

    stopifnot(require(raster))

    stopifnot(class(a)=='RasterBrick' | class(a)=='RasterStack')
    stopifnot(class(b)=='RasterBrick' | class(b)=='RasterStack')

    va <- values(a); stopifnot(all(!is.na(va)))
    vb <- values(b); stopifnot(all(!is.na(vb)))

    stopifnot(all(dim(va)==dim(vb)))

    fcall = .Fortran('f_matrix_add', PACKAGE='FrasteR', NAOK=FALSE, DUP=TRUE,
        m = as.integer(nrow(va)),
        n = as.integer(ncol(va)),
        a = as.matrix(va),
        b = as.matrix(vb),
        c = matrix(0, nrow=nrow(va), ncol=ncol(va)))

    c <- brick(extent(a), nrow=nrow(a), ncol=ncol(a), nl=nlayers(a))
    c <- setValues(c, fcall$c)

    return(c)
}


array_add <- function(a,b) {

    stopifnot(class(a)=='array')
    stopifnot(class(b)=='array')
    stopifnot(all(dim(a)==dim(b)))

    fcall = .Fortran('f_array_add', PACKAGE='FrasteR', NAOK=FALSE, DUP=TRUE,
        p = as.integer(dim(a)[1]),
        q = as.integer(dim(a)[2]),
        r = as.integer(dim(a)[3]),
        a = a,
        b = b,
        c = array(0, dim(a)))

  return(fcall$c)
}



matrix_add <- function(a,b) {

    stopifnot(class(a)=='matrix')
    stopifnot(class(b)=='matrix')
    stopifnot(all(dim(a)==dim(b)))

    fcall = .Fortran('f_matrix_add', PACKAGE='FrasteR', NAOK=FALSE, DUP=TRUE,
        m = as.integer(nrow(a)),
        n = as.integer(ncol(a)),
        a = as.matrix(a),
        b = as.matrix(b),
        c = matrix(0, nrow=nrow(a), ncol=ncol(a)))

  return(fcall$c)
}


vector_add <- function(a,b) {

    stopifnot(class(a)=='numeric')
    stopifnot(class(b)=='numeric')
    stopifnot(length(a)==length(b))

    fcall = .Fortran('f_vector_add', PACKAGE='FrasteR', NAOK=FALSE, DUP=TRUE,
        n = as.integer(length(a)),
        a = as.numeric(a),
        b = as.numeric(b),
        c = numeric(length(a)))

  return(fcall$c)
}


## ----- test -----------------------------------------------------------------

pd <- '/home/greg/src/rpkg/ftest'
solib <- file.path(pd,'src/ftest.so')

if (file.exists(solib)) {
    if (is.loaded('f_matrix_add')) dyn.unload(solib)
    dyn.load(solib)
}

is.loaded('f_matrix_add')

library(raster)
r <- raster(matrix(1:12, nrow=4, ncol=3))
r <- brick(stack(r, r*2, r*3))
s <- r + 1

brick_add(r,s)


if (file.exists(solib)) {
    if (is.loaded('f_array_add')) dyn.unload(solib)
    dyn.load(solib)
}

a <- array(as.double(1:18), c(3,2,3))
b <- array(as.double(2), c(3,2,3))

array_add(a,b)


if (file.exists(solib)) {
    if (is.loaded('f_matrix_add')) dyn.unload(solib)
    dyn.load(solib)
}

## nb: matrix_add requires integers to be stored as doubles
## otherwise the element boundaries will be misplaced and results wrong
a <- matrix(as.double(1:96),  nrow=12, ncol=8)
b <- a + 96

matrix_add(a,b)


if (file.exists(solib)) {
    if (is.loaded('f_vector_add')) dyn.unload(solib)
    dyn.load(solib)
}

p <- c(2.3, 8.4, 1.2)
q <- c(1.1, 0.4, 0.2)

vector_add(p,q)


## ----- eof ------------------------------------------------------------------
