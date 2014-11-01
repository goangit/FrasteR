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


raster_add <- function(a,b) {

    stopifnot( class(a) == 'RasterLayer' & class(b) == 'RasterLayer' )
    stopifnot( extent(a) == extent(b) )
    stopifnot( nrow(a) == nrow(b) )
    stopifnot( ncol(a) == ncol(b) )

    va <- values(a) ## row-major order
    vb <- values(b)

    c <- raster( matrix( vector_add(va,vb), ncol=ncol(a), byrow=TRUE) )

    return(c)
}


vector_add <- function(a,b) {

    stopifnot( is.numeric(a) & is.numeric(b) )
    stopifnot( length(a) == length(b) )
    stopifnot( all( !is.na(a) ) & all( !is.na(b) ) )

    fcall = .Fortran('f_vector_add', PACKAGE='FrasteR', NAOK=FALSE, DUP=TRUE,
        n = as.integer(length(a)),
        a = as.numeric(a),
        b = as.numeric(b),
        c = numeric(length(a)))

  return(fcall$c)
}


## Simple.R ends here
