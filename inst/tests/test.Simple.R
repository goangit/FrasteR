## ----- reference list -------------------------------------------------------

## expect_that(x, is_true())           expect_true(x)
## expect_that(x, is_false())          expect_false(x)
## expect_that(x, is_a(y))             expect_is(x, y)
## expect_that(x, equals(y))           expect_equal(x, y)
## expect_that(x, is_equivalent_to(y)) expect_equivalent(x, y)
## expect_that(x, is_identical_to(y))  expect_identical(x, y)
## expect_that(x, matches(y))          expect_matches(x, y)
## expect_that(x, prints_text(y))      expect_output(x, y)
## expect_that(x, shows_message(y))    expect_message(x, y)
## expect_that(x, gives_warning(y))    expect_warning(x, y)
## expect_that(x, throws_error(y))     expect_error(x, y)

## ----- fn name --------------------------------------------------------------

## test template

context('Vector')

test_that('vector_add(p,q)', {

    ## Handles Integer and Real inputs appropriately

    expect_true( class(1:3) == 'integer' )
    expect_false( is.double(1:3) )

    expect_true( class( c(1,2,3) ) == 'numeric' )
    expect_true( is.double( c(1,2,3) ))

    p <- c(1,2,3)
    q <- p
    r <- p + q

    expect_true( all( vector_add(p,q) == r ) )

    p <- 1:3
    q <- p
    r <- p + q

    expect_true( is.integer(p) )

    ## vector is double precision in Simple.f90
    ## but as.numeric() wrapper in vector_add()
    expect_true( all( vector_add(p,q) == r ) )
})

context('Matrix')

test_that('matrix_add(p,q)', {

    v <- 1:12

    expect_true( is.integer(v) )

    p <- matrix(v, ncol=3, nrow=4)
    q <- p
    r <- p + q

    ## matrix is double precision in Simple.f90
    expect_false( all( matrix_add(p,q) == r ) )

    v <- as.numeric(v) ## default double precision
    p <- matrix(v, ncol=3, nrow=4)
    q <- p
    r <- p + q

    expect_true( is.double(v) )
    expect_true( all( matrix_add(p,q) == r ) )

})


context('Array ')

test_that('array_add(p,q)', {

    v <- 1:24

    expect_true( is.integer(v) )

    p <- array(v, c(3,2,4))
    q <- p
    r <- p + q

    ## array is double precision in Simple.f90
    expect_false( all( array_add(p,q) == r ) )

    v <- as.numeric(v) ## default double precision
    p <- array(v, c(3,2,4))
    q <- p
    r <- p + q

    expect_true( is.double(v) )
    expect_true( all( array_add(p,q) == r ) )

})


context('Raster')

test_that('raster_add(p,q)', {

    v <- 1:12

    expect_true( is.integer(v) )

    p <- raster(matrix(v, ncol=3, nrow=4))
    q <- p
    r <- p + q
    s <- raster_add(p,q)

    expect_true( class(p)  == 'RasterLayer' )
    expect_true( class(s)  == class(p) )
    expect_true( nrow(s)   == nrow(p) )
    expect_true( ncol(s)   == ncol(p) )
    expect_true( extent(s) == extent(p) )

    ## raster_add() wrapper for vector_add()
    ## integer values wrapped as numeric
    expect_true( all( values( s == r ) ) )
})


## context('RasterBrick')

## test_that('brick_add(p,q,r)', {

## r <- raster(matrix(1:12, nrow=4, ncol=3))
## r <- brick(stack(r, r*2, r*3))
## s <- r + 1

## brick_add(r,s)



##     v <- 1:12

##     expect_true( is.integer(v) )

##     p <- matrix(v, ncol=3, nrow=4)
##     q <- matrix(v, ncol=3, nrow=4)
##     r <- p + q

##     ## matrix is double precision in Simple.f90
##     expect_false( all( brick_add(p,q) == r ) )

##     v <- as.numeric(v) ## default double precision

##     expect_true( is.double(v) )

##     p <- matrix(v, ncol=3, nrow=4)
##     q <- matrix(v, ncol=3, nrow=4)
##     r <- p + q

##     expect_true( all( brick_add(p,q) == r ) )

## })


## ----- test -----------------------------------------------------------------


## a <- array(as.double(1:18), c(3,2,3))
## b <- array(as.double(2), c(3,2,3))

## array_add(a,b)


## ## nb: matrix_add requires integers to be stored as doubles
## ## otherwise the element boundaries will be misplaced and results wrong
## a <- matrix(as.double(1:96),  nrow=12, ncol=8)
## b <- a + 96

## matrix_add(a,b)


##   ## todo: more robust (or at least local) validation:
##   load('/home/greg/src/rpkg/ndrp/data/sysdata.rda')
##   d <- bom.validation.data
##   ## head(d)

##   ## ## unique(d$day9) ## 61 unique days
##   ## ## current implementation of SMD requires 365-day multiples
##   t <- aggregate(d$temp,  list(d$day9), max)$x
##   t <- rep(t, ceiling(365/length(t)))[1:365] ## fudge to length 365

##   r <- aggregate(d$rnd24, list(d$day9), unique)$x
##   r <- rep(r, ceiling(365/length(r)))[1:365] ## fudge to length 365

##   s <- aggregate(d$SDI, list(d$day9), median)$x
##   s <- rep(s, ceiling(365/length(s)))[1:365] ## fudge to length 365

##   stopifnot(all(sapply(list(t,r,s), length)==365))

##   df <- droughtFactor(t,r,c,init=s[1]) ##
##   ## plot(df[2:41], type='l', ylim=c(0,10))
##   bdf <- aggregate(d$DF,  list(d$day9), median)$x
##   ## lines(bdf[20:59], col='red')
##   ## save(t,r,s,sdi, file='sdi.tmp.validation.rda')
##   ## load('sdi.tmp.validation.rda')

##   ## all within 3 index units of BoM database values
##   expect_true( all(abs(df[2:41] - bdf[20:59]) <= 3) )

##   ## more than 90% of values within 2 units of BoM database sample
##   expect_true( sum(abs(df[2:41] - bdf[20:59]) <= 1.7) / length(df[2:41]) > 0.9 )

##   ## more than 70% of values are within 0.25 units of BoM database sample
##   expect_true( sum(abs(df[2:41] - bdf[20:59]) <= 0.25) / length(df[2:41]) > 0.7 )


##   ## gridded data

##   v2b <- function(v,m=3,n=2) {
##       stopifnot(require(raster))
##       bv <- brick(array(rep(v,each=m*n), c(m,n,length(v))))
##       stopifnot( nlayers(bv) == length(v) )
##       stopifnot( all(values(bv)[1,] == v) )
##       if (nlayers(bv) == 1) bv <- bv[[1]] ## RasterLayer
##       bv
##   }

##   bt <- v2b(t)
##   br <- v2b(r)
##   rc <- v2b(c[1])

##   dfg <- droughtFactorGrid(bt, br, rc, init=s[1])

##   expect_true( class(dfg) == 'RasterBrick' )
##   expect_true( extent(dfg) == extent(bt) )
##   expect_true( nlayers(dfg) == nlayers(bt) - 20 )
##   expect_true( all(nrow(dfg) == nrow(bt), ncol(dfg) == ncol(bt)) )


##   check.grid.consistent <- function(b) {
##       vb <- values(b)
##       cat('\n')
##       cb <- cbind(df,vb[,1:2])
##       ##print(head(cb, nrow(dfg) * ncol(dfg) + 2))
##       print(head(cb,12))
##       ## all(vb[,1] == df)
##       ## all(unlist(lapply(seq(nrow(vb)), function(i) { all(vb[i] == df) }  )))
##       return(TRUE)
##   }

##   expect_true( check.grid.consistent(dfg) )

##   rm(d,df,r,s,t)

## })

## ## ----- eof ------------------------------------------------------------------
