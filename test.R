
test.the_classes <- function() {
    checkIdentical("integer", class(1L))
    checkIdentical("numeric", class(1))
    checkIdentical("numeric", class(1.1))
    checkIdentical("logical", class(TRUE))
    checkIdentical("character", class("a"))
    checkIdentical("list", class(list()))
    checkIdentical("data.frame", class(data.frame()))
}

test.simple_values_are_vectors <- function() {
    checkIdentical("numeric", class(1))
    checkTrue(is.vector(1))
    checkIdentical("numeric", class(1.1))
    checkTrue(is.vector(1.1))
    checkIdentical("logical", class(TRUE))
    checkTrue(is.vector(TRUE))
    checkIdentical("character", class("a"))
    checkTrue(is.vector("a"))
}

test.a_list_is_a_vector <- function() {
    checkTrue(is.vector(list()))
}

test.a_dataframe_is_not_a_vector <- function() {
    checkTrue(is.vector(list()))
    checkTrue(is.list(data.frame()))
    checkTrue(!is.vector(data.frame()))
}

test.integers_are_numeric <- function() {
    checkTrue("integer" == class(1L))
    checkTrue("numeric" != class(1L))
    checkTrue(is.integer(1L))
    checkTrue(is.numeric(1L))
}

test.digits_and_floats_are_numeric <- function() {
    checkTrue(!is.integer(3))
    checkTrue(is.numeric(3))
    checkTrue(is.numeric(3.3))
}

test.matrix_creation_and_behaviour <- function() {
    # ingredients
    mtrx <- 1:6
    dimensions <- c(2, 3) # dims are integer but accept numeric
    horizontal <- c("alpha", "beta", "gamma")
    vertical <- c("one", "two")
    dimension_names <- list(vertical, horizontal)
    # before
    checkTrue(is.null(dim(mtrx)))
    checkTrue(is.null(dimnames(mtrx)))
    checkIdentical('integer', class(mtrx))
    #after
    dim(mtrx) <- dimensions
    dimnames(mtrx) <- dimension_names
    checkIdentical('matrix', class(mtrx))
    checkTrue(is.array(mtrx))
    checkIdentical("integer", class(dim(mtrx)))
    checkIdentical(as.integer(dimensions), dim(mtrx))
    checkIdentical(dimension_names, dimnames(mtrx))
    # accessing data
    checkIdentical(4L, mtrx[4])
    checkIdentical(4L, mtrx['two', 'beta'])
    checkIdentical(vertical, names(mtrx[,'beta']))
    checkIdentical(3:4, as.integer(mtrx[,'beta']))
    # to dataframe
    df <- as.data.frame(mtrx)
    checkTrue(is.null(names(mtrx)))
    checkIdentical(horizontal, names(df))
    checkIdentical(horizontal, colnames(df))
    checkIdentical(vertical, rownames(df))
    checkIdentical(4L, df['two', 'beta'])
    checkIdentical(3:4, df$beta)
}

test.by_default_seq_are_integers <- function() {
    checkIdentical(1:3, seq(1,3))
    checkTrue(is.numeric(1:3))
    checkTrue(is.integer(1:3))
    checkIdentical("integer", class(1:3))
}

test.seq_by_are_numeric <- function() {
    checkIdentical("integer", class(seq(1,3)))
    checkIdentical("numeric", class(seq(1,3, by = 1)))
    checkIdentical("numeric", class(seq(1,3, by = 1L)))
}

test.inter_function_fills_zeros <- function() {
    checkIdentical(as.integer(c(0,0)), integer(2))
}

test.list_vs_vector <- function() {
    # the vector concatenates
    v <- c(1:2, 3:4)
    l <- list(1:2, 3:4)
    checkIdentical(v[4], l[[2]][2])
    checkTrue( identical(   c(c(1,2)),    c(1,2)))
    checkTrue(!identical(list(c(1,2)), list(1,2)))
}

test.listWithVector_vs_vectorAsList <- function() {
    v <- c("a", "b")
    checkIdentical("b",    list(v)[[1]][2]) # one vector  of lenght 2
    checkIdentical("b", as.list(v)[[2]][1]) # two vectors of length 1
}

test.split_behviour <- function() {
    df <- data.frame(numbers = 1:3, characters = c("a", "b", "a"))
    # groups the column of the first parameters by the second
    sp <- split(df$numbers, df$characters)
    # returns a list
    checkIdentical(list("a" = c(1L,3L), "b" = 2L), sp)
    # with vectors of the class of the first parameter
    checkIdentical(class(df$numbers), class(sp$a))
}

test.multidimensional_split <- function() {
    sp <- split(c(1,2,3,4), list(c(1,1,2,2), c("a", "a", "a", "b")))
    # the dimensions go into the names
    checkIdentical(c('1.a', '2.a', '1.b', '2.b'), names(sp))
    checkIdentical(sp$'1.a', c(1,2))
    # empty names are created
    checkIdentical(sp$'1.b', numeric(0))
}

## List apply
##
## Applies a function on a list of objects.
test.lapply_behaviour <- function() {
    # a list of objects
    # this could be created by split
    data <- list(1:10, 11:20, 21:30)
    result <- lapply(data, mean)
    # the result is list of the same length
    checkTrue(is.list(result))
    checkIdentical(length(data), length(result))
    checkIdentical(as.list(c(5.5, 15.5, 25.5)), result)
    # wight different classes
    result <- lapply(list("a", 1:2), class)
    checkIdentical(list("character", "integer"), result)
}

## SubseT apply (Table apply)
##
## Creates subsets by factors or alike
## and applies a function on each subset.
## This is much like combining split() and lapply().
test.tapply_behaviour <- function() {
    # expectations for sum of c(1, 2, 3) by  c("a", "b", "a")
    # subset split vector:
    splitv <- as.integer(c(1,2,1))
    # result array
    # a: 1 + 3 = 4
    # b: 2     = 2
    # of one dimension and length 2:
    wanted <- as.integer(c(4, 2))
    expect <- array(data = wanted, dim = c(2))
    names(expect) <- c("a", "b")
    checkIdentical(wanted, as.vector(expect))

    # without FUN it returns the split vector (integer)
    checkIdentical(splitv, tapply(1:3, c('a','b','a')))
    # with function (sum) it returns the result as array
    checkIdentical(expect, tapply(1:3, c('a','b','a'), FUN = sum))

    # same based on a dataframe
    df <- data.frame(numbers = 1:3, characters = c("a", "b", "a"))
    checkIdentical(splitv, tapply(df$numbers, df$characters))
    checkIdentical(expect,
       tapply(X = df$numbers, INDEX = df$characters, FUN = sum))
}

test.multidimensional_tapply <- function() {
    # resulting in a two dimensional matrix, a special array
    sp <- tapply(c(1,2,3,4), list(c(1,1,2,2), c("a", "a", "a", "b")), sum)
    checkEquals("matrix", class(sp))
    checkIdentical(list(c("1", "2"), c("a", "b")), dimnames(sp))
    checkTrue(is.na(sp["1", "b"]))
    checkIdentical(4, sp["2", "b"])
}

test.mapply_behaviour <- function() {
    # all parameters have the same length
    # walks them along in parallel and feeds them to FUN
    checkIdentical(as.integer(c(9, 12)), mapply(sum, 1:2, 3:4, 5:6))
    checkIdentical(mapply(sum, 1:2, 3:4), mapply(1:2, 3:4, FUN = sum))
    # if paramters don't have the same length
    # they are expanded by repetion
    checkIdentical(mapply(sum, 1:3, c(2,2,2)), mapply(sum, 1:3, 2))
    suppressWarnings( checkIdentical(mapply(sum, 1:3, c(1,2,1)),
        mapply(sum, 1:3, c(1,2))))
}

test.which_behaviour <- function() {
    c <- c(NA, 1, 2)
    # returns a list of positions as integer and skips NA
    checkIdentical(as.integer(c(2,3)), which(c > 0))
    # wants a logical vector, not missing NA
    checkIdentical(2:3, which(c(F, T, T)))
    checkIdentical(2:3, which(!is.na(c)))
    checkException( which(c), silent = TRUE)
    # NULL and 0 are not logical
    checkException( which(c(NULL)), silent = TRUE)
    checkException( which(c(0)), silent = TRUE)
    # only NA is casted to logical FALSE
    checkIdentical(2:3, which(c(NA, T, T, F)))
}

test.sum_behaviour <- function() {
    # sum coerces as few as possible and as much as necessary
    checkTrue("integer" == class(sum(1L)))
    checkTrue("numeric" == class(sum(1)))
    checkTrue("numeric" == class(sum(1, 1L)))
    # sums to a vector of lenght 1
    checkTrue(1 == length(sum(1:2, 3:5, 2L)))
    checkTrue(4 == sum(1:2, 1L))
    # accepts negative values
    checkTrue(0 == sum(-1, 2, -1))
}

