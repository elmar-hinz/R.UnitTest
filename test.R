
test.simple_values_are_vectors <- function() {
    checkTrue(is.vector(1))
    checkTrue(is.vector(1.1))
    checkTrue(is.vector(TRUE))
    checkTrue(is.vector("a"))
}

test.a_list_is_a_vector <- function() {
    checkTrue(is.vector(list()))
}

test.a_dataframe_is_not_a_vector <- function() {
    checkTrue(!is.vector(data.frame()))
}

test.digits_and_floats_are_numeric <- function() {
    checkIdentical("numeric", class(3))
}

test.sequences_are_integer <- function() {
    checkIdentical("integer", class(1:3))
    checkIdentical(1:3, seq(1,3))
}

test.list_vs_vector <- function() {
    v <- c(1:2, 3:4)
    l <- list(1:2, 3:4)
    # the vector concatenates
    checkIdentical(v[4], l[[2]][2])
}

test.listWithVector_vs_vectorAsList <- function() {
    v <- c("a", "b")
    checkIdentical("b",    list(v)[[1]][2]) # one vector  of lenght 2
    checkIdentical("b", as.list(v)[[2]][1]) # two vectors of length 1
}

test.split_behviour <- function() {
    df <- data.frame(numbers = 1:3, characters = c("a", "b", "a"))
    # groups the columns of the first parameters by the second
    sp <- split(df$numbers, df$characters)
    # returns a list
    checkIdentical("list", class(sp))
    # with vectors of the class of the first parameter
    checkIdentical(class(df$numbers), class(sp$a))
}

test.lapply_behaviour <- function() {
    # a list of vectors of the same class
    # this could be created by split
    data <- list(1:10, 11:20, 21:30)
    result <- lapply(data, mean)
    # the result is list of the same length
    checkTrue(is.list(result))
    checkIdentical(length(data), length(result))
    checkIdentical(as.list(c(5.5, 15.5, 25.5)), result)
}

test.tapply_behaviour <- function() {
    df <- data.frame(numbers = c(1, 2, 3), characters = c("a", "b", "a"))
    # without FUNction it returns the split vector (integer)
    checkIdentical(as.integer(c(1,2,1)),
       tapply(X = df$numbers, INDEX = df$characters))
    # with function (sum) it returns the result as array (numeric)
    # a: 1 + 3 = 4
    # b: 2     = 2
    # array of one dimension and length 2
    expectation <- array(data = c(4, 2), dim = c(2))
    names(expectation) <- c("a", "b")
    checkIdentical(expectation,
       tapply(X = df$numbers, INDEX = df$characters, FUN = sum))
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




