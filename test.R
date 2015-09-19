
test.simple_values_are_vecotrs <- function() {
    checkTrue(is.vector(1))
    checkTrue(is.vector(1.1))
    checkTrue(is.vector(TRUE))
    checkTrue(is.vector("a"))
}

test.a_list_is_a_vecotr <- function() {
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
    sp <- split(df, df$characters)
    # split returns a list
    checkIdentical("list", class(sp))
    # a list of the original class
    checkIdentical("data.frame", class(sp$a))
    # list and dataframe are accessible be names in the same way
    checkIdentical(as.integer(c(1,3)), sp$a$numbers)
}

test.tapply_behaviour <- function() {
    df <- data.frame(numbers = c(1, 2, 3), characters = c("a", "b", "a"))
    # without FUNction it returns the split vector
    checkIdentical(as.integer(c(1,2,1)),
           tapply(X = df$numbers, INDEX = df$characters))
    # with function it returns the result array
    expectation <- array(c(4, 2), 2)
    names(expectation) <- c("a", "b")
    checkIdentical(expectation,
       tapply(X = df$numbers, INDEX = df$characters, FUN = sum))
}

