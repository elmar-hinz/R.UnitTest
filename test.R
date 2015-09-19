
test.digit_is_numeric <- function() {
    checkIdentical("numeric", class(3))
}

test.sequence_is_integer <- function() {
    checkIdentical(as.integer(c(1,2,3)), 1:3)
    checkIdentical(1:3, seq(1,3))
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
    # list and dataframe are accessible be names
    checkIdentical(as.integer(c(1,3)), sp$a$numbers)
}

