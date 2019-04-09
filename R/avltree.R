#' Sentinel object representing an empty AVL tree map.
#'
#' @return
#' @export
#'
#' @examples
empty <- function() {
  structure(list(), class = "AVLEmptyNode")
}

get_hash <- function(x) {
  if(class(x) == "AVLCollisions") return(get_hash(x[[1]]))
  attr(x, "hash_code")
}

with_hash <- function(x) {
  hc <- get_hash(x)
  if (is.null(hc)) {
    hc <- hashFunction::murmur3.32(x)
    attr(x, "hash_code") <- hc
  }
  x
}

node <- function(key, value, left = empty, right = empty) {
  structure(
    list(
      key = with_hash(key),
      value = value,
      left = left,
      right = right
    ),
    class = "AVLNode"
  )
}

collisions <- function(pairs) {
  structure(pairs, class = "AVLCollisions")
}

# TODO cache height
height <- function(tree, count = 0) {
  if (identical(tree, empty)) {
    count
  } else {
    max(
      height(tree$left, count+1),
      height(tree$right, count+1)
    )
  }
}

balance_factor <- function(tree) {
  height(tree$left) - height(tree$right)
}

rotate_left <- function(tree) {
  if (identical(tree, empty)) {
    tree
  } else {
    node(
      tree$right$key,
      tree$right$value,
      node(tree$key, tree$value, tree$left, tree$right$left),
      tree$right$right
    )
  }
}

rotate_right <- function(tree) {
  if (identical(tree, empty)) {
    tree
  } else {
    node(
      tree$left$key,
      tree$left$value,
      tree$left$left,
      node(tree$key, tree$value, tree$left$right, tree$right)
    )
  }
}

is_left_case <- function(tree) {
  balance_factor(tree) < -1
}

is_left_right_case <- function(tree) {
  is_left_case(tree) && (balance_factor(tree$right) > 0)
}

is_right_case <- function(tree) {
  balance_factor(tree) > 1
}

is_right_left_case <- function(tree) {
  is_right_case(tree) && (balance_factor(tree$left) < 0)
}

count <- function(tree) {
  if (identical(empty, tree)) {
    0
  } else {
    1 + count(tree$left) + count(tree$right)
  }
}

balance <- function(tree) {
  if(is_right_left_case(tree)) {
    rotate_right(node(tree$key, tree$value, rotate_left(tree$left), tree$right))
  } else if (is_left_right_case(tree)) {
    rotate_left(node(tree$key, tree$value, tree$left, rotate_right(tree$right)))
  } else if (is_right_case(tree)) {
    rotate_right(tree)
  } else if (is_left_case(tree)) {
    rotate_left(tree)
  } else tree
}

insert <- function(tree, key, value) {
  key <- with_hash(key)
  if (identical(empty, tree)) {
    node(key, value)
  } else if (get_hash(key) < get_hash(tree$key)) {
    node(tree$key, tree$value, insert(tree$left, key, value), tree$right)
  } else if (get_hash(key) > get_hash(tree$key)) {
    node(tree$key, tree$value, tree$left, insert(tree$right, key, value))
  } else if (class(tree$value) == "AVLCollisions") {
      node(tree$key, collisions(append(tree$value, list(key, value))))
  } else {
      node(tree$key, collisions(list(list(tree$key, tree$value), list(key, value))))
  }
}

#' Look up the value associated with a key in an AVL tree map.
#'
#' @param tree
#' @param key
#' @param default
#' @param test
#'
#' @return
#' @export
#'
#' @examples
lookup <- function(tree, key, default = NULL, test = `==`) {
  key <- with_hash(key)
  while (TRUE) {
    if (identical(empty, tree)) return(default)
    if (get_hash(key) < get_hash(tree$key)) {
      tree <- tree$left
    } else if (get_hash(key) > get_hash(tree$key)) {
      tree <- tree$right
    } else if (get_hash(key) == get_hash(tree$key)
      && class(tree$value) == "AVLCollisions") {
      for (pair in tree$value) {
        if (test(key, pair[[1]])) return(pair[[2]])
      }
      return(default)
    } else if (get_hash(key) == get_hash(tree$key)
      && test(key, tree$key)) {
      return(tree$value)
    } else return(default)
  }
}

#' Determine the presence of a key in the AVL tree map.
#'
#' @param tree
#' @param key
#' @param test
#'
#' @return
#' @export
#'
#' @examples
contains <- function(tree, key, test = `==`) {
  sentinel <- function() {}
  !identical(sentinel, lookup(tree, key, sentinel, test = test))
}

#' Assoc(iate) a key-value pair with an existing AVL tree map.
#'
#' @param tree
#' @param key
#' @param value
#'
#' @return
#' @export
#'
#' @examples
assoc <- function(tree, key, value) {
  balance(insert(tree, key, value))
}

#' Title
#'
#' @param tree
#'
#' @return
#' @export
#'
#' @examples
keys <- function(tree) {
  if (identical(empty, tree)) {
    NULL
  } else {
    key <- tree$key
    attr(key, "hash_code") <- NULL
    c(key, keys(tree$left), keys(tree$right))
  }
}

benchmark <- function() {

  ks <- as.character(runif(1000))
  t1 <- empty
  e1 <- new.env(parent = emptyenv())

  microbenchmark::microbenchmark({
    for (k in ks) {
      t1 <- assoc(t1, k, k)
    }
  }, unit = "ms", times = 1)

  profvis::profvis(

  microbenchmark::microbenchmark({
    for (k in ks) {
      lookup(t1, k)
    }
  }, unit = "ms", times = 1)
  )
  microbenchmark::microbenchmark({
    for (k in ks) {
      lookup(t1, k)
    }
  }, unit = "ms", times = 1)

  microbenchmark::microbenchmark({
    for (k in ks) {
      e1[[k]] <- k
    }
  }, unit = "ms", times = 1)

  microbenchmark::microbenchmark({
    for (k in ks) {
      e1[[k]]
    }
  }, unit = "ms", times = 1)
}

# Usage
# t <- assoc(assoc(empty(), "foo", 1), "bar", 2)
# contains(t, "baz")
# contains(t, "bar")
# lookup(t, "foo")

