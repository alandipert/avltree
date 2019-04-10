hash_code <- function(x) {
  UseMethod("hash_code", x)
}

# The Java algorithm
hash_code.character <- function(s) {
  hash <- 0
  for (x in seq_along(s)) {
    for (y in seq(1, nchar(s[[x]]))) {
      hash <- hash*31 + utf8ToInt(substring(s[[x]], y, y))
    }
  }
  hash
}

hash_code.numeric <- sum

equals <- function(x, y) {
  UseMethod("equals", x)
}

# Used to compare keys when their hash codes collide.
equals.character <- `==`
equals.numeric <- `==`

#' Sentinel object representing an empty AVL tree map.
#'
#' @return
#' @export
#'
#' @examples
empty <- structure(list(), class = "AVLEmptyNode")

is_empty <- function(x) {
  class(x) == "AVLEmptyNode"
}

node <- function(key,
                 value,
                 key_hash = hash_code(key),
                 left = empty,
                 right = empty) {
  structure(
    list(
      key_hash = key_hash,
      key = key,
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
  if (is_empty(tree)) {
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
  if (is_empty(tree)) {
    tree
  } else {
    node(
      tree$right$key,
      tree$right$value,
      key_hash = tree$key_hash,
      left = node(
        tree$key,
        tree$value,
        key_hash = tree$key_hash,
        left = tree$left,
        right = tree$right$left
      ),
      right = tree$right$right
    )
  }
}

rotate_right <- function(tree) {
  if (is_empty(tree)) {
    tree
  } else {
    node(
      tree$left$key,
      tree$left$value,
      key_hash = tree$left$key_hash,
      left = tree$left$left,
      right = node(
        tree$key,
        tree$value,
        key_hash = tree$key_hash,
        left = tree$left$right,
        right = tree$right
      )
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
  if (is_empty(empty)) {
    0
  } else {
    1 + count(tree$left) + count(tree$right)
  }
}

balance <- function(tree) {
  if(is_right_left_case(tree)) {
    rotate_right(node(
        tree$key,
        tree$value,
        key_hash = tree$key_hash,
        left = rotate_left(tree$left),
        right = tree$right
      )
    )
  } else if (is_left_right_case(tree)) {
    rotate_left(node(
        tree$key,
        tree$value,
        key_hash = tree$key_hash,
        left = tree$left,
        right = rotate_right(tree$right)
      )
    )
  } else if (is_right_case(tree)) {
    rotate_right(tree)
  } else if (is_left_case(tree)) {
    rotate_left(tree)
  } else tree
}

insert <- function(tree, key, value, key_hash = hash_code(key)) {
  if (is_empty(tree)) {
    node(key, value)
  } else if (key_hash < tree$key_hash) {
    node(
      tree$key,
      tree$value,
      key_hash = tree$key_hash,
      left = insert(tree$left, key, value, key_hash),
      right = tree$right
    )
  } else if (key_hash > tree$key_hash) {
    node(
      tree$key,
      tree$value,
      key_hash = tree$key_hash,
      left = tree$left,
      right = insert(tree$right, key, value, key_hash = key_hash)
    )
  } else if (class(tree$value) == "AVLCollisions") {
    node(
      tree$key,
      collisions(append(tree$value, list(key, value))),
      key_hash = tree$key_hash
    )
  } else {
    node(
      tree$key,
      collisions(list(list(tree$key, tree$value), list(key, value))),
      key_hash = tree$key_hash
    )
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
lookup <- function(tree, key, default = NULL) {
  key_hash <- hash_code(key)
  while (TRUE) {
    if (is_empty(tree)) return(default)
    if (key_hash < tree$key_hash) {
      tree <- tree$left
    } else if (key_hash > tree$key_hash) {
      tree <- tree$right
    } else if (key_hash == tree$key_hash
        && class(tree$value) == "AVLCollisions") {
      for (pair in tree$value) {
        if (equals(key, pair[[1]])) return(pair[[2]])
      }
      return(default)
    } else if (key_hash == tree$key_hash
        && equals(key, tree$key)) {
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
contains <- function(tree, key) {
  sentinel <- function() {}
  !identical(sentinel, lookup(tree, key, sentinel))
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
  if (is_empty(tree)) {
    NULL
  } else {
    c(tree$key, keys(tree$left), keys(tree$right))
  }
}

#' Title
#'
#' @param node
#'
#' @return
#' @export
#'
#' @examples
as.list.AVLEmptyNode <- function(node) {
  NULL
}

#' Title
#'
#' @param node
#'
#' @return
#' @export
#'
#' @examples
as.list.AVLNode <- function(node) {
  self <- list()
  self[[node$key]] <- node$value
  c(as.list(node$left), self,  as.list(node$right))
}

#' Title
#'
#' @param node
#' @param k
#'
#' @return
#' @export
#'
#' @examples
`[[.AVLNode` <- function(node, k) {
  lookup(node, k)
}

benchmark <- function() {

  ks <- runif(1000)
  t1 <- empty
  l1 <- list()
  e1 <- new.env(parent = emptyenv())

  microbenchmark::microbenchmark({
    for (k in ks) {
      t1 <- assoc(t1, k, k)
    }
  }, unit = "ms", times = 1)

  microbenchmark::microbenchmark({
    for (k in ks) {
      l1[[k]] <- k
    }
  }, unit = "ms", times = 1)

  microbenchmark::microbenchmark({
    for (k in ks) {
      lookup(t1, k)
    }
  }, unit = "ms", times = 1)

  microbenchmark::microbenchmark({
    for (k in ks) {
      lookup(t1, k)
    }
  }, unit = "ms", times = 10)

  microbenchmark::microbenchmark({
    for (k in ks) {
      l1[[k]]
    }
  }, unit = "ms", times = 10)

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

