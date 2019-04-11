hash_code <- function(x) {
  UseMethod("hash_code", x)
}

hash_code.character <- hashFunction::murmur3.32
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

min_tree <- function(tree) {
  if (is_empty(tree$left)) tree else min_tree(tree$left)
}

#' Title
#'
#' @param tree
#' @param key
#' @param key_hash
#'
#' @return
#' @export
#'
#' @examples
remove <- function(tree, key, key_hash = hash_code(key)) {
  if (is_empty(tree)) {
    NULL
  } else if (key_hash < tree$key_hash) {
    node(
      tree$key,
      tree$value,
      key_hash = tree$key_hash,
      left = remove(left, key, key_hash = key_hash),
      right = right
    )
  } else if (key_hash > tree$key_hash) {
    node(
      tree$key,
      tree$value,
      key_hash = tree$key_hash,
      left = left,
      right = remove(right, key, key_hash = key_hash)
    )
  } else if (is_empty(tree$left)) {
    tree$right
  } else if (is_empty(tree$right)) {
    tree$left
  } else {
    min <- min_tree(tree$right)
    node(
      min$key,
      min$value,
      key_hash = min$key_hash,
      left = min$left,
      right = remove(tree$right, min$key, key_hash = min$key_hash)
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
as.list.AVLEmptyNode <- function(node) NULL

#' @export
as.list.AVLNode <- function(node) {
  self <- list()
  stopifnot(is.character(node$key) || is.symbol(node$key))
  self[[node$key]] <- node$value
  c(as.list(node$left), self,  as.list(node$right))
}

#' @export
`[[.AVLNode` <- lookup

#' @export
`[[<-.AVLNode` <- assoc

#' @export
`[[<-.AVLEmptyNode` <- assoc
