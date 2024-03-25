
#' Add a sphere(s) to an NGLView object
#' 
#' Uses a buffer to add spheres to an x
#' Get inspiration from this:
#' https://x.org/ngl/api/classes/bufferrepresentation.html#dispose
#' @export
#' @param x an `NGLVieweR` objects, or its proxy
#' @param position a numeric of length 3, or a matrix with three columns
#' @param color a color
#' @param radius the radius
addSphere <- function(x, position, color = c(1, 0, 0), radius = 0.25, 
                      name = NULL) {
  UseMethod("addSphere", x)
}

#' @noRd
#' @export
addSphere.NGLVieweR <- function(x, position, color = c(1, 0, 0), radius = 0.25,
                                name = NULL) {
  stopifnot(is(x, "NGLVieweR"))
  buffer <- .sphere_buffer(position, color, radius)
  
  x$x$spheres$position <- c(x$x$spheres$position, buffer$position)
  x$x$spheres$color <- c(x$x$spheres$color, buffer$color)
  x$x$spheres$radius <- c(x$x$spheres$radius, buffer$radius)
  x
}

#' @noRd
#' @export
addSphere.NGLVieweR_proxy <- function(x, position, color = c(1, 0, 0),
                                      radius = 0.25, name = NULL) {
  buffer <- .sphere_buffer(position, color, radius)
  message <- list(id = x$id, buffer = buffer)
  x$session$sendCustomMessage("NGLVieweR:addSphere", message)
  return(x)
}

.sphere_buffer <- function(position, color, radius) {
  
  if (!is.matrix(position)) {
    stopifnot(is.numeric(position), length(position) == 3)
    position <- matrix(position, ncol = 3)
  }
  stopifnot(is.matrix(position), ncol(position) == 3)
  
  if (!is.matrix(color)) {
    stopifnot(is.numeric(color), length(color) == 3)
    if (is.character(color)) {
      # todo: this is a color name, we can convert to a 3number thing
      stop("need to implement color name to c(r,g,b) representation")
      color <- colorname2int(color)
    }
    color <- matrix(rep(color, nrow(position)), ncol = 3, byrow = TRUE)
  }
  stopifnot(is.matrix(color), ncol(color) == 3, nrow(color) == nrow(position))
  
  stopifnot(is.numeric(radius)) 
  if (length(radius) == 1L) {
    radius <- rep(radius, nrow(position))
  }
  stopifnot(is.numeric(radius), length(radius) == nrow(position))
  
  list(
    position = as.numeric(t(position)),
    color = as.numeric(t(color)),
    radius = radius)
}

#' @noRd
#' @export
removeSphere <- function(x, name) {
  stopifnot(is(x, "NGLVieweR_proxy"))
  stopifnot(is.character(name), length(name) == 1L)
  message <- list(id = x$id, buffer_name = name)
  x$session$sendCustomMessage("NGLVieweR:removeSphere", message)
  return(x)
}
