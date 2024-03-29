##Este codigo se encargar� de crear en cach� la matriz requerida en la entrega del curso de R, que permitir� realizar funciones inversas sobre la misma.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function()x
  setearInverso <- function(inverso) m <<- inverso
  obtenerInverso <- function() m 
  list(set = set, get = get, 
       setearInverso = setearInverso, 
       obtenerInverso = obtenerInverso)
}

cacheSolve <- function(x, ...) {
  ## Retorno de una matriz que es el inverso de x
  m <- x$obtenerInverso()
  if(!is.null(m)){
    message("Obetiendo data de cach�")
    return(m)
  }
  mat <- x$get()
  m <- solve(mat,...)
  x$setearInverso(m)
  m
}