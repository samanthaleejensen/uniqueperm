.onUnload <- function (libpath) {
  library.dynam.unload("uniqueperm", libpath)
}
