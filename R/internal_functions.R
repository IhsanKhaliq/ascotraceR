`%notin%` <- function(x, table) {
   base::match(x, table, nomatch = 0L) == 0L
}
