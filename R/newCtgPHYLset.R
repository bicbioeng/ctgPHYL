#' Creates a new ctgPHYLset object.
#'
#' @return a new, empty ctgPHYLset object to store parameters, inputs, and
#'     outputs
#' @export
#' @examples
#' # create example ctgPHYLset
#' ctgPHYLset <- newctgPHYLset()
newctgPHYLset <- function()
{
    cs <- new("ctgPHYLset")

    cs
}
