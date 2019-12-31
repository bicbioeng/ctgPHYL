#' Methods for the ctgPHYLset class
#'
#' These methods operate on ctgPHYLset objects. Please note that
#' treeList<- and originalTrees<- are not intended to be
#' called directly.
#'
#' @param cs A ctgPHYLset object
#' @param value
#'    \code{cellscapeData(cs, pt)<-}: The value to use as a named parameter for
#'       cellscape, used by \code{generate_tree(treeType = "cellscape")} in the
#'       \pkg{cellTreeGenerator} workflow
#'
#'    \code{timescapeData(cs, pt)<-}: The value to use as a named parameter for
#'       timescape used by \code{generate_tree(treeType = "timescape")} in the
#'       \pkg{cellTreeGenerator} workflow
#'
#'    \code{mapscapeData(cs)<-}: The value to use as a named parameter for
#'       mapscape, used by \code{generate_tree(treeType = "mapscape")} in the
#'       \pkg{cellTreeGenerator} workflow
#'
#' @param tt The type of tree being stored
#' @param pt The name of the \pkg{cellscape}, \pkg{timescape}, or
#'     \pkg{mapscape} parameter to store
#' @return An updated ctgPHYLset object, or the contents of a slot of the
#'      ctgPHYLset object
#' @importFrom methods as is new
#' @name ctgPHYLset-methods
#' @examples
#' \dontrun{
#' cellTreeInfo(samplectgPHYLset) <- "Hours"
#' cti <- cellTreeInfo(samplectgPHYLset)
#' monocleInfo(samplectgPHYLset) <- c("gene_short_name", "MYF5",
#'                                      "ANPEP", "FPKM")
#' mi <- monocleInfo(samplectgPHYLset)
#' TSCANinfo(samplectgPHYLset) <- "ENSG00000000003.10"
#' ti <- TSCANinfo(samplectgPHYLset)
#' oncoNEMdata(samplectgPHYLset) <- my_snp_matrix
#' od <- oncoNEMdata(samplectgPHYLset)
#' CanopyData(samplectgPHYLset) <- my_Canopy_Data_list
#' cd <- CanopyData(samplectgPHYLset)
#' trees <- treeList(samplectgPHYLset)
#' originalTrees <- originalTrees(samplectgPHYLset)
#' }
NULL
#'
#' @rdname ctgPHYLset-methods
#' @aliases ctgPHYLset,ANY,ANY-method
#' @export

cellscapeData <- function(cs) {
    stopifnot(is(cs, "ctgPHYLset"))
    cs@cellscapeData
}

#' @rdname ctgPHYLset-methods
#' @aliases ctgPHYLset,ANY,ANY-method
#' @keywords internal
#' @export

`cellscapeData<-` <- function(cs, pt, value) {
    stopifnot(is(cs, "ctgPHYLset"))
    cs@cellscapeData[[pt]] <- value
    cs
}

#' @rdname ctgPHYLset-methods
#' @aliases ctgPHYLset,ANY,ANY-method
#' @export

timescapeData <- function(cs) {
    stopifnot(is(cs, "ctgPHYLset"))
    cs@timescapeData
}

#' @rdname ctgPHYLset-methods
#' @aliases ctgPHYLset,ANY,ANY-method
#' @keywords internal
#' @export

`timescapeData<-` <- function(cs, pt, value) {
    stopifnot(is(cs, "ctgPHYLset"))
    cs@timescapeData[[pt]] <- value
    cs
}

#' @rdname ctgPHYLset-methods
#' @aliases ctgPHYLset,ANY,ANY-method
#' @export

mapscapeData <- function(cs) {
    stopifnot(is(cs, "ctgPHYLset"))
    cs@mapscapeData
}

#' @rdname ctgPHYLset-methods
#' @aliases ctgPHYLset,ANY,ANY-method
#' @keywords internal
#' @export

`mapscapeData<-` <- function(cs, pt, value) {
    stopifnot(is(cs, "ctgPHYLset"))
    cs@mapscapeData[[pt]] <- value
    cs
}

#' @rdname ctgPHYLset-methods
#' @aliases ctgPHYLset,ANY,ANY-method
#' @export

treeList <- function(cs) {
    stopifnot(is(cs, "ctgPHYLset"))
    cs@treeList
}

#' @rdname ctgPHYLset-methods
#' @aliases ctgPHYLset,ANY,ANY-method
#' @keywords internal

`treeList<-` <- function(cs, tt, value) {
    stopifnot(is(cs, "ctgPHYLset"))
    cs@treeList[[tt]] <- value
    cs
}

#' @rdname ctgPHYLset-methods
#' @aliases ctgPHYLset,ANY,ANY-method
#' @export

originalTrees <- function(cs) {
    stopifnot(is(cs, "ctgPHYLset"))
    cs@originalTrees
}

#' @rdname ctgPHYLset-methods
#' @aliases ctgPHYLset,ANY,ANY-method
#' @keywords internal

`originalTrees<-` <- function(cs, tt, value) {
    stopifnot(is(cs, "ctgPHYLset"))
    cs@originalTrees[[tt]] <- value
    cs
}
