#' Methods for the ctgPHYLset class
#'
#' These methods operate on ctgPHYLset objects. Please note that
#' treeList<- and originalTrees<- are not exported and not intended to be
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
#' # load data for 'treeType = cellscape'
#' cs_tree_edges <- read.csv(system.file("extdata", "cnv_tree_edges.csv",
#'                          package = "cellscape"))
#' cs_cnv_data <- read.csv(system.file("extdata", "cnv_data.csv",
#'                                      package = "cellscape"))
#' cs_sc_annot <- read.csv(system.file("extdata", "cnv_annots.tsv",
#'                                     package = "cellscape"), sep="\t")
#' cs_clone_colours <- data.frame(clone_id = c("1","2","3"),
#'                                colour = c("7fc97f", "beaed4", "fdc086"))
#'
#' # create example ctgPHYLset and load data into it
#' ctgPHYLset <- newctgPHYLset()
#' cellscapeData(ctgPHYLset, "clone_colours") <- cs_clone_colours
#' cellscapeData(ctgPHYLset, "tree_edges") <- cs_tree_edges
#' cellscapeData(ctgPHYLset, "cnv_data") <- cs_cnv_data
#' cellscapeData(ctgPHYLset, "sc_annot") <- cs_sc_annot
#'
#' # run generate_tree()
#' ctgPHYLset <- generate_tree(dataSet = ctgPHYLset, treeType = "cellscape")
#'
#' trees <- treeList(ctgPHYLset)
#' originalTrees <- originalTrees(ctgPHYLset)
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
