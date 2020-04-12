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
#' ctgPHYLset <- ctgPHYLset()
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

cellscapeData <- function(cs, pt = NULL) {
    stopifnot(is(cs, "ctgPHYLset"))
    if(is.null(pt)){
        cs@cellscapeData
    } else {
        cs@cellscapeData[[pt]]
    }
}

#' @rdname ctgPHYLset-methods
#' @aliases ctgPHYLset,ANY,ANY-method
#' @keywords internal
#' @export

`cellscapeData<-` <- function(cs, pt, value) {
    stopifnot(is(cs, "ctgPHYLset"))
    params <- c("clone_colours","timepoint_title","clone_title","xaxis_title","yaxis_title",
                "phylogeny_title","node_type","display_node_ids","prop_of_clone_threshold",
                "vaf_threshold","show_warnings","width","height","cnv_data","mut_data",
                "mut_data_matrix","mut_order","tree_edges","gtype_tree_edges","sc_annot",
                "value_type")
    test_pt <- match(pt,params)
    if(is.na(test_pt)){
        warning(paste0("Entry ",pt," not a cellscape parameter. Check vignette for details."))
        cs
    } else {
        cs@cellscapeData[[pt]] <- value
        cs   
    }
}

#' @rdname ctgPHYLset-methods
#' @aliases ctgPHYLset,ANY,ANY-method
#' @export

timescapeData <- function(cs, pt = NULL) {
    stopifnot(is(cs, "ctgPHYLset"))
    if(is.null(pt)){
        cs@timescapeData
    } else {
        cs@timescapeData[[pt]]
    }
}

#' @rdname ctgPHYLset-methods
#' @aliases ctgPHYLset,ANY,ANY-method
#' @keywords internal
#' @export

`timescapeData<-` <- function(cs, pt, value) {
    stopifnot(is(cs, "ctgPHYLset"))
    params <- c("mutations","clone_colours","xaxis_title","yaxis_title","phylogeny_title",
                "alpha","genotype_position","perturbations","sort","show_warnings","width",
                "height","clonal_prev","tree_edges")
    test_pt <- match(pt,params)
    if(is.na(test_pt)){
        warning(paste0("Entry ",pt," not a timescape parameter. Check vignette for details."))
        cs
    } else {
        cs@timescapeData[[pt]] <- value
        cs
    }
}

#' @rdname ctgPHYLset-methods
#' @aliases ctgPHYLset,ANY,ANY-method
#' @export

mapscapeData <- function(cs, pt = NULL) {
    stopifnot(is(cs, "ctgPHYLset"))
    if(is.null(pt)){
        cs@mapscapeData
    } else {
        cs@mapscapeData[[pt]]
    }
}

#' @rdname ctgPHYLset-methods
#' @aliases ctgPHYLset,ANY,ANY-method
#' @keywords internal
#' @export

`mapscapeData<-` <- function(cs, pt, value) {
    stopifnot(is(cs, "ctgPHYLset"))
    params <- c("clone_colours","mutations","sample_ids","n_cells",
                "show_low_prev_gtypes","phylogeny_title","anatomy_title",
                "classification_title","show_warnings","width","height",
                "clonal_prev", "tree_edges","sample_locations","img_ref")
    test_pt <- match(pt,params)
    if(is.na(test_pt)){
        warning(paste0("Entry ",pt," not a mapscape parameter. 
                       Check vignette for details."))
        cs
    } else {
        cs@mapscapeData[[pt]] <- value
        cs
    }
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
