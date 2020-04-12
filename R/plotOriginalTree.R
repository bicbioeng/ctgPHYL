#' Display Original ctgPHYL Plots
#'
#' Displays the original plots created by the ctgPHYL package and
#' stored in the [originalTrees] slot of a ctgPHYLset object.
#'
#' @param dataSet a ctgPHYLset object
#' @param treeType the type of tree to display.  Must be one of
#'     \code{names(originalTrees(dataSet))}
#'
#' @return An object of class \code{c(<tree type>, "htmlwidget")}.
#' @export
#'
#' @examples
#' # load data for cellscape
#' cs_tree_edges <- read.csv(system.file("extdata", "cnv_tree_edges.csv",
#'                          package = "cellscape"))
#' cs_cnv_data <- read.csv(system.file("extdata", "cnv_data.csv",
#'                                      package = "cellscape"))
#' cs_sc_annot <- read.csv(system.file("extdata", "cnv_annots.tsv",
#'                                     package = "cellscape"), sep="\t")
#' cs_clone_colours <- data.frame(clone_id = c("1","2","3"),
#'                                colour = c("7fc97f", "beaed4", "fdc086"))
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
#' # view names of original trees
#' names(originalTrees(ctgPHYLset))
#'
#' # plot original trees
#' plotOriginalTree(ctgPHYLset, "cellscape")

plotOriginalTree <- function(dataSet, treeType) {
    stopifnot(is(dataSet, "ctgPHYLset"))
    stopifnot(treeType %in% names(originalTrees(dataSet)))
    treeData <- originalTrees(dataSet)[[treeType]]

    # ANY CHANGES MADE IN THE FOLLOWING CODE MUST BE CHECKED FOR
    # COMPATIBILITY WITH THEIR RESPECTIVE make METHODS

    if (grepl("scape", treeType)) {
        treeData
    }
}
