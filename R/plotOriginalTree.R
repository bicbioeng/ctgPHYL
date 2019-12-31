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
#' \dontrun{
#' # load data for cellscape
#' cs_tree_edges <- read.csv(system.file("extdata", "cnv_tree_edges.csv",
#'                          package = "cellscape"))
#' cs_cnv_data <- read.csv(system.file("extdata", "cnv_data.csv",
#'                                      package = "cellscape"))
#' cs_sc_annot <- read.csv(system.file("extdata", "cnv_annots.tsv",
#'                                     package = "cellscape"), sep="\t")
#' cs_clone_colours <- data.frame(clone_id = c("1","2","3"),
#'                                colour = c("7fc97f", "beaed4", "fdc086"))
#' # run generate_tree()
#' toyPHYLset <- generate_tree(dataSet = toyPHYLset, treeType = "cellscape")
#'
#' # view names of original trees
#' names(originalTrees(toyPHYLset))
#'
#' # plot original trees
#' plotOriginalTree(toyPHYLset, "cellscape")
#' }

plotOriginalTree <- function(dataSet, treeType) {
    stopifnot(is(dataSet, "ctgPHYLset"))
    stopifnot(treeType %in% names(originalTrees(dataSet)))
    treeData <- originalTrees(dataSet)[[treeType]]

    # ANY CHANGES MADE IN THE FOLLOWING CODE MUST BE CHECKED FOR
    # COMPATIBILITY WITH THEIR RESPECTIVE make METHODS

    #TODO figure out why this isn't plotting in Rstudio the way it should
    # probably something to do with usage of htmlwidgets
    if (grepl("scape", treeType)) {
        treeData
    }
}
