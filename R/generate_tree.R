#' Function to Generate Cell Trees
#'
#' This function builds a cell hierarchy tree of a chosen supported type with
#' a given data set, contained in a ctgPHYLset object.  Different tree
#' types require data from corresponding slots of the ctgPHYLset object.
#' See vignette for examples, usage details, and instructions on building a
#' ctgPHYLset object.
#' @param dataSet the ctgPHYLset object for creating the cell tree
#' @param treeType the type of tree generated
#' @param outputDir the directory where output should be saved, defaults to
#' the current working directory.
#' @return An updated ctgPHYLset object.  The generated tree is placed in
#'     \code{@@treeList[treeType]} slot, and can be accessed via
#'     \code{treeList(dataSet)$treeType}.  The function also creates a
#'     directory named "CTG-Output" and writes the plot(s) of the
#'     generated tree(s) and its SIF file to that directory.
#'
#' @export
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
#' # choose output directory
#' od <- getwd()
#' # run generate_tree()
#' ctgPHYLset <- generate_tree(dataSet = ctgPHYLset, treeType = "cellscape", 
#'                              outputDir = od)

generate_tree <- function(dataSet, treeType, outputDir = getwd()) {
    stopifnot(is(dataSet, "ctgPHYLset"))
    # get the matrix containing packages and corresponding functions
    method_matrix <- make_method_matrix()
    # find the correct package to use
    pack <- which(method_matrix[1,] == treeType)
    # create a directory and subdirectories for output, if they don't exist
    if(!dir.exists(file.path(outputDir,"CTG-Output"))){
        dir.create(file.path(outputDir,"CTG-Output"))
        dir.create(file.path(outputDir,"CTG-Output","Plots"))
        dir.create(file.path(outputDir,"CTG-Output","SIFs"))
    }
    # get the corresponding function
    func <- get(method_matrix[2, pack])
    # execute the function
    dataSet <- func(dataSet, outputDir)
    dataSet
}

#' A Function That Creates A Predefined Matrix Of Usable Packages
#'
#' This function constructs the predefined matrix that contains all possible
#' cell tree building packages and their corresponding functions
#' @keywords internal
#' @return a matrix containing all possible packages and their corresponding
#'     function

make_method_matrix <- function() {
    # create a matrix of the packages and corresponding function calls
    method_matrix = matrix(
        c(
            'cellscape',
            'timescape',
            'mapscape',
            'makeCellscape',
            'makeTimescape',
            'makeMapscape'
        ),
        nrow = 2,
        ncol = 3,
        byrow = TRUE,
        dimnames = list(c('package', 'function'))
    )
    # return the method matrix to the caller
    return(method_matrix)
}
