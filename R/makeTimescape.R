#' A Cell Tree Generating Function using timescape
#'
#' This function, called by \code{\link{generate_tree}}, creates visualizations
#' using the data and \pkg{timescape}.
#'
#' @param dataSet a ctgPHYLset object
#' @param outputDir the directory where output should be saved, defaults to
#' the current working directory.
#' @return an updated ctgPHYLset object
#' @keywords internal

makeTimescape <- function(dataSet, outputDir = ".") {
    # retrieve timescape data (returns NULL if nothing there)
    scapedata <- timescapeData(dataSet)
    # retrieve optional parameters that have default values
    mutations <- scapedata[["mutations"]]
    clone_colours <- scapedata[["clone_colours"]]
    xaxis_title <- scapedata[["xaxis_title"]]
    yaxis_title <- scapedata[["yaxis_title"]]
    phylogeny_title <- scapedata[["phylogeny_title"]]
    alpha <- scapedata[["alpha"]]
    genotype_position <- scapedata[["genotype_position"]]
    perturbations <- scapedata[["perturbations"]]
    sort <- scapedata[["sort"]]
    show_warnings <- scapedata[["show_warnings"]]
    width <- scapedata[["width"]]
    height <- scapedata[["height"]]
    clonal_prev <- scapedata[["clonal_prev"]]
    tree_edges <- scapedata[["tree_edges"]]

    # fill in defaults for optional parameters that aren't provided
    if (is.null(mutations))
        mutations <- "NA"
    if (is.null(clone_colours))
        clone_colours <- "NA"
    if (is.null(xaxis_title))
        xaxis_title <- "Time Point"
    if (is.null(yaxis_title))
        yaxis_title <- "Clonal Prevalence"
    if (is.null(phylogeny_title))
        phylogeny_title <- "Clonal Phylogeny"
    if (is.null(alpha))
        alpha <- 50
    if (is.null(genotype_position))
        genotype_position <- "stack"
    if (is.null(perturbations))
        perturbations <- "NA"
    if (is.null(sort))
        sort <- FALSE
    if (is.null(show_warnings))
        show_warnings <- TRUE
    if (is.null(width))
        width <- 900
    if (is.null(height))
        height <- NULL
    if (is.null(clonal_prev))
        clonal_prev <- NULL
    if (is.null(tree_edges))
        tree_edges <- NULL

    #format the filename
    filename <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")

    # run timescape
    tscape <- timescape::timescape(
        clonal_prev = clonal_prev,
        tree_edges = tree_edges,
        mutations = mutations,
        clone_colours = clone_colours,
        xaxis_title = xaxis_title,
        yaxis_title = yaxis_title,
        phylogeny_title = phylogeny_title,
        alpha = alpha,
        genotype_position = genotype_position,
        perturbations = perturbations,
        sort = sort,
        show_warnings = show_warnings,
        width = width,
        height = height
    )

    # store the result, and save the widget as an html file if htmlwidgets is
    # installed
    originalTrees(dataSet, "timescape") <- tscape
    if (requireNamespace("htmlwidgets", quietly = TRUE)) {
        htmlwidgets::saveWidget(tscape, file = file.path(
            outputDir,
            "CTG-Output",
            "Plots",
            paste0(filename, "timescape.html")
        ))
    }
    # convert data to standard cell tree format
    tree <- TS2CTF(tree_edges, filename, outputDir)
    treeList(dataSet, "timescape") <- tree2igraph(tree)
    return(dataSet)
}

# helper function to convert to SIF format
TS2CTF <- function(tree, fn, od = ".") {
    iTree <- igraph::graph_from_data_frame(tree)
    relationshipType <- "heuristic"
    cellEdges <- igraph::ends(iTree, es = igraph::E(iTree))
    relationships <-
        paste0(cellEdges[, 1], '\t', relationshipType, '\t',
                cellEdges[, 2])
    fileName <- file.path(od,"CTG-Output","SIFs",
                              paste0(fn,"_TS_CTF.sif"))
    write(relationships, fileName)
    relationships
}
