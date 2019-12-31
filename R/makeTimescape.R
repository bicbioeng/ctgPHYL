#' A Cell Tree Generating Function using timescape
#'
#' This function, called by \code{\link{generate_tree}}, creates visualizations
#' using the data and \pkg{timescape}.
#'
#' @param dataSet a ctgPHYLset object
#' @return an updated ctgPHYLset object
#' @keywords internal

makeTimescape <- function(dataSet) {
    # check if package is installed
    if (!requireNamespace("timescape", quietly = TRUE)) {
        stop(
            "Package 'timescape' is required for treeType = 'timescape',
            but is not installed.  See vignette for details on installing
            'timescape'",
            call. = FALSE
        )
    }

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
    filename <- as.character(Sys.time())
    filename <- gsub("/", "-", filename)
    filename <- gsub(":", "-", filename)
    filename <- gsub(" ", "_", filename)

    #TODO figure out why this isn't plotting in Rstudio the way it should
    # probably something to do with usage of htmlwidgets
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
            getwd(),
            "CTG-Output",
            "Plots",
            paste0(filename, "timescape.html")
        ))
    }
    # convert data to standard cell tree format
    #TODO uncomment the following two lines when TS2CTF works
    # tree <- TS2CTF(output.tree, filename)
    # treeList(dataSet, "timescape") <- tree2igraph(tree)
    return(dataSet)
}

# helper function to convert to SIF format
TS2CTF <- function(tree, fn) {
    #TODO Figure out how to convert timescape input to a tree, then how
    # to convert that tree to igraph and assign it to iTree
    iTree <- NULL
    #TODO Determine what type of relationship the nodes in the tree have to
    # one another
    relationshipType <- ""
    cellEdges <- igraph::ends(iTree, es = igraph::E(iTree))
    relationships <-
        paste0(cellEdges[, 1], '\t', relationshipType, '\t',
               cellEdges[, 2])
    fullFileName <- paste0("./CTG-Output/SIFs/", fn, "_CS_CTF.sif")
    write(relationships, fullFileName)
    relationships
}
