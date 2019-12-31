#' A Cell Tree Generating Function using mapscape
#'
#' This function, called by \code{\link{generate_tree}}, creates visualizations
#' using the data and \pkg{mapscape}.
#'
#' @param dataSet a ctgPHYLset object
#' @return an updated ctgPHYLset object
#' @keywords internal

makeMapscape <- function(dataSet) {
    # check if package is installed
    if (!requireNamespace("mapscape", quietly = TRUE)) {
        stop(
            "Package 'mapscape' is required for treeType = 'mapscape',
            but is not installed.  See vignette for details on installing
            'mapscape'",
            call. = FALSE
        )
    }

    # retrieve mapscape data (returns NULL if nothing there)
    scapedata <- mapscapeData(dataSet)
    # retrieve optional parameters that have default values
    clone_colours <- scapedata[["clone_colours"]]
    mutations <- scapedata[["mutations"]]
    sample_ids <- scapedata[["sample_ids"]]
    n_cells <- scapedata[["n_cells"]]
    show_low_prev_gtypes <- scapedata[["show_low_prev_gtypes"]]
    phylogeny_title <- scapedata[["phylogeny_title"]]
    anatomy_title <- scapedata[["anatomy_title"]]
    classification_title <- scapedata[["classification_title"]]
    show_warnings <- scapedata[["show_warnings"]]
    width <- scapedata[["width"]]
    height <- scapedata[["height"]]

    # fill in defaults for optional parameters that aren't provided
    if (is.null(clone_colours))
        clone_colours <- "NA"
    if (is.null(mutations))
        mutations <- "NA"
    if (is.null(sample_ids))
        sample_ids <- c("NA")
    if (is.null(n_cells))
        n_cells <- 100
    if (is.null(phylogeny_title))
        phylogeny_title <- "Clonal Phylogeny"
    if (is.null(anatomy_title))
        anatomy_title <- "Anatomy"
    if (is.null(classification_title))
        classification_title <- "Phylogenetic Classification"
    if (is.null(show_warnings))
        show_warnings <- TRUE
    if (is.null(width))
        width <- 960
    if (is.null(height))
        height <- 960
    if (is.null(show_low_prev_gtypes))
        show_low_prev_gtypes <- FALSE

    # null catching
    clonal_prev <- scapedata[["clonal_prev"]]
    tree_edges <- scapedata[["tree_edges"]]
    sample_locations <- scapedata[["sample_locations"]]
    img_ref <- scapedata[["img_ref"]]

    if (is.null(clonal_prev))
        clonal_prev <- NULL
    if (is.null(tree_edges))
        tree_edges <- NULL
    if (is.null(sample_locations))
        sample_locations <- NULL
    if (is.null(img_ref))
        img_ref <- NULL

    #format the filename
    filename <- as.character(Sys.time())
    filename <- gsub("/", "-", filename)
    filename <- gsub(":", "-", filename)
    filename <- gsub(" ", "_", filename)

    #TODO figure out why this isn't plotting in Rstudio the way it should
    # probably something to do with usage of htmlwidgets
    # run mapscape
    mscape <- mapscape::mapscape(
        clonal_prev = clonal_prev,
        tree_edges = tree_edges,
        sample_locations = sample_locations,
        img_ref = img_ref,
        clone_colours = clone_colours,
        mutations = mutations,
        sample_ids = sample_ids,
        n_cells = n_cells,
        show_low_prev_gtypes = show_low_prev_gtypes,
        phylogeny_title = phylogeny_title,
        anatomy_title = anatomy_title,
        classification_title = classification_title,
        show_warnings = show_warnings,
        width = width,
        height = height
    )

    # store the result, and save the widget as an html file if htmlwidgets is
    # installed
    originalTrees(dataSet, "mapscape") <- mscape
    if (requireNamespace("htmlwidgets", quietly = TRUE)) {
        htmlwidgets::saveWidget(mscape, file = file.path(
            getwd(),
            "CTG-Output",
            "Plots",
            paste0(filename, "mapscape.html")
        ))
    }
    # convert data to standard cell tree format
    #TODO uncomment the following two lines when MS2CTF works
    # tree <- MS2CTF(output.tree, filename)
    # treeList(dataSet, "mapscape") <- tree2igraph(tree)
    return(dataSet)
}

# helper function to convert to SIF format
MS2CTF <- function(tree, fn) {
    #TODO Figure out how to convert mapscape input to a tree, then how
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
