#' A Cell Tree Generating Function using cellscape
#'
#' This function, called by \code{\link{generate_tree}}, creates visualizations
#' using the data and \pkg{cellscape}.
#'
#' @param dataSet a ctgPHYLset object
#' @return an updated ctgPHYLset object
#' @keywords internal

makeCellscape <- function(dataSet) {
    # check if package is installed
    if (!requireNamespace("cellscape", quietly = TRUE)) {
        stop(
            "Package 'cellscape' is required for treeType = 'cellscape',
            but is not installed.  See vignette for details on installing
            'cellscape'",
            call. = FALSE
        )
    }

    # retrieve cellscape data (returns NULL if nothing there)
    scapedata <- cellscapeData(dataSet)
    # retrieve optional parameters that have default values
    clone_colours <- scapedata[["clone_colours"]]
    timepoint_title <- scapedata[["timepoint_title"]]
    clone_title <- scapedata[["clone_title"]]
    xaxis_title <- scapedata[["xaxis_title"]]
    yaxis_title <- scapedata[["yaxis_title"]]
    phylogeny_title <- scapedata[["phylogeny_title"]]
    node_type <- scapedata[["node_type"]]
    display_node_ids <- scapedata[["display_node_ids"]]
    prop_of_clone_threshold <-
        scapedata[["prop_of_clone_threshold"]]
    vaf_threshold <- scapedata[["vaf_threshold"]]
    show_warnings <- scapedata[["show_warnings"]]
    width <- scapedata[["width"]]
    height <- scapedata[["height"]]

    cnv_data <- scapedata[["cnv_data"]]
    mut_data <- scapedata[["mut_data"]]
    mut_data_matrix <- scapedata[["mut_data_matrix"]]
    mut_order <- scapedata[["mut_order"]]
    tree_edges <- scapedata[["tree_edges"]]
    gtype_tree_edges <- scapedata[["gtype_tree_edges"]]
    sc_annot <- scapedata[["sc_annot"]]
    value_type <- scapedata[["value_type"]]


    if (is.null(cnv_data))
        cnv_data <- NULL
    if (is.null(mut_data))
        mut_data <- NULL
    if (is.null(mut_data_matrix))
        mut_data_matrix <- NULL
    if (is.null(mut_order))
        mut_order <- NULL
    if (is.null(tree_edges))
        tree_edges <- NULL
    if (is.null(gtype_tree_edges))
        gtype_tree_edges <- NULL
    if (is.null(sc_annot))
        sc_annot <- NULL
    if (is.null(value_type))
        value_type <- NULL


    # fill in defaults for optional parameters that aren't provided
    if (is.null(clone_colours))
        clone_colours <- "NA"
    if (is.null(timepoint_title))
        timepoint_title <- "Timepoint"
    if (is.null(clone_title))
        clone_title <- "Clone"
    if (is.null(xaxis_title))
        xaxis_title <- "Time Point"
    if (is.null(yaxis_title))
        yaxis_title <- "Clonal Prevalence"
    if (is.null(phylogeny_title))
        phylogeny_title <- "Clonal Phylogeny"
    if (is.null(node_type))
        node_type <- "Cell"
    if (is.null(display_node_ids))
        display_node_ids <- FALSE
    if (is.null(prop_of_clone_threshold))
        prop_of_clone_threshold <- 0.2
    if (is.null(vaf_threshold))
        vaf_threshold <- 0.05
    if (is.null(show_warnings))
        show_warnings <- TRUE
    if (is.null(width))
        width <- 900
    if (is.null(height))
        height <- 800

    #format the filename
    filename <- as.character(Sys.time())
    filename <- gsub("/", "-", filename)
    filename <- gsub(":", "-", filename)
    filename <- gsub(" ", "_", filename)

    #TODO figure out why this isn't plotting in Rstudio the way it should
    # probably something to do with usage of htmlwidgets
    # run cellscape
    if (!is.null(cnv_data)) {
        if (!is.null(gtype_tree_edges)) {
            cscape <- cellscape::cellscape(
                cnv_data = cnv_data,
                mut_order = mut_order,
                tree_edges = tree_edges,
                gtype_tree_edges = gtype_tree_edges,
                sc_annot = sc_annot,
                value_type = value_type,
                clone_colours = clone_colours,
                timepoint_title = timepoint_title,
                clone_title = clone_title,
                xaxis_title = xaxis_title,
                yaxis_title = yaxis_title,
                phylogeny_title = phylogeny_title,
                node_type = node_type,
                display_node_ids = display_node_ids,
                prop_of_clone_threshold = prop_of_clone_threshold,
                vaf_threshold = vaf_threshold,
                show_warnings = show_warnings,
                width = width,
                height = height
            )
        } else {
            cscape <- cellscape::cellscape(
                cnv_data = cnv_data,
                mut_order = mut_order,
                tree_edges = tree_edges,
                sc_annot = sc_annot,
                value_type = value_type,
                clone_colours = clone_colours,
                timepoint_title = timepoint_title,
                clone_title = clone_title,
                xaxis_title = xaxis_title,
                yaxis_title = yaxis_title,
                phylogeny_title = phylogeny_title,
                node_type = node_type,
                display_node_ids = display_node_ids,
                prop_of_clone_threshold = prop_of_clone_threshold,
                vaf_threshold = vaf_threshold,
                show_warnings = show_warnings,
                width = width,
                height = height
            )
        }
    } else if (!is.null(mut_data)) {
        if (!is.null(gtype_tree_edges)) {
            cscape <- cellscape::cellscape(
                mut_data = mut_data,
                mut_order = mut_order,
                tree_edges = tree_edges,
                gtype_tree_edges = gtype_tree_edges,
                sc_annot = sc_annot,
                value_type = value_type,
                clone_colours = clone_colours,
                timepoint_title = timepoint_title,
                clone_title = clone_title,
                xaxis_title = xaxis_title,
                yaxis_title = yaxis_title,
                phylogeny_title = phylogeny_title,
                node_type = node_type,
                display_node_ids = display_node_ids,
                prop_of_clone_threshold = prop_of_clone_threshold,
                vaf_threshold = vaf_threshold,
                show_warnings = show_warnings,
                width = width,
                height = height
            )
        } else {
            cscape <- cellscape::cellscape(
                mut_data = mut_data,
                mut_order = mut_order,
                tree_edges = tree_edges,
                sc_annot = sc_annot,
                value_type = value_type,
                clone_colours = clone_colours,
                timepoint_title = timepoint_title,
                clone_title = clone_title,
                xaxis_title = xaxis_title,
                yaxis_title = yaxis_title,
                phylogeny_title = phylogeny_title,
                node_type = node_type,
                display_node_ids = display_node_ids,
                prop_of_clone_threshold = prop_of_clone_threshold,
                vaf_threshold = vaf_threshold,
                show_warnings = show_warnings,
                width = width,
                height = height
            )
        }
    } else {
        if (!is.null(gtype_tree_edges)) {
            cscape <- cellscape::cellscape(
                mut_data_matrix = mut_data_matrix,
                mut_order = mut_order,
                tree_edges = tree_edges,
                gtype_tree_edges = gtype_tree_edges,
                sc_annot = sc_annot,
                value_type = value_type,
                clone_colours = clone_colours,
                timepoint_title = timepoint_title,
                clone_title = clone_title,
                xaxis_title = xaxis_title,
                yaxis_title = yaxis_title,
                phylogeny_title = phylogeny_title,
                node_type = node_type,
                display_node_ids = display_node_ids,
                prop_of_clone_threshold = prop_of_clone_threshold,
                vaf_threshold = vaf_threshold,
                show_warnings = show_warnings,
                width = width,
                height = height
            )
        } else {
            cscape <- cellscape::cellscape(
                mut_data_matrix = mut_data_matrix,
                mut_order = mut_order,
                tree_edges = tree_edges,
                sc_annot = sc_annot,
                value_type = value_type,
                clone_colours = clone_colours,
                timepoint_title = timepoint_title,
                clone_title = clone_title,
                xaxis_title = xaxis_title,
                yaxis_title = yaxis_title,
                phylogeny_title = phylogeny_title,
                node_type = node_type,
                display_node_ids = display_node_ids,
                prop_of_clone_threshold = prop_of_clone_threshold,
                vaf_threshold = vaf_threshold,
                show_warnings = show_warnings,
                width = width,
                height = height
            )
        }
    }
    # store the result, and save the widget as an html file if htmlwidgets is
    # installed
    originalTrees(dataSet, "cellscape") <- cscape
    if (requireNamespace("htmlwidgets", quietly = TRUE)) {
        htmlwidgets::saveWidget(
            cscape,
            file = file.path(
                getwd(),
                "CTG-Output",
                "Plots",
                paste0(filename, "cellscape.html")
            ),
            selfcontained = TRUE
        )
    }
    # convert data to standard cell tree format
    tree <- CS2CTF(tree_edges, filename)
    treeList(dataSet, "cellscape") <- tree2igraph(tree)
    return(dataSet)
}

# helper function to convert to SIF format
CS2CTF <- function(tree, fn) {
    iTree <- igraph::graph_from_data_frame(tree)
    relationshipType <- "heuristic"
    cellEdges <- igraph::ends(iTree, es = igraph::E(iTree))
    relationships <-
        paste0(cellEdges[, 1], '\t', relationshipType, '\t',
                cellEdges[, 2])
    fullFileName <- paste0("./CTG-Output/SIFs/", fn, "_CS_CTF.sif")
    write(relationships, fullFileName)
    relationships
}
