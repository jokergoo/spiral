
#' @importFrom stats is.leaf
#' @import ComplexHeatmap
construct_dend_segments = function(dend, gp = gpar()) {

    if(is.null(attr(dend, "x"))) {
        dend = ComplexHeatmap::adjust_dend_by_x(dend)
    }

    x_is_unit = inherits(attr(dend, "x"), "unit")
    height_is_unit = inherits(attr(dend, "height"), "unit")

    env = new.env(parent = emptyenv())
    env$x0 = NULL
    env$y0 = NULL
    env$x1 = NULL
    env$y1 = NULL
    env$col = NULL
    env$lty = NULL
    env$lwd = NULL

    generate_children_dendrogram_segments = function(dend, env = NULL) {

        if(is.leaf(dend)) {
            
            return(NULL)
        }

        height = attr(dend, "height")
        height_is_zero = abs(height - 0) < 1e-10
        
        nc = length(dend)
        
        xl = lapply(seq_len(nc), function(i) attr(dend[[i]], "x"))
        yl = lapply(seq_len(nc), function(i) attr(dend[[i]], "height"))
        xl = unlist(xl)
        yl = unlist(yl)
        
        max_x = max(xl)
        min_x = min(xl)
        mid_x = (max_x + min_x)*0.5

        # graphic parameters for current branch
        edge_gp_list = lapply(seq_len(nc), function(i) as.list(attr(dend[[i]], "edgePar")))
        for(i in c(setdiff(seq_len(nc), c(1, nc)), c(1, nc))) {
            for(gp_name in c("col", "lwd", "lty")) {
                # gp for two segments
                if(is.null(edge_gp_list[[i]][[gp_name]])) {
                    gpa = rep(get.gpar(gp_name)[[gp_name]], 2)
                } else {
                    gpa = rep(edge_gp_list[[i]][[gp_name]], 2)
                }

                env[[gp_name]] = c(env[[gp_name]], gpa)
            }
           
            if(height_is_zero) {
                
                env$x0 = c(env$x0, xl[i])
                env$x1 = c(env$x1, mid_x)
                env$y0 = c(env$y0, height)
                env$y1 = c(env$y1, height)
                
            } else {
                env$x0 = c(env$x0, xl[i], xl[i])
                env$x1 = c(env$x1, xl[i], mid_x)
                env$y0 = c(env$y0, yl[i], height)
                env$y1 = c(env$y1, height, height)
            }
        }
    }

    # per depth
    if(is.leaf(dend)) {
        return(list())
    }

    dend_list = list(dend)

    env$node_x = c(env$node_x, attr(dend, "x"))
    env$node_y = c(env$node_y, attr(dend, "height"))

    while(1) {

        if(length(dend_list) == 0) break

        for(i in seq_along(dend_list)) {
            generate_children_dendrogram_segments(dend_list[[i]], env)
        }

        # on their children nodes for non-leaf nodes
        dend_list = dend_list[ !sapply(dend_list, is.leaf) ]
        dend_list2 = list()
        for(i in seq_along(dend_list)) {
            dend_list2 = append(dend_list2, dend_list[[i]])
        }
        dend_list = dend_list2
    }
    
    lt = as.list(env)

    if("col" %in% names(gp)) {
        lt$col = gp$col
    }
    if("lwd" %in% names(gp)) {
        lt$lwd = gp$lwd
    }
    if("lty" %in% names(gp)) {
        lt$lty = gp$lty
    }
    return(lt)

}

#' Draw dendrogram
#'
#' @param dend A [`stats::dendrogram`] object.
#' @param gp Graphical parameters of the dendrogram edges, mainly as a global setting.
#' @param track_index Index of the track. 
#'
#' @details
#' Graphical parameters for individual edges can be set via the `edgePar` attribute on each node in the dendrogram, see [`stats::dendrogram`]
#' for how to set `edgePar`.
#' 
#' The dendrogram edges can also be rendered by [`dendextend::color_branches()`].
#'
#' @return Height of the dendrogram.
#' @export
#' @examples
#' k = 500
#' dend = as.dendrogram(hclust(dist(runif(k))))
#' spiral_initialize(xlim = c(0, k), start = 360, end = 360*3)
#' spiral_track(height = 0.8, background_gp = gpar(fill = "#EEEEEE", col = NA))
#' spiral_dendrogram(dend)
#'
#' \donttest{
#' require(dendextend)
#' dend = color_branches(dend, k = 4)
#' spiral_initialize(xlim = c(0, k), start = 360, end = 360*3)
#' spiral_track(height = 0.8, background_gp = gpar(fill = "#EEEEEE", col = NA))
#' spiral_dendrogram(dend)
#' }
spiral_dendrogram = function(dend, gp = gpar(), track_index = current_track_index()) {

    lt = construct_dend_segments(dend, gp)

    xlim = range(lt[c("x0", "x1")])
        
    yrange = abs(get_track_data("ymax", track_index))

    ymax2 = max(lt$y0, lt$y1)

    lt$y0 = lt$y0/ymax2 * yrange
    lt$y1 = lt$y1/ymax2 * yrange
   
    spiral_segments(lt$x0, lt$y0, lt$x1, lt$y1, gp = gpar(lwd = lt$lwd, lty = lt$lty, col = lt$col), track_index = track_index)   

    invisible(ymax2)   
}


############# for phylo object ############

#' @importFrom circlize rand_color
construct_phylo_segments = function(obj, group = NULL, group_col = NULL) {
    if(!inherits(obj, "phylo")) {
        stop_wrap("The input should be a 'phylo' object.")
    }

    edge = obj$edge
    edge.length = obj$edge.length
    n_nodes = nrow(edge) + 1
    n_leaves = length(obj$tip.label)
    n_edges = nrow(edge)

    # position of all nodes, the index is the same as in `edge`
    node_pos = rep(-1, n_nodes)
    node_pos[1:n_leaves] = 1:n_leaves

    e = new.env(parent = emptyenv())
    while(1) {
        e$flag = TRUE
        # edge[, 1] is the parent node of each link
        tapply(1:nrow(edge), edge[, 1], function(ind) {
            # m contains one parent and its children nodes
            m = edge[ind, , drop = FALSE]
            # if the position for the parent is not set
            if(node_pos[ m[1, 1] ] < 0) {
                # if all its children have set the positions
                if(all(node_pos[ m[, 2] ] > 0)) {
                    node_pos[ m[1, 1] ] <<- mean(node_pos[ m[, 2] ])
                } else {
                    e$flag = FALSE
                }
            }
        })
        if(e$flag) break
    }

    # next calculate the height of each node
    node_height = rep(-1, n_nodes)
    node_height[1:n_leaves] = 0

    edge.length[is.na(edge.length)] = 0

    while(1) {
        e$flag = TRUE
        tapply(1:nrow(edge), edge[, 1], function(ind) {
            m = edge[ind, , drop = FALSE]
            if(node_height[ m[1, 1] ] < 0) {
                if(all(node_height[ m[, 2] ] >= 0)) {
                    node_height[ m[1, 1] ] <<- max(node_height[ m[, 2] ] + edge.length[ind])
                } else {
                    e$flag = FALSE
                }
            }
        })
        if(e$flag) break
    }

    # next assign colors for edges
    default_col = get.gpar("col")$col
    if(!is.null(group)) {
        if(length(group) != n_leaves) {
            stop_wrap("Length of `group` should be the same as number of leaves in the tree.")
        }
        group = as.character(group)
        if(is.null(group_col)) {
            levels = unique(group)
            group_col = rand_color(length(levels))
            names(group_col) = levels
        }

        # form simplicity, we first calculate colors for nodes
        node_col = rep(NA, n_nodes)
        gc = group_col[group]
        gc[is.na(gc)] = default_col
        node_col[1:n_leaves] = gc

        while(1) {
            e$flag = TRUE
            tapply(1:nrow(edge), edge[, 1], function(ind) {
                m = edge[ind, , drop = FALSE]
                if(is.na(node_col[ m[1, 1] ])) {
                    if(all(!is.na(node_col[ m[, 2] ]))) {
                        if(length(unique(node_col[ m[, 2] ])) == 1) {
                            node_col[ m[1, 1] ] <<- node_col[ m[1, 2] ]
                        } else {
                            node_col[ m[1, 1] ] <<- default_col
                        }
                    } else {
                        e$flag = FALSE
                    }
                }
            })
            if(e$flag) break
        }
    } else {
         node_col = rep(default_col, n_nodes)
    }

    lt = list(x0 = numeric(n_edges*2), y0 = numeric(n_edges*2), 
        x1 = numeric(n_edges*2), y1 = numeric(n_edges*2),
        col = character(n_edges*2))
    for(i in seq_len(n_edges)) {
        i1 = edge[i, 1]
        i2 = edge[i, 2]

        lt$x0[2*i + c(-1, 0)] = c(node_pos[i1], node_pos[i2])
        lt$y0[2*i + c(-1, 0)] = c(node_height[i1], node_height[i1])
        lt$x1[2*i + c(-1, 0)] = c(node_pos[i2], node_pos[i2])
        lt$y1[2*i + c(-1, 0)] = c(node_height[i1], node_height[i2])
        lt$col[2*i + c(-1, 0)] = c(node_col[i1], node_col[i2])
    }

    lt

}

#' Draw phylogenetic tree
#'
#' @param obj A [`stats::dendrogram`] object.
#' @param gp Graphical parameters of the tree edges, mainly as a global setting.
#' @param log Whether the height of the tree to be log-transformed `log10(x + 1)`?
#' @param reverse Whether the tree to be reversed?
#' @param group A categorical variable for splitting the tree.
#' @param group_col A named vector which contains group colors.
#' @param track_index Index of the track. 
#'
#' @return Height of the phylogenetic tree.
#' @export
#' @examples
#' require(ape)
#' data(bird.families)
#' n = length(bird.families$tip.label)
#' spiral_initialize(xlim = c(0, n), start = 360, end = 360*3)
#' spiral_track(height = 0.8)
#' spiral_phylo(bird.families)
spiral_phylo = function(obj, gp = gpar(), log = FALSE, reverse = FALSE, 
    group = NULL, group_col = NULL, track_index = current_track_index()) {

    lt = construct_phylo_segments(obj, group = group, group_col = group_col)

    if(log) {
        lt$y0 = log10(lt$y0 + 1)
        lt$y1 = log10(lt$y1 + 1)
        lt$y0[is.infinite(lt$y0)] = 0
        lt$y1[is.infinite(lt$y1)] = 0
        lt$y0[lt$y0 < 0] = 0
        lt$y1[lt$y1 < 0] = 0
    }

    yrange = abs(get_track_data("yrange", track_index))
    
    ymax2 = max(lt$y0, lt$y1)

    lt$y0 = lt$y0/ymax2 * yrange
    lt$y1 = lt$y1/ymax2 * yrange

    lt$x0 = lt$x0 - 0.5
    lt$x1 = lt$x1 - 0.5

    if(reverse) {
        spiral = spiral_env$spiral
        lt$x0 = spiral$xlim[2] - lt$x0
        lt$x1 = spiral$xlim[2] - lt$x1
    }

    gp$col = lt$col
    spiral_segments(lt$x0, lt$y0, lt$x1, lt$y1, gp = gp, track_index = track_index)

    invisible(ymax2)
}

#' @details
#' [`phylo_to_dendrogram()`] converts a `phylo` object to a `dendrogram` object.
#' 
#' The motivation is that phylogenetic tree may contain polytomies, which means at a certain node,
#' there are more than two children branches. Available tools that do the conversion only support binary trees.
#'
#' The returned `dendrogram` object is not in its standard format which means it can not be properly
#' drawn by the [`stats::plot.dendrogram()`] function. However, you can still apply [`stats::cutree()`] to the returned
#' `dendrogram` object with no problem and the dendrogram can be properly drawn with the **ComplexHeatmap** package (see examples).
#'
#' @return A [`stats::dendrogram`] object.
#' @rdname spiral_phylo
#' @export
#' @examples
#' require(ape)
#' data(bird.families)
#' d = phylo_to_dendrogram(bird.families)
#'
#' ComplexHeatmap::grid.dendrogram(d, test = TRUE)
phylo_to_dendrogram = function(obj, log = FALSE) {
    if(!inherits(obj, "phylo")) {
        stop_wrap("The input should be a 'phylo' object.")
    }

    edge = obj$edge
    edge.length = obj$edge.length
    n_nodes = nrow(edge) + 1
    n_leaves = length(obj$tip.label)
    n_edges = nrow(edge)

    node_pos = rep(-1, n_nodes)
    node_pos[1:n_leaves] = 1:n_leaves

    e = new.env(parent = emptyenv())
    while(1) {
        e$flag = TRUE
        tapply(1:nrow(edge), edge[, 1], function(ind) {
            m = edge[ind, , drop = FALSE]
            if(node_pos[ m[1, 1] ] < 0) {
                if(all(node_pos[ m[, 2] ] > 0)) {
                    node_pos[ m[1, 1] ] <<- mean(node_pos[ m[, 2] ])
                } else {
                    e$flag = FALSE
                }
            }
        })
        if(e$flag) break
    }

    node_height = rep(-1, n_nodes)
    node_height[1:n_leaves] = 0

    edge.length[is.na(edge.length)] = 0

    while(1) {
        e$flag = TRUE
        tapply(1:nrow(edge), edge[, 1], function(ind) {
            m = edge[ind, , drop = FALSE]
            if(node_height[ m[1, 1] ] < 0) {
                if(all(node_height[ m[, 2] ] >= 0)) {
                    node_height[ m[1, 1] ] <<- max(node_height[ m[, 2] ] + edge.length[ind])
                } else {
                    e$flag = FALSE
                }
            }
        })
        if(e$flag) break
    }

    if(log) {
        node_height = log10(node_height + 1)
        node_height[is.infinite(node_height)] = 0
        node_height[node_height < 0] = 0
    }

    lt_ind = vector("list", length = nrow(edge) + 1)
    dend = list()
    attributes(dend) = list(
        class = "dendrogram",
        node_id = edge[1, 1],
        height = node_height[edge[1, 1]],
        x = node_pos[edge[1, 1]],
        members = n_leaves,
        code = "0"
    )
    for(i in seq_len(nrow(edge))) {
        parent = edge[i, 1]
        children = edge[i, 2]
        if( parent == edge[1, 1] ) {
            i_children = length(dend)
        } else {
            i_children = length(dend[[ lt_ind[[parent]] ]])
        }

        lt_ind[[ children ]] = c(lt_ind[[parent]], i_children + 1)
        if(children <= n_leaves) {
            dend[[ lt_ind[[ children ]] ]]= children
            attributes(dend[[ lt_ind[[ children ]] ]]) = list(
                class = "dendrogram",
                leaf = TRUE,
                node_id = children,
                height = node_height[ children ],
                x = node_pos[ children ],
                members = 1,
                label = children,
                code = paste(c(0, lt_ind[[ children ]]), collapse = "")
            )
        } else {
            dend[[ lt_ind[[ children ]] ]]= list()
            attributes(dend[[ lt_ind[[ children ]] ]]) = list(
                class = "dendrogram",
                node_id = children,
                height = node_height[ children ],
                x = node_pos[ children ],
                code = paste(c(0, lt_ind[[ children ]]), collapse = "")
            )
        }
    }

    dend
}
