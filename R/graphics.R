

#' Add points to a track
#'
#' @param x X-locations of the data points.
#' @param y Y-locations of the data points.
#' @param pch Point type.
#' @param size Size of the points. Value should be a [`grid::unit()`] object.
#' @param gp Graphical parameters.
#' @param track_index Index of the track. 
#'
#' @return
#' No value is returned.
#' @export
#' @examples
#' spiral_initialize()
#' spiral_track()
#' spiral_points(x = runif(1000), y = runif(1000))
spiral_points = function(x, y, pch = 1, size = unit(0.4, "char"), gp = gpar(), 
	track_index = current_track_index()) {

	validate_xy(x, y)

	spiral = spiral_env$spiral
	x = spiral$get_x_from_data(x)

	df = xy_to_cartesian(x, y, track_index = track_index)
	x = unit(df$x, "native")
	y = unit(df$y, "native")

	grid.points(x, y, pch = pch, size = size, gp = gp)
}

#' Add lines to a track
#'
#' @param x X-locations of the data points.
#' @param y Y-locations of the data points.
#' @param type Type of the line. Value should be one of "l" and "h". When the value is "h", vertical lines (or radial lines if you consider the polar coordinates) relative to the baseline will be drawn.
#' @param gp Graphical parameters.
#' @param baseline Baseline used when `type` is `"l"` or `area` is `TRUE`.
#' @param area Whether to draw the area under the lines? Note `gpar(fill = ...)` controls the filled colors of the areas.
#' @param track_index Index of the track. 
#'
#' @return
#' No value is returned.
#' @export
#' @examples
#' x = sort(runif(1000))
#' y = runif(1000)
#' spiral_initialize()
#' spiral_track()
#' spiral_lines(x, y)
#'
#' spiral_initialize()
#' spiral_track()
#' spiral_lines(x, y, type = "h")
#'
#' spiral_initialize()
#' spiral_track()
#' spiral_lines(x, y, area = TRUE, gp = gpar(fill = "red", col = NA))
spiral_lines = function(x, y, type = "l", gp = gpar(),
	baseline = "bottom", area = FALSE, track_index = current_track_index()) {

	validate_xy(x, y)

	spiral = spiral_env$spiral
	x = spiral$get_x_from_data(x)

	if(baseline == "bottom") {
		baseline = get_track_data("ymin", track_index)
	} else if(baseline == "top") {
		baseline = get_track_data("ymax", track_index)
	}

	if(type == "l") {
		if(area) {
			if(!"fill" %in% names(gp)) {
				gp$fill = 2
			}
			n = length(x)
			x = c(x, x[n], x[1])
			y = c(y, baseline, baseline)
			spiral_polygon(x, y, gp = gp, track_index = track_index)
		} else {
			df = spiral_lines_expand(x, y, track_index = track_index)
			grid.lines(df$x, df$y, default.units = "native", gp = gp)
		}
	}
	if(type == "h") {
		df1 = xy_to_cartesian(x, y, track_index = track_index)
		df2 = xy_to_cartesian(x, baseline, track_index = track_index)
		if(!"lineend" %in% names(gp)) {
			gp$lineend = "butt"
		}
		grid.segments(df1$x, df1$y, df2$x, df2$y, default.units = "native", gp = gp)
	}

}

#' Add segments to a track
#'
#' @param x0 X-locations of the start points of the segments.
#' @param y0 Y-locations of the start points of the segments.
#' @param x1 X-locations of the end points of the segments.
#' @param y1 Y-locations of the end points of the segments.
#' @param gp Graphical parameters.
#' @param arrow A [`grid::arrow()`] object.
#' @param track_index Index of the track. 
#'
#' @return
#' No value is returned.
#' @export
#' @examples
#' n = 1000
#' x0 = runif(n)
#' y0 = runif(n)
#' x1 = x0 + runif(n, min = -0.01, max = 0.01)
#' y1 = 1 - y0
#'
#' spiral_initialize(xlim = range(c(x0, x1)))
#' spiral_track()
#' spiral_segments(x0, y0, x1, y1, gp = gpar(col = circlize::rand_color(n)))
#'
#' n = 100
#' x0 = runif(n)
#' y0 = runif(n)
#' x1 = x0 + runif(n, min = -0.01, max = 0.01)
#' y1 = 1 - y0
#'
#' spiral_initialize(xlim = range(c(x0, x1)))
#' spiral_track()
#' col = circlize::rand_color(n, luminosity = "bright")
#' spiral_segments(x0, y0, x1, y1, 
#'     arrow = arrow(length = unit(2, "mm")), gp = gpar(col = col))
#'
#' # if the segments are short and you want the straight "real" segments
#' spiral_initialize(xlim = range(c(x0, x1)))
#' spiral_track()
#' df0 = xy_to_cartesian(x0, y0)
#' df1 = xy_to_cartesian(x1, y1)
#' grid.segments(df0$x, df0$y, df1$x, df1$y, default.units = "native", 
#'     arrow = arrow(length = unit(2, "mm")), gp = gpar(col = col))
spiral_segments = function(x0, y0, x1, y1, gp = gpar(), arrow = NULL, 
	track_index = current_track_index()) {

	validate_xy(x0, y0, x1, y1)

	n1 = length(x0)
    n2 = length(y0)
    n3 = length(x1)
    n4 = length(y1)
    n = max(c(n1, n2, n3, n4))
    if(n1 == 1) x0 = rep(x0, n)
    if(n2 == 1) y0 = rep(y0, n)
    if(n3 == 1) x1 = rep(x1, n)
    if(n4 == 1) y1 = rep(y1, n)

    if(!is.null(arrow)) {
    	if(n > 1) {
    		for(i in 1:n) {
    			spiral_segments(x0[i], y0[i], x1[i], y1[i], gp = subset_gp(gp, i),
    				arrow = arrow, track_index = track_index)
    		}
    		return(invisible(NULL))
    	}
    }

	spiral = spiral_env$spiral
	x0 = spiral$get_x_from_data(x0)
	x1 = spiral$get_x_from_data(x1)
		
	if("col" %in% names(gp)) {
		if(length(gp$col) == 1) gp$col = rep(gp$col, n)
	} else {
		gp$col = rep(get.gpar("col")$col, n)
	}
	if("lwd" %in% names(gp)) {
		if(length(gp$lwd) == 1) gp$lwd = rep(gp$lwd, n)
	} else {
		gp$lwd = rep(get.gpar("lwd")$lwd, n)
	}
	if("lty" %in% names(gp)) {
		if(length(gp$lty) == 1) gp$lty = rep(gp$lty, n)
	} else {
		gp$lty = rep(get.gpar("lty")$lty, n)
	}

	buffer = 10000
	k = buffer
	ir = 0
	df_store = data.frame(x0 = rep(NA_real_, k), y0 = rep(NA_real_, k), x1 = rep(NA_real_, k), y1 = rep(NA_real_, k),
		col = vector(k, mode = mode(gp$col)), lwd = vector(k, mode = mode(gp$lwd)), lty = vector(k, mode = mode(gp$lty)))

	first_segment = last_segment = NULL

	for(i in seq_len(n)) {
		df = spiral_lines_expand(c(x0[i], x1[i]), c(y0[i], y1[i]), track_index = track_index)
		nb = nrow(df)
		df2 = data.frame(x0 = numeric(nb - 1), y0 = numeric(nb - 1), x1 = numeric(nb - 1), y1 = numeric(nb - 1))
		df2$x0 = df$x[1:(nb-1)]
		df2$y0 = df$y[1:(nb-1)]
		df2$x1 = df$x[2:nb]
		df2$y1 = df$y[2:nb]
		df2$col = gp$col[i]
		df2$lwd = gp$lwd[i]
		df2$lty = gp$lty[i]

		nb2 = nrow(df2)

		if(ir + nb2 > k) {
			# draw the segments
			df_store = df_store[!is.na(df_store$x0), , drop = FALSE]
			if(nrow(df_store)) {
				grid.segments(df_store$x0, df_store$y0, df_store$x1, df_store$y1, gp = gpar(col = df_store$col, lwd = df_store$lwd, lty = df_store$lty), default.units = "native")
				if(is.null(first_segment)) {
					first_segment = df_store[1, ]
				}
				last_segment = df_store[nrow(df_store), ]
			}

			ir = 0
			df_store = data.frame(x0 = rep(NA_real_, k), y0 = rep(NA_real_, k), x1 = rep(NA_real_, k), y1 = rep(NA_real_, k),
				col = character(k), lwd = integer(k), lty = integer(k))
		}

		if(nb2 > k) {
			grid.segments(df2$x0, df2$y0, df2$x1, df2$y1, gp = gpar(col = df2$col, lwd = df2$lwd, lty = df2$lty), default.units = "native")
			if(is.null(first_segment)) {
				first_segment = df2[1, ]
			}
			last_segment = df2[nrow(df2), ]
			next
		}

		ind = ir + 1:nb2
		df_store$x0[ind] = df2$x0
		df_store$y0[ind] = df2$y0
		df_store$x1[ind] = df2$x1
		df_store$y1[ind] = df2$y1
		df_store$col[ind] = df2$col
		df_store$lwd[ind] = df2$lwd
		df_store$lty[ind] = df2$lty
		ir = ir + nb2
		
	}
	if(ir > 0) {
		df_store = df_store[!is.na(df_store$x0), ]
		grid.segments(df_store$x0, df_store$y0, df_store$x1, df_store$y1, 
			gp = gpar(col = df_store$col, lwd = df_store$lwd, lty = df_store$lty), default.units = "native")
		if(is.null(first_segment)) {
			first_segment = df_store[1, ]
		}

		last_segment = df_store[nrow(df_store), ]
	}

	if(!is.null(arrow)) {
		if(arrow$ends %in% c(2, 3)) {  # last, both
			grid.segments(last_segment$x0, last_segment$y0, last_segment$x1, last_segment$y1, arrow = arrow,
				gp = gpar(col = last_segment$col, lwd = last_segment$lwd, lty = last_segment$lty), default.units = "native")
		} else if(arrow$ends %in% c(1, 3)) {
			grid.segments(first_segment$x1, first_segment$y1, first_segment$x0, first_segment$y0, arrow = arrow,
				gp = gpar(col = first_segment$col, lwd = first_segment$lwd, lty = first_segment$lty), default.units = "native")
		}
	}
	
}


spiral_radial_segments = function(x, y, offset, gp = gpar(), track_index = current_track_index()) {
	df = xy_to_cartesian(x, y, track_index = track_index)
	df2 = radial_extend(x, y, offset, track_index = track_index)

	grid.segments(df$x, df$y, df2$x, df2$y, default.units = "native", gp = gp)
}

#' Add rectangles to a track
#'
#' @param xleft X-locations of the left bottom of the rectangles.
#' @param ybottom Y-locations of the left bottom of the rectangles.
#' @param xright X-locations of the right top of the rectangles.
#' @param ytop Y-locations of the right top of the rectangles.
#' @param gp Graphical parameters.
#' @param track_index Index of the track. 
#'
#' @return
#' No value is returned.
#' @export
#' @examples
#' # to simulate heatmap
#' n = 1000
#' require(circlize)
#' col = circlize::colorRamp2(c(0, 0.5, 1), c("blue", "white", "red"))
#' spiral_initialize(xlim = c(0, n))
#' spiral_track(height = 0.9)
#'
#' x1 = runif(n)
#' spiral_rect(1:n - 1, 0, 1:n, 0.5, gp = gpar(fill = col(x1), col = NA))
#' x2 = runif(n)
#' spiral_rect(1:n - 1, 0.5, 1:n, 1, gp = gpar(fill = col(x2), col = NA))
spiral_rect = function(xleft, ybottom, xright, ytop, gp = gpar(), 
	track_index = current_track_index()) {

	validate_xy(xleft, ybottom, xright, ytop)
	
	spiral = spiral_env$spiral
	if(spiral$xclass %in% "Time") {
		xleft = spiral$get_x_from_data(xleft, "left")
		xright = spiral$get_x_from_data(xright, "right")
	} else {
		xleft = spiral$get_x_from_data(xleft)
		xright = spiral$get_x_from_data(xright)
	}

	n1 = length(xleft)
    n2 = length(ybottom)
    n3 = length(xright)
    n4 = length(ytop)
    n = max(c(n1, n2, n3, n4))
    if(n1 == 1) xleft = rep(xleft, n)
    if(n2 == 1) ybottom = rep(ybottom, n)
    if(n3 == 1) xright = rep(xright, n)
    if(n4 == 1) ytop = rep(ytop, n)

    x = NULL
	y = NULL
	id = NULL
	id_k = 0
	for(i in seq_len(n)) {

		x = c(x, c(xleft[i], xleft[i], xright[i], xright[i], xleft[i]))
		y = c(y, c(ybottom[i], ytop[i], ytop[i], ybottom[i], ybottom[i]))
		id_k = id_k + 1
		id = c(id, rep(id_k, 5))

	}

	spiral_polygon(x, y, id = id, gp = gp, track_index = track_index)
	
}

#' Add bars to a track
#'
#' @param pos X-locations of the center of bars.
#' @param value Height of bars. The value can be a simple numeric vector, or a matrix.
#' @param baseline Baseline of the bars. Note it only works when `value` is a simple vector.
#' @param bar_width Width of bars.
#' @param gp Graphical parameters.
#' @param track_index Index of the track. 
#'
#' @return
#' No value is returned.
#' @export
#' @examples
#' \donttest{
#' x = seq(1, 1000, by = 1) - 0.5
#' y = runif(1000)
#' spiral_initialize(xlim = c(0, 1000))
#' spiral_track(height = 0.8)
#' spiral_bars(x, y)
#'
#' # a three-column matrix
#' y = matrix(runif(3*1000), ncol = 3)
#' y = y/rowSums(y)
#' spiral_initialize(xlim = c(0, 1000))
#' spiral_track(height = 0.8)
#' spiral_bars(x, y, gp = gpar(fill = 2:4, col = NA))
#' }
spiral_bars = function(pos, value, baseline = get_track_data("ymin", track_index),
	bar_width = min(diff(pos)), gp = gpar(), track_index = current_track_index()) {

	spiral = spiral_env$spiral
	
	if(spiral$xclass == "Time") {
		bar_width = 1
		if(!is.numeric(pos)) {
			if(identical(spiral$other$normalize_year, TRUE)) {
				bar_width = 1/calc_days_in_year(year(as.POSIXlt(pos)))*360
			} 
		}
	}

	pos = spiral$get_x_from_data(pos)

	ymin = get_track_data("ymin", track_index)
	ymax = get_track_data("ymax", track_index)
	if(is.matrix(value)) {
		if(length(pos) != nrow(value)) {
			stop_wrap("Length of 'pos' should be the same as nrow of 'value'.")
		}
		n = ncol(value)
		for(i in 1:n) {
            if(i == 1) {
                spiral_rect(pos - bar_width/2, ymin, pos + bar_width/2, rowSums(value[, seq_len(i), drop = FALSE]), 
                	gp = subset_gp(gp, i), track_index = track_index)
            } else {
                spiral_rect(pos - bar_width/2, rowSums(value[, seq_len(i-1), drop = FALSE]), pos + bar_width/2, rowSums(value[, seq_len(i), drop = FALSE]), 
                	gp = subset_gp(gp, i), track_index = track_index)
            }
        }
    } else if(is.atomic(value)) {
    	validate_xy(pos, value)
    	spiral_rect(pos - bar_width/2, baseline, pos + bar_width/2, value, gp = gp, track_index = track_index)
	}
}

#' Add texts to a track
#'
#' @param x X-locations of the texts.
#' @param y Y-locations of the texts.
#' @param text A vector of texts.
#' @param offset Radial offset of the text. The value should be a [`grid::unit()`] object.
#' @param gp Graphical parameters.
#' @param facing Facing of the text.
#' @param letter_spacing Space between two letters. The value is a fraction of the width of current letter. It only works for curved texts.
#' @param nice_facing If it is true, the facing will be automatically adjusted for texts which locate at different positions of the spiral. Note `hjust` and `vjust` will also be adjusted.
#' @param just The justification of the text relative to (x, y). The same setting as in [`grid::grid.text()`].
#' @param hjust Horizontal justification. Value should be numeric. 0 means the left of the text and 1 means the right of the text.
#' @param vjust Vertical justification. Value should be numeric. 0 means the bottom of the text and 1 means the top of the text.
#' @param track_index Index of the track. 
#' @param ... Pass to [`grid::grid.text()`].
#'
#' @details
#' For the curved text, it only supports one-line text.
#'
#' @return
#' No value is returned.
#' @export
#' @importFrom grDevices pdf.options dev.size
#' @examples
#' x = seq(0.1, 0.9, length = 26)
#' text = strrep(letters, 6)
#' spiral_initialize(); spiral_track()
#' spiral_text(x, 0.5, text)
#'
#' spiral_initialize(); spiral_track()
#' spiral_text(x, 0.5, text, facing = "inside")
#'
#' spiral_initialize(); spiral_track()
#' spiral_text(x, 0.5, text, facing = "outside")
#'
#' x = seq(0.1, 0.9, length = 10)
#' text = strrep(letters[1:10], 20)
#' spiral_initialize(); spiral_track()
#' spiral_text(x, 0.5, text, facing = "curved_inside")
#'
#' spiral_initialize(); spiral_track()
#' spiral_text(x, 0.5, text, facing = "curved_outside")
spiral_text = function(x, y, text, offset = NULL, gp = gpar(),
	facing = c("downward", "inside", "outside", "clockwise", "reverse_clockwise", 
		"curved_inside", "curved_outside"),
	letter_spacing = 0,
	nice_facing = FALSE, just = "centre", hjust = NULL, vjust = NULL,
	track_index = current_track_index(), ...) {

	validate_xy(x, y, text)

	spiral = spiral_env$spiral
	x = spiral$get_x_from_data(x)

	n1 = length(x)
	n2 = length(y)
	n3 = length(text)
	n = max(n1, n2, n3)
	if(length(x) == 1) x = rep(x, n)
	if(length(y) == 1) y = rep(y, n)
	if(length(text) == 1) text = rep(text, n)

	text = as.character(text)

	if(is.null(offset)) {
		df = xy_to_cartesian(x, y, track_index = track_index)
	} else {
		df = radial_extend(x, y, offset, track_index = track_index)
	}

	just = grid::valid.just(just)
	if(is.null(hjust)) hjust = just[1]
	if(is.null(vjust)) vjust = just[2]

	facing = match.arg(facing)[1]
	if(facing == "downward") {
		grid.text(text, x = df$x, y = df$y, default.units = "native", gp = gp, hjust = hjust, vjust = vjust, ...)
	} else if(facing == "inside") {
		df2 = xy_to_polar(x, y, track_index = track_index, flip = FALSE)
		# degree = (as.degree(df2$theta) - 90) %% 360
		slope = spiral$tangent_slope(df2$theta)
		degree = atan(slope)

		degree = flip_theta(degree)
		degree = as.degree(degree)

		if(spiral$flip == "vertical") {
			l = df$y < 0 & (df$x > 0 & slope < 0 | df$x < 0) | df$y > 0 & df$x < 0 & slope > 0
		} else if(spiral$flip == "horizontal") {
			l = df$y < 0 & (df$x > 0 & slope < 0 | df$x < 0) | df$y > 0 & df$x < 0 & slope > 0
		} else {
			l = df$y < 0 & (df$x < 0 & slope < 0 | df$x > 0) | df$y > 0 & df$x > 0 & slope > 0
		}
		degree[l] = degree[l] + 180
	# }
		if(spiral$flip == "both") {
			degree = degree + 180
		} else if(spiral$flip == "horizontal") {
			degree = degree + 180
		}

		if(nice_facing) {
			l = df$y < 0
			if(any(l)) {
				grid.text(text[l], x = df$x[l], y = df$y[l], default.units = "native", gp = subset_gp(gp, which(l)), 
					hjust = 1 - hjust, vjust = 1 - vjust, rot = degree[l] + 180, ...)
			}
			if(any(!l)) {
				grid.text(text[!l], x = df$x[!l], y = df$y[!l], default.units = "native", gp = subset_gp(gp, which(!l)), 
					hjust = hjust, vjust = vjust, rot = degree[!l], ...)
			}
		} else {
			grid.text(text, x = df$x, y = df$y, default.units = "native", gp = gp, 
				hjust = hjust, vjust = vjust, rot = degree, ...)
		}
		
	} else if(facing == "outside") {
		df2 = xy_to_polar(x, y, track_index = track_index, flip = FALSE)
		# degree = (as.degree(df2$theta) + 90) %% 360
		slope = spiral$tangent_slope(df2$theta)
		degree = atan(slope)
		
		degree = flip_theta(degree)
		degree = as.degree(degree)

		if(spiral$flip == "vertical") {
			l = df$y > 0 & (df$x < 0 & slope < 0 | df$x > 0) | df$y < 0 & df$x > 0 & slope > 0
		} else if(spiral$flip == "horizontal") {
			l = df$y > 0 & (df$x < 0 & slope < 0 | df$x > 0) | df$y < 0 & df$x > 0 & slope > 0
		} else {
			l = df$y > 0 & (df$x > 0 & slope < 0 | df$x < 0) | df$y < 0 & df$x < 0 & slope > 0
		}

		degree[l] = degree[l] + 180
	# }
		if(spiral$flip == "both") {
			degree = degree + 180
		} else if(spiral$flip == "horizontal") {
			degree = degree + 180
		}

		if(nice_facing) {
			l = df$y > 0
			if(any(l)) {
				grid.text(text[l], x = df$x[l], y = df$y[l], default.units = "native", gp = subset_gp(gp, which(l)), 
					hjust = 1 - hjust, vjust = 1 - vjust, rot = degree[l] + 180, ...)
			}
			if(any(!l)) {
				grid.text(text[!l], x = df$x[!l], y = df$y[!l], default.units = "native", gp = subset_gp(gp, which(!l)), 
					hjust = hjust, vjust = vjust, rot = degree[!l], ...)
			}
		} else {
			grid.text(text, x = df$x, y = df$y, default.units = "native", gp = gp, hjust = hjust, vjust = vjust, rot = degree, ...)
		}
	} else if(facing == "clockwise") {
		df2 = xy_to_polar(x, y, track_index = track_index)
		degree = as.degree(df2$theta) %% 360
		if(nice_facing) {
			l = df$x < 0
			if(any(l)) {
				grid.text(text[l], x = df$x[l], y = df$y[l], default.units = "native", gp = subset_gp(gp, which(l)), 
					hjust = 1 - hjust, vjust = 1 - vjust, rot = degree[l] + 180, ...)
			}
			if(any(!l)) {
				grid.text(text[!l], x = df$x[!l], y = df$y[!l], default.units = "native", gp = subset_gp(gp, which(!l)), 
					hjust = hjust, vjust = vjust, rot = degree[!l], ...)
			}
		} else {
			grid.text(text, x = df$x, y = df$y, default.units = "native", gp = gp, 
				hjust = hjust, vjust = vjust, rot = degree, ...)
		}
	} else if(facing == "reverse_clockwise") {
		df2 = xy_to_polar(x, y, track_index = track_index)
		degree = (as.degree(df2$theta) + 180) %% 360
		if(nice_facing) {
			l = df$x > 0
			if(any(l)) {
				grid.text(text[l], x = df$x[l], y = df$y[l], default.units = "native", gp = subset_gp(gp, which(l)), 
					hjust = 1 - hjust, vjust = 1 - vjust, rot = degree[l] + 180, ...)
			}
			if(any(!l)) {
				grid.text(text[!l], x = df$x[!l], y = df$y[!l], default.units = "native", gp = subset_gp(gp, which(!l)), 
					hjust = hjust, vjust = vjust, rot = degree[!l], ...)
			}
		} else {
			grid.text(text, x = df$x, y = df$y, default.units = "native", gp = gp, 
				hjust = hjust, vjust = vjust, rot = degree, ...)
		}
	} else if(facing %in% c("curved_inside", "curved_outside")) {
		df2 = xy_to_polar(x, y, track_index = track_index)
		for(i in seq_len(n)) {
			curved_text(x[i], y[i], text[i], gp = subset_gp(gp, i), track_index = track_index, 
				facing = gsub("curved_", "", facing), nice_facing = nice_facing, vjust = vjust, hjust = hjust, letter_spacing = letter_spacing)
		}

		if(spiral_opt$help) {
			if(.Device == "pdf") {
				if(is.null(attr(.Device, "filepath"))) {
					if( abs(dev.size()[1] - pdf.options()$width) < 1e-8 && abs(dev.size()[2] == pdf.options()$height) < 1e-8 ) {
						message_wrap("It seems you are using 'grid::grid.grabExpr()' to capture the graphics. Note curved texts depend on the size of graphics device for the properly calculating the character positions of the texts. You should better manually set arguments 'width' and 'height' in 'grid.grabExpr()' to the same values as the size with which it will be on the comnined plot (e.g. if you use package cowplot). Set 'spiral_opt$help = FALSE' to turn off this message.")
					}
				}
			}
		}
	}

	# if(facing %in% c("inside", "outside")) {
	# 	df = xy_to_cartesian(x, y)
	# 	for(i in seq_along(x)) {
	# 		pushViewport(viewport(x = df$x[i], y = df$y[i], angle = degree[i], width = grobWidth(textGrob(text[i], gp = gp)), height = grobHeight(textGrob(text[i], gp = gp)), default.units = "native"))
	# 		grid.rect(gp = gpar(fill = "transparent", col = "red"))
	# 		popViewport()
	# 	}
	# }
}


# a single text
curved_text = function(x, y, text, gp = gpar(), track_index = current_track_index(), 
	facing = "inside", nice_facing = FALSE, vjust = 0.5, hjust = 0.5, letter_spacing = 0) {

	spiral = spiral_env$spiral

	letters = strsplit(text, "")[[1]]

	df = xy_to_cartesian(x, y, track_index)
	if(nice_facing) {
		if(facing == "inside" && df$y < 0) {
			facing = "outside"
			vjust = 1 - vjust
		} else if(facing == "outside" && df$y > 0) {
			facing = "inside"
			vjust = 1 - vjust
		}
	}

	if(facing == "inside") {
		if(spiral$reverse) {
			letters = rev(letters)
		} else if(!spiral$clockwise) {
			letters = rev(letters)
		}
	}
	n = length(letters)
	letters_len = sapply(1:n, function(i) convertWidth(grobWidth(textGrob(letters[i], gp = gp)), "native", valueOnly = TRUE))
	letters_len = letters_len*(1 + letter_spacing)

	x0 = numeric(n)
	for(i in seq_along(letters)) {
		# offset = sum(letters_len[1:i]) - letters_len[i]*0.5 - sum(letters_len)*(1-hjust)
		offset = sum(letters_len[1:i]) - letters_len[i]*0.5 - sum(letters_len)*0.5 + sum(letters_len)*(hjust - 0.5)
		x0[i] = circular_extend_on_x(x, y, offset, track_index, "xy")
	}
	spiral_text(x0, y, letters, gp = gp, track_index = track_index, facing = facing, vjust = vjust)
	# spiral_points(x0, y, gp = gpar(col = "red"))
	# spiral_points(x, y, pch = 16, gp = gpar(col = "blue"))

}


#' Add polygons to a track
#'
#' @param x X-locations of the data points.
#' @param y Y-locations of the data points.
#' @param id A numeric vector used to separate locations in x and y into multiple polygons.
#' @param gp Graphical parameters.
#' @param track_index Index of the track. 
#'
#' @return
#' No value is returned.
#' @export
#' @examples
#' x = seq(0, 2*pi*10, length = 1000)
#' y = c(sin(x), cos(rev(x)))
#' x2 = c(x, rev(x))
#' 
#' # in the normal cartesian coordinate system
#' plot(NULL, xlim = range(x2), ylim = range(y))
#' polygon(x2, y, col = "red")
#' 
#' # in the spiral coordinate system
#' spiral_initialize(xlim = range(x2))
#' spiral_track(ylim = range(y))
#' spiral_polygon(x2, y, gp = gpar(fill = "red"))
#' 
#' # try a different scale
#' spiral_initialize(xlim = range(x2), scale_by = "curve_length")
#' spiral_track(ylim = range(y))
#' spiral_polygon(x2, y, gp = gpar(fill = "red"))
spiral_polygon = function(x, y, id = NULL, gp = gpar(), track_index = current_track_index()) {

	validate_xy(x, y)

	spiral = spiral_env$spiral
	x = spiral$get_x_from_data(x)

	n = length(x)
	if(is.null(id)) {
		df = spiral_lines_expand(x, y, track_index = track_index)
	} else {
		df = do.call(rbind, tapply(seq_len(n), id, function(ind) {
			df = spiral_lines_expand(x[ind], y[ind], track_index = track_index)
			df$id = rep(id[ind][1], nrow(df))
			df
		}))
		id = df$id
	}
	grid.polygon(df$x, df$y, id = id, default.units = "native", gp = gp)
}

#' Draw axis along the spiral
#'
#' @param h Position of the axis. The value can be a character of "top" or "bottom".
#' @param at Breaks points on axis.
#' @param major_at Breaks points on axis. It is the same as `at`.
#' @param labels The corresponding labels for the break points.
#' @param curved_labels Whether are the labels are curved?
#' @param minor_ticks Number of minor ticks.
#' @param major_ticks_length Length of the major ticks. The value should be a [`grid::unit()`] object.
#' @param minor_ticks_length Length of the minor ticks. The value should be a [`grid::unit()`] object.
#' @param ticks_gp Graphical parameters for the ticks.
#' @param labels_gp Graphical parameters for the labels.
#' @param track_index Index of the track. 
#'
#' @return
#' No value is returned.
#' @export
#' @examples
#' spiral_initialize()
#' spiral_track()
#' spiral_axis()
#'
#' # if the spiral is interpolated by the curve length
#' spiral_initialize(scale_by = "curve_length"); spiral_track()
#' spiral_axis()
#'
#' spiral_initialize(xlim = c(0, 360*4), start = 360, end = 360*5); spiral_track()
#' spiral_axis(major_at = seq(0, 360*4, by = 30))
#'
#' spiral_initialize(xlim = c(0, 12*4), start = 360, end = 360*5); spiral_track()
#' spiral_axis(major_at = seq(0, 12*4, by = 1), labels = c("", rep(month.name, 4)))
#'
spiral_axis = function(h = c("top", "bottom"), at = NULL, major_at = at,
	labels = TRUE, curved_labels = FALSE, minor_ticks = 4, 
	major_ticks_length = unit(4, "bigpts"), minor_ticks_length = unit(2, "bigpts"),
	ticks_gp = gpar(), labels_gp = gpar(fontsize = 6), 
	track_index = current_track_index()) {

	reverse_y = get_track_data("reverse_y", track_index)

	h = match.arg(h)[1]
	if(h == "top") {
		if(reverse_y) {
			axis_on_top = FALSE
			h = get_track_data("ymin", track_index)
		} else {
			axis_on_top = TRUE
		h = get_track_data("ymax", track_index)
		}
	} else if(h == "bottom") {
		if(reverse_y) {
			axis_on_top = TRUE
			h = get_track_data("ymax", track_index)
		} else {
			axis_on_top = FALSE
			h = get_track_data("ymin", track_index)
		}
	}

	spiral = spiral_env$spiral

	at_specified = TRUE
	if(is.null(major_at)) {
		at_specified = FALSE
		# if(spiral$xclass == "Genomic positions") {
			nb = round(spiral$spiral_length_range/(spiral$curve(mean(spiral$theta_lim))^2*pi/360*20))
			major_at = pretty(spiral$xlim, nb)
		# } else {
		# 	major_by = spiral$xrange/2/20 # the circle have median size is split into 20 sectors
		# 	digits = as.numeric(gsub("^.*e([+-]\\d+)$", "\\1", sprintf("%e", major_by))) - 1
		# 	major_by = round(major_by, digits = -1*digits)
		# 	major_at = seq(floor(spiral$xlim[1]/major_by)*major_by, spiral$xlim[2], by = major_by)
		# 	major_at = c(major_at, major_at[length(major_at)] + major_by)
		# }

		labels = spiral$get_character_from_x(major_at)
	} else {
		major_at = spiral$get_x_from_data(major_at)
		if(!(identical(labels, NULL) | identical(labels, TRUE) | identical(labels, FALSE))) {
			if(length(labels) != length(major_at)) {
				stop_wrap("Length of `labels` should be the same as the length of `major_at`.")
			}
		}
	}

	l = major_at <= spiral$xlim[2] & major_at >= spiral$xlim[1]
	major_at = major_at[l]
	if(!(identical(labels, NULL) | identical(labels, TRUE) | identical(labels, FALSE))) {
		labels = labels[l]
		labels_gp = subset_gp(labels_gp, l)
	}
	spiral_radial_segments(major_at, h, offset = ifelse(axis_on_top, 1, -1)*major_ticks_length, track_index = track_index, gp = ticks_gp)

	minor_at = NULL
	if(identical(minor_ticks, FALSE)) {
		minor_ticks = 0
	}
	if(missing(minor_ticks) && spiral$xclass == "Time") {
		minor_ticks = 0
	}
	if(minor_ticks != 0) {
		major_at2 = major_at
		major_at2 = c(major_at[1] - diff(major_at)[1], major_at, major_at + diff(major_at)[length(major_at)-1])
		for(i in seq_along(major_at2)) {
			if(i == 1) next
			k = seq_len(minor_ticks) / (minor_ticks + 1)
			minor_at = c(minor_at, k * (major_at2[i] - major_at2[i - 1]) + major_at2[i - 1])
		}
		l = minor_at <= spiral$xlim[2] & minor_at >= spiral$xlim[1]
		minor_at = minor_at[l]
		spiral_radial_segments(minor_at, h, offset = ifelse(axis_on_top, 1, -1)*minor_ticks_length, track_index = track_index, gp = ticks_gp)
	}

	### labels
	if(is.null(labels)) labels = FALSE
	if(!identical(labels, FALSE)) {
		if(identical(labels, TRUE)) {
			if(at_specified) {
				labels = spiral$get_character_from_x(major_at)
			} else {
				labels = major_at
			}
		}

		if(axis_on_top) {
			h = h + convert_height_to_y(major_ticks_length + minor_ticks_length + unit(1, "bigpts"), track_index = track_index)
		} else {
			h = h - convert_height_to_y(major_ticks_length + minor_ticks_length + unit(1, "bigpts"), track_index = track_index)
		}
		df = xy_to_polar(major_at, h, track_index = track_index)
		degree = as.degree(df$theta)
		rot = ifelse(degree >= 0 & degree <= 180, degree - 90, degree + 90)
		vjust = ifelse(degree >= 0 & degree <= 180, 0, 1)

		if(!axis_on_top) vjust = ifelse(vjust == 0, 1, 0)

		if(missing(curved_labels)) {
			if(spiral$xclass == "Time") {
				curved_labels = TRUE
			}
		}

		if(curved_labels) {
			spiral_text(major_at, h, labels,
				facing = "curved_inside", gp = labels_gp, track_index = track_index, nice_facing = TRUE)
		} else {
			spiral_text(major_at, h, labels,
				rot = rot, vjust = vjust, gp = labels_gp, track_index = track_index)
		}
	}
}

#' @param ... All pass to `spiral_axis()`.
#' @rdname spiral_axis
#' @export
spiral_xaxis = function(...) {
	spiral_axis(...)
}

#' Draw y-axis
#'
#' @param side On which side of the spiral the y-axis is drawn? "start" means the inside of the spiral and "end" means the outside of the spiral.
#'    Note if `reverse` was set to `TRUE` in [`spiral_initialize()`], then "start" corresponds to the outside of the spiral.
#' @param at Break points.
#' @param labels Corresponding labels for the break points.
#' @param ticks_length Length of the tick. Value should be a [`grid::unit()`] object.
#' @param ticks_gp Graphical parameters for ticks.
#' @param labels_gp Graphical parameters for labels.
#' @param track_index Index of the track.
#'
#' @return
#' No value is returned.
#' @export
#' @examples
#' spiral_initialize(); spiral_track(height = 0.8)
#' spiral_yaxis("start")
#' spiral_yaxis("end", at = c(0, 0.25, 0.5, 0.75, 1), labels = letters[1:5])
spiral_yaxis = function(side = c("both", "start", "end"), at = NULL, labels = TRUE, 
	ticks_length = unit(2, "bigpts"), 
	ticks_gp = gpar(), labels_gp = gpar(fontsize = 6), 
	track_index = current_track_index()) {
		
	side = match.arg(side)[1]

	if(side == "both") {
		spiral_yaxis(side = "start", at = at, labels = labels, ticks_length = ticks_length, 
			ticks_gp = ticks_gp, labels_gp = labels_gp, track_index = track_index)
		spiral_yaxis(side = "end", at = at, labels = labels, ticks_length = ticks_length, 
			ticks_gp = ticks_gp, labels_gp = labels_gp, track_index = track_index)
		return(invisible(NULL))
	}

	spiral = spiral_env$spiral

	if(side == "start") {
		v = spiral$xlim[1]
	} else {
		v = spiral$xlim[2]
	}

	ylim = c(get_track_data("ymin", track_index), get_track_data("ymax", track_index))
	if(is.null(at)) {
		at = pretty(ylim, n = 3)
        labels = at
	} else {
		if(identical(labels, TRUE)) {
			labels = at
		} else if(identical(labels, NULL)) {
			labels = FALSE
		}
	}
	l = at >= get_track_data("ymin", track_index) & at <= get_track_data("ymax", track_index)
	at = at[l]
	if(!is.logical(labels)) {
		labels = labels[l]
	}

	if(spiral$flip == "none" && !spiral$reverse && side == "start") {
		just = "left"
		offset_sign = -1
	} else if(spiral$flip == "none" && !spiral$reverse && side == "end") {
		just = "right"
		offset_sign = 1
	} else if(spiral$flip == "none" && spiral$reverse && side == "start") {
		just = "right"
		offset_sign = -1
	} else if(spiral$flip == "none" && spiral$reverse && side == "end") {
		just = "left"
		offset_sign = 1
	} else if(spiral$flip == "horizontal" && !spiral$reverse && side == "start") {
		just = "right"
		offset_sign = -1
	} else if(spiral$flip == "horizontal" && !spiral$reverse && side == "end") {
		just = "left"
		offset_sign = 1
	} else if(spiral$flip == "horizontal" && spiral$reverse && side == "start") {
		just = "left"
		offset_sign = -1
	} else if(spiral$flip == "horizontal" && spiral$reverse && side == "end") {
		just = "right"
		offset_sign = 1
	} else if(spiral$flip == "vertical" && !spiral$reverse && side == "start") {
		just = "right"
		offset_sign = -1
	} else if(spiral$flip == "vertical" && !spiral$reverse && side == "end") {
		just = "left"
		offset_sign = 1
	} else if(spiral$flip == "vertical" && spiral$reverse && side == "start") {
		just = "left"
		offset_sign = -1
	} else if(spiral$flip == "vertical" && spiral$reverse && side == "end") {
		just = "right"
		offset_sign = 1
	} else if(spiral$flip == "both" && !spiral$reverse && side == "start") {
		just = "left"
		offset_sign = -1
	} else if(spiral$flip == "both" && !spiral$reverse && side == "end") {
		just = "right"
		offset_sign = 1
	} else if(spiral$flip == "both" && spiral$reverse && side == "start") {
		just = "right"
		offset_sign = -1
	} else if(spiral$flip == "both" && spiral$reverse && side == "end") {
		just = "left"
		offset_sign = 1
	}

	offset_sign = ifelse(spiral$reverse, -1, 1)*offset_sign

	if(length(at)) {
		x1 = rep(v, length(at))
		x2 = circular_extend_on_x(x1, at, offset = offset_sign*ticks_length, track_index = track_index, coordinate = "xy")
		spiral_segments(x1, at, x2, at, gp = ticks_gp)

		if(!identical(labels, FALSE)) {
			x2 = circular_extend_on_x(x1, at, offset = offset_sign*(ticks_length + unit(1, "bigpts")), track_index = track_index, coordinate = "xy")
			spiral_text(x2, at, labels, just = just, gp = labels_gp, facing = "inside", nice_facing = TRUE)
		}
	}
}

#' Draw horizon chart along the spiral
#'
#' @param x X-locations of the data points.
#' @param y Y-locations of the data points.
#' @param y_max Maximal absolute value on y-axis.
#' @param n_slices Number of slices.
#' @param slice_size Size of the slices. The final number of sizes is `ceiling(max(abs(y))/slice_size)`.
#' @param pos_fill Colors for positive values. 
#' @param neg_fill Colors for negative values.
#' @param use_bars Whether to use bars?
#' @param bar_width Width of bars.
#' @param negative_from_top Should negative distribution be drawn from the top?
#' @param track_index Index of the track. 
#'
#' @details
#' Since the track height is very small in the spiral, horizon chart visualization is an efficient way to visualize
#' distribution-like graphics.
#'
#' @return
#' A list of the following objects:
#' 
#' - a color mapping function for colors.
#' - a vector of intervals that split the data.
#' 
#' @seealso [`horizon_legend()`] for generating the legend.
#' @export
#' @examples
#' \donttest{
#' df = readRDS(system.file("extdata", "global_temperature.rds", package = "spiralize"))
#' df = df[df$Source == "GCAG", ]
#' spiral_initialize_by_time(xlim = range(df$Date), unit_on_axis = "months", period = "year",
#'     period_per_loop = 20, polar_lines_by = 360/20)
#' spiral_track()
#' spiral_horizon(df$Date, df$Mean, use_bar = TRUE)
#' 
#' # with legend
#' require(ComplexHeatmap)
#' spiral_initialize_by_time(xlim = range(df$Date), unit_on_axis = "months", period = "year",
#'     period_per_loop = 20, polar_lines_by = 360/20, 
#'     vp_param = list(x = unit(0, "npc"), just = "left"))
#' spiral_track()
#' lt = spiral_horizon(df$Date, df$Mean, use_bar = TRUE)
#' lgd = horizon_legend(lt, title = "Temperature difference")
#' draw(lgd, x = unit(1, "npc") + unit(2, "mm"), just = "left")
#' }
spiral_horizon = function(x, y, y_max = max(abs(y)), n_slices = 4, slice_size, 
	pos_fill = "#D73027", neg_fill = "#313695",
	use_bars = FALSE, bar_width = min(diff(x)),
	negative_from_top = FALSE, track_index = current_track_index()) {

	validate_xy(x, y)

	if(!(get_track_data("ymin", track_index) == 0 & get_track_data("ymax", track_index) == 1)) {
		stop_wrap("The horizon track must have 'ylim = c(0, 1)'.")
	}

	spiral = spiral_env$spiral

	if(use_bars) {
		if(spiral$xclass == "Time") {
			if(identical(spiral$other$normalize_year, TRUE)) {
				bar_width = 1/calc_days_in_year(year(as.POSIXlt(x)))*360
			}
		}
	}

	x = spiral$get_x_from_data(x)

	if(missing(slice_size)) {
		slice_size = y_max/n_slices
	}
	n_slices = ceiling(y_max/slice_size)

	if(n_slices == 0) {
		return(invisible(NULL))
	}

	n = length(x)
	if(length(bar_width) == 1) {
		bar_width = rep(bar_width, n)
	}

	l = is.na(x)
	x = x[!l]
	y = y[!l]

	l = is.na(y)
	y[l] = 0

	if(all(y >= 0)) {
		y_type = "positive"
	} else if(all(y <= 0)) {
		y_type = "negative"
	} else {
		y_type = "both"
	}

	pos_col_fun = colorRamp2(c(0, n_slices), c("white", pos_fill))
	neg_col_fun = colorRamp2(c(0, n_slices), c("white", neg_fill))
	if(y_type %in% c("positive", "both")) {
		for(i in seq_len(n_slices)) {
			l1 = y >= (i-1)*slice_size & y < i*slice_size
			l2 = y < (i-1)*slice_size
			l3 = y >= i*slice_size
			if(any(l1)) {
				x2 = x
				y2 = y
				bar_width2 = bar_width
				y2[l1] = y2[l1] - slice_size*(i-1)
				y2[l3] = slice_size
				x2[l2] = NA
				y2[l2] = NA
				bar_width2[l2] = NA

				if(use_bars) {
					add_horizon_bars(x2, y2, bar_width = bar_width2, slice_size = slice_size, 
						gp = gpar(fill = pos_col_fun(i), col = pos_col_fun(i)), track_index = track_index) 
				} else {
					add_horizon_polygons(x2, y2, slice_size = slice_size, 
						gp = gpar(fill = pos_col_fun(i), col = NA), track_index = track_index)
				}
			}
		}
	}
	if(y_type %in% c("negative", "both")) {
		y = -y
		for(i in seq_len(n_slices)) {
			l1 = y >= (i-1)*slice_size & y < i*slice_size
			l2 = y < (i-1)*slice_size
			l3 = y >= i*slice_size
			if(any(l1)) {
				x2 = x
				y2 = y
				bar_width2 = bar_width
				y2[l1] = y2[l1] - slice_size*(i-1)
				y2[l3] = slice_size
				x2[l2] = NA
				y2[l2] = NA
				bar_width2[l2] = NA

				if(use_bars) {
					add_horizon_bars(x2, y2, bar_width = bar_width2, slice_size = slice_size, from_top = negative_from_top, 
						gp = gpar(fill = neg_col_fun(i), col = neg_col_fun(i)), track_index = track_index)
				} else {
					add_horizon_polygons(x2, y2, slice_size = slice_size, from_top = negative_from_top, 
						gp = gpar(fill = neg_col_fun(i), col = NA), track_index = track_index)
				}
			}
		}
	}

	interval = 0:n_slices*slice_size
	if(y_type == "positive") {
		col_fun = colorRamp2(c(0, n_slices*slice_size), c("white", pos_fill))
		return(invisible(list(col_fun = col_fun, interval = interval)))
	} else if(y_type == "negative") {
		col_fun = colorRamp2(c(-n_slices*slice_size, 0), c(neg_fill, "white"))
		return(invisible(list(col_fun = col_fun, interval = -rev(interval))))
	} else {
		col_fun = colorRamp2(c(-n_slices*slice_size, 0, n_slices*slice_size), c(neg_fill, "white", pos_fill))
		return(invisible(list(col_fun = col_fun, interval = seq(-n_slices, n_slices)*slice_size)))
	}
}

add_horizon_polygons = function(x, y, slice_size = NULL, from_top = FALSE, ...) {
	ltx = split_vec_by_NA(x)
	lty = split_vec_by_NA(y)

	for(i in seq_along(ltx)) {
		x0 = ltx[[i]]
		y0 = lty[[i]]
		if(from_top) {
			x0 = c(x0[1], x0, x0[length(x0)], x0[1])
			y0 = c(slice_size, slice_size - y0, slice_size, slice_size)
		} else {
			x0 = c(x0[1], x0, x0[length(x0)], x0[1])
			y0 = c(0, y0, 0, 0)
		}
		spiral_polygon(x0, y0/slice_size, ...)
	}
}

add_horizon_bars = function(x, y, bar_width, slice_size = NULL, from_top = FALSE, ...) {
	ltx = split_vec_by_NA(x)
	lty = split_vec_by_NA(y)
	lbw = split_vec_by_NA(bar_width)

	all_x = NULL
	all_y = NULL
	all_bw = NULL
	if(length(bar_width) <= 1) {
		all_bw = bar_width
	}
	for(i in seq_along(ltx)) {
		x0 = ltx[[i]]
		y0 = lty[[i]]
		bw = lbw[[i]]

		all_x = c(all_x, x0)
		all_y = c(all_y, y0)
		if(length(bar_width) > 1) {
			all_bw = c(all_bw, bw)
		}
	}
	if(from_top) {
		spiral_bars(all_x, all_y/slice_size, bar_width = all_bw, baseline = 1, ...)
	} else {
		spiral_bars(all_x, all_y/slice_size, bar_width = all_bw, ...)
	}
}

# https://stat.ethz.ch/pipermail/r-help/2010-April/237031.html
split_vec_by_NA = function(x) {
	idx = 1 + cumsum(is.na(x))
	not.na = !is.na(x)
	split(x[not.na], idx[not.na])
}

#' Legend for the horizon chart
#'
#' @param lt The object returned by [`spiral_horizon()`].
#' @param title Title of the legend.
#' @param format Number format of the legend labels.
#' @param template Template to construct the labels.
#' @param ... Pass to [`ComplexHeatmap::Legend()`].
#'
#' @return
#' A [`ComplexHeatmap::Legend`] object.
#' @export
#' @examples
#' # see examples in `spiral_horizon()`.
horizon_legend = function(lt, title = "", format = "%.2f",
	template = "[{x1}, {x2}]", ...) {

	interval = lt$interval
	col_fun = lt$col_fun
	
	n = length(interval)
	at = interval[interval != 0]
	interval = sprintf(format, interval)
	x1 = interval[1:(n - 1)]
	x2 = interval[2:n]
	labels = qq(template, collapse = FALSE, code.pattern = "\\{CODE\\}")

	if(all(interval >= 0)) {
		l = at > 0
		at = at[l]
		labels = labels[l]
		at = rev(at)
		labels = rev(labels)
	} else if(all(interval <= 0)) {
		l = at < 0
		at = at[l]
		labels = labels[l]
	}

	ComplexHeatmap::Legend(title = title, at = at, labels = labels, legend_gp = gpar(fill = col_fun(at)), ...)
}

#' Add image to a track
#'
#' @param x X-locations of the center of the image.
#' @param y Y-locations of the center of the image.
#' @param image A vector of file paths of images. The format of the image is inferred from the suffix name of the image file.
#'       NA value or empty string means no image to drawn. Supported formats are png/svg/pdf/eps/jpeg/jpg/tiff.
#' @param width Width of the image. See Details. 
#' @param height Height of the image. See Details. 
#' @param facing Facing of the image.
#' @param nice_facing Whether to adjust the facing.
#' @param scaling Scaling factor when ``facing`` is set to `"curved_inside"` or `"curved_outside"`.
#' @param track_index Index of the track. 
#'
#' @details
#' When `facing` is set to one of `"downward"`, `"inside"` and `"outside"`, both of `width` and `height` should be [`grid::unit()`] objects. 
#' It is suggested to only set one of `width` and `height`, the other dimension will be automatically calculated from the aspect ratio of the image.
#'
#' When `facing` is set to one of `"curved_inside"` and `"curved_outside"`, the value can also be numeric, which are the values
#' measured in the data coordinates. Note when the segment in the spiral that corresponds to `width` is very long, drawing the curved
#' image will be very slow because each pixel is actually treated as a single rectangle.
#'
#' @return
#' No value is returned.
#' @export
#' @examples
#' image = system.file("extdata", "Rlogo.png", package = "circlize")
#' x = seq(0.1, 0.9, length = 10)
#'
#' spiral_initialize()
#' spiral_track()
#' spiral_raster(x, 0.5, image)
#'
#' spiral_initialize()
#' spiral_track()
#' spiral_raster(x, 0.5, image, facing = "inside")
#'
spiral_raster = function(x, y, image, width = NULL, height = NULL, 
	facing = c("downward", "inside", "outside", "curved_inside", "curved_outside"), 
	nice_facing = FALSE, scaling = 1, track_index = current_track_index()) {

	spiral = spiral_env$spiral

	facing = match.arg(facing)[1]

	if(facing %in% c("curved_inside", "curved_outside")) {
		spiral_raster_curved(x, y, image = image, width = width, height = height, facing = facing,
			nice_facing = nice_facing, scaling = scaling, track_index = track_index)
		return(invisible(NULL))
	}
	
	if(is.character(image) && is.atomic(image)) {
		n1 = length(x)
		n2 = length(y)
		n3 = length(image)
		n = max(n1, n2, n3)

		if(n1 == 1) x = rep(x, n)
		if(n2 == 1) y = rep(y, n)
		if(n3 == 1) {
			img = read_image(image)
			image = vector("list", n)
			for(i in seq_len(n)) {
				image[[i]] = img[[1]]
			}
		} else {
			image = read_image(image)
		}
		if(!is.null(width)) {
			if(length(width) == 1) width = rep(width, n)
		}
		if(!is.null(height)) {
			if(length(height) == 1) height = rep(height, n)
		}

		for(i in seq_len(n)) {
			if(identical(image[[i]], NA)) next
			spiral_raster(x[i], y[i], image[[i]], width = width[i], height = height[i], 
				facing = facing, nice_facing = nice_facing, track_index = track_index)
		}
		return(invisible(NULL))
	}
	
	spiral = spiral_env$spiral

	df = xy_to_polar(x, y, track_index = track_index)
	df2 = polar_to_cartesian(df$theta, df$r)
	theta = as.degree(df$theta)
	
	if(inherits(image, "array")) {
		asp = ncol(image)/nrow(image) # width/height
	} else if(inherits(image, "Picture")) {
		asp = max(image@summary@xscale)/max(image@summary@yscale)
	}

	if(is.null(width) && is.null(height)) {
		height = convertHeight(unit(0.8*spiral$dist * get_track_data("rel_height", track_index), "native"), "mm")
		width = height*asp
	} else if(is.null(width)) {
		width = height*asp
	} else if(is.null(height)) {
		if(facing %in% c("downward", "inside", "outside")) {
			height = 1/asp*width
		} else {
			height = 0.8*get_track_data("yrange", track_index)
		}
	}
	
	if(facing == "downward") {
		rot = 0
	} else if(facing == "inside") {
		rot = theta - 90 
		if(nice_facing) {
			if(df2$y < 0) {
				rot = rot + 180
			}
		}
	} else if(facing == "outside") {
		rot = theta + 90 
		if(nice_facing) {
			if(df2$y > 0) {
				rot = rot + 180
			}
		}
	}

	image_class = attr(image, "image_class")

	pushViewport(viewport(x = df2$x, y = df2$y, width = width, height = height, angle = rot, default.units = "native"))
	if(is.null(image_class)) {
		grid.raster(image)
	} else if(image_class == "raster") {
		grid.raster(image)
	} else if(image_class == "grImport::Picture") {
		grid.picture = getFromNamespace("grid.picture", ns = "grImport")
		grid.picture(image)
	} else if(image_class == "grImport2::Picture") {
		grid.picture = getFromNamespace("grid.picture", ns = "grImport2")
		grid.picture(image)
	}
	popViewport()
	
}

#' @importFrom grDevices as.raster png dev.off dev.size
spiral_raster_curved = function(x, y, image, width = NULL, height = NULL, 
	facing = c("curved_inside", "curved_outside"), 
	nice_facing = FALSE, scaling = 1, track_index = current_track_index()) {

	if(!requireNamespace("magick")) {
		stop_wrap("Package 'magick' should be installed.")
	}

	if(inherits(image, "matrix") || inherits(image, "raster")) {
		image = as.raster(image)
		temp_file = tempfile(fileext = ".png")
		png(temp_file, width = ncol(image), height = nrow(image))
		grid.raster(image)
		dev.off()
		image = temp_file

		on.exit(file.remove(temp_file))
	}

	spiral = spiral_env$spiral

	if(is.null(width)) width = spiral$xrange
	if(is.null(height)) height = get_track_data("yrange", track_index)

	n1 = length(x)
	n2 = length(y)
	n3 = length(image)
	n = max(n1, n2, n3)

	if(n1 == 1) x = rep(x, n)
	if(n2 == 1) y = rep(y, n)
	if(n3 == 1) image = rep(image, n)
	if(!is.null(width)) {
		if(length(width) == 1) width = rep(width, n)
	}
	if(!is.null(height)) {
		if(length(height) == 1) height = rep(height, n)
	}

	if(n > 1) {
		for(i in seq_len(n)) {
			if(identical(image[[i]], NA)) next
			spiral_raster_curved(x[i], y[i], image[[i]], width = width[i], height = height[i], 
				facing = facing, nice_facing = nice_facing, track_index = track_index)
		}
		return(invisible(NULL))
	}
	
	df = xy_to_polar(x, y, track_index = track_index)
	theta = as.degree(df$theta)

	if(nice_facing) {
        if(theta > 180 & theta < 360) {
            if(facing == "curved_inside") {
              facing = "curved_outside"
            }
            else {
              facing = "curved_inside"
            }
        }
    }

    if(is.unit(width)) {
    	width = convertWidth(width, "native", valueOnly = TRUE)/spiral$spiral_length_range*spiral$xrange
    }

	width2 = width/spiral$xrange*spiral$spiral_length_range
	width2 = unit(width2, "native")
	width2 = convertWidth(width2, "inch", valueOnly = TRUE)*96

	if(is.unit(height)) {
		height = convert_height_to_y(height, track_index)
	}
    
	height2 = height/get_track_data("yrange", track_index)*get_track_data("rrange", track_index)
	height2 = unit(height2, "native")
	height2 = convertHeight(height2, "inch", valueOnly = TRUE)*72 # or convert to bigpts
    
	image = magick::image_read(image)
    image = magick::image_resize(image, paste0(width2, "x", height2, "!"))
    image = as.raster(image)

	nr = nrow(image)
	nc = ncol(image)

	row_index = rep(1:nr, nc)
	col_index = rep(nc:1, each = nr)

	if(facing == "curved_inside") {
		yv = 1 - (row_index - 0.5)/nr
		xv = (col_index - 0.5)/nc
	} else {
		yv = (row_index - 0.5)/nr
		xv = 1 - (col_index - 0.5)/nc
	}
	xv = x - width/2 + xv*width
	yv = y - height/2 + yv*height
	l = !grepl("^#FFFFFF", image)
	ind = which(l)

	i = 0
	n = length(ind)
	while(i <= n) {
		if(i + 1 + 5000 > n) {
			ind2 = seq(i+1, n) 
		} else {
			ind2 = 1:5000 + i
		}
		spiral_rect(xv[ind2] - width/nc/2, yv[ind2] - height/nr/2, xv[ind2] + width/nc/2, yv[ind2] + height/nr/2,
			gp = gpar(fill = image[ind2], col = image[ind2]), track_index = track_index)

		i = i + 5000
	}
}

#' Draw arrows in the spiral direction
#'
#' @param x1 Start of the arrow.
#' @param x2 End of the arrow.
#' @param y Y-location of the arrow.
#' @param width Width of the arrow. The value can be the one measured in the data coordinates or a [`grid::unit()`] object.
#' @param arrow_head_length Length of the arrow head.
#' @param arrow_head_width Width of the arrow head.
#' @param arrow_position Position of the arrow. If the value is `"end"`, then the arrow head is drawn at `x = x2`. If the value
#'    is `"start"`, then the arrow head is drawn at `x = x1`. 
#' @param tail The shape of the arrow tail.
#' @param gp Graphical parameters.
#' @param track_index Index of the track. 
#'
#' @seealso
#' Note [`spiral_segments()`] also supports drawing line-based arrows.
#'
#' @return
#' No value is returned.
#' @export
#' @examples
#' spiral_initialize()
#' spiral_track()
#' spiral_arrow(0.3, 0.6, gp = gpar(fill = "red"))
#' spiral_arrow(0.8, 0.9, gp = gpar(fill = "blue"), tail = "point", arrow_position = "start")
spiral_arrow = function(
	x1, x2, 
	y = get_track_data("ycenter", track_index), 
	width = get_track_data("yrange", track_index)/3, 
	arrow_head_length = unit(4, "mm"),
	arrow_head_width = width*2, 
	arrow_position = c("end", "start"),
	tail = c("normal", "point"), 
	gp = gpar(),
	track_index = current_track_index()) {

	spiral = spiral_env$spiral
	x1 = spiral$get_x_from_data(x1)
	x2 = spiral$get_x_from_data(x2)

	arrow_position = match.arg(arrow_position)[1]
	tail = match.arg(tail)[1]

	if(x2 <= x1) {
		x3 = x1
		x1 = x2
		x2 = x3
		arrow_position = setdiff(c("end", "start"), arrow_position)
	}
	
	spiral = spiral_env$spiral

	if(is.unit(width)) {
		width = convert_height_to_y(width, track_index = track_index)
	}

	if(is.unit(arrow_head_length)) {
		arrow_head_length = convertWidth(arrow_head_length, "native", valueOnly = TRUE)
		arrow_head_length = arrow_head_length/spiral$spiral_length(spiral$theta_lim[2])*(spiral$xlim[2] - spiral$xlim[1])
	}
	if(is.unit(arrow_head_width)) {
		arrow_head_width = convertWidth(arrow_head_width, "native", valueOnly = TRUE)
		arrow_head_width = arrow_head_width/(get_track_data("rmax", track_index) - get_track_data("rmin", track_index))*get_track_data("yrange", track_index)
	}

	if(abs(x2 - x1 - arrow_head_length) < 1e-6) {
		stop_wrap("Arrow head is too long that it is even longer than the arrow itself.")
	}

	if(arrow_position == "end") {
		arrow_head_coor = rbind(c(x2 - arrow_head_length, y + arrow_head_width/2),
			                    c(x2, y),
			                    c(x2 - arrow_head_length, y - arrow_head_width/2))
		if(tail == "normal") {
			arrow_body_coor = rbind(c(x2 - arrow_head_length, y - width/2),
				                    c(x1, y - width/2),
				                    c(x1, y + width/2),
				                    c(x2 - arrow_head_length, y + width/2))
			
		} else {
			arrow_body_coor = rbind(c(x2 - arrow_head_length, y - width/2),
				                    c(x1, y),
				                    c(x2 - arrow_head_length, y + width/2))
			
		}

		coor = rbind(arrow_body_coor, arrow_head_coor)
	} else {
		
		arrow_head_coor = rbind(c(x1 + arrow_head_length, y + arrow_head_width/2),
			                    c(x1, y),
			                    c(x1 + arrow_head_length, y - arrow_head_width/2))
		if(tail == "normal") {
			arrow_body_coor = rbind(c(x1 + arrow_head_length, y - width/2),
				                    c(x2, y - width/2),
				                    c(x2, y + width/2),
				                    c(x1 + arrow_head_length, y + width/2))
		} else {
			arrow_body_coor = rbind(c(x1 + arrow_head_length, y - width/2),
				                    c(x2, y),
				                    c(x1 + arrow_head_length, y + width/2))
		}
		coor = rbind(arrow_body_coor, arrow_head_coor)
	}
	coor = rbind(coor, coor[1, ])
	
	spiral_polygon(coor[, 1], coor[, 2], gp = gp, track_index = track_index)
}


#' Highlight a section of the spiral
#'
#' @param x1 Start location of the highlighted section.
#' @param x2 End location of the highlighted section.
#' @param type Type of the highlighting. "rect" means drawing transparent rectangles covering the whole track.
#'      "line" means drawing annotation lines on top of the track or at the bottom of it.
#' @param padding When the highlight type is "rect", it controls the padding of the highlighted region. The value should be a [`grid::unit()`] object
#'     or a numeric value which is the fraction of the length of the highlighted section. The length can be one or two.
#'      Note it only extends in the radial direction.
#' @param line_side If the highlight type is "line", it controls which side of the track to draw the lines.
#' @param line_width Width of the annotation line. Value should be a [`grid::unit()`] object.
#' @param gp Graphical parameters.
#' @param track_index Index of the track.
#'
#' @return
#' No value is returned.
#' @export
#' @examples
#' spiral_initialize(); spiral_track()
#' spiral_highlight(0.4, 0.6)
#' spiral_highlight(0.1, 0.2, type = "line", gp = gpar(col = "blue"))
#' spiral_highlight(0.7, 0.8, type = "line", line_side = "outside")
spiral_highlight = function(x1, x2, type = c("rect", "line"), padding = unit(1, "mm"),
	line_side = c("inside", "outside"), line_width = unit(1, "pt"),
	gp = gpar(fill = "red"), track_index = current_track_index()) {

	spiral = spiral_env$spiral
	if(identical(x1, "start")) {
		x1 = spiral$xlim[1]
	} else {
		x1 = spiral$get_x_from_data(x1)
	}
	if(identical(x2, "end")) {
		x2 = spiral$xlim[2]
	} else {
		x2 = spiral$get_x_from_data(x2)
	}

	if(x1 > x2) {
		foo = x1
		x1 = x2
		x2 = foo
	}

	type = match.arg(type)[1]
	if(type == "rect") {
		track_index = sort(track_index)
		if(length(track_index) > 1) {
			if(any(diff(track_index)) > 1) {
				stop_wrap("If `track_index` is set with multiple tracks, the value should be incremental by 1, or you can consider to use `spiral_highlight()` multiple times.")
			}
		}
		if("fill" %in% names(gp)) {
			gp$fill = add_transparency(gp$fill, 0.75)
		}
		if(!"col" %in% names(gp)) {
			gp$col = NA
		}
		
		if(length(padding) == 1) {
			padding = rep(padding, 2)
		}
		if(is.unit(padding)) {
			offset = convert_height_to_y(padding, track_index = track_index)
		} else {
			offset = get_track_data("yrange", track_index)*padding
		}
		if(length(track_index) == 1) {
			spiral_rect(x1, get_track_data("ymin", track_index) - offset[1],
				        x2, get_track_data("ymax", track_index) + offset[2],
				        gp = gp, track_index =  track_index)
		} else {
			y1 = get_track_data("ymin", track_index[1])
			h = sum(sapply(track_index, function(i) {
				get_track_data("rel_height", i)
			}))
			y2 = h/get_track_data("rel_height", track_index[1])*get_track_data("yrange", track_index[1]) + get_track_data("ymin", track_index[1])
			spiral_rect(x1, y1 - offset[1],
				        x2, y2 + offset[2],
				        gp = gp, track_index =  track_index[1])
		}
	} else {
		line_side = match.arg(line_side)[1]

		if(line_side == "inside") {
			track_index = min(track_index)
		} else {
			track_index = max(track_index)
		}
		ymin = get_track_data("ymin", track_index)
		ymax = get_track_data("ymax", track_index)

		if(!"col" %in% names(gp)) {
			gp$col = gp$fill
		}
		if("col" %in% names(gp) && !("fill" %in% names(gp))) {
			gp$fill = gp$col
		}
		offset = convert_height_to_y(line_width, track_index = track_index)
		if(line_side == "inside") {
			spiral_rect(x1, ymin, x2, ymin - offset, gp = gp, track_index = track_index)
		} else {
			spiral_rect(x1, ymax, x2, ymax + offset, gp = gp, track_index = track_index)
		}
	}
}

#' Highlight a sector
#'
#' @param x1 Start location which determines the start of the sector.
#' @param x2 End location which determines the end of the sector. Note `x2` should be larger than x1 and the angular difference between `x1` and `x2` should be smaller than a circle.
#' @param x3 Start location which determines the start of the sector on the upper border.
#' @param x4 End location which determines the end of the sector on the upper border.
#' @param padding It controls the radial extension of the sector. The value should be a [`grid::unit()`] object with length one or two.
#' @param gp Graphical parameters.
#'
#' @details
#' `x1` and `x2` determine the position of the highlighted sector. If `x3` and `x4` are not set, the sector extends until the most outside loop.
#' If `x3` and `x4` are set, they determine the outer border of the sector. In this case, if `x3` and `x4` are set, `x3` should be larger than `x2`.
#'
#' @return
#' No value is returned.
#' @export
#' @examples
#' spiral_initialize(xlim = c(0, 360*4), start = 360, end = 360*5)
#' spiral_track()
#' spiral_axis()
#' spiral_highlight_by_sector(36, 72)
#' spiral_highlight_by_sector(648, 684)
#' spiral_highlight_by_sector(216, 252, 936, 972, gp = gpar(fill = "blue"))
spiral_highlight_by_sector = function(x1, x2, x3 = NULL, x4 = NULL, padding = unit(1, "mm"),
	gp = gpar(fill = "red")) {

	spiral = spiral_env$spiral
	if(spiral$scale_by != "angle") {
		stop_wrap("spiral_highlight_by_sector() can only be used when scale_by = 'angle'.")
	}
	# just to make sure x1/x2 is always smaller than x3/x4
	if(x1 > x2) {
		foo = x1
		x1 = x2
		x2 = foo
	}

	spiral = spiral_env$spiral
	x1 = spiral$get_x_from_data(x1)
	x2 = spiral$get_x_from_data(x2)

	upper_defined = !is.null(x3) && !is.null(x4)

	if(upper_defined) {
		if(x3 > x4) {
			foo = x3
			x3 = x4
			x4 = foo
		}

		if(x2 > x3) {
			stop_wrap("x3/x4 should be larger than x1/x2.")
		}

		spiral = spiral_env$spiral
		x3 = spiral$get_x_from_data(x3)
		x4 = spiral$get_x_from_data(x4)
	}

	if(abs(diff(get_theta_from_x(c(x1, x2), flip = FALSE))) > 2*pi) {
		stop_wrap("Angular difference between x1 and x2 should not be larger than a circle.")
	}
	if(upper_defined) {
		if(abs(diff(get_theta_from_x(c(x3, x4), flip = FALSE))) > 2*pi) {
			stop_wrap("Angular difference between x3 and x4 should not be larger than a circle.")
		}
	}

 	if(!is.unit(padding)) {
		stop_wrap("`padding` can only be a unit object.")
	}
	if(length(padding) == 1) {
		padding = rep(padding, 2)
	}
	offset = convertWidth(padding, "native", valueOnly = TRUE)
	

 	if(spiral$reverse) {
 		df1 = xy_to_polar(c(x1, x2), rep(get_track_data("ymax", n_tracks()), 2), track_index = n_tracks(), flip = FALSE)
		theta1 = seq(df1[2, 1], df1[1, 1], by = 0.5/180*pi)
		
		if(upper_defined) {  # when reverse is TRUE, upper is actually inside of the spiral
			df2 = xy_to_polar(c(x3, x4), rep(get_track_data("ymin", 1), 2), track_index = 1, flip = FALSE)
			theta2 = seq(df2[2, 1], df2[1, 1], by = 0.5/180*pi)
		} else {
			theta2 = get_theta_from_x(c(x1, x2), flip = FALSE)
			while(1) {
				if(theta2[1] - 2*pi < spiral$theta_lim[1]) {
					break
				}
				theta2 = theta2 - 2*pi
			}
			theta2 = seq(theta2[2], theta2[1], by = 0.5/180*pi)
		}
		theta = c(rev(theta1), theta2)

		r1 = spiral$curve(theta1) + sum(sapply(1:n_tracks(), function(i) get_track_data("rmax", i) - get_track_data("rmin", i)))
		r2 = spiral$curve(theta2)
		
		r1 = r1 + offset[1]
		r2 = r2 - offset[2]

		r = c(rev(r1), r2)

 	} else {
		df1 = xy_to_polar(c(x1, x2), rep(get_track_data("ymin", 1), 2), track_index = 1, flip = FALSE)
		theta1 = seq(df1[1, 1], df1[2, 1], by = 0.5/180*pi)
		
		if(upper_defined) {
			df2 = xy_to_polar(c(x3, x4), rep(get_track_data("ymax", n_tracks()), 2), track_index = n_tracks(), flip = FALSE)
			theta2 = seq(df2[1, 1], df2[2, 1], by = 0.5/180*pi)
		} else {
			theta2 = get_theta_from_x(c(x1, x2), flip = FALSE)
			while(1) {
				if(theta2[1] + 2*pi > spiral$theta_lim[2]) {
					break
				}
				theta2 = theta2 + 2*pi
			}
			theta2 = seq(theta2[1], theta2[2], by = 0.5/180*pi)
		}
		theta = c(theta1, rev(theta2))

		r1 = spiral$curve(theta1)
		r2 = spiral$curve(theta2) + sum(sapply(1:n_tracks(), function(i) get_track_data("rmax", i) - get_track_data("rmin", i)))
		
		r1 = r1 - offset[1]
		r2 = r2 + offset[2]

		r = c(r1, rev(r2))
	
	}

	# now consider flipping
	theta = flip_theta(theta)

	df = polar_to_cartesian(c(theta, theta[1]), c(r, r[1]))

	if("fill" %in% names(gp)) {
		gp$fill = add_transparency(gp$fill, 0.75)
	}
	if(!"col" %in% names(gp)) {
		gp$col = NA
	}

	grid.polygon(c(df$x, 0), c(df$y, 0), gp = gp, default.units = "native")
}

#' @importFrom grDevices col2rgb rgb
add_transparency = function (col, transparency = 0){
	col1 = col2rgb(col, alpha = TRUE)
	col1[4, col1[4, ] == 255] = round((1 - transparency)*255)
    rgb(t(col1/255), alpha = col1[4, ]/255)
}

