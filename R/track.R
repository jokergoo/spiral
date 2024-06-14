
track_env = new.env()

track_env$current_track = 0
empty_track_data = data.frame(
	i = integer(0), 
	ymin = numeric(0),
	ymax = numeric(0),
	rmin = numeric(0),
	rmax = numeric(0),
	rel_height = numeric(0),
	reverse_y = logical(0)
)

track_env$track_data = empty_track_data

#' Helper functions for handling tracks
#'
#' @rdname track_helper
#' @export
#' @return
#' `current_track_index()` returns the index of the current track.
current_track_index = function() {
	track_env$current_track
}

#' @param track_index The index of the track.
#'
#' @rdname track_helper
#' @export
#' @return
#' `set_current_track()` returns no value.
set_current_track = function(track_index) {
	if(!track_existed(track_index)) {
		stop_wrap(qq("Track @{track_index} does not exist."))
	}
	track_env$current_track = track_index
}

add_track = function(i, new_track_data) {

	height_remain = 1 - sum(track_env$track_data[, "rel_height"])
	if(new_track_data$rel_height > height_remain) {
		stop_wrap(qq("There is no space for the new track (height = @{new_track_data$rel_height}). The value should be smaller than @{height_remain}. Note 'height' is a relative fraction between 0 and 1."))
	}

	track_env$track_data = rbind(track_env$track_data, new_track_data)
	track_env$current_track = i
	set_current_track(i)
}

#' Meta-data of a track
#'
#' @param field Name of the field, see the **Details** section.
#' @param track_index The index of the track.
#'
#' @details
#' There are following fields that can be retrieved for a given track:
#'
#' - `ymin`: Minimal value on the y-axis.
#' - `ymax`: Maximal value on the y-axis.
#' - `ycenter`: `(ymin + ymax)/2`.
#' - `ylim`: `c(ylim, ymax)`.
#' - `yrange`: `ymax - ymin`.
#' - `height`: Height of the track, measured as the fraction of the distance between two neighbouring spiral loops.
#'
#' It is more suggested to directly use [`TRACK_META`] to retrieve meta data for the current track.
#' 
#' @export
#' @return
#' A numeric vector (of length one or two) for the corresponding field.
get_track_data = function(field, track_index = current_track_index()) {

	if(!track_existed(track_index)) {
		stop_wrap(qq("Track @{track_index} does not exist."))
	}

	if(field == "ycenter") {
		(track_env$track_data[track_index, "ymin"] + track_env$track_data[track_index, "ymax"])/2
	} else if(field == "ylim") {
		c(track_env$track_data[track_index, "ymin"], track_env$track_data[track_index, "ymax"])
	} else if(field == "yrange") {
		abs(track_env$track_data[track_index, "ymax"] - track_env$track_data[track_index, "ymin"])
	} else if(field == "rrange") {
		track_env$track_data[track_index, "rmax"] - track_env$track_data[track_index, "rmin"]
	} else if(field == "height") {
		track_env$track_data[track_index, "rel_height"]
	} else {
		track_env$track_data[track_index, field]
	}
}

track_existed = function(track_index) {
	track_index <= nrow(track_env$track_data)
}

#' @rdname track_helper
#' @export
#' @return
#' `n_tracks()` returns the number of available tracks.
n_tracks = function() {
	nrow(track_env$track_data)
}


#' Get meta data in the current track
#'
#' @rdname track_meta
#' @details
#' The variable `TRACK_META` can only be used to get meta data from the "current" track. If the current track
#' is not the one you want, you can first use [`set_current_track()`] to change the current track.
#'
#' Don't directly use `TRACK_META`. The value of `TRACK_META` itself is meaningless. Always use in form of `TRACK_META$name`.
#'
#' There are the following meta data for the current track:
#'
#' - `xlim`: Data range on x-axis.
#' - `xmin`: `xlim[1]`.
#' - `xmax`: `xlim[2]`.
#' - `xrange`: `xlim[2] - xlim[1]`.
#' - `xcenter`: `mean(xlim)`.
#' - `theta_lim`: Range of the angles on the spiral, measured in radians.
#' - `theta_min`: `theta_lim[1]`.
#' - `theta_max`: `theta_lim[2]`.
#' - `theta_range`: `theta_lim[2] - theta_lim[1]`.
#' - `theta_center`: `mean(theta_lim)`.
#' - `ylim`: Data range on y-axis.
#' - `ymin`: `ylim[1]`.
#' - `ymax`: `ylim[2]`.
#' - `yrange`: `ylim[2] - ylim[1]`.
#' - `ycenter`: `mean(ylim)`.
#' - `rel_height`: Fraction of height of the track to the distance between two neighbouring loops.
#' - `abs_height`: The height of the track, which is `rel_height` multiplied by the distance between two neighbouring loops.
#' - `track_index`: Current track index.
#'
#' @export
#' @examples
#' spiral_initialize(xlim = c(0, 1))
#' spiral_track(ylim = c(0, 1))
#' for(nm in names(TRACK_META)) {
#'     cat(nm, ":\n", sep = "")
#'     print(TRACK_META[[nm]])
#'     cat("\n")
#' }
TRACK_META = NA
class(TRACK_META) = "TRACK_META"

#' @param x The `TRACK_META` object.
#' @rdname track_meta
#' @export
#' @examples
#' names(TRACK_META)
names.TRACK_META = function(x) {
	
	nm = c("xlim", "xmin", "xmax", "xcenter", "xrange",
		   "theta_lim", "theta_min","theta_max", "theta_center", "theta_range",
		   "ylim", "ymin", "ymax", "ycenter", "yrange",
		   # "radius_lim", "radius_min", "radius_max", "radius_center", "radius_range",
		   "abs_height", "rel_height", "track_index")

	return(nm)
}


#' @param name Name of the meta name. For all supported names, type `names(TRACK_META)`.
#' @rdname track_meta
#' @export
"$.TRACK_META" = function(x, name) {
	spiral = current_spiral()

	if(n_tracks() == 0) {
		stop_wrap("No track has been created.")
	}

	if(name == "xlim") {
		spiral$get_data_from_x(spiral$xlim)
	} else if(name == "xmin") {
		spiral$get_data_from_x(spiral$xlim[1])
	} else if(name == "xmax") {
		spiral$get_data_from_x(spiral$xlim[2])
	} else if(name == "xrange") {
		spiral$xlim[2] - spiral$xlim[1]
	} else if(name == "xcenter") {
		mean(spiral$xlim)
	} else if(name == "theta_lim") {
		spiral$theta_lim
	} else if(name == "theta_min") {
		spiral$theta_lim[1]
	} else if(name == "theta_max") {
		spiral$theta_lim[2]
	} else if(name == "theta_range") {
		spiral$theta_lim[2] - spiral$theta_lim[1]
	} else if(name == "theta_center") {
		mean(spiral$theta_lim)
	} else if(name == "ylim") {
		c(get_track_data("ymin"), get_track_data("ymax"))
	} else if(name == "ymin") {
		get_track_data("ymin")
	} else if(name == "ymax") {
		get_track_data("ymax")
	} else if(name == "yrange") {
		get_track_data("ymax") - get_track_data("ymin")
	} else if(name == "ycenter") {
		(get_track_data("ymin") + get_track_data("ymax"))/2
	} else if(name == "abs_height") {
		get_track_data("rel_height")*( get_track_data("rmax") - get_track_data("rmin") )
	} else if(name == "rel_height") {
		get_track_data("rel_height")
	} else if(name == "track_index") {
		current_track_index()
	} else {
		stop_wrap(qq("'@{name}' is not supported in TRACK_META."))
	}
}

#' @param i Name of the meta name. For all supported names, type `names(TRACK_META)`.
#' @param exact Please ignore.
#' @rdname track_meta
#' @export
"[[.TRACK_META" = function(x, i, exact = TRUE) {
	`$.TRACK_META`(x, i)
}

#' @rdname track_meta
#' @export
"[.TRACK_META" = function(x, i) {
	`$.TRACK_META`(x, i)
}

#' @param ... Additional parameters.
#' @rdname track_meta
#' @export
print.TRACK_META = function(x, ...) {
	cat("Please use in form of `TRACK_META$name`. Type `names(TRACK_META)` for supported names.\n")
	cat("\n")

	if(n_tracks() == 0) {
		cat("No track has been created.\n")
	} else {
		cat("There are the following meta data for the current track:\n")
		for(nm in names(TRACK_META)) {
			cat("  ", nm, ": ", sep = "")
			if(length(TRACK_META[[nm]]) == 1) {
				cat(TRACK_META[[nm]], "\n")
			} else {
				cat("c(", TRACK_META[[nm]][1], ", ", TRACK_META[[nm]][2], ")\n")
			}
		}
	}
}

#' @param x X-location of data points.
#' @param y Y-location of data points.
#'
#' @details
#' `is_in_track()` tests whether data points are inside a certain track.
#'
#' @rdname track_helper
#' @export
#' @return
#' `is_in_track()` returns a logical vector.
is_in_track = function(x, y, track_index = current_track_index()) {
	s = current_spiral()
	xlim = s$xlim
	ylim = get_track_data("ylim", track_index)
	ylim = sort(ylim)

	x >= xlim[1] & x <= xlim[2] & y >= ylim[1] & y <= ylim[2]
}


#' Add a new track or move to an existed track
#'
#' @param ylim Data range of the y-locations.
#' @param height Height of the track. The value can be the fraction of the distance of the two neighbour spiral loops. The value can also be a [`grid::unit()`] object.
#' @param background Whether to draw the background of the track, i.e. border and filled color of background.
#' @param background_gp Graphical parameters of the background.
#' @param reverse_y Whether reverse the direction of y-axis (i.e. pointing to the center of the spiral)?
#' @param gradient Whether draw the background in gradient? The value can be a positive integer of the number of gradients from `background_gp$fill` to white.
#' @param track_index Index of the track. 
#'
#' @details
#' If the track is already existed, the function simply mark the track as the current track and does nothing else.
#'
#' @export
#' @importFrom circlize colorRamp2
#' @importFrom GetoptLong qq
#' @return No value is returned.
#'
#' @examples
#' spiral_initialize()
#' spiral_track(height = 0.8)
#'
#' spiral_initialize()
#' spiral_track(height = 0.4, background_gp = gpar(fill = "red"))
#' spiral_track(height = 0.2, background_gp = gpar(fill = "green"))
#' spiral_track(height = 0.1, background_gp = gpar(fill = "blue"))
#' 
#' spiral_initialize()
#' spiral_track(height = 0.8, gradient = TRUE) # by default 10 gradients
#' 
#' spiral_initialize()
#' spiral_track(height = 0.8, background_gp = gpar(fill = "red"), gradient = 5)
spiral_track = function(ylim = c(0, 1), height = 0.8, background = TRUE, 
	background_gp = gpar(fill = "#EEEEEE"), reverse_y = FALSE,
	gradient = FALSE, track_index = current_track_index() + 1) {

	spiral = spiral_env$spiral
	dist = spiral$dist

	if(is.unit(height)) {
		height = convertHeight(height, "native", valueOnly = TRUE)
		height = height/dist
	}

	if(track_existed(track_index)) {
		# only reset current track
		set_current_track(track_index)
	} else {
		# a new track
		if(!track_existed(track_index - 1)) {
			stop_wrap(qq("There are only @{n_tracks()} existed. The value of `track_index` should not be larger than @{n_tracks() + 1}."))
		} else {
			sum_height = sum(track_env$track_data[, "rel_height"])
			new_track_data = data.frame(i = track_index, 
				ymin = ylim[1], ymax = ylim[2], 
				rmin = sum_height*dist, rmax = (sum_height + height)*dist, 
				rel_height = height, reverse_y = reverse_y)
			add_track(track_index, new_track_data)
		}

		if(spiral$theta_range/2/pi > 30) { # if there are more than 30 loops
			if(missing(background)) {
				background = FALSE
			}
		}

		if(background) {
			if(!"col" %in% names(background_gp)) {
				background_gp$col = NA
			}
			if(!"fill" %in% names(background_gp)) {
				background_gp$fill = "#EEEEEE"
			}
			ymin = get_track_data("ymin")
			ymax = get_track_data("ymax")
			yrange = ymax - ymin
			if(identical(gradient, FALSE)) {
				spiral_rect(spiral$xlim[1], ymin, spiral$xlim[2], ymax, gp = background_gp)
			} else {
				if(identical(gradient, TRUE)) {
					gradient = 10
				}

				col_fun = colorRamp2(c(0, gradient), c("white", background_gp$fill))
				for(i in seq_len(gradient)) {
					spiral_rect(spiral$xlim[1], ymin + yrange*(i-1)/gradient, spiral$xlim[2], ymin + yrange*i/gradient, gp = gpar(fill = col_fun(i), col = col_fun(i)))
				}

				if(!identical(background_gp$col, NA)) {
					background_gp2 = background_gp
					background_gp2$fill = NA
					spiral_rect(spiral$xlim[1], ymin, spiral$xlim[2], ymax, gp = background_gp2)
				}
			}
		}
	}
}

