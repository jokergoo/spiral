
#' Transform between coordinate systems
#'
#' @param x X-locations of the data points.
#' @param y Y-locations of the data points.
#' @param track_index Index of the track. 
#'
#' @details
#' There are three coordinate systems: the data coordinate system (xy), the polar coordinate system (polar)
#' and the canvas coordinate system (cartesian). The canvas coordinates correspond to the "native" coordinates of the viewport where the graphics are drawn.
#' 
#' Note different settings of `flip` and `reverse` in [`spiral_initialize()`] affect the conversion.
#' 
#' `xy_to_cartesian()` converts from the data coordinate system to the canvas coordinate system. 
#'
#' @export
#' @rdname coordinate
#' @return
#' `xy_to_cartesian()` returns A data frame with two columns: `x` and `y`.
#' 
#' @examples
#' x = runif(2)
#' y = runif(2)
#' spiral_initialize(xlim = c(0, 1))
#' spiral_track(ylim = c(0, 1))
#' spiral_points(x, y)
#' xy_to_cartesian(x, y)
#' xy_to_polar(x, y)
xy_to_cartesian = function(x, y, track_index = current_track_index()) {
	df_polar = xy_to_polar(x, y, track_index = track_index)
	polar_to_cartesian(df_polar$theta, df_polar$r)
}

#' @param flip If it is `FALSE`, it returns theta for the original spiral (before flipping).
#'
#' @details
#' `xy_to_polar()` converts from the data coordinate system to the polar coordinate system.
#'
#' @export
#' @rdname coordinate
#' @return
#' `xy_to_polar()` returns a data frame with two columns: `theta` (in radians) and `r` (the radius).
xy_to_polar = function(x, y, track_index = current_track_index(), flip = TRUE) {

	if(track_index == 0) {
		stop_wrap("No track has been created. Maybe you need to call `spiral_track()` first.")
	}

	spiral = spiral_env$spiral

	if(spiral$scale_by == "angle") {
		theta = (x - spiral$xlim[1]) * spiral$theta_range/spiral$xrange + spiral$theta_lim[1]

		if(spiral$reverse) {
			theta = spiral$theta_lim[2] - (theta - spiral$theta_lim[1])
		}
	} else if(spiral$scale_by == "curve_length") {
		if(spiral$reverse) {
			len = (spiral$xlim[2] - x) * spiral$spiral_length_range/spiral$xrange + spiral$spiral_length_lim[1]
		} else {
			len = (x - spiral$xlim[1]) * spiral$spiral_length_range/spiral$xrange + spiral$spiral_length_lim[1]
		}
		theta = solve_theta_from_spiral_length(len)
	}

	ymin = get_track_data("ymin", track_index)
	ymax = get_track_data("ymax", track_index)
	rmin = get_track_data("rmin", track_index)
	rmax = get_track_data("rmax", track_index)
	reverse_y = get_track_data("reverse_y", track_index)
	if(reverse_y) {
		d = rmax - (y - ymin) * (rmax - rmin)/(ymax - ymin) 
	} else {
		d = (y - ymin) * (rmax - rmin)/(ymax - ymin) + rmin
	}
	r = spiral$curve(theta) + d

	if(flip) theta = flip_theta(theta)

	data.frame(theta = theta, r = r)
}

#' @param theta Angles, in radians.
#' @param r Radius.
#' 
#' @details
#' `polar_to_cartesian()` converts from the polar coordinate system to the canvas coordinate system.
#'
#' @export
#' @rdname coordinate
#' @return
#' `polar_to_cartesian()` returns a data frame with two columns: `x` and `y`.
polar_to_cartesian = function(theta, r) {
	x = cos(theta)*r
	y = sin(theta)*r
	
	data.frame(x = x, y = y)
}

#' @details
#' `cartesian_to_polar()` converts from the canvas coordinate system to the polar coordinate system.
#'
#' @export
#' @rdname coordinate
#' @return
#' `cartesian_to_polar()` returns a data frame with two columns: `theta` (in radians) and `r` (the radius).
cartesian_to_polar = function(x, y) {
	theta = atan(y/x)
	r = sqrt(x*x + y*y)

	data.frame(theta = theta, r = r)
}

# split a spiral line into d seguments
# return in cartesian coordinates
spiral_lines_expand = function(x, y, track_index = current_track_index()) {

	df = xy_to_polar(x, y, track_index = track_index)
	n = nrow(df)

	## here x and y are theta and r
	x = df$theta
	y = df$r
	nx = x[1]
	ny = y[1]
	min_segment_len = spiral_opt$min_segment_len
	for(i in seq_len(n)) {
		if(i == 1) {
			next
		}
		if(is.na(x[i]) || is.na(y[i])) {
            nx = c(nx, NA)
            ny = c(ny, NA)
            next
        }
        if(is.na(x[i-1]) || is.na(y[i-1])) {
            next
        }

        d = min_segment_len/(6*pi)
        if(x[i-1] <= 4*pi) {
        	d = min_segment_len
        } else {
        	d = min_segment_len*(4*pi/x[i-1])
        }

        if(abs(x[i] - x[i-1]) <= d) {
        	nc = 2
        } else {
        	nc = ceiling(abs(x[i] - x[i-1])/d)
        }

        nx = c(nx, seq(x[i-1], x[i], length.out = nc)[-1])
        ny = c(ny, seq(y[i-1], y[i], length.out = nc)[-1])
	}

	polar_to_cartesian(nx, ny)
}


# return in cartesian coordinates
radial_extend = function(x, y, offset, track_index = current_track_index()) {
	df = xy_to_polar(x, y, track_index = track_index)
	offset_x = convertWidth(offset*cos(df$theta), "native", valueOnly = TRUE)
	offset_y = convertHeight(offset*sin(df$theta), "native", valueOnly = TRUE)
	df = polar_to_cartesian(df$theta, df$r)
	df2 = df
	df2$x = df2$x + offset_x
	df2$y = df2$y + offset_y

	df2
}


# in xy coordinates
circular_extend_on_x = function(x, y, offset, track_index = track_index, coordinate = "polar") {
	spiral = spiral_env$spiral

	if(is.unit(offset)) {
		offset = convertWidth(offset, "native", valueOnly = TRUE)
	}

	df = xy_to_polar(x, y, track_index = track_index)
	if(spiral$scale_by == "curve_length") {
		v_offset = convert_y_to_height(y, track_index = track_index)
		x_offset = offset/(spiral$spiral_length(spiral$theta_lim[2], v_offset) - spiral$spiral_length(spiral$theta_lim[1], v_offset))*spiral$xrange

		df$theta = flip_theta_back(df$theta)
		t = solve_theta_from_spiral_length(spiral$spiral_length(df$theta, v_offset) + offset, 
				c(spiral$spiral_length(-spiral$theta_lim[2], max(v_offset)), 
				  spiral$spiral_length(2*spiral$theta_lim[2], max(v_offset))),
				v_offset)
		t = flip_theta(t)
		if(coordinate == "polar") {
			t
		} else {
			polar_to_x(t)
		}
	} else {
		v_offset = convert_y_to_height(y, track_index = track_index)
		df$theta = flip_theta_back(df$theta)
		t = solve_theta_from_spiral_length(spiral$spiral_length(df$theta, v_offset) + offset, 
				c(spiral$spiral_length(-spiral$theta_lim[2], max(v_offset)), 
				  spiral$spiral_length(2*spiral$theta_lim[2], max(v_offset))),
				v_offset)
		t = flip_theta(t)
		if(coordinate == "polar") {
			t
		} else {
			polar_to_x(t)
		}
	}
}

# given theta in spiral, it returns the corresponding x
polar_to_x = function(theta) {

	spiral = spiral_env$spiral

	theta = flip_theta_back(theta)

	if(spiral$scale_by == "angle") {
		x = (theta - spiral$theta_lim[1])/spiral$theta_range*spiral$xrange + spiral$xlim[1]
	} else {
		x = (spiral$spiral_length(theta) - spiral$spiral_length_lim[1])/spiral$spiral_length_range*spiral$xrange + spiral$xlim[1]
	}

	if(spiral$reverse) {
		x = spiral$xlim[2] - x + spiral$xlim[1]
	}
	x
}

# given a offset in absolute unit, it returns the offset in xy coordinates
convert_height_to_y = function(offset, track_index = current_track_index()) {
	offset = convertWidth(offset, "native", valueOnly = TRUE)

	ymin = get_track_data("ymin", track_index)
	ymax = get_track_data("ymax", track_index)
	rmin = get_track_data("rmin", track_index)
	rmax = get_track_data("rmax", track_index)

	offset/(rmax - rmin)*(ymax - ymin)
}

# here `height` is the offset to the base spiral
convert_y_to_height = function(y, track_index = current_track_index()) {
	spiral = spiral_env$spiral

	ymin = get_track_data("ymin", track_index)
	ymax = get_track_data("ymax", track_index)
	rmin = get_track_data("rmin", track_index)
	rmax = get_track_data("rmax", track_index)
	reverse_y = get_track_data("reverse_y", track_index)
	
	if(reverse_y) {
		v_offset = (ymax - y)/(ymax - ymin)*(rmax - rmin)
	} else {
		v_offset = (y - ymin)/(ymax - ymin)*(rmax - rmin)
	}
	if(track_index > 1) {
		for(i in 1:(track_index - 1)) {
			v_offset = v_offset + get_track_data("rmax", i) - get_track_data("rmin", i)
		}
	}
	v_offset
}


get_theta_from_x = function(x, ...) {
	xy_to_polar(x, rep(0, length(x)), ...)$theta
}

#' Get theta from given spiral lengths
#'
#' @param len A vector of spiral lengths.
#' @param interval Interval to search for the solution.
#' @param offset Offset of the spiral. In the general form: ` r = a + r*theta`, offset is the value of `a`.
#'
#' @details
#' The length of the spiral has a complicated form, see [https://downloads.imagej.net/fiji/snapshots/arc_length.pdf](https://downloads.imagej.net/fiji/snapshots/arc_length.pdf).
#' Let's say the form is `l = f(theta)` where `f()` is the complex equation for calculating `l`, `solve_theta_from_spiral_length()` tries to find theta with a known `l`.
#' It uses [`stats::uniroot()`] to search for the solutions.
#'
#' @return The theta value.
#' @export
#' @importFrom stats uniroot
#' @examples
#' spiral_initialize()
#' s = current_spiral()
#' theta = pi*seq(2, 3, length = 10)
#' theta
#' len = s$spiral_length(theta)
#' solve_theta_from_spiral_length(len) # should be very similar as theta
solve_theta_from_spiral_length = function(len, interval = NULL, offset = 0) {
	n = length(len)
	theta = numeric(n)

	spiral = spiral_env$spiral
	if(is.null(interval)) {
		interval = c(spiral$theta_lim[1] - spiral$theta_range, spiral$theta_lim[2] + spiral$theta_range)
	}

	offset = offset
	if(length(offset) == 1) offset = rep(offset, n)
	for(i in seq_len(n)) {
		theta[i] = uniroot(function(theta, a) {
			spiral$spiral_length(theta, offset[i]) - a
		}, interval = interval, a = len[i])$root
	}

	theta
}


flip_theta = function(theta) {
	spiral = spiral_env$spiral

	if(spiral$flip == "horizontal") {
		theta = pi - theta 
	} else if(spiral$flip == "vertical") {
		theta = 2*pi - theta
	} else if(spiral$flip == "both") {
		theta = theta - pi
	} else {
		theta
	}
}

flip_theta_back = function(theta) {
	spiral = spiral_env$spiral

	if(spiral$flip == "horizontal") {
		theta = pi - theta 
	} else if(spiral$flip == "vertical") {
		theta = 2*pi - theta
	} else if(spiral$flip == "both") {
		theta = theta + pi
	} else {
		theta
	}
	theta
}


#' @details
#' `cartesian_to_xy()` converts from the canvas coordinate system to the data coordinate system. 
#' The data points are assigned to the nearest inner spiral loops (if the point is located inside a certain spiral loop, the distance is zero). 
#'
#' @return
#' `cartesian_to_xy()` returns a data frame with two columns: `x` and `y`.
#' @rdname coordinate
#' @export
#' @examples
#' 
#' x = runif(100, -4, 4)
#' y = runif(100, -4, 4)
#' spiral_initialize(xlim = c(0, 1))
#' spiral_track(ylim = c(0, 1))
#' df = cartesian_to_xy(x, y)
#' # directly draw in the viewport
#' grid.points(x, y, default.units = "native")
#' # check whether the converted xy are correct (should overlap to the previous points)
#' spiral_points(df$x, df$y, pch = 16, gp = gpar(col = 2))
cartesian_to_xy = function(x, y, track_index = current_track_index()) {

	if(length(x) > 1) {
		n = length(x)

		df = data.frame(x = numeric(n), y = numeric(n))

		for(i in seq_len(n)) {
			df2 = cartesian_to_xy(x[i], y[i], track_index)
			df[i, 1] = df2[1, 1]
			df[i, 2] = df2[1, 2]
		}

		return(df)
	}

	s = spiral_env$spiral
	r0 = sqrt(x^2 + y^2)

	if(r0 == 0) {
		theta = mean(s$theta_lim)
	} else {

		# make sure alpha is between [0, 2pi)
		alpha = atan(y/x)
		if(y >= 0 & x < 0) {
			alpha = alpha + pi
		} else if(y < 0 & x < 0) {
			alpha = alpha + pi
		} else if(x >= 0 & y < 0) {
			alpha = alpha + 2*pi
		}


		while(alpha < s$theta_lim[1]) {
			alpha = alpha + 2*pi
		}
		if(alpha > s$theta_lim[2]) {
			stop_wrap("The radial line the connects data point and the origin should intersect the spiral.")
		}
		t2 = seq(alpha, s$theta_lim[2], by = 2*pi)

		all_r = s$curve(t2)
		if(all_r[length(all_r)] <= r0) {
			theta = t2[length(t2)]
		} else if(all_r[1] >= r0) {
			theta = t2[1]
		} else {
			ind = which(all_r <= r0)
			theta = t2[ ind[length(ind)] ]
		}
	}

	x2 = polar_to_x(theta)
	d0 = r0 - s$curve(theta)

	ymin = get_track_data("ymin", track_index)
	ymax = get_track_data("ymax", track_index)
	rmin = get_track_data("rmin", track_index)
	rmax = get_track_data("rmax", track_index)
	reverse_y = get_track_data("reverse_y", track_index)

	if(reverse_y) {
		y2 = ymax - (d0 - rmin)/(rmax - rmin)*(ymax - ymin)
	} else {
		y2 = (d0 - rmin)/(rmax - rmin)*(ymax - ymin) + ymin
	}

	data.frame(x = x2, y = y2)
}
