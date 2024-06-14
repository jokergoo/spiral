### a reference class for a spiral object

#' @import methods
create_spiral = function(start = 360, end = 360*5, xlim = c(0, 1), scale_by = "angle", 
	flip = "none", reverse = FALSE) {

	a = 0
	b = 1/2/pi  # the radial distance between two neighbouring circles is 1

	start = start/180*pi
	end = end/180*pi

	xrange = xlim[2] - xlim[1]
	theta_lim = c(start, end)
	theta_range = end - start

	spiral_length = function(theta) {
		b/2*(theta*sqrt(1+theta*theta) + log(theta+sqrt(1+theta*theta)))
	}
	spiral_length_lim = spiral_length(c(start, end))
	spiral_length_range = spiral_length_lim[2] - spiral_length_lim[1]
	
    max_radius = max(b*end)
    dist = b * 2*pi

    if(!reverse) {
    	clockwise = switch(flip,
    		"none" = FALSE,
    		"horizontal" = TRUE,
    		"vertical" = TRUE,
    		"both" = FALSE
    	)
    } else {
    	clockwise = !switch(flip,
    		"none" = FALSE,
    		"horizontal" = TRUE,
    		"vertical" = TRUE,
    		"both" = FALSE
    	)
    }

    spiral(
    	a = a,
		b = b,
		xlim = xlim,
		xrange = xrange,
		theta_lim = theta_lim,
		theta_range = theta_range,
		spiral_length_lim = spiral_length_lim,
		spiral_length_range = spiral_length_range,
		max_radius = max_radius,
		dist = dist,
		scale_by = scale_by,
		flip = flip,
		reverse = reverse,
		clockwise = clockwise
	)
}

spiral = setRefClass("spiral",
	fields = list(
		a = "numeric",
		b = "numeric",
		xlim = "numeric",
		xrange = "numeric",
		theta_lim = "numeric",
		theta_range = "numeric",
		spiral_length_lim = "numeric",
		spiral_length_range = "numeric",
		max_radius = "numeric",
		dist = "numeric",
		scale_by = "character",
		flip = "character",
		reverse = "logical",
		xclass = "character",
		get_x_from_data = "function",   # given an input data type, returns the internal numeric 
		get_data_from_x = "function",   # given an internal numeric type, return the data type of the input
		get_character_from_x = "function", # given an input data type, returns its character representation
		clockwise = "logical",
		other = "list"
	),
	methods = list(
		show = function() {
			qqcat(
"An Archimedean spiral (r = b*theta) with the following parameters:
  b: @{.self$b}
  distance between two neighbouring circles: @{.self$dist}
  xlim: [@{.self$xlim[1]}, @{.self$xlim[2]}] @{ifelse(.self$reverse, '(from outside of the curve to the inside)', '')}
  range of theta (in degrees): [@{round(as.degree(.self$theta_lim[1], scale = FALSE))}, @{round(as.degree(.self$theta_lim[2], scale = FALSE))}]
  The spiral curve is linearly scaled by @{.self$scale_by}.
")
			if(.self$flip == "horizontal") {
				cat("  spiral is flipped horizontally.\n")
			} else if(.self$flip == "vertical") {
				cat("  spiral is flipped vertically.\n")
			} else if(.self$flip == "both") {
				cat("  spiral is flipped both horizontally and vertically.\n")
			}
		},
		curve = function(theta, offset = 0) {
			.self$b*theta + offset
		},
		# https://downloads.imagej.net/fiji/snapshots/arc_length.pdf
		spiral_length = function(theta, offset = 0) {
			b2 = .self$b
			a2 = offset

			s1 = (2*b2*b2*theta + 2*a2*b2)/sqrt(4*b2*b2*(b2*b2+a2*a2) - 4*a2*a2*b2*b2)
			s1 = log(s1+sqrt(1+s1*s1))/2/b2
			s2 = sqrt(b2*b2*theta*theta + 2*a2*b2*theta+b2*b2+a2*a2)/2

			-a2*a2*s1 + (b2*b2 + a2*a2)*s1 + theta*s2 + a2*s2/b2
		},
		tangent_slope = function(theta) {
			(tan(theta) + theta)/(1 - theta*tan(theta))
		},
		initialize = function(..., xclass = "numeric", get_x_from_data = function(x) x, get_data_from_x = function(x) x,
			get_character_from_x = function(x) x, other = list()) {
			callSuper(..., xclass = xclass, get_x_from_data = get_x_from_data, get_data_from_x = get_data_from_x, get_character_from_x = get_character_from_x, other = other)
		},
		draw_spiral = function(start = as.degree(.self$theta_lim[1], scale = FALSE), end = as.degree(.self$theta_lim[2], scale = FALSE), 
			offset = 0) {
			theta = seq(start, end, by = 1)
			theta = theta/180*pi
			r = .self$curve(theta, offset)

			df = polar_to_cartesian(theta, r)
			rg = max(abs(df$x), abs(df$y))
			grid.newpage()
			pushViewport(viewport(xscale = c(-rg, rg), yscale = c(-rg, rg), width = unit(0.9, "snpc"), height = unit(0.9, "snpc")))
			grid.lines(df$x, df$y, default.units = "native")
			popViewport()
		},
		get_theta = function(x, degree = TRUE) {
			d = (x - .self$xlim[1])/.self$xrange*.self$theta_range + .self$theta_lim[1]
			if(degree) {
				d = d*180/pi
			}
			d
		}
	)
)

#' Get the current spiral object
#'
#' @details
#' The returned value is an object of `spiral` reference class. The following methods might be useful (assume the object is named `s`):
#'
#' - `s$curve()`: It returns the radius for given angles (in radians).
#' - `s$spiral_length()`: It returns the length of the spiral (from the origin) for a given angle (in radians), thus if you want to get the length of a spiral segment, 
#'     it will be `s$spiral_length(theta2) - s$spiral_length(theta1)`.
#'
#' Also there are the following meta-data for the current spiral:
#'
#' - `s$xlim`: Data range.
#' - `s$xrange`: `s$xlim[2] - s$xlim[1]`.
#' - `s$theta_lim`: The corresponding range of theta.
#' - `s$theta_range`: `s$theta_lim[2] - s$theta_lim[1]`.
#' - `s$spiral_length_lim`: The corresponding range of spiral length.
#' - `s$spiral_length_range`: `s$spiral_length_lim[2] - s$spiral_length_lim[1]`.
#' - `s$max_radius`: Radius at `s$theta_lim[2]`.
#'
#' @return
#' A `spiral` object.
#' @export
#' @examples
#' spiral_initialize()
#' s = current_spiral()
#' s$curve(2*pi*2)
#' s$spiral_length(2*pi*2)
current_spiral = function() {
	spiral = spiral_env$spiral

	if(is.null(spiral)) {
		stop_wrap("No spiral has been initialized.")
	}

	spiral
}
