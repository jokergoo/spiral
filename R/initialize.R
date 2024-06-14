
#' Initialize the spiral
#'
#' @param xlim Range on x-locations.
#' @param start Start of the spiral, in degree. `start` and `end` should be positive and `start` should be smaller than `end`.
#' @param end End of the spiral, in degree.
#' @param scale_by How scales on x-axis are equally interpolated? The values can be one of "angle" and "curve_length". If
#'      the value is "angle", equal angle difference corresponds to equal difference of data. In this case, in outer loops,
#'      the scales are longer than in the inner loops, although the difference on the data are the same. If
#'      the value is "curve_length", equal curve length difference corresponds to the equal difference of the data.
#' @param period Under "angle" mode, the number of loops can also be controlled by argument `period` which controls the length
#'       of data a spiral loop corresponds to. Note in this case, argument `end` is ignored and the value for `end` is 
#'       internally recalculated.
#' @param clockwise Whether the curve is in a closewise direction. If it is set to `TRUE`, argument `flip` and `reverse` are ignored.
#' @param flip How to flip the spiral? By default, the spiral starts from the origin of the coordinate and grows reverseclockwisely.
#'       The argument controls the growing direction of the spiral.
#' @param reverse By default, the most inside of the spiral corresponds to the lower boundary of x-location. Setting the value to `FALSE`
#'        can reverse the direction.
#' @param polar_lines Whether draw the polar guiding lines.
#' @param polar_lines_by Increment of the polar lines. Measured in degree. The value can also be a vector that defines where to add polar lines.
#' @param polar_lines_gp Graphics parameters for the polar lines.
#' @param padding Padding of the plotting region. The value can be a [`grid::unit()`] of length of one to two.
#' @param newpage Whether to apply [`grid::grid.newpage()`] before making the plot?
#' @param vp_param A list of parameters sent to [`grid::viewport()`].
#'
#' @return
#' No value is returned.
#' @export
#' @import grid
#' @examples
#' spiral_initialize(); spiral_track()
#' spiral_initialize(start = 180, end = 360+180); spiral_track()
#' spiral_initialize(flip = "vertical"); spiral_track()
#' spiral_initialize(flip = "horizontal"); spiral_track()
#' spiral_initialize(flip = "both"); spiral_track()
#' spiral_initialize(); spiral_track(); spiral_axis()
#' spiral_initialize(scale_by = "curve_length"); spiral_track(); spiral_axis()
#'
#' # the following example shows the difference of `scale_by` more clearly:
#' make_plot = function(scale_by) {
#'     n = 100
#'     require(circlize)
#'     col = circlize::colorRamp2(c(0, 0.5, 1), c("blue", "white", "red"))
#'     spiral_initialize(xlim = c(0, n), scale_by = scale_by)
#'     spiral_track(height = 0.9)
#' 
#'     x = runif(n)
#'     spiral_rect(1:n - 1, 0, 1:n, 1, gp = gpar(fill = col(x), col = NA))
#' }
#' make_plot("angle")
#' make_plot("curve_length")
spiral_initialize = function(xlim = c(0, 1), start = 360, end = 360*5, 
	scale_by = c("angle", "curve_length"), period = NULL,
	clockwise = FALSE, flip = c("none", "vertical", "horizontal", "both"), 
	reverse = FALSE, polar_lines = scale_by == "angle", polar_lines_by = 30, 
	polar_lines_gp = gpar(col = "#808080", lty = 3), 
	padding = unit(5, "mm"), newpage = TRUE, vp_param = list()) {

	if(inherits(xlim, c("POSIXt", "Date", "character"))) {
		stop_wrap("`xlim` is set as a time object, please use `spiral_initialize_by_time()` instead.")
	}

	spiral_clear(check_vp = FALSE)

	if(length(xlim) != 2) {
		stop_wrap("Length of `xlim` should be two.")
	}

	if(start < 0 || end < 0) {
		stop_wrap("`start` or `end` cannot be negative.")
	}
	if(start >= end) {
		stop_wrap("`start` should be smaller than `end`.")
	}

	scale_by = match.arg(scale_by)[1]
	flip = match.arg(flip)[1]

	if(!is.null(period)) {
		if(scale_by != "angle") {
			stop_wrap("You can only set `period` under 'angle' mode.")
		}
		xrange = xlim[2] - xlim[1]
		end = xrange/period*360 + start
	}

	if(clockwise) {
		rg = end - start
		start = 180 - start %% 360 + floor(start / 360)*360
		end = start + rg
		flip = "horizontal"
		reverse = FALSE
	}

	spiral = create_spiral(start = start, end = end, xlim = xlim, scale_by = scale_by, flip = flip, reverse = reverse)
	spiral_env$spiral = spiral
	spiral_env$i_spiral = spiral_env$i_spiral + 1

	# push a viewport
	theta = seq(spiral$theta_lim[1], spiral$theta_lim[2], by = 2/180*pi)

	r = spiral$b*theta
	dist = spiral$dist

	df1 = polar_to_cartesian(theta, r)
	df2 = polar_to_cartesian(theta, r + dist)

	x = c(df1$x, rev(df2$x))
	y = c(df1$y, rev(df2$y))
	abs_max = max(abs(x), abs(y))

	if(length(padding) == 1) {
		padding = rep(padding, 2)
	}
	if(newpage) {
		grid.newpage()
	}

	vp_param2 = list(name = paste0("spiral_", spiral_env$i_spiral),
		width = unit(1, "snpc") - padding[1], 
		height = unit(1, "snpc") - padding[2],
		xscale = c(-abs_max, abs_max), yscale = c(-abs_max, abs_max))
	if("width" %in% names(vp_param)) {
		vp_param2$width = NULL
	}
	if("height" %in% names(vp_param)) {
		vp_param2$height = NULL
	}
	vp_param = vp_param[!names(vp_param) %in% c("xscale", "yscale")]
	vp_param2 = c(vp_param2, vp_param)
	pushViewport(do.call(viewport, vp_param2))

	if(polar_lines) {
		if(length(polar_lines_by) == 1) {
			d = seq(0, 360, by = polar_lines_by)
		} else {
			d = polar_lines_by
		}
		if(d[length(d)] == 360) d = d[-length(d)]
		dm = matrix(nrow = length(d), ncol = 4)
		for(i in seq_along(d)) {
			r0 = max(r + dist)# + convertWidth(max(padding), "native", valueOnly = TRUE)
			dm[i, ] = c(0, 0, cos(d[i]/180*pi)*r0, sin(d[i]/180*pi)*r0)
		}
		grid.segments(dm[, 1], dm[, 2], dm[, 3], dm[, 4], default.units = "native", gp = polar_lines_gp)
	}
}

#' Initialize the spiral from time objects
#'
#' @param xlim Range of the time. The value can be time object such as [`base::Date`], [`base::POSIXlt`] or [`base::POSIXct`]. The value can also be characters and it is converted to time objects automatically.
#' @param start Start of the spiral, in degrees. By default it is automatically calculated.
#' @param end End of the spiral, in degrees. By default it is automatically calculated.
#' @param unit_on_axis Units on the axis.
#' @param period Which period to use?
#' @param normalize_year Whether to enforce one spiral loop to represent a complete year?
#' @param period_per_loop How many periods to put in a spiral loop?
#' @param polar_lines_by By default different value of `polar_lines_by` is set for different `period`. E.g. 360/7 is set if `period` is "weeks" or 360/24 is set if `peroid` is set to "hours".
#'              When `period` is year and `unit_on_axis` is day, the proportion of sectors by polar lines corresponds to the proportion of month days in a year.
#' @param verbose Whether to print messages?
#' @param ... All pass to [`spiral_initialize()`].
#'
#' @details
#' "start" and "end" are automatically calculated for different "unit_on_axis" and "period". For example, if "unit_on_axis" is "days" and "period" is "years", then
#' the first day of each each year is always put on `theta = 0 + 2*pi*k` where `k` is the index of spiral loops.
#'
#' @return
#' No value is returned.
#' @export
#' @import lubridate
#' @examples
#' spiral_initialize_by_time(xlim = c("2014-01-01", "2021-06-17"))
#' spiral_track(height = 0.6)
#' spiral_axis()
#'
#' spiral_initialize_by_time(xlim = c("2021-01-01 00:00:00", "2021-01-05 00:00:00"))
#' spiral_track(height = 0.6)
#' spiral_axis()
#'
#' spiral_initialize_by_time(xlim = c("2021-01-01 00:00:00", "2021-01-01 00:10:00"),
#'     unit_on_axis = "secs", period = "mins")
#' spiral_track(height = 0.6)
#' spiral_axis()
spiral_initialize_by_time = function(xlim, start = NULL, end = NULL,
	unit_on_axis = c("days", "months", "weeks", "hours", "mins", "secs"), 
	period = c("years", "months", "weeks", "days", "hours", "mins"), 
	normalize_year = FALSE, period_per_loop = 1, polar_lines_by = NULL, 
	verbose = TRUE, ...) {


	xlim_is_date = inherits(xlim, "Date")
	xlim = as.POSIXlt(xlim)

	if(missing(unit_on_axis)) {
		if(xlim_is_date) {
			unit_on_axis = "days"
		} else {
			td = time_difference(xlim[1], xlim[2], "days")
			if(td >= 180) {
				unit_on_axis = "days"
			} else {
				td = time_difference(xlim[1], xlim[2], "hours")
				if(td > 100) {
					unit_on_axis = "hours"
				} else {
					td = time_difference(xlim[1], xlim[2], "mins")
					if(td > 100) {
						unit_on_axis = "mins"
					} else {
						unit_on_axis = "secs"
					}
				}
			}
		}
		if(verbose) qqcat("'unit_to_axis' is set to '@{unit_on_axis}'.\n")
	} else {
		unit_on_axis = match.arg(unit_on_axis)[1]
	}
	if(missing(period)) {
		if(unit_on_axis %in% c("days", "months", "weeks")) {
			period = "years"
		} else if(unit_on_axis %in% c("hours", "mins")) {
			period = "days"
		} else if(unit_on_axis == "secs") {
			period = "hours"
		}
		if(verbose) qqcat("'period' is set to '@{period}'.\n")
	} else {
		period = match.arg(period)[1]
	}

	if(period == "months" && period_per_loop > 1) {
		if(verbose) {
			message_wrap(qq("Internally, 30 days are set for a month, thus when there are more than one period (months) in a loop, each month won't have the same width in the circle. If you want to ensure the width of each month to be equal, you can set the following arguments: `peroid = 'year', period_per_loop = @{period_per_loop}/12, normalize_year = TRUE`. Set `verbose = FALSE` to turn off this message."))
		}
	}


	## check whether the value of unit and period make sense
	if(unit_on_axis == "days") {
		if(!period %in% c("years", "months", "weeks")) {
			stop_wrap("If 'unit_on_axis' is 'days', 'period' can only be one of 'years', 'months' and 'weeks'.")
		}
	}
	if(unit_on_axis == "months") {
		if(!period %in% c("years")) {
			stop_wrap("If 'unit_on_axis' is 'months', 'period' can only be 'years'.")
		}
	}
	if(unit_on_axis == "weeks") {
		if(!period %in% c("years")) {
			stop_wrap("If 'unit_on_axis' is 'weeks', 'period' can only be 'years'.")
		}
	}
	if(unit_on_axis == "hours") {
		if(!period %in% c("months", "weeks", "days")) {
			stop_wrap("If 'unit_on_axis' is 'hours', 'period' can only be one of 'months', 'weeks' and 'days'.")
		}
	}
	if(unit_on_axis == "mins") {
		if(!period %in% c("days", "hours")) {
			stop_wrap("If 'unit_on_axis' is 'mins', 'period' can only be one of 'days' and 'hours'.")
		}
	}
	if(unit_on_axis == "secs") {
		if(!period %in% c("hours", "mins")) {
			stop_wrap("If 'unit_on_axis' is 'secs', 'period' can only be one of 'hours' and 'mins'.")
		}
	}

	if(verbose) {
		if(period == "years" && unit_on_axis == "days" && !normalize_year) {
			message_wrap("When the period is year and the unit on axis is day, a spiral loop can only represent 364 days (52 weeks) under default settings, which helps to correspond weekdays between years, but 1 or 2 days from the current year will be moved and accumulated to the next year. You can set argument `normalize_year = TRUE` to enforce every spiral loop to represent a complete year, but note you might not be able to perfectly correspond weekdays between different years. Set argument `verbose = FALSE` to turn off this message.")
		}
	}

	if(is.null(polar_lines_by)) {
		if(period == "years" && period_per_loop == 1) {
			if(normalize_year && unit_on_axis == "days") {
				polar_lines_by = cumsum(days_in_month(1:12))/365*360
				polar_lines_by = c(0, polar_lines_by[-12])
			} else {
				polar_lines_by = (360/12)/period_per_loop
			}
		} else if(period == "weeks"&& period_per_loop == 1) {
			polar_lines_by = (360/7)/period_per_loop
		} else if(period == "days"&& period_per_loop == 1) {
			polar_lines_by = (360/24)/period_per_loop
		} else if(period_per_loop > 1) {
			polar_lines_by = 360/period_per_loop
		} else {
			polar_lines_by = 30
		}
	}

	time_start = xlim[1]
	time_end = xlim[2]

	get_x_from_data = local({
		time_start = time_start
		unit_on_axis = unit_on_axis
		function(x, where = "center") {
			if(is.numeric(x)) {
				return(x)
			}
			x = as.POSIXlt(x)
			v = time_difference(time_start, x, unit_on_axis)  # assume time_start with x = 0
			if(where == "right") {
				v + 0.5
			} else if(where == "left") {
				v - 0.5
			} else {
				v
			}
		}
	})

	get_data_from_x = local({
		time_start = time_start
		unit_on_axis = unit_on_axis
		function(x) {
			x = x - current_spiral()$other$x_offset[1]
			x = round(x)
			add_time(time_start, x, unit_on_axis)
		}
	})

	if(unit_on_axis == "days") {
		get_character_from_x = local({
			time_start = time_start
			function(x) {
				as.character(get_data_from_x(x))
			}
		})
	} else if(unit_on_axis == "months") {
		get_character_from_x = local({
			time_start = time_start
			function(x) {
				t = get_data_from_x(x)
				format(t, "%Y-%m")
			}
		})
	} else if(unit_on_axis == "weeks") {
		get_character_from_x = local({
			time_start = time_start
			function(x) {
				t = get_data_from_x(x)
				format(t, "%Y-%m-%d")
			}
		})
	} else if(unit_on_axis == "hours") {
		get_character_from_x = local({
			time_start = time_start
			in_a_same_year = year(time_end) == year(time_start)
			in_a_same_day = day(time_end) == day(time_start)
			function(x) {
				t = get_data_from_x(x)
				if(in_a_same_year && in_a_same_day) {
					format(t, "%Hh")
				} else if(in_a_same_year) {
					format(t, "%m-%d %Hh")
				} else {
					format(t, "%Y-%m-%d %Hh")
				}
			}
		})
	} else if(unit_on_axis == "mins") {
		get_character_from_x = local({
			time_start = time_start
			in_a_same_year = year(time_end) == year(time_start)
			in_a_same_day = day(time_end) == day(time_start)
			function(x) {
				t = get_data_from_x(x)
				if(in_a_same_year && in_a_same_day) {
					format(t, "%H:%M")
				} else if(in_a_same_year) {
					format(t, "%m-%d %H:%M")
				} else {
					format(t, "%Y-%m-%d %H:%M")
				}
			}
		})
	} else if(unit_on_axis == "secs") {
		get_character_from_x = local({
			time_start = time_start
			in_a_same_year = year(time_end) == year(time_start)
			in_a_same_day = day(time_end) == day(time_start)
			function(x) {
				t = get_data_from_x(x)
				if(in_a_same_year && in_a_same_day) {
					format(t, "%H:%M:%S")
				} else if(in_a_same_year) {
					format(t, "%m-%d %H:%M:%S")
				} else {
					format(t, "%Y-%m-%d %H:%M:%S")
				}
			}
		})
	}

	## redefine 
	if(normalize_year && period== "years" && unit_on_axis == "days") {
		get_x_from_data = local({
			time_start = time_start
			period_per_loop = period_per_loop
			function(x, where = "center") {

				if(is.numeric(x)) {
					return(x)
				}
				x = as.POSIXlt(x)
				y = year(x)

				first_year = year(time_start)
				first_day_in_year = as.POSIXlt(paste0(y, "-01-01"))
				last_day_in_year = as.POSIXlt(paste0(y, "-12-31"))
				days_in_year = calc_days_in_year(y)

				v = (y - first_year)*360 + time_difference(first_day_in_year, x)/days_in_year*360
				if(where == "left") {
					v - 0.5/calc_days_in_year(y)*360/period_per_loop
				} else if(where == "right") {
					v + 0.5/calc_days_in_year(y)*360/period_per_loop
				} else {
					v
				}
			}
		})

		get_data_from_x = local({
			time_start = time_start
			period_per_loop = period_per_loop
			function(x) {
				x = x - current_spiral()$other$x_offset[1]
				first_year = year(time_start)
				which_loop = ceiling(x/360) - 1; #which_loop[x %% 360 == 0] = which_loop[x %% 360 == 0] + 1
				first_day_in_year = as.POSIXlt(paste0(first_year + which_loop, "-01-01"))
				last_day_in_year = as.POSIXlt(paste0(first_year + which_loop, "-12-31"))
				days_in_year = calc_days_in_year(first_year + which_loop)

				add_time(first_day_in_year, round((x %% 360)/360*period_per_loop*days_in_year), "days")
			}
		})

		get_character_from_x = local({
			time_start = time_start
			function(x) {
				as.character(get_data_from_x(x))
			}
		})
	}


	get_start_of_the_year = function(t) {
		month(t) = 1
		day(t) = 1
		t
	}
	get_start_of_the_month = function(t) {
		day(t) = 1
		hour(t) = 0
		minute(t) = 0
		second(t) = 0
		t
	}
	get_start_of_the_day = function(t) {
		hour(t) = 0
		minute(t) = 0
		second(t) = 0
		t
	}
	get_start_of_the_hour = function(t) {
		minute(t) = 0
		second(t) = 0
		t
	}
	get_start_of_the_minute = function(t) {
		second(t) = 0
		t
	}

	if(unit_on_axis == "months") {
		if(is.null(start)) start = (1 + time_difference(get_start_of_the_year(time_start), time_start, unit_on_axis)/12/period_per_loop)*360
		end = 360*(time_range(time_start, time_end, unit_on_axis)/12/period_per_loop) + start
	} else if(unit_on_axis == "weeks") {
		if(is.null(start)) start = (1 + time_difference(get_start_of_the_year(time_start), time_start, unit_on_axis)/52/period_per_loop)*360
		end = 360*(time_range(time_start, time_end, unit_on_axis)/52/period_per_loop) + start
	} else if(unit_on_axis == "days") {
		if(period == "years") {
			if(is.null(start)) start = (1 + time_difference(get_start_of_the_year(time_start), time_start, unit_on_axis)/(52*7)/period_per_loop)*360
			end = 360*(time_range(time_start, time_end, unit_on_axis)/(52*7)/period_per_loop) + start
		} else if(period == "months") {
			if(is.null(start)) start = (1 + time_difference(get_start_of_the_month(time_start), time_start, unit_on_axis)/30/period_per_loop)*360
			end = 360*(time_range(time_start, time_end, unit_on_axis)/30/period_per_loop) + start
		} else if(period == "weeks") {
			if(is.null(start)) start = (1 + time_difference(get_start_of_the_month(time_start), time_start, unit_on_axis)/7/period_per_loop)*360
			end = 360*(time_range(time_start, time_end, unit_on_axis)/7/period_per_loop) + start
		} 
	} else if(unit_on_axis == "hours") {
		if(period == "months") {
			if(is.null(start)) start = (1 + time_difference(get_start_of_the_month(time_start), time_start, unit_on_axis)/(30*24)/period_per_loop)*360
			end = 360*(time_range(time_start, time_end, unit_on_axis)/(30*24)/period_per_loop) + start
		} else if(period == "weeks") {
			if(is.null(start)) start = (1 + time_difference(get_start_of_the_month(time_start), time_start, unit_on_axis)/(7*24)/period_per_loop)*360
			end = 360*(time_range(time_start, time_end, unit_on_axis)/(7*24)/period_per_loop) + start
		} else if(period == "days") {
			if(is.null(start)) start = (1 + time_difference(get_start_of_the_day(time_start), time_start, unit_on_axis)/24/period_per_loop)*360
			end = 360*(time_range(time_start, time_end, unit_on_axis)/24/period_per_loop) + start
		}
	} else if(unit_on_axis == "mins") {
		if(period == "days") {
			if(is.null(start)) start = (1 + time_difference(get_start_of_the_day(time_start), time_start, unit_on_axis)/(24*60)/period_per_loop)*360
			end = 360*(time_range(time_start, time_end, unit_on_axis)/(24*60)/period_per_loop) + start
		} else if(period == "hours") {
			if(is.null(start)) start = (1 + time_difference(get_start_of_the_hour(time_start), time_start, unit_on_axis)/60/period_per_loop)*360
			end = 360*(time_range(time_start, time_end, unit_on_axis)/60/period_per_loop) + start
		}
	} else if(unit_on_axis == "secs") {
		if(period == "hours") {
			if(is.null(start)) start = (1 + time_difference(get_start_of_the_hour(time_start), time_start, unit_on_axis)/(60*60)/period_per_loop)*360
			end = 360*(time_range(time_start, time_end, unit_on_axis)/(60*60)/period_per_loop) + start
		} else if(period == "mins") {
			if(is.null(start)) start = (1 + time_difference(get_start_of_the_minute(time_start), time_start, unit_on_axis)/60/period_per_loop)*360
			end = 360*(time_range(time_start, time_end, unit_on_axis)/60/period_per_loop) + start
		}
	}

	x_xlim = get_x_from_data(xlim)
	x_xlim[1] = x_xlim[1] - 0.5
	x_xlim[2] = x_xlim[2] + 0.5

	x_offset = c(-0.5, 0.5)

	if(normalize_year && period== "years" && unit_on_axis == "days") {

		year_start = year(time_start)
		year_end = year(time_end)

		if(is.null(start)) {
			start = time_difference(as.POSIXlt(paste0(year_start, "-01-01")), time_start, "days")/calc_days_in_year(year_start)*360/period_per_loop + 360
		}

		if(year_start == year_end) {
			end = (1 + time_difference(time_start, time_end, "days"))/calc_days_in_year(year_start)*360/period_per_loop + start
		} else {
			end = start - time_difference(as.POSIXlt(paste0(year_start, "-01-01")), time_start, "days")/calc_days_in_year(year_start)*360/period_per_loop +
				  (year(time_end) - year(time_start))*360/period_per_loop +
			      (1 + time_difference(as.POSIXlt(paste0(year_end, "-01-01")), time_end, "days"))/calc_days_in_year(year_end)*360/period_per_loop
		}
		x_xlim = get_x_from_data(xlim)
		x_offset = c(- 0.5/calc_days_in_year(year(xlim[1]))*360/period_per_loop, 0.5/calc_days_in_year(year(xlim[2]))*360/period_per_loop)
		x_xlim = x_xlim + x_offset
	}

	spiral_initialize(xlim = x_xlim, start = start, end = end, polar_lines_by = polar_lines_by, ...)

	spiral = current_spiral()
	spiral$xclass = "Time"
	spiral$get_x_from_data = get_x_from_data
	spiral$get_character_from_x = get_character_from_x
	spiral$get_data_from_x = get_data_from_x

	spiral$other = list(unit_on_axis = unit_on_axis, 
		                period = period,
		                normalize_year = normalize_year,
		                x_offset = x_offset)

	invisible(NULL)
}


calc_days_in_year = function(y) {
	time_difference(as.POSIXlt(paste0(y, "-01-01")), as.POSIXlt(paste0(y, "-12-31"))) + 1
}


# unit: months, weeks, days, hours, mins, secs
time_difference = function(x, y, unit = "days") {
	n = max(length(x), length(y))
	if(length(x) == 1) x = rep(x, n)
	if(length(y) == 1) y = rep(y, n)

	if(unit %in% c("days", "weeks", "hours", "mins", "secs")) {
		as.double(y - x, unit)
	} else if(unit == "months") {
		year_1 = lubridate::year(x)
		month_1 = lubridate::month(x)

		year_2 = lubridate::year(y)
		month_2 = lubridate::month(y)

		l1 = year_1 == year_2
		l2 = year_1 < year_2
		l3 = year_1 > year_2

		d = numeric(length(x))
		if(any(l1)) d[l1] = month_2[l1] - month_1[l1]
		if(any(l2)) d[l2] = 12 - month_1[l2] + month_2[l2] + (year_2[l2] - year_1[l2] - 1)*12
		if(any(l3)) d[l3] = 12 - month_2[l3] + month_1[l3] + (year_1[l3] - year_2[l3] - 1)*12

		d
	}
}

time_range = function(...) {
	time_difference(...) + 1
}

add_time = function(t, diff, unit) {
	if(unit == "days") {
		t + lubridate::days(diff)
	} else if(unit == "weeks") {
		t + lubridate::weeks(diff)
	} else if(unit == "hours") {
		t + lubridate::hours(diff) 
	} else if(unit == "mins") {
		t + lubridate::minutes(diff)
	} else if(unit == "secs") {
		t + lubridate::seconds(diff)
	} else if(unit == "months") {
		month2 = (month(t) + diff - 1) %% 12 + 1
		year2 = year(t) + floor((month(t) + diff - 1)/12)
		year(t) = year2
		month(t) = month2
		t
	}
}

#' Initialize the spiral with genomic coordinates
#'
#' @param xlim Range of the genomic coordinates.
#' @param scale_by For genomic plot, axis is linearly scaled by the curve length.
#' @param ... All pass to `spiral_initialize`.
#'
#' @details
#' It is basically the same as [`spiral_initialize()`]. The only difference is the axis labels are automatically
#' formated for genomic coordinates.
#'
#' @return
#' No value is returned.
#' @export
#' @examples
#' spiral_initialize_by_gcoor(c(0, 1000000000))
#' spiral_track()
#' spiral_axis()
spiral_initialize_by_gcoor = function(xlim, scale_by = "curve_length", ...) {

	get_character_from_x = function(x) {
		x2 = character(length(x))
		l1 = x >= 1e6
		x2[l1] = paste(x[l1]/1000000, "MB", sep = "")
		l2 = x >= 1e3 & !l1
		x2[l2] = paste(x[l2]/1000, "KB", sep = "")
		
		l3 = !(l1 | l2)
		last_unit = "bp"
		if(all(l1 | x == 0)) {
			last_unit = "MB"
		} else if(all(l1 | l2 | x == 0)) {
			last_unit = "KB"
		}
		x2[l3] = paste(x[l3], last_unit, sep = "")
		x2
	}

	spiral_initialize(xlim = xlim, scale_by = scale_by, ...)

	spiral = current_spiral()
	spiral$xclass = "Genomic positions"
	spiral$get_character_from_x = get_character_from_x

	invisible(NULL)
}
