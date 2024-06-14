
#' Clear the spiral curve
#'
#' @param check_vp Whether to check the viewport.
#'
#' @details
#' It basically sets the internally spiral object to `NULL`, and reset all the global options.
#' @export
#' @return
#' No value is returned.
spiral_clear = function(check_vp = TRUE) {
	spiral_env$spiral = NULL
	track_env$track_data = empty_track_data
	track_env$current_track = 0
	spiral_opt(RESET = TRUE)

	if(check_vp) {
		vp = current_spiral_vp()
		while(1) {
			if(current.viewport()$name == vp) {
				popViewport()
				break
			}
			if(current.viewport()$name == "ROOT") {
				break
			}
			popViewport()
		}
	}
}

#' Information of the current spiral
#'
#' @details
#' It prints information of the current spiral.
#'
#' @return
#' No value is returned.
#' @export
#' @importFrom GetoptLong qqcat
#' @examples
#' spiral_initialize()
#' spiral_track(ylim = c(0, 1), height = 0.4)
#' spiral_track(ylim = c(-10, 10), height = 0.4)
#' spiral_info()
spiral_info = function() {
	spiral = spiral_env$spiral
	if(is.null(spiral)) {
		cat("No spiral has been initialized.\n")
	} else {
		print(spiral_env$spiral)

		cat("\n")
		nt = n_tracks()
		if(nt < 1) {
			cat("  No track has been created.\n")
		} else {
			for(i in seq_len(nt)) {
				qqcat("  track @{i}:\n")
				qqcat("    ylim: [@{get_track_data('ymin', i)}, @{get_track_data('ymax', i)}]\n")
				qqcat("    height: @{get_track_data('rel_height', i)} (fraction of the distance of two neighbour loops)\n")
				if(i < nt) cat("\n")
			}
		}
	}
}

