

#' Global options
#'
#' @param ... Arguments for the parameters, see "details" section.
#' @param RESET Whether to reset to default values.
#' @param READ.ONLY Please ignore.
#' @param LOCAL Please ignore.
#' @param ADD Please ignore.
#' 
#' @details
#' There are the following global parameters:
#' 
#' - `min_segment_len` Minimal length of the segment that partitions a curve.
#' - `help` Whether to print the help messages?
#'
#' To access the value of an option: `spiral_opt$name` where `name` is the name of the option. To set a new value
#' for an option: `spiral_opt$name = new_value`.
#'
#' @return A list of options.
#' @export
#' @import GlobalOptions
#' @examples
#' spiral_opt
spiral_opt = setGlobalOptions(
	min_segment_len = 1/180*pi,
	help = TRUE
)

spiral_env = new.env()
spiral_env$i_spiral = 0
spiral_env$spiral = NULL


#' Viewport name of the current spiral
#'
#' @return A string of the viewport name.
#' @export
current_spiral_vp = function() {
	paste0("spiral_", spiral_env$i_spiral)
}
