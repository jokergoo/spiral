

#' Visualize git commits
#'
#' @param repo Path of the git repo. The value can be a single repo or a vector of repos.
#' @param show_legend Whether to show the legend.
#' @param start Start date. By default it is the first date of the commit. The value can be a string such as "2022-01-01" or a [`base::Date`] object.
#' @param end End date. By default it is the current date. The value can be a string such as "2022-01-01" or a [`base::Date`] object.
#' @param pt_range Range of the point sizes. The default is between 1 and the 90 percentile of daily commits.
#' @param commits_range Range of the numbers of commits.
#' @param type Type of the plot.
#' @param colors If type is the heatmap, it controls the list of colors.
#'
#' @export
#' @importFrom utils read.table
#' @examples
#' \dontrun{
#' spiral_git_commits("~/project/development/ComplexHeatmap")
#' spiral_git_commits("~/project/development/ComplexHeatmap", type = "heatmap")
#' }
spiral_git_commits = function(repo = ".", show_legend = TRUE,
    start = NULL, end = Sys.Date(),
    pt_range = c(2, 16), commits_range = c(1, ceiling(quantile(n[n>0], 0.95))),
    type = c("points", "heatmap"),
    colors = c("#3288BD", "#99D594", "#E6F598", "#FFFFBF", 
               "#FEE08B", "#FC8D59", "#D53E4F")) {

    type = match.arg(type)[1]

    df_all = list()
    for(rp in repo) {
        if(!file.exists(paste0(rp, "/.git/"))) {
            stop_wrap(qq("'@{rp}' is not a git repository."))
        }

        rp = normalizePath(rp)

        od = getwd()
        oe = try({
            setwd(rp)
            df = read.table(pipe("git log --date=short --pretty=format:%ad | sort | uniq -c"))
        }, silent = TRUE)
        setwd(od)

        if(inherits(oe, "try-error")) {
            stop(oe)
        }
        colnames(df) = c("commits", "date")

        df$date = as.Date(df$date)
        df_all[[rp]] = df
    }

    if(length(repo) == 1) {
        repo_name = basename(normalizePath(repo))
    } else {
        repo_name = paste0(length(repo), " packages")
    }

    df_all = do.call(rbind, df_all)

    if(is.null(start)) {
        start = min(df_all$date)
    } else {
        start = as.Date(start)
    }
    if(is.null(end)) {
        end = max(df_all$date)
    } else {
        end = as.Date(end)
    }

    df_all = df_all[df_all$date >= start & df_all$date <= end, , drop = FALSE]

    start_year = year(start)
    end_year = year(end)

    d = start + seq(1, end - start + 1) - 1
    n = numeric(length(d))

    for(i in seq_len(nrow(df_all))) {
        ind = as.double(difftime(df_all[i, "date"], start), "days") + 1
        n[ind] = n[ind] + df_all[i, "commits"]
    }
    if(type == "points") {
        l = n > 0
        n = n[l]
        d = d[l]
    }

    pt_range = pt_range + 0
    commits_range = commits_range + 0
    calc_pt_size = function(x) {
        pt_size = (pt_range[2] - pt_range[1])/(commits_range[2] - commits_range[1])*(x - commits_range[1]) + pt_range[1]
        pt_size[x > commits_range[2]] = pt_range[2]
        pt_size[x < commits_range[1]] = pt_range[1]
        pt_size
    }

    if(type == "heatmap") {
        col_fun = circlize::colorRamp2(seq(max(min(n), commits_range[1]), min(max(n), commits_range[2]), length = length(colors)), colors)
    }

    spiral_initialize_by_time(c(start, end), verbose = FALSE, normalize_year = TRUE)
    spiral_track()
    if(type == "points") {
        spiral_points(d, 0.5, pch = 16, size = unit(calc_pt_size(n), "pt"))
    } else {
        spiral_rect(d-0.5, 0, d+0.5, 1, gp = gpar(fill = col_fun(n), col = NA))
    }
    
    for(y in start_year:end_year) {
        spiral_text(paste0(y, "-01-01"), 0.5, y, gp = gpar(fontsize = 8, col = ifelse(type == "points", "#808080", "black")), facing = "inside")
    }

    upViewport()
    grid.text(repo_name, x = unit(0, "npc") + unit(4, "pt"), y = unit(1, "npc") - unit(4, "pt"), just = c("left", "top"), gp = gpar(fontsize = 14))

    if(show_legend) {
        breaks = pretty(commits_range, 3)
        if(breaks[1] == 0) {
            breaks[1] = 1
        }
        breaks = unique(breaks)
        labels = breaks
        if(max(n) > commits_range[2]) {
            labels[length(labels)] = paste0("[", labels[length(labels)], ", ", max(n), "]")
        }
        if(type == "points") {
            lgd = ComplexHeatmap::Legend(title = "#commits", at = breaks, labels = labels, type = "points", pch = 16, size = unit(calc_pt_size(breaks), "pt"))
        } else {
            lgd = ComplexHeatmap::Legend(title = "#commits", at = breaks, labels = labels, col_fun = col_fun)
        }
        ComplexHeatmap::draw(lgd, x = unit(0, "npc") + unit(4, "pt"), y = unit(1, "npc") - unit(24, "pt"), just = c("left", "top"))
    }

}



#' Visualize package downloads
#'
#' @param pkg A single CRAN package name.
#' @param from Starting date.
#' @param to Ending date.
#' @param show_legend Whether to show the legend.
#' 
#' @details
#' The **cranlogs** package is used to retrieve the download history from the Rstudio server.
#'
#' @export
#' @importFrom stats quantile
#' @examples
#' \donttest{
#' spiral_pkg_downloads("ggplot2")
#' }
spiral_pkg_downloads = function(pkg, from = "2012-10-01", to = "last-day",
    show_legend = TRUE) {

    df = cranlogs::cran_downloads(pkg, from = from, to = to)
    rg = range(which(df[, 2] > 0))
    df = df[seq(rg[1], rg[2]), , drop = FALSE]

    if(nrow(df) == 0) {
        cat("No download for package '", pkg, "' from ", from, " to ", to, " .\n", sep = "")
    }

    day_diff = as.double(df$date[nrow(df)] - df$date[1], "days")
    year_mean = tapply(df$count, lubridate::year(df$date), function(x) mean(x[x > 0]))

    df$diff = log2(df$count/year_mean[as.character(lubridate::year(df$date))])
    df$diff[is.infinite(df$diff)] = 0
    q = quantile(abs(df$diff), 0.99)  # adjust outliers
    df$diff[df$diff > q] = q
    df$diff[df$diff < -q] = -q

    years = unique(gsub("^(\\d+)-.*$", "\\1", df[, 1]))

    spiral_initialize_by_time(xlim = range(df[, 1]), padding = unit(2, "cm"))

    spiral_track(height = 0.8)
    spiral_horizon(df$date, df$diff, use_bars = TRUE)

    if(length(years) > 1) {
        for(iy in seq_along(years)) {
            if(iy == 1) {
                spiral_highlight("start", paste0(years[1], "-12-31"), type = "line", gp = gpar(col = iy+1))
            } else if(iy == length(years)) {
                spiral_highlight(paste0(years[iy], "-01-01"), "end", type = "line", gp = gpar(col = iy+1))
            } else {
                spiral_highlight(paste0(years[iy], "-01-01"), paste0(years[iy], "-12-31"), type = "line", gp = gpar(col = iy+1))
            }
        }
    } else {
        spiral_highlight("start", "end", type = "line", gp = gpar(col = 2))
    }

    s = current_spiral()
    d = seq(15, 360, by = 30) %% 360
    for(i in seq_along(d)) {
        foo = polar_to_cartesian(d[i]/180*pi, (s$max_radius + 1)*1.05)
        grid.text(month.name[i], x = foo[1, 1], y = foo[1, 2], default.units = "native",
            rot = ifelse(d[i] > 0 & d[i] < 180, d[i] - 90, d[i] + 90), gp = gpar(fontsize = 10))
    }

    upViewport()
    grid.text(pkg, x = unit(0, "npc") + unit(4, "pt"), y = unit(1, "npc") - unit(4, "pt"), just = c("left", "top"), gp = gpar(fontsize = 14))

    if(show_legend) {

        lgd = ComplexHeatmap::packLegend(
            ComplexHeatmap::Legend(title = "Difference to\nyearly average", at = c("higher", "lower"),
                legend_gp = gpar(fill = c("#D73027", "#313695"))),
            ComplexHeatmap::Legend(title = "Year", type = "lines", at = years,
                legend_gp = gpar(col = seq_along(years)+1))
        )
        ComplexHeatmap::draw(lgd, x = unit(0, "npc") + unit(4, "pt"), y = unit(1, "npc") - unit(24, "pt"), just = c("left", "top"))
    }
}

