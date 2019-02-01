createIrregTimeSlices <- function(y, slices, horizon, fixedWindow, unit, plot = FALSE) {

  # generate the sequence of date/time values over which to split. These will always be in ascending order, with no missing date/times.
  y <- y[order(y)]
  yvals <- seq(min(y), max(y), by = unit)
  initialWindow <- (length(yvals) - slices)

  # determine the start and stop date/times for each time slice
  stops <- seq_along(yvals)[seq(initialWindow, (length(yvals) - horizon), horizon)]
  if (fixedWindow) {
    starts <- stops - initialWindow + 1
  } else {
    starts <- rep(1, length(stops))
  }

  # function that returns the indices of y that are between the start and stop date/time for a slice
  train <- mapply(function(start, stop) which(y >= yvals[start] & y <= yvals[stop]),
                  start = starts,
                  stop = stops,
                  SIMPLIFY = FALSE)
  test <- mapply(function(start, stop) which(y > yvals[start] & y <= yvals[stop]),
                 start = stops,
                 stop = (stops + horizon),
                 SIMPLIFY = FALSE)

  names(train) <- paste0("Training_", gsub(" ", "0", format(seq(along = train))))
  names(test) <- paste0("Testing_", gsub(" ", "0", format(seq(along = test))))
  out <- list(train = train, test = test)

  # Introspection
  if (plot) {
    print(test %>%
            lapply(as_data_frame) %>%
            bind_rows(.id = "id") %>%
            tidyr::separate(id, c("set", "resample_num"), "_") %>%
            ggplot2::ggplot(ggplot2::aes(value, resample_num)) +
            ggplot2::geom_path() +
            ggplot2::ylab(paste0("Resample # (", horizon, " ", unit, " periods)")) +
            ggplot2::scale_x_continuous(name = "Test Index",
                                        position = "top",
                                        sec.axis = ggplot2::dup_axis(name = "Forecast Origin",
                                                                     labels = function(idx) as.character(y)[idx])))
  }
  out
}
