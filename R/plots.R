plot_key = function(x, key, xlim = NULL, ylim = NULL, interactive = TRUE, medfilt_radius = 18, ...){
  if(medfilt_radius >= 1){
    x = medfilt_nan(x, n = 1 + 2 * medfilt_radius)
  }
  key_indices = grep(key, names(x))
  if(is.null(xlim)){
    xlim = as.numeric(range(x$time, na.rm = TRUE))
  }
  if(is.null(ylim)){
    ylim = range(x[,key_indices], na.rm = TRUE)
  }
  w = which((x$time > xlim[1]) & (x$time < xlim[2]))
  plot(x$time[w], w, type = 'n', xlim = xlim, ylim = ylim, xlab = 'Time', ylab = key, ...)
  for(i in key_indices){
    #lines(x$time[w], x[[i]][w], col = which(i == key_indices))
    points(x$time[w], x[[i]][w], col = which(i == key_indices), pch = '.')
  }
  legend('topright', legend = names(x)[key_indices], lty = 1, col = 1:length(key_indices))
  if(key == 'CO2') abline(h=400)
  if(interactive){
    title('2 left clicks to zoom; 1 left 1 right to un-zoom; 1 right to quit')
    l = locator(2)
    if(length(l$x) == 2){
      xlimtmp = range(l$x)
      ylimtmp = range(l$y)
    }else if(length(l$x) == 1){
      xlimtmp = NULL
      ylimtmp = NULL
    }else if(length(l$x) == 0){
      return(invisible()) # return no output
    }else{ # should never happen
      print(l)
    }
    plot_key(x, key, xlim = xlimtmp, ylim = ylimtmp, interactive = TRUE, ...)
  }
}



plot_CO2 = function(x, medfilt_radius = 30, ...) plot_key(x, 'CO2', medfilt_radius = medfilt_radius, ...)
plot_temp = function(x, medfilt_radius = 30, ...) plot_key(x, 'temp', medfilt_radius = medfilt_radius, ...) 
plot_RH = function(x, medfilt_radius = 30, ...) plot_key(x, 'RH', medfilt_radius = medfilt_radius, ...)
plot_batt = function(x, medfilt_radius = 30, ...) plot_key(x, 'batt', medfilt_radius = medfilt_radius, ...)


#' Quick overview of Artemisia data
#'
#' Make plots of CO2, temperature, and relative humidity for all sensors, and battery voltage/GPS 
#'
#' @param x Data frame to plot (output of [read_data()])
#' @return None
#' @seealso [plot_CO2], [plot_temp], [plot_RH], [plot_key]
#' @export
#' @examples
quicklook = function(x){
  if(is.character(x)){
    x = read_data(x)
  }
  par(mfrow = c(4,1), mar = c(3,3,3,1), mgp = c(1.75, 0.5, 0))
  plot_CO2(x, interactive = FALSE, main = 'CO2')
  plot_temp(x, interactive = FALSE, main = 'Temperature')
  plot_RH(x, interactive = FALSE, main = 'Humidity')
  plot_batt(x, interactive = FALSE, main = 'Battery')
  for(i in which(!is.na(x$lat))){
    lines(c(x$time[i], x$time[i]), c(0, 1e6), col = 'green', lty = 'dotted')
  }

}