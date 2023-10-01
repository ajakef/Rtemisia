read_one_file = function(filename){
  header = c('sec_of_file', 'CO2_ppm_0', 'temp_0', 'RH_0', 'CO2_ppm_1', 'temp_1', 'RH_1', 'CO2_ppm_2', 'temp_2', 'RH_2', 'CO2_ppm_3', 'temp_3', 'RH_3', 'CO2_ppm_4', 'temp_4', 'RH_4', 'CO2_ppm_5', 'temp_5', 'RH_5', 'year', 'month', 'date', 'hour', 'min', 'sec', 'lat', 'lon', 'HDOP', 'satellites', 'batt_voltage', 'dummy')
  x = read.table(filename, skip = 5, sep = ',', col.names = header)[,1:30]
  x$time = as.POSIXlt(paste0(x$year, '-', x$month, '-', x$date, ' ', x$hour, ':', x$min, ':', x$sec), tz = 'GMT')
  return(x)
}

read_data = function(filenames){
  filenames = Sys.glob(filenames) # allows either list of filenames or wildcards
  output = NULL
  for(file in filenames){
    output = rbind(output, read_one_file(file))
  }
  return(output)
}

plot_key = function(x, key, ...){
  key_indices = grep(key, names(x))
  ylim = range(x[,key_indices], na.rm = TRUE)
  plot(x$time, 1:nrow(x), type = 'n', ylim = ylim, xlab = 'Time', ylab = key, ...)
  for(i in key_indices){
    lines(x$time, x[[i]], col = which(i == key_indices))
  }
}

find_calib = function(x){
  step_x = vector('list', 6) # weird errors come up if I try this as a vector
  #step_y = rep(0, 6)
  print('For each trace, left-click once on each side of the calibration step, if there is one, or right-click once if there is no step')
  for(i in 1:6){
    data = x[[paste0('CO2_ppm_', i-1)]]
    plot(x$time, data, type = 'l')
    l = locator(2)$x
    print(l)
    if(length(l) == 2){
      data_trim = runmed(data, 51)[(x$time > min(l)) & (x$time < max(l))]
      w = which(abs(diff(data_trim)) == max(abs(diff(data_trim))))[1]
      #step_y[i] = diff(data_trim)[w]
      #browser()
      step_x[[i]] = x$time[which((x$time > min(l)) & x$time < max(l))[w]]
      #print(step_y[i])
      print(step_x[i])
    }
  }
  plot_CO2(x)
  for(i in 1:6){
    lines(c(step_x[[i]], step_x[[i]]), c(0, 1e6), col = i, lty = 'dotted')
  }
  return(step_x)
}


plot_CO2 = function(x) plot_key(x, 'CO2', main = 'CO2 Concentration')
plot_temp = function(x) plot_key(x, 'temp', main = 'Temperature') 
plot_RH = function(x) plot_key(x, 'RH', main = 'Humidity')
plot_batt = function(x) plot_key(x, 'batt', main = 'Battery and GPS')

quicklook = function(x){
  par(mfrow = c(4,1), mar = c(3,3,3,1), mgp = c(1.75, 0.5, 0))
  plot_CO2(x)
  plot_temp(x)
  plot_RH(x)
  plot_batt(x); abline(v = which(!is.na(x$lat)), col = 'green', lty = 'dotted')
}
