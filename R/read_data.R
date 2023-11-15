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



medfilt_nan = function(x, n=3){
  if(is.data.frame(x)){
    for(i in 2:19){
      x[,i] = medfilt_nan(x[,i], n)
    }
    return(x)
  }else{
    # n is median filter *diameter*, not radius. Must be odd.
    w = which(is.na(x))
    x[w] = median(x, na.rm = TRUE)
    y = runmed(x, n)
    y[w] = NaN
    return(y)
  }
}
