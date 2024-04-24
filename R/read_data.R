#' Read data files
#'
#' Reads one or more data files, concatenates them, and returns a data frame.
#'
#' @param filenames string(s) including paths and filenames (wildcards permitted)
#' @return Data frame including date-time and sensor readings
#' @export
#' @examples
#' read_data()


read_data = function(filenames){
  filenames = Sys.glob(filenames) # allows either list of filenames or wildcards
  output = NULL
  for(file in filenames){
    output = rbind(output, read_one_file(file))
  }
  return(output)
}


read_one_file = function(filename){
  header = c('sec_of_file', 'CO2_ppm_0', 'temp_0', 'RH_0', 'CO2_ppm_1', 'temp_1', 'RH_1', 'CO2_ppm_2', 'temp_2', 'RH_2', 'CO2_ppm_3', 'temp_3', 'RH_3', 'CO2_ppm_4', 'temp_4', 'RH_4', 'CO2_ppm_5', 'temp_5', 'RH_5', 'year', 'month', 'date', 'hour', 'min', 'sec', 'lat', 'lon', 'HDOP', 'satellites', 'batt_voltage', 'dummy')
  x = try(read.table(filename, skip = 5, sep = ',', col.names = header)[,1:30], silent = TRUE)
  if(class(x) == 'try-error'){
      x = read.table(filename, skip = 5, sep = ',', col.names = header[1:30])
  }
  x$time = as.POSIXlt(paste0(x$year, '-', x$month, '-', x$date, ' ', x$hour, ':', x$min, ':', x$sec), format = '%Y-%m-%d %H:%M:%S', tz = 'GMT')
  return(x)
}

#' Median filter for Rtemisia data frame
#'
#' Runs a median filter on all data columns (not time, location, etc)
#'
#' @param data Data frame from read_data()
#' @param n integer; width (diameter) of moving median window
#' @return median-filtered data frame
#' @export
#' @examples
#' medfilt_nan(data, 5)
medfilt_nan = function(x, n=3){
  if(is.data.frame(x)){
    ## loop through data columns
    for(i in which(substr(names(x), 1, 3) %in% c('CO2', 'tem', 'RH_'))){
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

correct_times = function(data, starttime = NULL, endtime = NULL){
  # look for times before first GPS fix, and correct them
  if(is.null(starttime) && is.null(endtime)){
    w = which(!is.na(data$lat))
  }else{
    w = dim(data)[1]
    if(!is.null(endtime)){
      endtime = as.POSIXlt(endtime, format='%Y%m%d_%H%M%S', tz = 'GMT')
      data$time[w] = endtime
    }else{
      starttime = as.POSIXlt(starttime, format='%Y%m%d_%H%M%S', tz = 'GMT')
      data$time[w] = starttime + data$sec_of_file[w] - data$sec_of_file[1]
    }
  }
  if(length(w) == 0){
    warning(paste('No GPS data found'))
    return(data)
  }
  correct_sec_offset = mean((data$time - data$sec_of_file)[w])
  data$time[1:w[1]] = data$sec_of_file[1:w[1]] + correct_sec_offset
  data$year = as.numeric(strftime(data$time, '%Y'))
  data$month = as.numeric(strftime(data$time, '%m'))
  data$date = as.numeric(strftime(data$time, '%d'))
  data$hour = as.numeric(strftime(data$time, '%H'))
  data$min = as.numeric(strftime(data$time, '%M'))
  data$sec = as.numeric(strftime(data$time, '%S'))
  return(data)
}

#' Correct file times
#'
#' Fix times for data file with initial missing GPS, and write new file
#'
#' @param filename string of existing file name to process
#' @param starttime Optional; user-provided file start time as YYYYmmdd_HHMMSS string. 
#' @param endtime Optional; user-provided file end time as YYYYmmdd_HHMMSS string
#' @note If neither starttime nor endtime is provided, they will be calculated using GPS data found in the file. This is normally the best approach unless a file has no GPS data at all. Only one of starttime and endtime should be given; if both are provided, starttime will be ignored.
#' @return writes output file with the normal file name based on start time and serial number
#' @export
#' @examples
#' medfilt_nan(data, 5)
fix_file_time = function(filename, starttime = NULL, endtime = NULL){
  data = read_data(filename)
  data = correct_times(data, starttime, endtime)
  header = scan(filename, what = character(), n = 5, sep = '\n', quiet = TRUE)
  SN = strsplit(header[3], ' ')[[1]][4]
  new_filename = paste0(strftime(data$time[1], '%Y%m%d_%H%M%S'), '_', SN, '.csv')
  write.table(header, file = new_filename, quote = FALSE, row.names = FALSE, col.names = FALSE)
  write.table(data[names(data) != 'time'], file = new_filename, append = TRUE, row.names = FALSE, quote = FALSE, sep = ',', col.names = FALSE) # leave NAs as "NA" so we can distinguish corrected files: no "na = '' "
  print(paste('Wrote new file', new_filename))
}
