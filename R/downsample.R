require(signal)
downsample_trace = function(data, t1, t2, dt_out_sec, field, plot_results = FALSE){
    ## Parse the 'data' input
    if (is.character(data)) {
        data = read_data(data)
    }
    ## Parse start and end times
    tryFormat = c( '%Y-%m-%d %H:%M:%S',
		   '%Y-%m-%dT%H:%M:%S',
    		   '%Y-%m-%d_%H:%M:%S',
		   '%Y-%m-%d %H:%M',
		   '%Y-%m-%dT%H:%M',
		   '%Y-%m-%d_%H:%M',
		   '%Y%m%d %H%M%S',
		   '%Y%m%d_%H%M%S',
		   '%Y%m%d %H%M',
		   '%Y%m%d_%H%M',
		   '%Y-%m-%d',
		   '%Y%m%d')
    if(is.character(t1)){
      t1 = as.POSIXlt(t1, tryFormat = tryFormat, tz = 'GMT')
    }
    if(is.character(t2)){
      t2 = as.POSIXlt(t2, tryFormat = tryFormat, tz = 'GMT')
    }
    ## Throw out impossible data
    t_in = data$time
    y_in = data[[field]]
    w = t_in > as.POSIXlt('2023-01-01', tz = 'GMT') & t_in < as.POSIXlt(Sys.time(), tz = 'GMT')
    t_in = t_in[w]
    y_in = y_in[w]
    

    ## Drop data outside the user-provided window
    w = which((t_in >= (t1 - 2 * dt_out_sec)) & (t_in <= (t2 + 2 * dt_out_sec)))
    t_in = t_in[w]
    y_in = y_in[w]
    if(plot_results) plot(t_in, y_in, pch = '.', col=1)
    
    ## Identify and fill in NaNs (filtering will not work if NaNs are present). The cubic interpolation used here should avoid edge effects around long sections of missing data.
    is_na = is.na(y_in)
    if(any(is_na)){
	y_in[is_na] = spline(x = t_in[!is_na], y = y_in[!is_na], xout = t_in[is_na])$y
	if(plot_results) points(t_in, y_in, pch = '.', col=2)
	if(plot_results) points(t_in[is_na], y_in[is_na], pch = 'o', col=3)
    }		 

    ## Filter data using a causal Butterworth filter (lowpass at the new Nyquist)
    dt_in_sec = as.numeric(median(diff(t_in)), units = 'secs')
    nyquist_in_Hz = 0.5/dt_in_sec # Hz
    fc_Hz = 0.5/dt_out_sec # Hz; Nyquist freq at new sample rate
    y_filtered = signal::filter(signal::butter(6, fc_Hz/nyquist_in_Hz, 'low'), y_in - y_in[1]) + y_in[1]
    if(plot_results) points(t_in, y_filtered, pch = '.', col=4)

    ## Restore NaNs, and downsample the filtered data. approx() is ok because the filtered function is smooth, and it handles NaNs nicely.
    t_out = seq(t1, t2, dt_out_sec)
    y_in[is_na] = NaN
    y_filtered[is_na] = NaN
    y_out = approx(x = t_in, y = y_in, xout = t_out, na.rm = FALSE)$y

    output = list(time = t_out)
    output[[field]] = y_out
    if(plot_results) points(output[['time']], output[[field]], pch = '.', col=5)
    return(as.data.frame(output))
}



#' Downsample an Artemisia dataframe
#'

#' @param data Data frame to downsample, or file name containing the data
#' @param t1 start time (UTC), either string (various YMD[HMS] formats accepted) or POSIXt type
#' @param t2 end time (UTC), either string (various YMD[HMS] formats accepted) or POSIXt type
#' @param dt_out_sec sample interval for downsampled output (seconds)
#' @param fields vector of field names to downsample (default is all CO2, temp, RH, battery fields)
#' @details For each field identified by the user, this function applies an
#' anti-aliasing filter (sixth-order Butterworth, causal, low-pass below the
#' new Nyquist frequency) and downsamples the data. It attempts to preserve
#' missing sections of data (NaNs) in this process (as opposed to interpolating
#' through them, which could give inaccurate results for long sensor dropouts).
#' @return Dataframe containing fields 'time' and all user-specified data fields.
#' @export
#' @examples
#' ## In this example, 'data' can either by the output of read_data(), or a file name with path
#' downsample(data, t1 = '20230922', t2 = '2023-10-12 23:59:59', dt_out_sec = 15*60)
downsample = function(data, t1, t2, dt_out_sec, fields = character()){
    ## Parse the 'data' input
    if (is.character(data)) {
        data = read_data(data)
    }
    ## Parse start and end times
    tryFormat = c( '%Y-%m-%d %H:%M:%s',
		   '%Y-%m-%dT%H:%M:%s',
    		   '%Y-%m-%d_%H:%M:%s',
		   '%Y-%m-%d %H:%M',
		   '%Y-%m-%dT%H:%M',
		   '%Y-%m-%d_%H:%M',
		   '%Y%m%d %H%M%S',
		   '%Y%m%d_%H%M%S',
		   '%Y%m%d %H%M',
		   '%Y%m%d_%H%M',
		   '%Y-%m-%d',
		   '%Y%m%d')
    if(is.character(t1)){
      t1 = as.POSIXlt(t1, tryFormat = tryFormat, tz = 'GMT')
    }
    if(is.character(t2)){
      t2 = as.POSIXlt(t2, tryFormat = tryFormat, tz = 'GMT')
    }
    ## Apply the default 'fields' input if needed
    if((length(fields) == 0) || ((length(fields) == 1) && (nchar(fields) == 0))){
        fields = names(data)[grep('CO2|temp|RH|batt', names(data))]
    }
    ## Iterate through the fields and merge the downsampled traces to the output
    output = list()
    for(field in fields){
      if(!(field %in% names(data))){
        warning(paste('field', field, 'not found in data; skipping'))
        next
      }
      iteration_output = downsample_trace(data, t1, t2, dt_out_sec, field)
      if('time' %in% names(output)){
        output = merge(output, iteration_output, by = 'time', all.x = TRUE, all.y = TRUE)
      }else{
        output = as.data.frame(iteration_output) # this only happens for the first field, when 'output' is still empty
      }
    }
    return(output)
}
