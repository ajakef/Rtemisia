#' Correct calibration steps for all sensors
#'
#' Data may include approximately weekly "steps" where the CO2 reading changes 
#' abruptly as part of the sensor's self-calibration routine. For underground
#' recording, this is undesirable and must be corrected using this function.
#' Step detections involve manual review using the function's basic GUI to
#' prevent false positives from being counted.
#'
#' @param df Data frame to plot (output of [read_data()])
#' @param output_file .csv file to write as output (default none)
#' @param corrections_file .RData file to save corrections (default none)
#' @param key_pattern search key to identify channels to correct (e.g., CO2_ppm_[01])
#' @details This function uses a very basic GUI to accept or reject detected
#' steps. For each step, left-click the plot to accept it, or right-click to
#' reject. Reject rates of 50% or more are plausible and not cause for concern.
#' After all detections for a sensor are reviewed, the original and
#' corrected data will be plotted; if you made any mistakes, you can reject it
#' by right clicking (and re-do the review for that sensor), or left-click to
#' accept the corrected data and move on to the next sensor. This is only needed
#' for CO2 data, not for temperature or humidity. Note that ctrl-c does not
#' work immediately in interactive mode; it takes effect after the next right
#' click.
#' @return data frame with calibration steps corrected, optionally with output files written
#' @export
#' @examples
#' fix_calibration()


fix_calibrations = function(df, output_file = NULL, corrections_file = NULL, key_pattern = 'CO2'){
  steps_corrected = list()
  key_indices = grep(key_pattern, names(df))
  keys = names(df)[key_indices]
  for(key in keys){
    print(paste('Correcting channel', key))
    y = df[[key]]
    x = df$time
    while(TRUE){
      steps = find_steps(x, y)
      steps_reviewed = review_steps(x, y, steps)
      df[[key]] = correct_steps(x, y, steps_reviewed)

      ## show all steps in gray
      for(j in steps$i){
        abline(v = as.numeric(x[j]), col = 'gray', lty = 'dashed', lwd = 2)
      }
      ## show accepted steps in blue
      for(j in steps_reviewed$i){
        abline(v = as.numeric(x[j]), col = 'blue', lty = 'dashed', lwd = 2)
      }
      legend(x = 'topright', legend=c('original', 'corrected', 'accepted step', 'rejected step'), lwd = c(2, 1, 2, 2), col = c('black', 'red', 'blue', 'gray'), lty = c('solid', 'solid', 'dashed', 'dashed'), bg = 'white')

      ## UI: accept the step if left click; add to output. Move on if right click.
      title('left-click accept corrections; right-click reject')
      l = locator(1)
      if(length(l$x) == 1){ 
        print(paste('Done with channel', key))
        break
      }else{
        print(paste('Rejected corrections; re-doing channel', key))
      }
    }
    steps_corrected[[key]] = list(x = x[steps_reviewed$i], y = steps_reviewed$y)
  }
  attr(df, 'steps_corrected') = steps_corrected
  if(!is.null(corrections_file) && is.character(corrections_file)){
    print('Writing corrections file')
    save(steps_corrected, file = corrections_file)
  }
  if(!is.null(output_file) && is.character(output_file)){
    print('Writing output data file')
    write.table(df, file = output_file, quote = FALSE, row.names = FALSE, col.names = TRUE, sep = ',')
  }
  return(df)
}

#' Identify possible calibration steps
#'
#' @param x Input x values
#' @param y Input y values
#' @return list of step x values, beginning and end y values, and step distance
#' @export
#' @examples
#' find_steps()
find_steps = function(x, y){
  min_dx = 14400 # must have at least this much time between identified self-calibrations
  min_dy = 3 # threshold for being a step
  d = c(runmed(diff(runmed(y, 29)), 9),0) # this works well empirically for JDT3bc_2 in Sept-Oct 2023
  o = order(abs(d), decreasing = TRUE)
  step_x = 1 # initializing with 1 means that no steps will be detected at the very beginning (good)
  step_y1 = y[1]
  step_y2 = y[1]
  step_dy = 0
  #browser()
  for(i in o){ #loop through possible steps in decreasing height order
    ## first, verify that the possible step is not at a data gap
    if(difftime(x[min(length(x), i + 10)], x[max(1, i - 10)], units = 'hours') > 1){
      next
    }
    if(abs(d[i]) < min_dy){
      break # done looking; all other points are too small to be a step
    }
    ## make sure it's far from all previously detected steps
    if(any(abs(step_x - i) <= min_dx)){
      next
    }
    y1 = median(y[i + -50:0], na.rm = TRUE)
    y2 = median(y[i + 50:100], na.rm = TRUE)
    if(!is.na(y1) && !is.na(y2)){
      step_x = c(step_x, i)
      step_y1 = c(step_y1, y1) # step begins between i and i+1 so i itself is pre-step; step settles by (i+50)
      step_y2 = c(step_y2, y2) # step begins between i and i+1 so i itself is pre-step; step settles by (i+50)
      step_dy = c(step_dy, y2 - y1)
    }
  }
  o = order(step_x)[-1]
  return(list(i=step_x[o], y1=step_y1[o], y2=step_y2[o], dy = step_dy[o]))
}

review_steps = function(x, y, steps){
  output = list(i = numeric())
  for(i in steps$i){
    par(mfrow = c(2, 1), mar = c(3,3,3,2), mgp = c(1.75, 0.5, 0))
    
    ## zoom-out view, with decimated data for efficiency
    N = 10
    w = N * (1:(length(x)/N))
    plot(x[w], y[w], pch = '.')
    for(j in steps$i){
      if(j > i){
        abline(v = as.numeric(x[j]), col = 'black', lty = 'dashed', lwd = 2) 
      }else if(j == i){
        abline(v = as.numeric(x[j]), col = 'red', lty = 'dashed', lwd = 2) 
      }else if(j %in% output$i){
        abline(v = as.numeric(x[j]), col = 'blue', lty = 'dashed', lwd = 2) 
      }else{
        abline(v = as.numeric(x[j]), col = 'gray', lty = 'dashed', lwd = 2) 
      }  
    }
    #abline(v = as.numeric(x[i]), col = 'red', lwd = 2)
    abline(h = 400, col = 'gray', lwd = 2)

    ## zoom-in view
    #w = i + -100:150 # if there's a data gap nearby, this shows both sides of the data gap (bad)
    w = which(((x - x[i+2]) > -500) & ((x - x[i+2]) < 750))
    k = which(steps$i == i)
    print(c(steps$y1[k], steps$y2[k], steps$dy[k]))
    plot(x[w], y[w])
    abline(h = c(steps$y1[k], steps$y2[k]), col = 'red', lwd = 2)
    abline(v = as.numeric(x[i]), col = 'red', lwd = 2)
    title('left-click accept, right-click reject')

    ## UI: accept the step if left click; add to output. Move on if right click.
    l = locator(1)
    if(length(l$x) == 1){ 
      output$i = c(output$i, i)
      output$dy = c(output$dy, steps$dy[k])
      output$y1 = c(output$y1, steps$y1[k])
      output$y2 = c(output$y2, steps$y2[k])
      print(paste('Accepted step at', x[i]))
    }else{
      print(paste('Rejected step at', x[i]))
    }
  }
  return(output)
}

#' Undo calibration steps
#'
#' @param x Input x values
#' @param y Input y values
#' @param steps list of step information from find_steps or review_steps
#' @return list of step x values, beginning and end y values, and step distance
#' @export
#' @examples
#' correct_steps(x, y)
correct_steps = function(x, y, steps){
  if(length(steps$dy) == 0){
    return(y)
  }
  par(mfrow = c(1,1))
  plot(x, y, pch = '.', cex=3, ylim = c(min(y, na.rm = TRUE), max(y, na.rm = TRUE)-sum(steps$dy)))
  N = length(x)
  for(i in 1:length(steps$dy)){
    j = steps$i[i]
    y1 = median(y[j + (-8:0)], na.rm = TRUE)
    y2 = median(y[j + (48:52)], na.rm = TRUE)
    if(is.na(y1) || is.na(y2)) browser()
    y[j:(j+50)] = seq(y1, y2-steps$dy[i], length.out = 51)
    y[(j+51):N] = y[(j+51):N] - steps$dy[i]
  }
  points(x, y, pch = '.', col = 'red')
  legend(x = 'topright', legend=c('original', 'corrected'), lwd = c(2, 1), col = c(1,2))
  return(y)
}

#########################
## not used

OLD_find_calib = function(x){
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
