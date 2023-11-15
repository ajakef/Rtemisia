
fix_calibrations = function(df){
  for(i in 0:5){
    key = paste0('CO2_ppm_', i)
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
  }
  return(df)
}

find_steps = function(x, y){
  min_dx = 900 # must have at least 900 seconds between identified self-calibrations
  min_dy = 3 # threshold for being a step
  d = c(0, runmed(diff(runmed(y, 29)), 9)) # this works well empirically for JDT3bc_2 in Sept-Oct 2023
  o = order(abs(d), decreasing = TRUE)
  step_x = 1 # initializing with 0 means that no steps will be detected at the very beginning (good)
  step_y = y[1]
  #browser()
  for(i in o){
    if(abs(d[i]) < min_dy){
      break # done looking; all other points are too small to be a step
    }
    if(all(abs(step_x - i) > min_dx)){
      step_x = c(step_x, i)
      step_y = c(step_y, median(y[i + 50:100]) - median(y[i + -50:0])) # step begins between i and i+1 so i itself is pre-step; step settles by (i+50)
    }
  }
  o = order(step_x)[-1]
  return(list(i=step_x[o], y=step_y[o]))
}

review_steps = function(x, y, steps){
  output = list(i = numeric(), y = numeric())
  for(i in steps$i){
    par(mfrow = c(2, 1), mar = c(3,3,3,2), mgp = c(1.75, 0.5, 0))
    plot(x, y, pch = '.')
    for(j in steps$i){
      abline(v = as.numeric(x[j]), col = 'blue', lty = 'dashed', lwd = 2)
    }
    abline(v = as.numeric(x[i]), col = 'red', lwd = 2)
    w = i + -100:150
    plot(x[w], y[w])
    abline(h = c(median(y[i + 50:100]), median(y[i + -50:0])), col = 'red', lwd = 2)
    abline(v = as.numeric(x[i]), col = 'red', lwd = 2)
    title('left-click accept, right-click reject')

    ## UI: accept the step if left click; add to output. Move on if right click.
    l = locator(1)
    if(length(l$x) == 1){ 
      output$i = c(output$i, i)
      output$y = c(output$y, steps$y[steps$i == i])
      print(paste('Accepted step at', x[i]))
    }else{
      print(paste('Rejected step at', x[i]))
    }
  }
  return(output)
}

correct_steps = function(x, y, steps){
  par(mfrow = c(1,1))
  plot(x, y, pch = '.', cex=3, ylim = c(min(y, na.rm = TRUE), max(y, na.rm = TRUE)-sum(steps$y)))
  N = length(x)
  for(i in 1:length(steps$y)){
    j = steps$i[i]
    y[j:(j+50)] = seq(y[j], y[j+50]-steps$y[i], length.out = 51)
    y[(j+51):N] = y[(j+51):N] - steps$y[i]
  }
  points(x, y, pch = '.', col = 'red')
  legend(x = 'topright', legend=c('original', 'corrected'), lwd = c(2, 1), col = c(1,2))
  return(list(x=x, y=y))
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
