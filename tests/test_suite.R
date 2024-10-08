library(Rtemisia)

data_path = '/home/jake/Dropbox/CO2_sensor/Rtemisia/Rtemisia/data/'

is_approx = function(x, y, d = 0.01) return(((x-y)/y) < d)

## add tests here; failure = error, and must require no user input.
filenames_to_test = c('20231001_000016_10001.csv', # format 0
                      '20240925_041055_32594.csv', # format 1, no config line
                      '20241010_000000_32594.csv') # format 1, with config line
for(filename in filenames_to_test){
  print(filename)
  data_full = read_data(paste0(data_path, filename))
  data_sub = data_full[,c(1:4, (dim(data_full)[2]-11):dim(data_full)[2])] # just look at sensor 0

  invisible(medfilt_nan(data_full))
  quicklook(data_full)
  plot_CO2(data_full, interactive = FALSE)
  plot_temp(data_full, interactive = FALSE)
  plot_RH(data_full, interactive = FALSE)
  plot_batt(data_full, interactive = FALSE)

  invisible(medfilt_nan(data_sub))
  quicklook(data_sub)
  plot_CO2(data_sub, interactive = FALSE)
  plot_temp(data_sub, interactive = FALSE)
  plot_RH(data_sub, interactive = FALSE)
  plot_batt(data_sub, interactive = FALSE)
}
## calibration test: flat line offset by an exponential-decay step; should be corrected to a flat line with minimal deviation
N = 10000
x = (1:N) - N/2
y = 100*(as.numeric(x < 0) + as.numeric(x >= 0) * exp(-x/10))
steps = find_steps(x, y)
y_corrected = correct_steps(x, y, steps)
stopifnot(x[steps$i] == 0) # steps$i is first point constituting a step
stopifnot(is_approx(y_corrected[N], y[1]))
stopifnot(all(is_approx(y_corrected, 100))) # deviation from y=100 should be <1% unless decay is made much slower

## file timing correction test: for files where GPS fix wasn't available at first, find the correct time base and change all the times to match.
fix_file_time(paste0(data_path, '20000101_000009_10001.csv')) # fix a file that lacked GPS at first
x = read_data('20240220_160959_10001.csv') # make sure newly-created corrected file can be read
system('rm 20240220_160959_10001.csv') # remove newly-created corrected file

# downsampling test
x = read_data(paste0(data_path, '20231001_000016_10001.csv')) # make sure newly-created corrected file can be read
downsample(x, t1 = '20231001_0600', t2 = '20231001_0900', dt_out_sec = 15*60)
downsample(paste0(data_path, '20231001_000016_10001.csv'), t1 = '20231001_0600', t2 = '20231001_0900', dt_out_sec = 15*60)
