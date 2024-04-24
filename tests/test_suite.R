library(Rtemisia)

is_approx = function(x, y, d = 0.01) return(((x-y)/y) < d)

## add tests here; failure = error, and must require no user input.
data_full = read_data('Rtemisia/data/20231001_000016_10001.csv')
data_sub = data_full[,c(1:4, 20:31)] # just look at sensor 0

medfilt_nan(data_full)
quicklook(data_full)
plot_CO2(data_full)
plot_temp(data_full)
plot_RH(data_full)
plot_batt(data_full)

medfilt_nan(data_sub)
quicklook(data_sub)
plot_CO2(data_sub)
plot_temp(data_sub)
plot_RH(data_sub)
plot_batt(data_sub)

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
fix_file_time('Rtemisia/data/20000101_000009_10001.csv') # fix a file that lacked GPS at first
x = read_data('20240220_160959_10001.csv') # make sure newly-created corrected file can be read
system('rm 20240220_160959_10001.csv') # remove newly-created corrected file
