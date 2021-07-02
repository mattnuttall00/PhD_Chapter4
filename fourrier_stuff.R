# Here is a rewrite of the initial code, which might give a bit more information
# and allow you to see the component frequencies underlying the whole curve

acq.freq <- 100                    # data acquisition frequency (Hz)
time     <- 50                      # measuring time interval (seconds)
ts       <- seq(0,time,1/acq.freq) # vector of sampling time-points (s) 
f.0      <- 1/time                 # fundamental frequency (Hz)

dc.component       <- 500
component.freqs    <- c(5,7,3)          # frequency of signal components (Hz)
component.delay    <- c(35,0,20)          # delay of signal components (radians)
component.strength <- c(2.5,2.5,5.75)  # strength of signal components

f <- function(t, w, cs, cf, cd) { 
  ft <- dc.component + sum( cs * sin(cf*w*t + cd));
  return(ft);
}

# Before, this was using a trick in R that allowed the author to define the
# function from within 'sapply'. I'm not a fan of this, nor do I think that
# The use of sapply is terribly instructive. I've recoded to show how the
# function 'f' itself is passed to the function below and used inside 'lapply',
# Which essentially applies the function to all elements in ts and makes a list
plot.fourier <- function(f_function, f.0, ts, cs, cf, cd) {
  w <- 2*pi*f.0
  traj_list    <- lapply(ts, f_function, w = w, cs = cs, cf = cf, cd = cd);
  trajectory   <- unlist(x = traj_list);
  minval       <- min(trajectory);
  maxval       <- max(trajectory);
  trajectory_c <- NULL; # For the components
  for(i in 1:length(cf)){
    traj_list         <- lapply(ts, f, w = w, cs = cs[i], cf = cf[i], 
                                cd = cd[i]);
    trajectory_c[[i]] <- unlist(x = traj_list);
    # Don't worry about these maxval and minval lines line -- just to help plot
    if(minval > min(trajectory_c[[i]])){
      minval <- min(trajectory_c[[i]])
    }
    if(maxval < max(trajectory_c[[i]])){
      maxval <- max(trajectory_c[[i]])
    }
  }
  plot(x = ts, y = trajectory, type="l", xlab = "time", ylab = "f(t)", lwd = 2,
       ylim = c(minval, maxval));
  for(i in 1:length(cf)){
    points(x = ts, y = trajectory_c[[i]], type = "l", lwd = 0.35, col = i + 1);  
  }
  points(x = ts, y = trajectory, type="l", lwd = 2); # put to foreground
  abline(h = 500,lty = 3);
}

plot.fourier(f = f, f.0 = f.0, ts = ts, cs = component.strength,
             cf = component.freqs, cd = component.delay);  
# Component frequencies are now given by the different coloured curves

# Now let's say that we want to play around and make the function gradually
# increasing over time. I can add another frequency that is much higher

new_freqs    <- c(5, 7, 3, 0.5)        # frequency of signal components (Hz)
new_delay    <- c(35, 0, 20, 0)       # delay of signal components (radians)
new_strength <- c(2.5, 2.5, 5.75, 7)  # strength of signal components

plot.fourier(f = f, f.0 = f.0, ts = ts, cs = new_strength, cf = new_freqs, 
             cd = new_delay);

# Maybe we don't want it to come down so hard though? Reducing the other amps

new_freqs    <- c(5, 7, 3, 0.5)        # frequency of signal components (Hz)
new_delay    <- c(35, 0, 20, 0)       # delay of signal components (radians)
new_strength <- c(1, 1, 2, 7)  # strength of signal components

plot.fourier(f = f, f.0 = f.0, ts = ts, cs = new_strength, cf = new_freqs, 
             cd = new_delay);







