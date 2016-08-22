rm(list = ls())
source('head.R')
load(file.path(dir_data,'tsa3.rda'))
opar <- par(no.readonly = T)
par(mar = rep(2,4))

# E1.1
plot(jj, type="o", ylab="Quarterly Earnings per Share")

# E1.2
plot(gtemp, type="o", ylab="Global Temperature Deviations")

# E1.3
plot(speech)

# E1.4
plot(nyse, ylab="NYSE Returns")

# E1.5
plot(soi, ylab="", xlab="", main="Southern Oscillation Index")
plot(rec, ylab="", xlab="", main="Recruitment")

# E1.6
par(mfrow=c(2,1), mar=c(3,2,1,0)+.5, mgp=c(1.6,.6,0))
ts.plot(fmri1[,2:5], lty=c(1,2,4,5), ylab="BOLD", xlab="",
           main="Cortex")
ts.plot(fmri1[,6:9], lty=c(1,2,4,5), ylab="BOLD", xlab="",
          main="Thalamus & Cerebellum")
mtext("Time (1 pt = 2 sec)", side=1, line=2)

# E1.7
plot(EQ5, main="Earthquake")
plot(EXP6, main="Explosion")


# E1.8 - E1.9
w = rnorm(500,0,1) # 500 N(0,1) variates
v = filter(w, sides=2, rep(1/3,3)) # moving average
par(mfrow=c(2,1))
plot.ts(w, main="white noise")
plot.ts(v, main="moving average")

# E1.10
w = rnorm(550,0,1) # 50 extra to avoid startup problems
x = filter(w, filter=c(1,-.9), method="recursive")[-(1:50)]
plot.ts(x, main="autoregression")

# E1.11
set.seed(154) # so you can reproduce the results
w = rnorm(200,0,1); x = cumsum(w) # two commands in one line
wd = w +.2; xd = cumsum(wd)
plot.ts(xd, ylim=c(-5,55), main="random walk")
lines(x); lines(.2*(1:200), lty="dashed")

# E1.12
cs = 2*cos(2*pi*1:500/50 + .6*pi)
w = rnorm(500,0,1)
par(mfrow=c(3,1), mar=c(3,2,2,1), cex.main=1.5)
plot.ts(cs, main=expression(2*cos(2*pi*t/50+.6*pi)))
plot.ts(cs+w, main=expression(2*cos(2*pi*t/50+.6*pi) + N(0,1)))
plot.ts(cs+5*w, main=expression(2*cos(2*pi*t/50+.6*pi) + N(0,25)))

par(opar)
