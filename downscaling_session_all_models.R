
library(gts)


#Coordonnées pour extraire les données des ESMs : 
#latitude : -70,10  et longitude : 120,300 , 
#coordonnées de 0 à 360 avec le méridien a gauche dans l'ocean pacifique

#domain = "south-pacific"
#lon = c(120, 300)
#lat = c(-70, 10)
#gridFile = NULL


# ESM
global = read_gts("input/ipsl-cm5a-lr_historical_to_zs_monthly_195001_200512.nc4")

# change prime meridian
global2 = global
longitude(global2) = longitude(global2, "center") # put the prime meridian to the center instead of the left (because it was centered on the pacific)

global3 = global2
longitude(global3) = longitude(global3, "left")


# observations
obs = read_gts("input/avhrr-only-v2-humboldt-n.198109-201706.nc4")
#there is no mask in this object so we need to create one 

# obs$grid$mask = mask(obs$x)
mask(obs) = mask(obs$x)
image(mask(obs))
# other_mask = mask(obs)

# change coordinates to observations is another option (won't affect us)
obs2 = obs
longitude(obs2) = longitude(obs2, "left")

# spatial subsetting (ESM)
sim = subset(global2, grid=obs) # will only get the points that are in the domain of obs
#so here we have lots of data missing on the coast 
sim = subset(global2, grid=obs, expand=5) #expand means we will expand the area by a number of degrees in every direction 
#sim = subset(global2, grid=obs, expand=c(5,10)) #gives expansion in longitude and latitude of different size


# temporal subseting
sim = window(sim, start=start(obs))
obs = window(obs, end=end(sim))
#now both have the same time period 
#if issue then put start and end in the same one (if observations end before simulations)
#start / end / frequency (data name) can give you first/last/frequency of data in a year of serie

# first regriding: bilinear by default
sim1 = regrid(object = sim, grid = obs)
#regriding here can't normally be used for downscalling and to make scientific purposes because if isn't precise enough and isn't taking into consideration particular phenomenons
#this is downscaling and regriding because we are reducing the scale of the map as well 

# second regridding: akima 
sim2 = regrid(object = sim, grid = obs, method="akima", control=list(link="log"))
#log is using a trick to make sure your variable will remain positive and transformaing it back after

# to be implemented: knn, IDW, krigging
sim3 = regrid(object = sim, grid = obs, method="akima", extrap=TRUE)


par(mfrow=c(2,2))
plot(sim)
plot(sim1)
plot(sim2)
plot(sim3)
#plot all 4 and compare them 

par(mfrow=c(2,1))
plot(obs)
plot(sim3)

sim3 = sim3 - 273.15 # change to celsius

write_ncdf(sim3,filename="my_temperature_in_celsius.nc") 
#if use this along side obs = and change to celsius then open file, changes temperature from Kelvin to celsius and creates new ncdf file

par(mfrow=c(2,1))
plot(obs)
plot(sim3)

dif = obs - sim3
col = colorful::divergencePalette(
  zlim=range(dif, na.rm=TRUE), symmetric = TRUE)
plot(dif, col=col)
#blue = observations are lower than the model (we predict higher temperatures) and red higher then the model (meaning we model colder temperatures)


# compute climatologies
clim_obs = climatology(obs)
clim_sim = climatology(sim3)

# delta bias correction
sim_corr = sim3 - clim_sim + clim_obs
#mostly used because it is simple to implement but not very precise 
#DELTA method : adds difference between observations and simulated data to the simulated data 

par(mfrow=c(1,3))
zlim = range(range(obs, na.rm=TRUE), range(sim3, na.rm=TRUE))
plot(obs, zlim=zlim)
plot(sim3, zlim=zlim)
plot(sim_corr, zlim=zlim) #zlim is made to get the same scale on the right side of the figure 

range(obs, na.rm=TRUE)
range(sim3, na.rm=TRUE)
range(sim_corr, na.rm=TRUE)
#shows min and max value of each object 
#we can see that the range of sim_corr is closer to obs than the range of sim3 and thus that the DELTA method helped correct the bias

mean(obs, na.rm=TRUE) #mean temperature across all time serie
mean_time_obs = mean(obs, by="time")  #mean temperature of observations for every time step (here every month)
mean_time_sim = mean(sim3, by="time")    #mean temperature of simulations for every time step (here every month)
mean_time_simcorr = mean(sim_corr, by="time")    #mean temperature of corrected simulations for every time step (here every month)
mean_space = mean(obs, by="space")   #mean of temperature for every grid cell

par(mfrow=c(2,1))

plot(mean_time_obs) #mean for the temperature for every time step fo the time serie 
lines(mean_time_simcorr, col="red", lwd=2)
lines(mean_time_sim, col="blue", lwd=2)
#gives a graph of the temperatures for every model (observations vs simulation with/without correction) --> way to see the biases
#can see that the corrected simulation outputs are indeed closer to the observations than the bleu (non corrected outputs)

image(mean_space)


# exercise: delta correction for RCP 2.6

# Read the scenarios
rcp26 = read_gts("input/ipsl-cm5a-lr_rcp26_to_zs_monthly_200601_21001231.nc4")
rcp85 = read_gts("input/ipsl-cm5a-lr_rcp85_to_zs_monthly_200601_21001231.nc4")

longitude(rcp26) = longitude(rcp26, "center")
longitude(rcp85) = longitude(rcp85, "center")

# subset
sim26 = subset(rcp26, grid=obs, expand=5)
sim85 = subset(rcp85, grid=obs, expand=5)

sim26 = regrid(object = sim26, grid = obs, method="akima", extrap=TRUE)
sim85 = regrid(object = sim85, grid = obs, method="akima", extrap=TRUE)

sim26 = sim26 - 273.15
sim85 = sim85 - 273.15

#applying the DELTA METHOD
sim26_dm = sim26 - clim_sim + clim_obs
sim85_dm = sim85 - clim_sim + clim_obs

m26 = mean(sim26_dm, by="time")
m85 = mean(sim85_dm, by="time")

plot(mean_time_simcorr, xlim=c(1980, 2100), ylim=c(20, 30), las=1)
lines(m26, col="blue")
lines(m85, col="red")
#see the historical corrected model outputs as well as 2 RCP scenarios predictions (best and worse)

slice80.26 = window(sim26, start=c(2080,1), end=c(2089,12))
slice80.85 = window(sim85, start=c(2080,1), end=c(2089,12))
#take a subset of values from the time series for each RCP scenario

clim80.26 = climatology(slice80.26)
clim80.85 = climatology(slice80.85)
#calculate climatology for each subset of the RCP scenario selected above

c80.26 = mean(clim80.26, by="time")
c80.85 = mean(clim80.85, by="time")
#calculate mean temperature per month for each RCP scenario 

plot(mean(clim_obs, by="time"), ylim=c(20, 30), axes=FALSE)#we will add the axis manually later
lines(c80.26, col="blue")
lines(c80.85, col="red")
axis(2, las=1)
axis(1, at=(1:12 - 0.5)/12, labels=month.abb) #add axis 
box()
#gives a graph with mean temperature per month for observations and the two RCP scenarios 


# quantile mapping

xobs = obs$x[1,3,] #extract a temporal serie (1st latitude, 3rd longitude) = 1 given grid cell
xsim = sim$x[1,3,]
par(mfrow=c(2,1))
hist(xobs)
hist(xsim-273.15)
#way to visually compare distributions of observed / simulated temperatures


probs = seq(0, 1, by=0.05) #quantiles to calculate
quant_sim = quantile(sim3, probs=probs)
quant_obs = quantile(obs, probs=probs)
#calculate qantiles

dat_sim = melt(quant_sim)
dat_obs = melt(quant_obs)
#transform vectors into dataframes  to facilitate comparison between data

dat = merge(dat_obs, dat_sim, all = TRUE)
#merge quantile of observed and simulated data in ONE dataframe 

# dat = dat[complete.cases(dat), ] #delete incomplete lines (containing NA/ missing values)
library(mgcv)
library(scam)
library(dplyr)
library(tictoc)

#change the columns names to make the equations more readable 
dat <- dat %>%
  rename(lon = longitude, lat = latitude)

#will need to add covariates after 
#Global linear
mod0.0 = gam(sst ~ offset(to), data=dat, method="REML")

mod0.1 = gam(sst ~ offset(to) +ti(lon, k=20) + ti(lat,k=30) +ti(lon,lat,k=15), data=dat)

mod0.2 = gam(sst ~ to +ti(lon, k=20) + ti(lat,k=30) +ti(lon,lat,k=15), data=dat)

#mod0.3 = gam(sst ~ to + te(lon,lat,k=15), data=dat)

#Global non linear
mod1.0 = gam(sst ~ s(to, bs="ad", k=40) +ti(lon, k=20) + ti(lat,k=25) +ti(lon,lat,k=15), data=dat)
#tic()
mod1.1 = scam(sst ~ s(to, bs="mpi", k=10) +ti(lon, k=20) + ti(lat,k=25) +ti(lon,lat,k=15), data=dat) #scam package 
#toc()
mod1.2 = scam(sst ~ s(to, bs="micx", k=10) +ti(lon, k=20) + ti(lat,k=25) +ti(lon,lat,k=15), data=dat) #scam package
mod1.3 = scam(sst ~ s(to, bs="micv",k=10) +ti(lon, k=20) + ti(lat,k=25) +ti(lon,lat,k=15), data=dat) #scam package

#Spatially-explicit linear correction 
mod2.0 = gam(sst ~ te(by=to,lon,lat, k=20) +ti(lon, k=20) + ti(lat,k=30) +ti(lon,lat,k=15), data=dat)

mod2.1 = gam(sst ~ te(by=to,lon, k=20) + te(by=to,lat, k=20) + ti(lon, k=20) + ti(lat,k=30) +ti(lon,lat,k=15), data=dat)

mod2.2 = gam(sst ~ to + to:lon +to:lat +ti(lon, k=20) + ti(lat,k=30) +ti(lon,lat,k=15), data=dat)

#Spatially explicit non-linear correction
mod3.0 = gam(sst ~ s(to, bs="ad" ,k=40) + ti(to, lon, k=20) + ti(to, lat, k=20) +ti(lon, k=20) + ti(lat,k=30) +ti(lon,lat,k=15), data=dat)

mod3.1 = scam(sst ~ s(to, bs="mpi", k=20) + ti(to, lon, k=) + ti(to, lat, k=) +ti(lon, k=20) + ti(lat,k=30) +ti(lon,lat,k=15), data=dat)

mod3.2 = scam(sst ~ s(to, bs="micx", k=20) + ti(to, lon, k=) + ti(to, lat, k=) +ti(lon, k=20) + ti(lat,k=30) +ti(lon,lat,k=15), data=dat)

mod3.3 = scam(sst ~ s(to, bs="micv",k=20) + ti(to, lone, k=) + ti(to, lat, k=) +ti(lon, k=20) + ti(lat,k=30) +ti(lon,lat,k=15), data=dat)

mod3.4 = gam(sst ~  ti(to, lon, k=20) + ti(to, lat, k=) +ti(lon, k=20) + ti(lat,k=30) +ti(lon,lat,k=15), data=dat) #why came back to te instead of ti here? 

mod3.5 = gam(sst ~ te(to, lon, k=20) +ti(lon, k=20) + ti(lat,k=30) +ti(lon,lat,k=15), data=dat)

mod3.6 = gam(sst ~ te(to, lat, k=20) +ti(lon, k=20) + ti(lat,k=30) +ti(lon,lat,k=15), data=dat)

#Spatially-varying mixed
mod4.0 = gam(sst ~ te(by=to, lon, lat, k=20) + s(to, bs="ad", k=40) +ti(to, lon, k=20) + ti(to, lat, k=20) + te(lon, lat, k=15),data=dat)

mod4.1 = gam(sst ~ te(by=to, lon, lat, k=20) + ti(to, lon, k=20) + ti(to, lat, k=20) + te(lon, lat, k=15),data=dat)

mod4.2 = gam(sst ~ te(by=to, lon, lat, k=20) + s(to, bs="ad", k=40) +te(lon, lat, k=15),data=dat)

mod4.3 = gam(sst ~ te(lon, lat, to, k=10) +te(lon, lat, k=15),data=dat)

mod = mod1.1

dat$pred = predict(mod, newdata=dat)

#test = data.frame(to=seq(13, 30, by=0.01))
#test$pred = predict(mod, newdata=test)

#plot(pred ~ to, data=test)
#abline(c(0,1), col="red", lty=3)


fsim3 = melt(sim3) # create a data.frame with rcp26 ? 
colnames(fsim3)[colnames(fsim3) == "longitude"] <- "lon"
colnames(fsim3)[colnames(fsim3) == "latitude"] <- "lat"
fsim3$sst = predict(mod, newdata=fsim3) # predict with downscaling model
sim_bc = sim3 # copy the original gts object
sim_bc$x[] = fsim3$sst # replace values with the predictions

# projection


fdat26 = melt(sim26) # create a data.frame with rcp26
colnames(fdat26)[colnames(fdat26) == "longitude"] <- "lon"
colnames(fdat26)[colnames(fdat26) == "latitude"] <- "lat"
fdat26$sst = predict(mod, newdata=fdat26) # predict with downscaling model
sim26_bc = sim26 # copy the original gts object
sim26_bc$x[] = fdat26$sst # replace values with the predictions


par(mfrow=c(1,2))
zlim = range(range(sim26, na.rm=TRUE), range(sim26_bc, na.rm=TRUE))
plot(sim26, zlim=zlim)
plot(sim26_bc, zlim=zlim)
#compare temperatures before/after bias correction 

#temporal mean of non corrected/ corrected temperatures
m_qm = mean(sim_bc, by="time") #past
m26_qm = mean(sim26_bc, by="time") #future

plot(mean_time_simcorr, xlim=c(1980, 2100), ylim=c(20, 30), las=1)
lines(m26, col="blue")
# lines(m85, col="red")
lines(m26_qm, col="green")

mmsim = stats::filter(mean_time_simcorr, filter=rep(1, 12)/12)
mm26 = stats::filter(m26, filter=rep(1, 12)/12)
mm26_qm = stats::filter(m26_qm, filter=rep(1, 12)/12)
mm_qm = stats::filter(m_qm, filter=rep(1, 12)/12)

plot(mm26, col="blue", ylim=c(20, 30), xlim=c(1980, 2100))
lines(mm26_qm, col="green")
lines(mmsim)
lines(mm_qm)

write_ncdf(sim26_bc, filename="output_sprfmo.nc")

