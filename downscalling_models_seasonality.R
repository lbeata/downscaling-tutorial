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