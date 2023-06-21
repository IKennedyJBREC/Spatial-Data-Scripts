library(arcgisbinding)
library(sf)
library(tidyverse)
library(GWmodel)    
library(tmap)

arc.check_product()

download.file("https://github.com/gwverse/gw/blob/main/data/georgia.rda?raw=True",
              "./georgia.RData", mode = "wb")
load("georgia.RData")

df = st_drop_geometry(georgia)

m = lm(MedInc ~ PctRural+PctBach+PctEld+PctFB+PctPov+PctBlack, data = df) 
summary(m)

# determine studentised residuals and attach to georgia
s.resids <- rstudent(m)
georgia$s.resids = s.resids
# map the spatial distribution of outliers
tm_shape(georgia) + 
  tm_polygons('s.resids', breaks = c(min(s.resids), -1.5, 1.5, max(s.resids)), 
              palette = c("indianred1","antiquewhite2","powderblue"), 
              title = "Residuals") +
  tm_layout(legend.format= list(digits = 1), frame = F) 

# moran test
library(spdep)
geor.nb <- poly2nb(georgia) 
geor.lw = nb2listw(geor.nb)
moran.test(georgia$s.resids, geor.lw) 


tm_shape(georgia) +
  tm_polygons('PctPov') +
  tm_layout(legend.format = list(digits = 0), frame = F, legend.title.fontfamily = 'sansserif', title = '% in Poverty', legend.outside = TRUE, main.title = "Poverty in Georgia", main.title.fontfamily = 'sansserif')

# convert to sp
georgia.sp = as(georgia, "Spatial")

# determine the kernel bandwidth
bw <- bw.gwr(MedInc~PctRural+PctBach+PctEld+PctFB+PctPov+PctBlack,
             approach = "AIC",
             adaptive = T,
             data=georgia.sp) 

# determine the kernel bandwidth
bw2 <- bw.gwr(MedInc~PctRural+PctBach+PctEld+PctFB+PctPov+PctBlack,
              approach = "AIC",
              adaptive = F,
              data=georgia.sp) 

summary(as.vector(st_distance(georgia)))


# fit the GWR model
m.gwr <- gwr.basic(MedInc~PctRural+PctBach+PctEld+PctFB+PctPov+PctBlack, 
                   adaptive = T,
                   data = georgia.sp,
                   bw = bw)  
m.gwr

tab.gwr <- rbind(apply(m.gwr$SDF@data[, 1:7], 2, summary), coef(m))
rownames(tab.gwr)[7] <- "Global"
tab.gwr <- round(tab.gwr, 1)
t(tab.gwr)


gwr_sf = st_as_sf(m.gwr$SDF)
gwr_sf

tm_shape(gwr_sf) +
  tm_fill(c("PctBach", "PctPov"), palette = "viridis", style = "kmeans") +
  tm_borders(col = 'black', lwd = .04) +
  tm_layout(legend.position = c("right","top"), frame = F)


# determine which are significant
tval = gwr_sf %>%dplyr::select(all_of("PctBlack_TV")) %>% st_drop_geometry()
signif = tval < -1.96 | tval > 1.96

# map the counties
tm_shape(gwr_sf) +
  tm_fill("PctBlack",midpoint = 0) + tm_style("col_blind")+
  tm_layout(legend.position = c("right","top"))+
  # now add the tvalues layer
  tm_shape(gwr_sf[signif,]) + tm_borders()




gw.ms <- gwr.multiscale(MedInc~ PctRural + PctBach + PctEld + PctFB + PctPov + PctBlack, 
                        data = as(georgia, "Spatial"),
                        adaptive = T, 
                        max.iterations = 1000,
                        criterion="CVR",
                        kernel = "bisquare",
                        #Initial bandwidths to test, one for each predictor variable
                        bws0=c(100,100,100,100,100,100),
                        verbose = F, 
                        #Vector of logicals for each predictor variable, decides if the predictor should be centered
                        predictor.centered=rep(T, 6))

bws = gw.ms$GW.arguments$bws
bws

coefs_msgwr = apply(gw.ms$SDF@data[, 1:7], 2, summary)
round(coefs_msgwr,1)


tab.mgwr = data.frame(Bandwidth = bws, 
                      t(round(coefs_msgwr,1)))
names(tab.mgwr)[c(3,6)] = c("Q1", "Q3")
tab.mgwr

mgwr_sf = st_as_sf(gw.ms$SDF)
mgwr_sf


p1 = tm_shape(mgwr_sf) +
  tm_fill(c("Intercept", "PctPov"), palette = "viridis", style = "kmeans") +
  tm_layout(legend.position = c("right","top"), frame = F)
# coefficients whose values flip 
p2 = tm_shape(mgwr_sf) +
  tm_fill(c("PctFB", "PctBlack"),midpoint = 0, style = "kmeans") +
  tm_style("col_blind")+
  tm_layout(legend.position = c("right","top"), frame = F) 
tmap_arrange(p1, p2, nrow = 2)

# determine which are significant
tval = mgwr_sf$PctBlack_TV
signif = tval < -1.96 | tval > 1.96
# map the counties
tm_shape(mgwr_sf) +
  tm_fill("PctBlack",midpoint = 0) + tm_style("col_blind")+
  tm_layout(legend.position = c("right","top"))+
  # now add the tvalues layer
  tm_shape(mgwr_sf[signif,]) + tm_borders(lwd = 1.5)

gwr.iqr <- t(tab.gwr[c(2,5,7),])
mgwr.iqr <- tab.mgwr[, c(1,3,6)]
cbind(gwr.iqr, mgwr.iqr)

# gwr
p1 = tm_shape(gwr_sf) +
  tm_fill(c("Intercept", "PctPov"), palette = "viridis", style = "kmeans") +
  tm_layout(legend.position = c("right","top"), frame = F)
# mgwr
p2 = tm_shape(mgwr_sf) +
  tm_fill(c("Intercept", "PctPov"), palette = "viridis", style = "kmeans") +
  tm_layout(legend.position = c("right","top"), frame = F)
tmap_arrange(p1, p2, nrow = 2)



# gwr
tval = gwr_sf$PctFB_TV
signif = tval < -1.96 | tval > 1.96
# map the counties
p1 = tm_shape(gwr_sf) +
  tm_fill("PctFB",midpoint = 0) + tm_style("col_blind")+
  tm_layout(legend.position = c("right","top"))+
  # now add the tvalues layer
  tm_shape(mgwr_sf[signif,]) + tm_borders()
# mgwr
tval = mgwr_sf$PctFB_TV
signif = tval < -1.96 | tval > 1.96
# map the counties
p2 = tm_shape(mgwr_sf) +
  tm_fill("PctFB",midpoint = 0) + tm_style("col_blind")+
  tm_layout(legend.position = c("right","top"))+
  # now add the tvalues layer
  tm_shape(mgwr_sf[signif,]) + tm_borders()
tmap_arrange(p1, p2, nrow = 1)

# Make predictions using the GWR model
predictions <- gw.ms$lm$fitted.values
res <- gw.ms$lm$residuals
df <- df %>%
  cbind(predictions, res)
