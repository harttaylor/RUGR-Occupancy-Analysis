#Run an occupancy model using the unmarked package
#Install and load the unmarked package
install.packages("unmarked")
library(unmarked)

#Prepare your data for analysis; data should include presence/absence information for each species 
#in each survey period and should be in "long" format, 
#with one row for each species-site-survey combination

# Fit an occupancy model with covariates on detection but assuming constant occupancy
occ1 <- occu(~ doy + hr ~ LandCover_VegTreed_v1.565m, umf) 
occ1.1 <- occu(~ doy + doy2 + hr + hr2 ~ 1, umf)
str(doy)
str(siteCovs)
#compare naive occupancy model 
occ.n <- occu(~ 1 ~ 1, umf)

#Summarize and plot the residuals
summary(occ.n)
summary(occ1)
summary(occ1.1)
plot(occ1)
plot(occ1.1)

# transform the numeric value (x) into a probability between 0 and 1 using the logistic function: 
plogis(0.00461)#occ1= 0.5011525
plogis(0.0382)#occnaive= 0.5095488
plogis(2.97e-10) #occ1.1= 0.5

#Plot the effects of day of year and time of day on detection probability
det.doy <- data.frame(doy = seq(1.1, 1.5, 0.01), doy2 = seq(1.1, 1.5, 0.01)^2, hr = mean(umf@obsCovs$hr, na.rm = TRUE), hr2 = mean(umf@obsCovs$hr2, na.rm = TRUE))
pred.doy <- predict(occ1, newdata = det.doy, type = "det") 

plot(det.doy$doy, pred.doy$Predicted)

det.hr <- data.frame(doy = mean(occ.data@obsCovs$doy, na.rm = TRUE), doy2 = mean(occ.data@obsCovs$doy2, na.rm = TRUE), hr = seq(5, 8, 0.1))

pred.hr <- predict(occ1, newdata = det.hr, type = "det") 

plot(det.hr$hr, pred.hr$Predicted)

# Extract the detection probability for the first model
pdetect1 <- pdetect(occ1)
??pdetect



# run the occupancy model on the for oen random visit per site 
# Fit an occupancy model with covariates on detection and occupancy 
occ1.1vis <- occu(~ doy + hr ~ LandCover_VegTreed_v1.565m + LandCover_Veg_v1.565m, umf1visit) 
summary(occ1.1vis)

#compare naive occupancy model 
occ.n.1vis <- occu(~ 1 ~ LandCover_VegTreed_v1.565m + LandCover_Veg_v1.565m, umf1visit)
summary(occ.n.1vis)
plogis(0.0591)
plogis(1.37798)
