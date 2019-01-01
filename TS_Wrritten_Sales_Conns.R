###################################################################################
#########Loading Libraries
# install.packages("odbc")
# install.packages("DBI")
# install.packages("dplyr")
# install.packages("dbplyr")
# install.packages("ggplot2")
# install.packages("forecast")
# install.packages("tseries")
# install.packages("graphics")
# install.packages("reshape")
# install.packages("gridExtra")
# install.packages("gsubfn")

library(DBI)
library(dplyr)
library(dbplyr)
library(ggplot2)
library(forecast)
library(tseries)
require(graphics)
library(ggplot2)
library(reshape)
library(gridExtra)
library(gsubfn)

########################################################################################
#######Loading Data
con <- DBI::dbConnect(odbc::odbc(),
                      Driver = "Hortonworks Hive ODBC Driver",
                      Host   = "172.29.167.159",
                      Schema = "traffic",
                      UID    = rstudioapi::askForPassword("Database user"),
                      PWD    = rstudioapi::askForPassword("Database password"),
                      Port   = 10000)
## reading date and written sales value
Base_data<- dbGetQuery(con,'select writtenamt, activiyt_date  from traffic.strtraffic')

nrow(Base_data)#235027
summary(Base_data)
###################################################################
head(Base_data)
##chnageing to date
Base_data$Order_date<-as.Date(Base_data$strtraffic.activiyt_date,'%Y-%m-%d')
View(Base_data)
##range of data
max(Base_data$Order_date)#"2018-11-29" ## ignore november 2018 from the analysis, as cmplete month data is not present
min(Base_data$Order_date)#"2009-01-09" ## ignore janunary 2009
## agregating at a month level
Month_wise_aggregation<-Base_data%>%
  group_by (sale_year=as.numeric(format(Order_date,'%Y')),sales_month=as.numeric(format(Order_date,'%m')))%>%
   summarise(Total_sales=sum(strtraffic.writtenamt))

Month_wise_aggregation<-as.data.frame(Month_wise_aggregation)
class(Month_wise_aggregation)
names(Month_wise_aggregation)
nrow(Month_wise_aggregation)#109
str(Month_wise_aggregation)
write.csv(Month_wise_aggregation,file='C:/Users/hashmap/Downloads/Cons_Unfiltered_Monthly_sales.csv',row.names=F)
# Month_wise_aggregation<-read.csv('C:/Users/hashmap/Downloads/Cons_Unfiltered_Monthly_sales.csv')
# str(Month_wise_aggregation)
#####################################################

#####validating if all values were coverted properly and their ranges
sum(is.na(Month_wise_aggregation$sales_month))
min(Month_wise_aggregation$sales_month)#1
max(Month_wise_aggregation$sales_month)#12
######
sum(is.na(Month_wise_aggregation$sale_year))
min(Month_wise_aggregation$sale_year)#2009
max(Month_wise_aggregation$sale_year)#2018
#####
sum(is.na(Month_wise_aggregation$Total_sales))
min(Month_wise_aggregation$Total_sales)#25682
max(Month_wise_aggregation$Total_sales)#184337004
#minimum seems to be a much lower side, data may not be consistant
##lets validate 
Month_wise_aggregation[order(Month_wise_aggregation$sale_year,Month_wise_aggregation$sales_month),]
#year 2009 has missing months

##checking if all month for all years are present
Month_wise_aggregation%>%
  group_by (sale_year)%>%
  summarise(Total_months=n())
#even 2010 has a month misisng
## ignorning 2009 and 2010 from the data frame since the value are missing
Month_wise_aggregation<-Month_wise_aggregation[which(!(Month_wise_aggregation$sale_year %in% c(2009,2010))),]

Month_wise_aggregation%>%
  group_by (sale_year)%>%
  summarise(Total_months=n())
#since november 2018 doesn't have data for all days(Till 29th only) dropping that from the data set
Month_wise_aggregation<-Month_wise_aggregation[which(!(Month_wise_aggregation$sale_year==2018 & Month_wise_aggregation$sales_month==11 )),]
Month_wise_aggregation%>%
  group_by (sale_year)%>%
  summarise(Total_months=n())

nrow(Month_wise_aggregation)#94
plot(ts(Month_wise_aggregation$Total_sales,frequency=12))
axis(1,at=seq(1,10))
View(Month_wise_aggregation)
#the grpaph doesn't seem to be consitant across
#first 30 months(2.5 years) seems to be following diifferent TS, may be data collection was not properly done
#we will remove those, jan 2011 - june2013
####
#tried removing 45 months as well to get a clean series  details are present below

Month_wise_aggregation<-Month_wise_aggregation[31:94,]#46:94
nrow(Month_wise_aggregation)#64
write.csv(Month_wise_aggregation,file='C:/Users/hashmap/Downloads/Cons_Filtered_Monthly_sales.csv',row.names=F)

# Month_wise_aggregation<-read.csv('C:/Users/hashmap/Downloads/Cons_Filtered_Monthly_sales.csv')
# str(Month_wise_aggregation)
###################################  Train & Test  ############################################################

##Breaking into train and test set , last year we are predciting and remaining we will use for training
Train<-Month_wise_aggregation[1:52,]
Validate<-Month_wise_aggregation[53:64,]
#plotting time series for train
ts_train<-ts(Train$Total_sales,frequency = 12)
plot(ts_train)

#plotting time series for validate
ts_validate<-ts(Validate$Total_sales,frequency = 12)
plot(ts_validate)

####lets check the (p)acf plots for our traing data
# Compute and plot the ACF
acf(ts_train,lag.max =11)
#MA(1) or may be 3 even; but both of them very slightly

# Compute and plot the ACF for the time series
acf(ts_train, type="partial",lag.max =11)
#AR(2) ever so slightly but its imapct seems to be even less than the MA counter part

####################################### Modellling  ##########################################################

#####Generic(recommended) Seasonal ariam model (0, 1, 1),(0, 1, 1)
autoarima_normal<-arima(ts_train, order = c(0, 1, 1),
                        seasonal = list(order = c(0, 1, 1)))
autoarima_normal
##########################################################################
#modle with first 30 montsh removed
# arima(x = ts_train, order = c(0, 1, 1), seasonal = list(order = c(0, 1, 1)))
# 
# Coefficients:
#          ma1     sma1
#        -0.3416  -0.0926
# s.e.   0.1316   0.1804
# 
# sigma^2 estimated as 4.929e+13:  log likelihood = -670.26,  aic = 1346.52
##########################################################################
##########################################################################
#Model if first 45 months were removed instead of 30
# arima(x = ts_train, order = c(0, 1, 1), seasonal = list(order = c(0, 1, 1)))
# Coefficients:
#          ma1     sma1
#       -0.3416  -0.0926
# s.e.   0.1316   0.1804#SE is greater than the actual componet magnitude wise
# 
# sigma^2 estimated as 4.929e+13:  log likelihood = -670.26,  aic = 1346.52  # aic value is decently high
#####################################################################
#plotting what we have versus the actual
plot(ts_train, col="black")
lines(fitted(autoarima_normal), col="red")
#residual component
resi_auto_arima <- ts_train - fitted(autoarima_normal)
plot(as.numeric(resi_auto_arima))## residue seems to be increasing over the period

#test for stationarity
kpss.test(resi_auto_arima)
# KPSS Test for Level Stationarity
# 
# data:  resi_auto_arima
# KPSS Level = 0.14441, Truncation lag parameter = 3, p-value = 0.1 #null hypotheisi of series being stationay can be rejected

# Compute and plot the ACF for the time remaining series
acf(resi_auto_arima,lag.max =11)
#Looks good

# Compute and plot the ACF for the time series
acf(resi_auto_arima, type="partial",lag.max =11)
#looks good
#let's see the performance of the recommend model on the series

fcast_auto_arima <- forecast(autoarima_normal, h = 12)
names(fcast_auto_arima)
##binding data to view it together
Normal_compare<-cbind(predcited=as.data.frame(fcast_auto_arima$mean),actual=as.data.frame(Validate$Total_sales),Validate$sale_year,Validate$sales_month)
Normal_compare$diff<-(Normal_compare$`Validate$Total_sales`- Normal_compare$x)*100/Normal_compare$`Validate$Total_sales`
Normal_compare
##-9 6 for 45 month removal
#-7 to 8 for 30 month removal, but november and december have etter predicton here
#### erro marhgin varies from -7 till 8
MAPE_auto_arima_normal <- accuracy(fcast_auto_arima$mean,Validate$Total_sales)
MAPE_auto_arima_normal
#####################################################################
# performance on 30 first month were removed
# ME    RMSE     MAE       MPE     MAPE
# Test set -1085173 6555480 5834720 -0.885983 5.055792
#####################################################################
##performance when first 45 months were removed
#             ME    RMSE     MAE       MPE     MAPE
# Test set -2714726 6259375 5415992 -2.350892 4.711927
#####################################################################
############################################################################################################################
####### Performance for a few other modlles evaluated they were tested only for 30 intial month removed
# order = c(1, 1, 1),
# seasonal = list(order = c(0, 1, 1)
#     ME    RMSE     MAE       MPE     MAPE
# -1887427 6704352 5936824 -1.596046 5.142666
#######################################################################
# order = c(2, 1, 1),
# seasonal = list(order = c(0, 1, 1)
#     ME    RMSE     MAE       MPE     MAPE
#  -1623228 6695816 5891596 -1.373106 5.115456
########################################################################
###################################################################################################################################
auto_arima_pred <- c(fitted(autoarima_normal),ts(fcast_auto_arima$mean))
plot(ts(Month_wise_aggregation[,'Total_sales']), col = "black")
lines(auto_arima_pred, col = "red")

##############################################################################################################
#############################################################################################################
##Let's check what model auto arima model does predicts
autoarima<-auto.arima(ts_train,stepwise=FALSE, approximation=FALSE)

autoarima
###################################################################
# 30 month removed data is much closer to the expectd seasonal model, on seasonal ma(1) component missing
# Series: ts_train 
# ARIMA(0,1,1)(0,1,0)[12] 
# 
# Coefficients:
#   ma1
# -0.3380
# s.e.   0.1317
# 
# sigma^2 estimated as 5.107e+13:  log likelihood=-670.39
# AIC=1344.79   AICc=1345.12   BIC=1348.12 # similar to recomended model
###########################################################
#######################################################
#with 45 months removed the auto ariam algo is thrown offgaurd with non season ar coming out to be 3
#the prediction are thrown of by a big margin as well
# Series: ts_train 
# ARIMA(3,1,0)(0,1,0)[12] 

# Coefficients:
#   ar1     ar2     ar3
# -0.3172  0.0646  0.7308
# s.e.   0.1375  0.1651  0.1452
# 
# sigma^2 estimated as 3.102e+13:  log likelihood=-423.58
# AIC=855.16   AICc=857.16   BIC=860.03
plot(ts_train, col="black")
lines(fitted(autoarima), col="red")
lines(fitted(autoarima_normal), col="blue")
#the model are very similar as expected
resi_auto_arima <- ts_train - fitted(autoarima)

#tets for stationarity
plot(as.numeric(resi_auto_arima))
kpss.test(resi_auto_arima)
# KPSS Test for Level Stationarity
# 
# data:  resi_auto_arima
# KPSS Level = 0.1525, Truncation lag parameter = 3, p-value = 0.1, assumtion of series being stationary can't be rejected
#Kpss test show residual for the auto arima model


#forcasting for comparison
fcast_auto_arima <- forecast(autoarima, h = 12)
names(fcast_auto_arima)
##binding data to view it together
Arima_compare<-cbind(predcited=as.data.frame(fcast_auto_arima$mean),actual=as.data.frame(Validate$Total_sales),Validate$sale_year,Validate$sales_month)
Arima_compare$diff<-(Arima_compare$`Validate$Total_sales`- Arima_compare$x)*100/Normal_compare$`Validate$Total_sales`
Arima_compare


###comparing two model closere
View(as.data.frame(cbind(Total_sales=Normal_compare$`Validate$Total_sales`,Month=Normal_compare$`Validate$sales_month`,Recm_Pred=Normal_compare$x,Auto_Pred=Arima_compare$x,
                     Recm_Per=Normal_compare$diff,Auto_Pre=Arima_compare$diff,Diff_per=abs(Normal_compare$diff)-abs(Arima_compare$diff))))

Rec_Vs_auto<-as.data.frame(cbind(Total_sales=Normal_compare$`Validate$Total_sales`,Month=Normal_compare$`Validate$sales_month`,Recm_Pred=Normal_compare$x,Auto_Pred=Arima_compare$x,
                    Recm_Per=Normal_compare$diff,Auto_Pre=Arima_compare$diff,Diff_per=abs(Normal_compare$diff)-abs(Arima_compare$diff)))
write.csv(Month_wise_aggregation,file='C:/Users/hashmap/Downloads/Rec_Vs_auto.csv',row.names=F)


#recommeded model peroformes better for 8 months as compare to the 4 montsh of auto arima model performce
#But the spikes of November and December are better address by auto arima model

#recomed model perfoms better from march to october
sum(Normal_compare[which(Normal_compare$"Validate$sales_month" %in% seq(3,10)),"Validate$Total_sales"])
#888632356
mean(Normal_compare[which(Normal_compare$"Validate$sales_month" %in% seq(3,10)),"Validate$Total_sales"])
#111079045

#Auto model perform better on Nov to Feb
sum(Normal_compare[which(Normal_compare$"Validate$sales_month" %in% c(1,2,11,12)),"Validate$Total_sales"])
#489309010
mean(Normal_compare[which(Normal_compare$"Validate$sales_month" %in% c(1,2,11,12)),"Validate$Total_sales"])
#122327253
  
## We woudl prefer teh auto model as of now since the peak month performce is better their,
## we chnge thsi after discusion with the client regarding overall performace versus peak performance
#nov-till feb errors are high
MAPE_auto_arima_normal <- accuracy(fcast_auto_arima$mean,Validate$Total_sales)
MAPE_auto_arima_normal
################################################
#for 30 month removed
# ME    RMSE     MAE        MPE     MAPE
# Test set -973117.5 6722959 5978396 -0.7966873 5.192833
#####################################################
# for 45 montsh removed , as expected teh arima 
# ME     RMSE      MAE       MPE     MAPE
# Test set -15289154 16642536 15289154 -13.63156 13.63156
################################################

##########################################  Classical Decomposition  ###############################################################
#####Smotthening
#####Recomendation for reatil seasonal is a Trimester(4)
#moving avg method
plot(ts_train)
width<-c(2,3,4,6)
cols <- c("red", "blue", "green","orange", "black")
labels <- c(paste("width =", width), "Original")
for (i in 1:length(width)){
  smoothedseries <- stats::filter(ts_train, filter=rep(1/width[i], width[i]),
                                  method="convolution", sides=2)
  
  lines(smoothedseries, col=cols[i], lwd=2)
  print(length(smoothedseries))
}

legend("topleft", labels, col=cols, lwd=2, cex=.5,bg=8)

# we will use the width 4 the recommended number, we did teh exercise without smoothing nad with quater smootering as well
smoothedseries <- stats::filter(ts_train, filter=rep(1/4, 4),
                                method="convolution", sides=2)
sum(is.na(smoothedseries)) 
smoothedseries
length(smoothedseries)#52
#first  and last two are na, since we don't have the range, 
#we will use the last steps as differnce to fill in these values
#intial 1 values
smoothedseries[1]<-smoothedseries[2]-(smoothedseries[3]-smoothedseries[2])
#last  values
length(smoothedseries)#52
smoothedseries[51]<-smoothedseries[50]+(smoothedseries[50]-smoothedseries[49])
smoothedseries[52]<-smoothedseries[51]+(smoothedseries[51]-smoothedseries[50])

####created new series
smoothedseries<-cbind.data.frame(date_order=1:length(smoothedseries),
                 Total_sales=as.vector(smoothedseries))
######################################################################
get_residual_additive<- function (x,timevals){
  
  lmfit <- lm(Total_sales ~ poly(date_order,options[x,]$i)+sin(options[x,]$j*date_order)
              +   cos((1-options[x,]$j)*date_order)
              , data=smoothedseries)
  globalpred <- predict(lmfit, Month=timevals)      
  localpred <- ts_train - globalpred
  sum(localpred^2)
  
}

######################################################################

get_residual_multiplicative<- function (x,timevals){
  
  lmfit <- lm(Total_sales ~ poly(date_order,options[x,]$i)*sin(options[x,]$j*date_order)
              +   poly(date_order,options[x,]$i)*cos((1-options[x,]$j)*date_order)
              , data=smoothedseries)
  globalpred <- predict(lmfit, Month=timevals)      
  localpred <- ts_train - globalpred
  sum(localpred^2)
  
}
####################################################################

#running regression for additive model
sum_of_sq_err<-sapply(1:nrow(options),get_residual_additive,12)
plot(sum_of_sq_err)
axis(side=1, at=1:30)
#14,15,17,18, 29, 30 are  leaving less residue
options[order(sum_of_sq_err),]
#    i  j
# 15 3 0.5
# 14 2 0.5
# 17 2 0.6
# 18 3 0.6
# 29 2 1.0
# 30 3 1.0
#for quater based smoothing top two remains same and 17,18 and 29,30 interchange their respective position

#degree two model was evaluated owing to it being simpler but in did not perform well on the validation set
#we will use the degree 3 with 0.5 division
options[15,]# 3 0.5
sum_of_sq_err[15]# Trimester smoothing9.897604e+15  #Quater smoothieng for the first 2nd degree & sin weightage 1 simpler model :-1.016288e+16


#running regression for multiplicative model
sum_of_sq_err<-sapply(1:nrow(options),get_residual_multiplicative,12)
plot(sum_of_sq_err)
axis(side=1, at=1:30)
options[order(sum_of_sq_err),]
#    i   j
# 15 3 0.5
# 12 3 0.4
# 9  3 0.3
# 14 2 0.5
# 11 2 0.4
# 18 3 0.6
#9 & 14 interchnage your their position for quater smoothing

options[15,]# 3 0.5
sum_of_sq_err[15]#8.828066e+15  # similar error number on the quater smoothening with simpler degree 2 & sin weightage 1  model 

## we will use teh additive model for its simplicity as compared to the multiplicative model
## we did evvaluate the additiive and multiplicative model for both trimester and quater smoothing 
##and found that additive model with trimester smootheing(Recommended) performs best 
##aditive model for smotthed series
lmfit <- lm(Total_sales ~ poly(date_order,options[15,]$i)+sin(options[15,]$j*date_order)
            +   cos((1-options[15,]$j)*date_order)
            , data=smoothedseries)# we can even use train data set 
summary(lmfit)
# Residual standard error: 6027000 on 46 degrees of freedom
# Multiple R-squared:  0.8211,	Adjusted R-squared:  0.8016 #Acceptable value
# F-statistic: 42.22 on 5 and 46 DF,  p-value: 4.393e-16
globalpred <- predict(lmfit, Month=seq(1:52))      
localpred <- ts_train - globalpred# subtracting from teh orriginal series not the smoothed one

#######################################
plot(ts_train,col='green', type = "l")#actual time series
lines(globalpred, col='red', type = "l")#trend line
lines(localpred, col='blue', type = "l")##not in range
plot(localpred, col='blue', type = "l")## their still seems to be an auto regressive residual pattern

#######################################################
#Checking teh Locla autoregressive part of Time series
#######################################################
# Compute and plot the ACF for the time remaining series
acf(localpred,lag.max =11)
#2,10 are the one shooting above, pointing to and ma(2) or ma(10) fit

# Compute and plot the ACF for the time series
acf(localpred,lag.max =11, type="partial")
#again 2, 10 are shoothing out 

## let's try and fit a series to it using auto.arima
armafit_local <- auto.arima(localpred)
armafit_local
######################################################
##4 month(trimester) smoothing and additive
# Series: localpred 
# ARIMA(1,0,0)(0,1,0)[12] 
# 
# Coefficients:
#         ar1
#       0.7083
# s.e.  0.1140 # is low enough
# 
# sigma^2 estimated as 5.028e+13:  log likelihood=-687.57
# AIC=1379.15   AICc=1379.47   BIC=1382.52 ## similar to the original auto ariam fit
###############################################################################################
##smoothed and avg 4 months(trimester) & multiplicative
# Series: localpred 
# ARIMA(1,0,0)(1,1,0)[12] 
# 
# Coefficients:
#         ar1     sar1
#       0.4033  -0.3318
# s.e.  0.1916   0.2016 erroe compoent very high
# 
# sigma^2 estimated as 4.838e+13:  log likelihood=-686.72
# AIC=1379.44   AICc=1380.11   BIC=1384.51
##############################################
## smoothed and avg 3 months(quater) & multiplicative
# Series: localpred 
# ARIMA(1,0,0)(0,1,0)[12] 
# 
# Coefficients:
#       ar1
#       0.7143
# s.e.  0.1209 acceptabel erro component
#
# sigma^2 estimated as 5.241e+13:  log likelihood=-688.41
# AIC=1380.82   AICc=1381.15   BIC=1384.2
#almost simlar series smothting has not impacted much
#################################################################
##############################################################
# Model with no smoothing
#Series: localpred 
# ARIMA(1,0,0)(0,1,0)[12] 
# 
# Coefficients:
#       ar1
#       0.6572
# s.e.  0.1300 # acceptable error component
# 
# sigma^2 estimated as 5.055e+13:  log likelihood=-687.61
# AIC=1379.23   AICc=1379.55   BIC=1382.61
######################################################################
##Similar Residual models for all options even non smoothen one but trimester and additive is best in term of validation performce

#calculation residual
resi <- localpred-fitted(armafit_local)
# Compute and plot the ACF for the time series
acf(resi,lag.max =11)
#seems fine now

# Compute and plot the ACF for the time series
acf(resi, type="partial",lag.max =11)
# their  much improvement in the pacf graph as well

##Lets perform teh stationary test
adf.test(resi, alternative="stationary")
#p-value = 0.6728, null hypothesis of not being stationary can't be rejected still
kpss.test(resi)
#p-value = 0.1, hence the null hypothesis of series being stationary can't be rejected
#ADF test seems to be telling us otehrwise but from acf and pacf plot and kpss test we will assume that series is stationary  and move forward

########################################################
#Prediction and Accuracy mesurements
#########################################################
#lets predict and see the mape value


global_pred_out <- predict(lmfit,data.frame(date_order = 53:64))
auto_arima_pred_out <- predict(armafit_local, n.ahead = 12)
global_pred_out<-global_pred_out+auto_arima_pred_out$pred
MAPE_sales <- accuracy(global_pred_out,Validate$Total_sales)
MAPE_sales
# ME     RMSE      MAE     MPE     MAPE
# Test set 11294901 14751573 12456204 10.2704 11.09098


#six month predcition plot
plot(53:64,Validate$Total_sales,col='green', type = "l")#actual time series
lines(53:64, global_pred_out, col='red', type = "l")#predcited time series
View(as.data.frame(cbind(Total_sales=Validate$Total_sales,Month=Validate$sales_month,CD_Pred=global_pred_out,
                         CD_per=(Validate$Total_sales-global_pred_out)*100/Validate$Total_sales)))
CD_Validation<-as.data.frame(cbind(Total_sales=Validate$Total_sales,Month=Validate$sales_month,CD_Pred=global_pred_out,
                                   CD_per=(Validate$Total_sales-global_pred_out)*100/Validate$Total_sales))
#modle is doing a good job at capturing the behaviour of peak season Nov, Dec , and a decent one for jab and feb
# buts its failing measubaly for the remaing months

##overall picture
length((Validate$Total_sales-global_pred_out)*100/Validate$Total_sales)
Rec_Vs_auto_Vs_LD<-as.data.frame(cbind(Rec_Vs_auto,CD_Pred=CD_Validation$CD_Pred,CD_per=CD_Validation$CD_per))
View(Rec_Vs_auto_Vs_LD)


##?????????????????????????????????????????????????????????????????????????????????????
#Is it accpetable to combine models in the sense we give the 4 months prediction from one model and remaining from another one.
# couldn't find an conclusive read on this one, your advice woudl help greatly!
##???????????????????????????????????????????????????????????????????????????????

## For now we will use the AUto arima model as its is the closet to an overall predcition prespective and
#does slightly  better on the peak months

fcast_auto_arima <- forecast(autoarima, h = 24)
auto_arima_pred <- c(fitted(autoarima),ts(fcast_auto_arima$mean))
plot(ts(Month_wise_aggregation[,'Total_sales']), col = "black")
lines(auto_arima_pred, col = "red")


########################################################################################################################
#analysis for data ahead of 2016 and 2014 to  validate 
#performance in general
Month_wise_aggregation_2016<-Month_wise_aggregation[which(Month_wise_aggregation$sale_year>=2016),]#2014
names(Month_wise_aggregation_2016)
nrow(Month_wise_aggregation_2016)#58
str(Month_wise_aggregation_2016)
#2014
# Train<-Month_wise_aggregation_2016[1:46,]
# Test<-Month_wise_aggregation_2016[47:58,]
#2016
Train<-Month_wise_aggregation_2016[1:28,]
Validate<-Month_wise_aggregation_2016[29:34,]
#plotting time series for train
ts_train<-ts(Train$Total_sales,frequency = 12)
plot(ts_train)
axis(1,at=seq(1,80,2))

#plotting time series for validate
ts_validate<-ts(Validate$Total_sales,frequency = 12)
plot(ts_validate)

autoarima<-auto.arima(ts_train,stepwise=FALSE, approximation=FALSE)

autoarima
####################################################
#2016
# Series: ts_train 
# ARIMA(0,1,0)(0,1,0)[12] 
# 
# sigma^2 estimated as 4.861e+13:  log likelihood=-257.64
# AIC=517.29   AICc=517.59   BIC=517.99
##############################################
#2014 onwards
# Series: ts_train 
# ARIMA(1,1,0)(0,1,0)[12] 
# 
# Coefficients:
#   ar1
# -0.3839
# s.e.   0.1636
# 
# sigma^2 estimated as 5.63e+13:  log likelihood=-568.81
# AIC=1141.63   AICc=1142.03   BIC=1144.62
#######################################################

plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")
resi_auto_arima <- ts_train - fitted(autoarima)


# Augmented Dickey-Fuller Test
adf.test(resi_auto_arima,alternative = "stationary")
# Augmented Dickey-Fuller Test
# 
###################################################
#2016
#Augmented Dickey-Fuller Test
# 
# data:  resi_auto_arima
# Dickey-Fuller = -3.1179, Lag order = 3, p-value = 0.1449
# alternative hypothesis: stationary, null hypotheiss can''t be rejected
###
##2014
# data:  resi_auto_arima
# data:  resi_auto_arima
# Dickey-Fuller = -1.9996, Lag order = 3, p-value = 0.5741
# alternative hypothesis: stationary
# KPSS Test for Level Stationarity
kpss.test(resi_auto_arima)
# KPSS Test for Level Stationarity
#######################################################
#2016
# data:  resi_auto_arima
# KPSS Level = 0.16964, Truncation lag parameter = 2, p-value = 0.1
############################################################
# 2014
# data:  resi_auto_arima
# KPSS Level = 0.084672, Truncation lag parameter = 3, p-value = 0.1
######################################################################
#null hypothesis of it being stationary can be rejected
fcast_auto_arima <- forecast(autoarima, h = 6)#12 for 2014
names(fcast_auto_arima)
fcast_auto_arima$mean
#higher values in jan insted of dec , do they close the books next month
str(fcast_auto_arima)
MAPE_auto_arima <- accuracy(fcast_auto_arima$mean,Validate$Total_sales)
MAPE_auto_arima
#2016 
# ME    RMSE     MAE        MPE    MAPE
# Test set -794167.2 7481274 7112888 -0.9573609 6.45594
# #2014
# ME     RMSE      MAE       MPE     MAPE
# Test set 1009781 20620922 18694328 0.8286848 14.75728

auto_arima_pred <- c(fitted(autoarima),ts(fcast_auto_arima$mean))
plot(ts(Month_wise_aggregation_2016[,'Total_sales']), col = "black")
lines(auto_arima_pred, col = "red")
################################################################################################################

