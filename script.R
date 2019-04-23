###################
###Notes on data###
###################


#Response variable: 
#visits = number of visits to nature reserve (NR)

#Explanatory variables:
#age, gender, ethnicity, nationality = profile of surveyee
#timeInSG = time surveyee lived in SG
#education = education level
#major = area of study in education
#visitedNR = visited NR before?
#nearestNR = nearest NR to the residence of surveyee 
#NumNRvisited = number of NR the surveyee has visited
#dist = distance of NR to residence of surveyee (km), calculated via postal code
#timeSpent = average hours spent in NR per visit
#timeOfDay = time of day (morning, afternoon, night) of visits
#rationale = reason to visit NR
#modeTransport = mode of transport employed to travel to NR
#company = visit NRs alone or with friends/ family/ organizations
#obstacles = reasons discouraging visit (logistics = no water/ food available, 
#interest = no interest, Danger = fear of danger, Heat = hot and humid)


##################
###Sorting data###
##################


setwd("C:/Users/Wei Yuan/Desktop/play")
data <- read.csv("natureReserve.csv", header = T)

data$todAfternoon <- as.factor(data$todAfternoon) 
data$todNight <- as.factor(data$todNight)
data$todMorning <- as.factor(data$todMorning)
data$ratLeisure <- as.factor(data$ratLeisure)
data$ratEducation <- as.factor(data$ratEducation)
data$ratLeisure <- as.factor(data$ratLeisure)
data$comFamily  <- as.factor(data$comFamily)
data$comAlone <- as.factor(data$comAlone)
data$comOrganization <- as.factor(data$comOrganization)
data$comFriends <- as.factor(data$comFriends)
data$obLogistic <- as.factor(data$obLogistic)
data$obDanger <- as.factor(data$obDanger)
data$obHeat <- as.factor(data$obHeat)
data$obInterest <- as.factor(data$obInterest)

str(data)


#####################
###Choice of Model###
#####################


#Since our response variable is a count data, we could either use GLM 
#with poisson errors or negative binomial errors. To decide, we do:

var(data$visits)/mean(data$visits)

#Thus, our solution was to:
#(1) Propose models with each combination of 3 variables using GLM with Poisson errors
#(2) Check each model for overdispersion; if overdispersed, the model is changed from
#    GLM with Poisson errors to GLM with negative binomial errors.
#(3) Check each model for collinearity issues. If there are problems, change formula from 
#    A*B*C to A+B+C
#(4) Run model selection on all proposed models using model.sel of MuMIn package.


######################################################
#############List of Models proposed##################
###(Prior to collinearity and overdispersion check)###
######################################################


library(MASS)

#Null model
Model0 <- glm.nb(visits ~ 1, data = data) 

#Individual Variables: 
Model1 <- glm.nb(visits ~ age, data = data)
Model2 <- glm.nb(visits ~ gender, data = data)
Model3 <- glm.nb(visits ~ ethnicity, data = data)
Model4 <- glm.nb(visits ~ education, data = data)
Model5 <- glm.nb(visits ~ major, data = data)
Model6 <- glm.nb(visits ~ dist, data = data)
Model7 <- glm.nb(visits ~ timeSpent, data = data)
Model8 <- glm.nb(visits ~ todAfternoon, data = data)
Model9 <- glm.nb(visits ~ todNight, data = data)
Model10 <- glm.nb(visits ~ todMorning, data = data)
Model11 <- glm.nb(visits ~ ratLeisure, data = data)
Model12 <- glm.nb(visits ~ ratEducation, data = data)
Model13 <- glm.nb(visits ~ modeTransport, data = data)
Model14 <- glm.nb(visits ~ comFamily, data = data)
Model15 <- glm.nb(visits ~ comAlone, data = data)
Model16 <- glm.nb(visits ~ comOrganization, data = data)
Model17 <- glm.nb(visits ~ comFriends, data = data)
Model18 <- glm.nb(visits ~ obLogistic, data = data)
Model19 <- glm.nb(visits ~ obDanger, data = data)
Model20 <- glm.nb(visits ~ obHeat, data = data)
Model21 <- glm.nb(visits ~ obInterest, data = data)
Model22 <- glm.nb(visits ~ todMorning*comAlone*obDanger, data=data)

#Variables used: todMorning*comAlone*obDanger
Model23<-glm.nb(visits~todMorning*comAlone, data= data)
Model24<-glm.nb(visits~todMorning*obDanger, data=data)
Model25<-glm.nb(visits~comAlone*obDanger, data=data)

#Variables used: todMorning*comFriends*obDanger
Model26<-glm.nb(visits~ todMorning*comFriends*obDanger, data=data)
Model27<-glm.nb(visits~todMorning*comFriends, data=data)
Model28<-glm.nb(visits~todMorning*obDanger, data=data)
Model29<-glm.nb(visits~comFriends*obDanger, data=data)

#Variables used: todMorning*comFamily*obDanger
Model30<-glm.nb(visits ~todMorning* comFamily*obDanger, data=data)
Model31<-glm.nb(visits~todMorning+comFamily,data=data)
#Model32<-glm(visits~todMorning*obDanger,family=poisson,data=data)
Model33<-glm.nb(visits~comFamily*obDanger,data=data)

#Variables used: todMorning*comOrganization*obDanger
Model34<-glm.nb(visits~todMorning+comOrganization+obDanger, data=data)
Model35<-glm.nb(visits~todMorning+comOrganization, data= data)
Model36<-glm.nb(visits~todMorning+obDanger, data=data)
Model37 <-glm.nb(visits~comOrganization*obDanger, data=data)

#Variables used: age*gender*ethnicity
#Reason: age, gender and ethnicity related differences in preferences. 
Model38 <- glm.nb(visits ~ age+gender, data = data)
Model39<- glm.nb(visits ~ age+ethnicity, data = data)
Model40 <- glm.nb(visits ~ gender+ethnicity,  data = data)
Model41 <- glm.nb(visits ~ age+gender+ethnicity,  data = data)

#Variables used: dist*modeTransport*comFriends
#Reason: Visits to NR is likely linked to the distance (discourages visit if NR is too far away) and #linked with mode of transport (longer distances does not pose a problem for those with #personal vehicles). Company with friends prove to be an important variable to encourage #visits.
Model42 <- glm.nb(visits ~ dist+modeTransport, data = data)
Model43 <- glm.nb(visits ~ dist+comFriends, data = data)
Model44 <- glm.nb(visits ~ comFriends+modeTransport, data = data)
Model45 <- glm.nb(visits ~ dist+modeTransport+comFriends, data = data)

#Variables used: dist*modeTransport*comAlone
#Reason: Visits to NR is likely linked to the distance (discourages visit if NR is too far away) and #linked with mode of transport (longer distances does not pose a problem for those with #personal vehicles). Visiting NR alone may prove to be an important variable to encourage visits #for some 'alone time'.
Model46 <- glm.nb(visits ~ dist+modeTransport, data = data)
Model47 <- glm.nb(visits ~ dist+comAlone, data = data)
Model48 <- glm.nb(visits ~ comAlone+modeTransport,  data = data)
Model49 <- glm.nb(visits ~ dist+modeTransport+comFriends, data = data)

#Variables used: dist*modeTransport*comOrganization
#Reason: Visits to NR is likely linked to the distance (discourages visit if NR is too far away) and linked with mode of transport (longer distances does not pose a problem for those with personal vehicles). Going to NR when there are tours or other form of events may prove to be an important variable to encourage visits.
#Reason: age, gender and ethnicity related differences in preferences. 
Model50 <- glm.nb(visits ~ dist+modeTransport, data = data)
Model51 <- glm.nb(visits ~ dist+comOrganization, data = data)
Model52 <- glm.nb(visits ~ comOrganization+modeTransport, data = data)
Model53 <- glm.nb(visits ~ dist+modeTransport+comFriends,  data = data)

#Variables used: age*gender*education
#Reason: education, age and gender can indicate different preferences
Model54 <- glm.nb(visits ~ gender+education,  data=data)
Model55 <- glm.nb(visits ~ age+gender, data=data)
Model56 <- glm.nb(visits ~ age+education, data=data)
Model57 <- glm.nb(visits ~ gender+education, data=data)

#Variables used: major*dist*todMorning
#Reason: major and distance to nearest NR may give different reasons for time of visit
Model58<- glm.nb(visits ~ major+dist, data=data)
Model59 <- glm.nb(visits ~ dist+todAfternoon, data=data)
Model60<- glm.nb(visits ~ dist+todMorning, data=data)
Model61<-glm.nb(visits~dist+todNight,data=data)
Model62<-glm.nb(visits~major+todAfternoon,data=data)
Model63<-glm.nb(visits ~ major+todMorning,data=data)
Model64<-glm.nb(visits~major+todNight,data=data)
Model65<-glm.nb(visits~major+dist+todAfternoon,data=data)
Model66<-glm.nb(visits~major+dist+todMorning,data=data)
Model67<-glm.nb(visits ~ major+dist+todNight, data=data)

#Variables used: modeTransport*obLogistic*obHeat*obDanger*obInterest
#Reason: 
Model68<- glm.nb(visits~modeTransport+obLogistic+obHeat,data=data)
Model69<- glm.nb(visits~modeTransport+obLogistic+obDanger,data=data)
Model70<- glm.nb(visits~modeTransport+obLogistic+obInterest,data=data)
Model71<- glm.nb(visits~modeTransport+obHeat+obDanger, data=data)
Model72<- glm.nb(visits~modeTransport+obHeat+obInterest,data=data)
Model73<- glm.nb(visits~modeTransport+obDanger+obInterest,data=data) 

#Variables used: ratLeisure*todAfternoon*comFriends
#Reason: Going to NR with company for leisure in the afternoon
Model74 <- glm.nb(visits ~ ratLeisure+todAfternoon+comFriends, data = data)
Model75 <- glm.nb(visits ~ ratLeisure+todAfternoon, data = data)
Model76 <- glm.nb(visits ~ todAfternoon+comFriends, data = data)
Model77 <- glm.nb(visits ~ ratLeisure+comFriends,data = data)

#Variables used: ratEducation*todMorning*comOrganization
#Reason: Going to NR with company for education in the morning
Model78 <- glm.nb(visits ~ ratEducation+comOrganization+todMorning, data = data)
Model79 <- glm.nb(visits ~ ratEducation+comOrganization,data = data)
Model80 <- glm.nb(visits ~ comOrganization+todMorning, data = data)
Model81<- glm.nb(visits ~ ratEducation+todMorning, data = data)

#Variables used: modeTransport*obLogistic*comFamily
Model82<- glm.nb(visits~modeTransport+obLogistic+comFamily, data=data)

#Variables used: modeTransport*obLogistic*comOrganization
Model83<- glm.nb(visits~modeTransport+obLogistic+comOrganization, data=data)

#Variables used: modeTransport*obLogistic*comFriends

Model84<- glm.nb(visits~modeTransport+obLogistic+comFriends, data=data)

#Variables used: obLogistic*comFamily*comOrganization
Model85<- glm.nb(visits~obLogistic+comFamily, data=data)
Model86<- glm.nb(visits~obLogistic+comOrganization, data=data)

#Variables used: obLogistic*comFriends*
Model87<- glm.nb(visits~obLogistic+comFriends, data=data) 

#Variables used: modeTransport*comFamily*Organization
Model88<- glm.nb(visits~modeTransport+comFamily, data=data)
Model89<- glm.nb(visits~modeTransport+comOrganization, data=data)

#Variables used: modeTransport*obDanger*comFriends
Model90<- glm.nb(visits~modeTransport+comFamily+comOrganization, data=data)
Model91<-glm.nb(visits~modeTransport+comFriends, data=data)
Model92<-glm.nb(visits~modeTransport+obDanger+comFriends, data=data)
Model93<- glm.nb(visits~modeTransport+obDanger+comOrganization,data=data)

#Variables used: ratLeisure*todAfternoon*comFriends
#Reason: Going to NR with company for leisure in the afternoon
Model94 <- glm.nb(visits ~ ratLeisure+todAfternoon+comFriends, data = data)
Model95 <- glm.nb(visits ~ ratLeisure+todAfternoon, data = data)
Model96 <- glm.nb(visits ~ todAfternoon+comFriends,data = data)
Model97 <- glm.nb(visits ~ ratLeisure+comFriends,data = data)

#Variables used: todAfternoon*comAlone*obDanger
Model98<-glm.nb(visits~todAfternoon+comAlone+obDanger,data=data)
Model99<-glm.nb(visits~todAfternoon*comAlone,data=data)
Model100<-glm.nb(visits~todAfternoon*obDanger,data=data)
Model101<-glm.nb(visits~comAlone*obDanger, data=data)

#Variables used: todAfternoon*comFamily*obDanger
Model102<-glm.nb(visits~todAfternoon+comFamily+obDanger,data=data)
Model103<-glm.nb(visits~todAfternoon+comFamily,data=data)
Model104<-glm.nb(visits~todAfternoon*obDanger,data=data)
Model105<-glm.nb(visits~comFamily*obDanger,data=data)

#Variables used: todAfternoon*comFriends*obDanger
Model106<-glm.nb(visits~todAfternoon+comFriends+obDanger,data=data)
Model107<-glm.nb(visits~todAfternoon+comFriends,data=data)
Model108<-glm.nb(visits~todAfternoon*obDanger,data=data)
Model109<-glm.nb(visits~comFriends+obDanger,data=data)


#Variables used: todAfternoon*comOrganization*obDanger
Model110<-glm.nb(visits~todAfternoon*comOrganization*obDanger,data=data)
Model111<-glm.nb(visits~todAfternoon*comOrganization,data=data)
Model112<-glm.nb(visits~todAfternoon*obDanger,data=data)
Model113<-glm.nb(visits~comOrganization*obDanger,data=data)

#Variables used: obHeat*tobMorning
#Reason: Heat varies throughout the day
#Model114<-glm.nb(visits ~ todMorning,data=data)

#Variables used: dist*timeSpent*comAlone
Model115<-glm.nb(visits~dist+timeSpent+comAlone,data=data)
#Model116<-glm.nb(visits~dist*comAlone,data=data)
Model117<-glm.nb(visits~dist+timeSpent,data=data)
Model118<-glm.nb(visits~timeSpent+comAlone,data=data)

#Variables used: obHeat*tobAfternoon
#Reason: Heat varies throughout the day
Model119<-glm.nb(visits ~ obHeat*todAfternoon, data=data)

#Variables used: obHeat*tobNight
#Reason: Heat varies throughout the day
Model120<-glm.nb(visits ~ obHeat+todNight, data=data)

#Variables used: obDanger*obHeat*obInterest
#Reason: Possible obstacles to NR visits may interact.
Model121<- glm.nb(visits ~ obDanger*obHeat, data = data)
Model122<- glm.nb(visits ~ obHeat*obInterest, data = data)
Model123<- glm.nb(visits ~ obDanger*obInterest, data = data)
Model124<- glm.nb(visits ~ obDanger*obHeat*obInterest, data = data)

#Variables used: dist*obInterest*comAlone
#Reason: If you alone you need to be interested to travel DE DIST
Model125 <- glm.nb(visits ~ dist+obInterest+comAlone, data = data)
Model126<- glm.nb(visits ~ dist+obInterest, data = data)
Model127 <- glm.nb(visits ~ obInterest*comAlone, data = data)
Model128 <- glm.nb(visits ~ dist+comAlone, data = data)

#Variables used: dist*ratLeisure*modeTransport
#Reason: People who live near to NR may choose leisure activities in NR. Or people who live far from NRs with transport may choose leisure activities in NR.
Model129 <- glm.nb(visits ~ obDanger*obHeat, data = data)
Model130 <- glm.nb(visits ~ obHeat*obInterest, data = data)
Model131<- glm.nb(visits ~ obDanger*obInterest, data = data)
Model132<- glm.nb(visits ~ obDanger*obHeat*obInterest, data = data)

#Variables used: gender*obDanger*comAlone
#Reason: If you boi maybe you more brave lol 
Model133<- glm.nb(visits ~gender + obDanger + comAlone, data = data)
Model134<- glm.nb(visits ~gender*obDanger, data = data)
Model135<- glm.nb(visits ~obDanger*comAlone, data = data)
Model136<- glm.nb(visits ~gender*comAlone, data = data)

#Variables used: education*ratEducation*ratLeisure
#Reason: Do people with higher education visit NRs more for education and leisure?
Model137<- glm.nb(visits ~gender+obDanger+comAlone, data = data)
Model138<- glm.nb(visits ~gender*obDanger, data = data)
Model139<- glm.nb(visits ~obDanger*comAlone, data = data)
Model140<- glm.nb(visits ~gender*comAlone, data = data)


####################################
###Selection of the best model(s)###
####################################


library(MuMIn)
model_selection <- model.sel(Model0, Model1, Model2, Model3, Model4, Model5, Model6, Model7, Model8, Model9, Model10, Model11, Model12, Model13, Model14, Model15, Model16, Model17, Model18, Model19, Model20, Model21, Model22, Model23, Model24, Model25, Model26, Model27, Model28, Model29, Model30, Model31, Model33, Model34, Model35, Model36, Model37, Model38, Model39, Model40, Model41, Model42, Model43, Model44, Model45, Model46, Model47, Model48, Model49, Model50, Model51, Model52, Model53, Model54, Model55, Model56, Model57, Model58, Model59, Model60, Model61, Model62, Model63, Model64, Model65, Model66, Model67, Model68, Model69, Model70, Model71, Model72, Model73, Model74, Model75, Model76, Model77, Model78, Model79, Model80, Model81, Model82, Model83, Model84, Model85, Model86, Model87, Model88, Model89, Model90, Model91, Model92, Model93, Model94, Model95, Model96, Model97, Model98, Model99, Model100, Model101, Model102, Model103, Model104, Model105, Model106, Model107, Model108, Model109, Model110, Model111, Model112, Model113, Model115, Model117, Model118, Model119, Model120, Model121, Model122, Model123, Model124, Model125, Model126, Model127, Model128, Model129, Model130, Model131, Model132, Model133, Model134, Model135, Model136, Model137, Model138, Model139, Model140, rank = AIC)
model_selection

#Model127 is the best model with delta = 0, weight = 0.367.
#Second best model is Model111, but delta = 3.63 and wight = 0.060; no need for model averaging

Model127 <- glm.nb(visits ~ obInterest*comAlone, data = data)


#Checking assumptions for negative binomial models
#1. Dependent variable is count data - OK
#2. One or more independent variables - OK
#3. Independence of observations - Violated due to nonrandom sampling
#4. Variance and mean are not identical; variance > mean for response variable - OK
#5. Negative binomial distribution not recomended for small samples - OK
#6. No excess of zeroes in response variable, so no need for zero-inflated model - OK
#   Note: no assumption of homoscedasticity
#Source: https://statistics.laerd.com/spss-tutorials/poisson-regression-using-spss-statistics.php
#        https://stats.idre.ucla.edu/r/dae/negative-binomial-regression/


####################################
####Model Proposal using Dredge#####
####################################


library(car)

modelsD1 <- glm.nb(visits ~ age + gender + ethnicity + education + major +
                 nearestNR + NumNRvisited + dist + timeSpent +
                 todAfternoon + todNight + todMorning + ratLeisure +
                 ratEducation + modeTransport + comFamily + comAlone +
                 comOrganization + comFriends + obLogistic + obDanger +
                 obHeat + obInterest, data = data,
               na.action = "na.exclude")
vif(modelsD1)

#GVIF^(1/(2*Df) of company is 5.783675; remove age
modelsD2 <- glm.nb(visits ~ gender + ethnicity + education + major +
                  nearestNR + NumNRvisited + dist + timeSpent +
                  todAfternoon + todNight + todMorning + ratLeisure +
                  ratEducation + modeTransport + comFamily + comAlone +
                  comOrganization + comFriends + obLogistic + obDanger +
                  obHeat + obInterest, data = data,
                na.action = "na.exclude")
vif(modelsD2)

#GVIF^(1/(2*Df)) of comFriends is 4.095760; remove comFriends
modelsD3 <- glm.nb(visits ~ gender + ethnicity + education + major +
                  nearestNR + NumNRvisited + dist + timeSpent +
                  todAfternoon + todNight + todMorning + ratLeisure +
                  ratEducation + modeTransport + comFamily + comAlone +
                  comOrganization + obLogistic + obDanger +
                  obHeat + obInterest, data = data,
                na.action = "na.exclude")
vif(modelsD3)

#GVIF^(1/(2*Df)) of timeSpent is 3.448125; remove NumNRvisited
modelsD4 <- glm.nb(visits ~ gender + ethnicity + education + major +
                  nearestNR +  dist + timeSpent +
                  todAfternoon + todNight + todMorning + ratLeisure +
                  ratEducation + modeTransport + comFamily + comAlone +
                  comOrganization + obLogistic + obDanger +
                  obHeat + obInterest, data = data,
                na.action = "na.exclude")
vif(modelsD4)

#GVIF^(1/(2*Df)) of nearestNR is 3.092380; remove nearestNR
modelsD5 <- glm.nb(visits ~ gender + ethnicity + education + major +
                    dist + timeSpent +
                  todAfternoon + todNight + todMorning + ratLeisure +
                  ratEducation + modeTransport + comFamily + comAlone +
                  comOrganization + obLogistic + obDanger +
                  obHeat + obInterest, data = data,
                na.action = "na.fail")
vif(modelsD5)

#Model is now good to go for Dredge

automated_model_selection_nb = dredge(modelsD5, trace = TRUE
                                      ,m.lim = c(1,3) #lower and upper limit of number of terms in model allowed
                                      ,rank = "AIC")

#dredge proposed 458752 models and ranked them by descending values of AIC

automated_model_selection_nb[1:10,] #views top ten models

Results:
#        (Intrc)  cmAln  cmOrg gendr mdTrn   obDng   obHet   obInt tdMrn
#132101 -0.12240        0.7481                     -0.9442         1.132
#133122 -0.09602 1.0400                                    -0.7235 1.196
#132098 -0.05551 0.8203                            -0.7732         1.149
#1541    1.02800        0.7729             -0.7639 -0.8367              

#  df   logLik   AIC delta weight
#132101  5 -117.184 244.4  0.00  0.201
#133122  5 -117.499 245.0  0.63  0.147
#132098  5 -117.722 245.4  1.08  0.118
#1541    5 -117.793 245.6  1.22  0.110

modelBest1 <- glm.nb(visits ~ comOrganization+obHeat+todMorning, data = data)
modelBest2 <- glm.nb(visits ~ comAlone+obInterest+todMorning, data = data)
modelBest3 <- glm.nb(visits ~ comAlone+obHeat+todMorning, data = data)
modelBest4 <- glm.nb(visits ~ comOrganization+obDanger+obHeat, data = data)

#Checking for collinearity problems with the dredge models
vif(modelBest1)
vif(modelBest2)
vif(modelBest3)
vif(modelBest4)

#Since delta of top four models by dredge is less than 2, we can do
#model averaging

model_average <- model.avg(automated_model_selection_nb,subset=delta <2)
summary(model_average)


####################################
######Plots for Best Models#########
####################################

#By model proposal
data$comAlone <- factor(data$comAlone, labels = c("Do not visit alone", "Visit alone"))
data$obInterest <- factor(data$obInterest, labels = c("No", "Yes"))
Model127 <- glm.nb(visits ~ obInterest*comAlone, data = data)
str(data)
library(visreg)
visreg(Model127, "obInterest", "comAlone", xlab = "Obstacles involving interest", ylab = "Number of visits", legend = TRUE, plot.type = "gg")

#By dredge and model averaging
modelBest1 <- glm.nb(visits ~ comOrganization+obHeat+todMorning, data = data)
modelBest2 <- glm.nb(visits ~ comAlone+obInterest+todMorning, data = data)
modelBest3 <- glm.nb(visits ~ comAlone+obHeat+todMorning, data = data)
modelBest4 <- glm.nb(visits ~ comOrganization+obDanger+obHeat, data = data)
model_average <- model.avg(automated_model_selection_nb,subset=delta <2)
summary(model_average)





