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
#####Workflow#####
##################
install.packages("DiagrammeR")
library(DiagrammeR)
grViz("digraph flowchart {
      # node definitions with substituted label text
      node [fontname = Helvetica, shape = rectangle]        
      tab1 [label = '@@1']
      tab2 [label = '@@2']
      tab3 [label = '@@3']
      tab4 [label = '@@4']
      tab5 [label = '@@5']
      tab6 [label = '@@6']
      tab7 [label = '@@7']
      
      # edge definitions with the node IDs
      tab1 -> tab2 -> tab3
      tab2 -> tab4
      tab3 -> tab5
      tab4 -> tab5
      tab5 -> tab6 -> tab7;
      }
      
      [1]: 'Questionnaire sent to n=58 respondents'
      [2]: 'Chose GLM with negative binomial errors as var/mean > 3'
      [3]: 'Proposed 140 meaningful models'
      [4]: 'Dredge with different combinations of three explanatory variables'
      [5]: 'Checking of assumptions and multicollinearity'
      [6]: 'Model selection using AIC'
      [7]: 'Interpretation and analysis'
      ")


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
pairs(data)

str(data$comFriends)
head(data$comFriends)


###############################
###Exploratory Data Analysis###
###############################

Tab<-data.frame(values=colSums(data1, na.rm=TRUE), names = names(data1))
plot1<-ggplot(data=Tab, aes(x=names, y=values, fill=names)) +
  geom_bar(stat="identity") + labs(x="Type of Company",y="Frequency indicated")+    
  ggtitle("Frequency of the types of company during NR visits") +
  theme(plot.title = element_text(size=38, face="bold", hjust = 0.5, vjust = 0.5),
        legend.title = element_text(size=30, face="bold"), 
        legend.text = element_text(size = 28),
        axis.text.x = element_text(size = 28),
        axis.text.y = element_text(size = 28),
        axis.title.x = element_text(size=30),
        axis.title.y = element_text(size=30),
        panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  theme_classic()+
  scale_fill_manual(breaks=c ("comAlone", "comFamily", "comFriends", "comOrganization"), values = c("#999999", "#E69F00","#D55E00", "#F0E442"))

par(mfrow = c(2,2))
plot(data$visits ~ data$comAlone)
plot(data$visits ~ data$comFriends)
plot(data$visits ~ data$comFamily)
plot(data$visits ~ data$comOrganization)

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
#Reason: Visits done alone in the morning due to the fear of danger at night.
Model23<-glm.nb(visits~todMorning*comAlone, data= data)
Model24<-glm.nb(visits~todMorning*obDanger, data=data)
Model25<-glm.nb(visits~comAlone*obDanger, data=data)

#Variables used: todMorning*comFriends*obDanger
#Reason: Due to fear of danger, visit in the morning and with friends (safety in groups).
Model26<-glm.nb(visits~ todMorning*comFriends*obDanger, data=data)
Model27<-glm.nb(visits~todMorning*comFriends, data=data)
Model28<-glm.nb(visits~todMorning*obDanger, data=data)
Model29<-glm.nb(visits~comFriends*obDanger, data=data)

#Variables used: todMorning*comFamily*obDanger
#Reason: Due to fear of danger, visit in the morning and with family (safety in groups).
Model30<-glm.nb(visits ~todMorning* comFamily*obDanger, data=data)
Model31<-glm.nb(visits~todMorning+comFamily,data=data)
#Model32<-glm(visits~todMorning*obDanger,family=poisson,data=data)
Model33<-glm.nb(visits~comFamily*obDanger,data=data)

#Variables used: todMorning*comOrganization*obDanger
#Reason: Due to fear of danger, visit in the morning and with eg. tour groups (safety in groups).
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
#Reason: All possible obstacles and its interaction with mode of transport. Strong 
#        reasons that hinder visits to NR
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
#Reason:modeTransport to the reserve is usually related to the company you go with to the reserve
#       and company usually affects logistic issues as more things have to be taken into account
#       such as group safety, facilities, accesibility etc. the following models are just various combinations
#       of comapny with logistic issues and modeTransport
Model82<- glm.nb(visits~modeTransport+obLogistic+comFamily, data=data)

#Variables used: modeTransport*obLogistic*comOrganization
#Reason: simplication of previous 3 models, logistic issues and comapny might play a bigger part thatn modeTransport
Model83<- glm.nb(visits~modeTransport+obLogistic+comOrganization, data=data)

#Variables used: modeTransport*obLogistic*comFriends
Model84<- glm.nb(visits~modeTransport+obLogistic+comFriends, data=data)

#Variables used: obLogistic*comFamily*comOrganization
#Reason: simplication of model 82-84, modeTransport and company might play a bigger part thatn logistic issues
Model85<- glm.nb(visits~obLogistic+comFamily, data=data)
Model86<- glm.nb(visits~obLogistic+comOrganization, data=data)

#Variables used: obLogistic*comFriends*
Model87<- glm.nb(visits~obLogistic+comFriends, data=data) 

#Variables used: modeTransport*comFamily*Organization
#Reason: simplication of model 82-84, modeTransport and comapny might play a bigger part thatn logistic issues
Model88<- glm.nb(visits~modeTransport+comFamily, data=data)
Model89<- glm.nb(visits~modeTransport+comOrganization, data=data)

#Variables used: modeTransport*obDanger*comFriends
#Reason: due to fear of danger, go to NR with friends with different mode of transport?
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
Model103<-glm.nb(visits~todAfternoon*comFamily,data=data)
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
#Reason: Short distance, more visits there alone, and more time spent
#        without peer pressure to leave.
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
Model133<- glm.nb(visits ~gender * obDanger * comAlone, data = data)
Model134<- glm.nb(visits ~gender*obDanger, data = data)
Model135<- glm.nb(visits ~obDanger*comAlone, data = data)
Model136<- glm.nb(visits ~gender*comAlone, data = data)

#Variables used: education*ratEducation*ratLeisure
#Reason: Do people with higher education visit NRs more for education and leisure?
Model137<- glm.nb(visits ~gender*obDanger*comAlone, data = data)
Model138<- glm.nb(visits ~gender*obDanger, data = data)
Model139<- glm.nb(visits ~obDanger*comAlone, data = data)
Model140<- glm.nb(visits ~gender*comAlone, data = data)



####################################
###Selection of the best model(s)###
####################################


library(MuMIn)
model_selection <- model.sel(Model0, Model1, Model2, Model3, Model4, Model5, Model6, Model7, Model8, Model9, Model10, Model11, Model12, Model13, Model14, Model15, Model16, Model17, Model18, Model19, Model20, Model21, Model22, Model23, Model24, Model25, Model26, Model27, Model28, Model29, Model30, Model31, Model33, Model34, Model35, Model36, Model37, Model38, Model39, Model40, Model41, Model42, Model43, Model44, Model45, Model46, Model47, Model48, Model49, Model50, Model51, Model52, Model53, Model54, Model55, Model56, Model57, Model58, Model59, Model60, Model61, Model62, Model63, Model64, Model65, Model66, Model67, Model68, Model69, Model70, Model71, Model72, Model73, Model74, Model75, Model76, Model77, Model78, Model79, Model80, Model81, Model82, Model83, Model84, Model85, Model86, Model87, Model88, Model89, Model90, Model91, Model92, Model93, Model94, Model95, Model96, Model97, Model98, Model99, Model100, Model101, Model102, Model103, Model104, Model105, Model106, Model107, Model108, Model109, Model110, Model111, Model112, Model113, Model115, Model117, Model118, Model119, Model120, Model121, Model122, Model123, Model124, Model125, Model126, Model127, Model128, Model129, Model130, Model131, Model132, Model133, Model134, Model135, Model136, Model137, Model138, Model139, Model140, rank = AIC)
model_selection

#cmA:gnd gnd:obD cmA:gnd:obD       family init.theta df   logLik   AIC delta weight
#Model122                             NB(1.4318,l)       1.43  5 -110.889 231.8  0.00  0.126
#Model130                             NB(1.4318,l)       1.43  5 -110.889 231.8  0.00  0.126
#Model21                              NB(1.2629,l)       1.26  3 -113.014 232.0  0.25  0.111
#Model123                             NB(1.3975,l)        1.4  5 -111.318 232.6  0.86  0.082
#Model131                             NB(1.3975,l)        1.4  5 -111.318 232.6  0.86  0.082
#Model72                               NB(1.499,l)        1.5  6 -110.444 232.9  1.11  0.072
#Model127                             NB(1.2977,l)        1.3  4 -112.582 233.2  1.39  0.063
#Model126                             NB(1.2647,l)       1.26  4 -112.873 233.7  1.97  0.047
#Model73                                NB(1.41,l)       1.41  6 -111.338 234.7  2.90  0.030


#Stepwise simplification of the models with delta < 2
Model122 <- glm.nb(visits ~ obHeat*obInterest, data = data)
summary(Model122)
Model122_update <- glm.nb(visits ~ obHeat+obInterest, data = data)
anova(Model122, Model122_update)
summary(Model122_update)

Model130 <- glm.nb(visits ~ obHeat*obInterest, data = data)
summary(Model130)
Model130_update1 <- glm.nb(visits ~ obHeat+obInterest, data = data)
anova(Model130, Model130_update1)
summary(Model130_update1)
Model130_update2 <- glm.nb(visits ~ obInterest, data = data)
anova(Model130_update1,Model130_update2)
summary(Model130_update2)

Model21 <- glm.nb(visits ~ obInterest, data = data)
summary(Model21)

Model123 <- glm.nb(visits ~ obDanger*obInterest, data = data)
summary(Model123)
Model123_update1 <- glm.nb(visits ~ obDanger+obInterest, data = data)
anova(Model123, Model123_update1)
summary(Model123_update1)
Model123_update2 <- glm.nb(visits ~ obInterest, data = data)
anova(Model123_update1, Model123_update2)
summary(Model123_update2)


Model131 <- glm.nb(visits ~ obDanger*obInterest, data = data)
summary(Model131)
Model131_update1 <- glm.nb(visits ~ obDanger+obInterest, data = data)
anova(Model131, Model131_update1)
summary(Model131_update1)
Model131_update2 <- glm.nb(visits ~ obInterest, data = data)
anova(Model131_update1, Model131_update2)
summary(Model131_update2)

Model72 <- glm.nb(visits~modeTransport+obHeat+obInterest,data=data)
summary(Model72)
Model72_update1 <- glm.nb(visits~obHeat+obInterest,data=data)
anova(Model72, Model72_update1)
summary(Model72_update1)
Model72_update1 <- glm.nb(visits~obInterest,data=data)
anova(Model72, Model72_update1)
summary(Model72_update1)

Model127 <- glm.nb(visits ~ obInterest*comAlone, data = data)
summary(Model127)
Model127_update1 <- glm.nb(visits ~ obInterest+comAlone, data = data)
anova(Model127, Model127_update1)
summary(Model127_update1)
Model127_update2 <- glm.nb(visits ~ obInterest, data = data)
anova(Model127_update1, Model127_update2)
summary(Model127_update2)

Model126 <- glm.nb(visits ~ dist+obInterest, data = data)
summary(Model126)
Model126_update1 <- glm.nb(visits ~ obInterest, data = data)
anova(Model126, Model126_update1)
summary(Model126_update1)


#Results from stepwise simplification: 
Model122_update <- glm.nb(visits ~ obHeat+obInterest, data = data)
Model130_update2 <- glm.nb(visits ~ obInterest, data = data)

AIC(Model122_update, Model130_update2)
#Model122_update is the best model by AIC.

#Since there are 8 top models with delta < 2:
#Thus, we do model averaging:
model_average <- model.avg(model_selection,subset=delta <2)
summary(model_average)

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


modelBest1 <- glm.nb(visits ~ obHeat + obInterest + todMorning, data = data)
modelBest2 <- glm.nb(visits ~ obInterest + todMorning + comOrganization, data = data)

summary(modelBest1)
summary(modelBest2)

#Since comOrganization1 was not significant, we try to remove it.
modelBest2_update <- glm.nb(visits ~ obInterest + todMorning, data = data)
anova(modelBest2, modelBest2_update)
#p-value is 0.1099503; model did not worsen after removal of comOrganization
AIC(modelBest2, modelBest2_update)
#Yet model with comOrganization1 had lower AIC
#Given the literature suggesting company as an important variable, we decide to keep it anyway despite
#statistical insignificance due to its highly rated 'biological' significance

#Checking for collinearity problems with the dredge models
vif(modelBest1)
vif(modelBest2)

#Since delta of top four models by dredge is less than 2, we can do
#model averaging

model_average <- model.avg(automated_model_selection_nb,subset=delta <2)
summary(model_average)
