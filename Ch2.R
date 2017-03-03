##############################################################################
######################## Translating Statistics R Code #######################
##############################################################################

##### Note - to run code in R either copy and paste into the R Console window
##### or place cursor anywhere on the line, hold Ctrl and press r

##############################################################################
######################### Chapter 2 - Data Collection ######################## 
##############################################################################

##### Possible formatting errors

	a = read.csv("MyData.csv")

	Gender = c("M","F"); b = data.frame(Gender)
	class(b$Gender)

	Day = c(1:7); c = data.frame(Day)
	class(c$Day)
	c$Day = factor(c$Day); class(c$Day)

	Smoker = rep(c("N","y","Y"), c(7,1,4)); d = data.frame(Smoker)
	levels(d$Smoker)

	Likert = rep(c("1","2","3","4","5","12"), c(3,2,1,4,5,1))
	e = data.frame(Likert)
	levels(e$Likert)

	Age = c(0,34,36,41,37,73,41,42,39,42,59,75,65,46,68,38,85,42,81,85,220)
	f = data.frame(Age)
	min(f$Age); max(f$Age)

	mean(f$Age); median(f$Age)
	summary(f$Age) 

	e$Response = e$Likert
	length(e$Response)

	ParticipantGroup = rep(c("A","B","C","D",""), c(4,4,4,3,1))
	g = data.frame(ParticipantGroup)
	xtabs( ~ g$ParticipantGroup)

