##############################################################################
######################## Translating Statistics R Code #######################
##############################################################################

##### Note - to run code in R either copy and paste into the R Console window
##### or place cursor anywhere on the line, hold Ctrl and press r

##############################################################################
#################### Chapter 3 - Exploratory Data Analysis ###################
##############################################################################

##### Bar Chart

	Cars = rep(c("Ford", "Vauxhall", "Nissan", "Audi", "BMW"), 
		c(13,12,8,10,7))

	windows(6,6)
	barplot(table(Cars), xlab = "Car", ylab = "Count", 
		main = "Cars Bought at a Local Garage in August 2015", 
		space = NULL, ylim = c(0,14))

##### Dot Plot

	time = c(17.75666, 19.03402, 32.11146, 32.93151, 29.38551, 15.84137)
	Airport = c("Birmingham","East Midlands","Edinburgh","Gatwick",
		"Heathrow","Manchester")
	dot = data.frame(time, Airport); dot

	windows(6,6)
	dotchart(dot$time, groups = dot$Airport, 
		xlab = "Baggage Waiting Time (mins)", xlim = c(0,40),
		main = "Average Baggage Waiting Times at UK Airports")

##### Parallel Lines Plot

	library(PairedData)

	Person = c("A","B","C","D","E","F","G","H","I","J")
	Before = c(15.02, 18.54, 17.66, 16.75, 13.6, 18.36, 14.34, 18.94, 
		16.71, 14.65)
	After = c(10.83, 16.47, 12.89, 12.46, 13.5, 15.95, 15.56, 16.32, 
		13.84, 12.12)
	plp = data.frame(Person, Before, After); plp

	windows(7,4)
	paired.plotMcNeil(plp, "Before", "After", subjects = "Person") + 
		theme_bw() + scale_x_continuous(limits = c(10,20), 
			breaks = seq(10,20, by = 1)) + 
		ggtitle("Task Completion Time Before and After Training") + 
		xlab("Task Completion Time (mins)") + ylab("Subject")

##### Histogram

	Heights = c(65.78,71.52,69.40,68.22,67.79,68.70,69.80,70.01,67.90,
		66.78,66.49,67.62,68.30,67.12,68.28,71.09,66.46,68.65,71.23,
		67.13,67.83,68.88,65.48,68.42,67.63,67.21,70.84,67.49,66.53,
		65.44,69.52,65.81,67.82,70.60,71.80,69.21,66.80,67.66,67.81,
		66.05,68.57,65.18,69.66,67.97,65.98,68.67,66.88,67.70,69.82,
		69.09,69.91,67.33,70.27,69.10,65.38,70.18,70.41,66.54,66.36,
		67.54,66.50,69.00,68.30,67.01,70.81,68.22,69.06,67.73,67.22,
		67.37,65.27,70.84,69.92,64.29,68.25,66.36,68.36,65.48,69.72,
		67.73,68.64,66.78,70.05,66.28,69.20,69.13,67.36,70.09,70.18,
		68.23,68.13,70.24,71.49,69.20,70.06,70.56,66.29,68.43,66.77,
		68.89) 
	Height = Heights*2.54

	windows(6,6)
	hist(Height, xlim = c(160,185), ylim = c(0,20), xlab = "Height (cm)")

##### Scatter Plot

	Conc = c(4,3.5,3.5,4,5.5,5,5.5,3.5,5,4,6,5,4.5)
	Yield = c(78,30.3,26.4,96,265.4,216,291.2,72,233.6,124,354,187,150)

	windows(6,6)
	plot(Conc, Yield, xlab = "Log Concentration", ylim = c(0,350), 
		main = "Yield by Log Concentration")
	abline(lm(Yield ~ Conc), col = "green")

##### Line Graph

	Responses = c(77,62,68,70,69,73)
	Year = c("2010","2011","2012","2013","2014","2015")

	windows(6,6)
	matplot(Year, Responses, type = "b", lty = 1, ylab = "Year", 
		xlab = "Response Rate", main = "Response Rate by Year", 
		pch = 1, ylim = c(60,80))

##### Box Plot - won't exactly match plot in book

	Location = rep(c("Florida", "Barcelona", "London", "Paris", 
		"Milan", "Las Vegas"), each = 20)
	Temp = c(sample(21:28,20, replace = TRUE), sample(18:24,20, 
		replace = TRUE), sample(13:18,20, replace = TRUE), 
		sample(14:20,20, replace = TRUE), sample(17:25,20, 
		replace = TRUE), sample(21:34,20, replace = TRUE))

	windows(6.5,6.5)
	boxplot(Temp ~ Location, xlab = "Holiday Destination", 
		ylab = "Temperature (°C)", ylim = c(10,35),
		main = "Summer Temperatures by Holiday Destination")

##### Likert

	library(HH)
	Question = c("Ease of Use", "Comfort", "Reliability")
	Likert = c("Strongly Disagree", "Disagree", 
		"Neither Disagree nor Agree","Agree","Strongly Agree")
	R = array(c(1,5,0,  0,3,1,  1,0,2,  4,5,8,  9,2,4), dim = c(3,5), 
		dimnames = list(Statement = Question, "Likert Scale" = Likert))
	R

	windows(7,4)
	plot.likert(R, as.percent = TRUE, 
		main = "Responses to Statements for the Test Equipment", 
		col = brewer.pal.likert(5, "RdYlGn"), 
		scales = list(x = list(limits = c(-102,102), 
		at = seq(-100,100,10))))	

##### Trellis Plot

	library(ggplot2)	
	Accuracy = c(37.8,35.2,39.1,38.4,39.6,40.9,  
		33.4,32.1,34.8,35.7,34.6,35.7, 33.1,32.5,32.4,33.6,33.9,34.6,  
		31.7,31.9,30.8,32.1,32,31.4,	30.9,31,30.7,31.1,31.8,32.6, 
		28,28.6,27.7,28.3,29,28.4, 45.3,42.8,47.5,49.7,46.3,49.8, 
		39.8,39.2,39.7,40.2,40.1,41,  37.1,35.3,39.2,38.4,39.6,40,  
		36.2,35.6,35.7,36,36.3,37.1,  33.6,33.8,33.9,33.7,34.9,35.8,  
		31.3,31.6,31.8,31,32,31.4,  49.8,47.1,50.8,53.4,51.7,54.2,  
		45.6,44.3,47.9,43.7,49.8,53.9,  42.1,42.9,41.5,41.9,42.9,43.6,  
		39.1,38.5,38.4,38.7,39.5,40.5, 38.9,38.2,38.7,39,37.8,37.1,  
		35.3,35.4,35.2,35.2,35.7,36)
	Target = rep(c("Small","Medium","Large"), each = 36)
	Ammo = rep(c("A","B"), 54)
	Operator = rep(c("Op1","Op2","Op3"), each = 2, 18)
	Distance = rep(c(1:6), each = 6, 3)
	tp = data.frame(Target, Ammo, Operator, Distance, Accuracy)

	windows(6,6)
	ggplot(tp, aes(x = Distance, y = Accuracy, colour = Target)) + 
		theme_bw() + facet_wrap(~ Ammo + Operator) + geom_point() + 
		xlab("Distance (m)") + ggtitle("Accuracy by Distance, Ammo Type, 
		Operator and Target Size") + 
		scale_x_continuous(limits = c(1,6), breaks = seq(1,6, by = 1)) + 
		scale_y_continuous(limits = c(25,55), breaks = seq(25,55, by = 5))

##### Outliers

	Age = c(18,20,24,28,31,32,37,40,45,58,60,220)
	RaceTime = c(90,86,65,71,64,69,106,97,113,129,141,79)

	windows(6,6)
	plot(Age, RaceTime, xlab = "Age", ylim = c(60,150), 
		main = "Time to Complete a Race by Age", 
		ylab = "Race Time (mins)")

	Age = c(18,20,24,28,31,32,37,40,45,58,60)
	RaceTime = c(90,86,65,71,64,69,106,97,113,129,141)

	windows(6,6)
	plot(Age, RaceTime, xlab = "Age", ylim = c(60,150), 
		main = "Time to Complete a Race by Age", 
		ylab = "Race Time (mins)")

### Won't exactly match plot in book

	BookTime = c(rnorm(20,9.75), rnorm(5,5), rnorm(5,14.25), 
		rnorm(20,7.5), rnorm(5,13.2), rnorm(5,6), rnorm(20,6), 
		rnorm(5,8), rnorm(5,5), rnorm(20,5.25), rnorm(5,9), 
		rnorm(5,4.75))
	YearGroup = rep(c("11-12","13-14","15-16","17-18"), each = 30)

	windows(6,6)
	boxplot(BookTime ~ YearGroup, xlab = "Year Group", 
		ylab = "Reading Time (mins)", 
		main = "Time to Read a 300 Page Book by Year Group", 
		ylim = c(0,15))

##### Distribution

	x = seq(-4,4, length = 50)
	nx = dnorm(x)
	windows(6,6)
	plot(x, nx, type = "b", lty = 1, xlab = "x value", ylab = "Density", 
		main = "Standard Normal Distribution")

### Won't exactly match plot in book

	Ct.Value = c(rnorm(20,27.37))
	windows(6,6)
	qqnorm(Ct.Value); qqline(Ct.Value)
	shapiro.test(Ct.Value)

### Won't exactly match plot in book

	Value = c(rnorm(992,50),47,51,48,52,46,52,46,51)
	windows(6,6)
	qqnorm(Value); qqline(Value)
	shapiro.test(Value)

	x = seq(0,5,length = 25)
	ex = dexp(x, rate = 1)
	plot(x, ex, type = "b", lty = 1, xlab = "x value", ylab = "Density",
		main = "Exponential Distribution")

	x = seq(0,6,length = 25)
	gx = dgamma(x, shape = 2, rate = 1, log = FALSE)
	plot(x, gx, type = "b", lty = 1, xlab = "x value", ylab = "Density", 
		main = "Gamma Distribution")

	x = seq(0,20,by = 1)
	bx = dbinom(x, 50, prob = 0.1)
	plot(x, bx, type = "b", lty = 1, xlab = "x value", ylab = "Density", 
		main = "Binomial Distribution")

	x = seq(0,15,by = 1)
	px = dpois(x, lambda = 3)
	plot(x, px, type = "b", lty = 1, xlab = "x value", ylab = "Density", 
		main = "Poisson Distribution")

