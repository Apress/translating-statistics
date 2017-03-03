##############################################################################
######################## Translating Statistics R Code #######################
##############################################################################

##### Note - to run code in R either copy and paste into the R Console window
##### or place cursor anywhere on the line, hold Ctrl and press r

##############################################################################
##################### Chapter 4 - Descriptive Statistics #####################
##############################################################################

##### Skewness

	library(e1071)

	windows(6,6)
	plot(density(rbeta(10000000,5,2)), col = "red", lwd = 2, 
		xlab = "Negative Skew")

	plot(density(rbeta(10000000,2,5)), col = "red", lwd = 2,
		xlab = "Positive Skew")

	a = rbeta(10000000,5,2)
	mean(a); median(a); 
	skewness(a)

	b = rbeta(10000000,2,5)
	mean(b); median(b)
	skewness(b)

##### Kurtosis

	c = rlogis(10000000, location = 0, scale = 2)

	plot(density(rlogis(10000000, location = 0, scale = 2)), 
		col = "red", lwd = 2, xlab = "Positive Kurtosis")

	d = sample(seq(-30,30,0.001),10000000, replace = TRUE)

	plot(density(d), col = "red", lwd = 2, xlab = "Negative Kurtosis")

	kurtosis(c)
	kurtosis(d)	

##### Transformations

	e = c(9.246734, 7.399515, 10.747294, 3.569408, 4.337869, 3.172818, 
		14.205624, 30.076914, 15.747489, 6.751340, 8.868595, 9.067760,
		8.168440, 7.499503, 7.377515, 14.883616, 19.688646, 26.299868, 
		6.351835, 14.180845, 8.291489, 6.923344, 8.540164, 11.488742,
		22.694856, 16.868368, 31.439693, 10.700027, 17.887367, 
		10.008738, 10.678093, 13.064685, 24.202956, 12.361150, 
		12.772815, 13.436628, 14.336022, 4.701801, 6.078979, 16.039244, 
		13.830606, 11.857714, 11.927977, 4.661250, 28.652883, 6.391380, 
		4.378959, 8.361308, 11.056678, 7.521961)

	windows(6,6)
	hist(e, xlab = "Temperature (°C)", main = "Weekly UK Temperatures", 
		ylim = c(0,20))
	qqnorm(e); qqline(e)
	shapiro.test(e)

	e2 = log10(e)
	shapiro.test(e2)
	hist(e2, xlab = expression("Log Temperature (Log"[10]*" °C)"), 
		ylim = c(0,20), main = "Weekly UK Log Temperatures")
	qqnorm(e2); qqline(e2)

##### Location

### Mode

	Children = c(0,1,2,3,4)
	Freq = c(5,10,12,2,1)
	barplot(xtabs(Freq ~ Children), space = 0, xlab = "Number of Children", 
		ylab = "Count", main = "Number of Children per Family Sampled")

##### Example 4.1 - Mean

	data3 = c(6.61, 7.88, 7.54, 8.08, 8.07, 7.2, 6.81, 6.45, 7.34, 6.27, 
		6.19, 6.63, 19.98, 7.36, 7.18, 7.86, 7.33, 19.02, 8.03, 
		8.04, 7.16, 7.14, 7.61, 7.3, 6.75, 6.71, 20.23, 7.67, 6.89, 
		7.15, 7.52, 8.17, 7.55, 6.8, 19.72, 6.43, 8.05, 6.88, 13.08,
		10.16)

	median(data3)

	data4 = rnorm(40, mean = 7, sd = 0.75)

	median(data4)

##### Example 4.2 - Median

	mean(data3)

	mean(data4)

### Plots

	dens = density(data3, adjust = 2)

	n = length(dens$y); dx = mean(diff(dens$x))
	y.unit = sum(dens$y)*dx; dx = dx / y.unit 
	x.mean = sum(dens$y*dens$x)*dx 
	y.mean = dens$y[length(dens$x[dens$x < x.mean])] 
	y.cs = cumsum(dens$y)                    
	x.med = dens$x[i.med <- length(y.cs[2*y.cs <= y.cs[n]])] 
	y.med = dens$y[i.med]

	windows(6,6)
	plot(dens, xlim = c(0,25), xlab = "Skewed Data", 
		main = "Density Plot of Skewed Data", lwd = 2)
	temp = mapply(function(x,y,c) lines(c(x,x), c(0,y), lwd = 2, col = c), 
               c(x.mean, x.med), c(y.mean, y.med), c("Red", "Blue"))
	legend(18,0.35, c("Mean","Median"), lty = c(1,1), lwd = c(2,2), 
		col = c("red", "blue"))

	dens2 = density(data4)

	n = length(dens2$y); dx = mean(diff(dens2$x))
	y.unit = sum(dens2$y)*dx; dx = dx / y.unit 
	x.mean = sum(dens2$y*dens2$x)*dx 
	y.mean = dens2$y[length(dens2$x[dens2$x < x.mean])] 
	y.cs = cumsum(dens2$y)                    
	x.med = dens2$x[i.med <- length(y.cs[2*y.cs <= y.cs[n]])] 
	y.med = dens2$y[i.med]

	plot(dens2, xlim = c(4.5,9.5), xlab = "Normal Data", 
		main = "Density Plot of Normal Data", lwd = 2)
	temp = mapply(function(x,y,c) lines(c(x,x), c(0,y), lwd = 2, col = c), 
               c(x.mean, x.med), c(y.mean, y.med), c("Red", "Blue"))
	legend(8.5,0.52, c("Mean","Median"), lty = c(1,1), lwd = c(2,2), 
		col = c("red", "blue"))

##### Example 4.3 - Group means

	Temp = c(72, 70, 71, 70, 90, 88, 87, 83, 75, 89, 91, 79, 93, 74, 86,
		84, 86, 90, 92, 75, 74, 87, 83, 81, 90, 50, 61, 59, 51, 55, 
		58, 52, 52, 56, 55, 52, 61, 54, 56, 59, 57, 53, 72, 67, 83, 
		76, 80, 65, 85, 77, 83, 71, 84, 78, 74, 65, 72, 75, 79, 76, 
		69, 78, 71, 74, 65, 69, 66, 76, 70, 79, 66, 69)
	Groups = c(rep("A", 25), rep("B", 17), rep("C", 30))
	data5 = data.frame(Temp, Groups)

	tapply(data5$Temp, data5$Groups, mean)

	mean(data5$Temp)

##### Example 4.4 - Weighted means

	data5$Scores = data5$Temp

	tapply(data5$Scores, data5$Groups, mean)

	mean(c(82.4, 55.35294, 73.8))

	tapply(data5$Scores, data5$Groups, length)

	25/(25+17+30); 17/(25+17+30); 30/(25+17+30) 
	(0.3472222*82.4) + (0.2361111*55.35294) + (0.4166667*73.8)

	mean(data5$Scores)

##### Spread

### Standard Deviation

	f = qnorm(p = seq(0.01,0.99,0.01), mean = 20, sd = 1)
	g = qnorm(p = seq(0.01,0.99,0.01), mean = 20, sd = 2)
	h = qnorm(p = seq(0.01,0.99,0.01), mean = 20, sd = 3)
	i = qnorm(p = seq(0.01,0.99,0.01), mean = 20, sd = 5)

	windows(6,6)
	plot(density(f), ylim = c(0.0,0.4), xlim = c(0,40), lwd = 2, 
		xlab = "Time (mins)", main = "Time to Complete a Task 
		(Changing the Variation in the Data)")
	lines(density(g), col="red", lwd=2) 
	lines(density(h), col="blue", lwd=2) 
	lines(density(i), col="darkgreen", lwd=2) 
	legend(27,0.41, c("SD = 1","SD = 2","SD = 3","SD = 5"), 
		title = c("Standard Deviation"), lty = c(1,1), 
		lwd = c(2,2), col = c("black", "red", "blue", "darkgreen"))

##### Example 4.5 - Range

	range(data3)

	max(data3) - min(data3)

	range(data4) 

	max(data4) - min(data4)

##### Example 4.6 - Quantiles and Percentiles

	quantile(data3, prob = seq(0, 1, length = 6)) 
	quantile(data4, prob = seq(0, 1, length = 6))

	quantile(data3); quantile(data4)

	quantile(data3, prob = c(0.95, 0.99)) 
	quantile(data4, prob = c(0.95, 0.99)) 

### IQR and SIQR

	IQR(data3); IQR(data3)/2

	IQR(data4); IQR(data4)/2

##### Example 4.7 - MAD

	x = c(1, 4, 3, 5, 6, 2, 4, 2, 3, 4)
	mad(x, constant = 1)

	mad(data3, constant = 1)
	mad(data4, constant = 1)

##### Example 4.8 - MAD

	abs(x - median(x)) / mad(x, constant = 1)

	x2 = c(10, 4, 3, 5, 6, 2, 4, 2, 3, 4)
	abs(x2 - median(x2)) / mad(x2, constant = 1)

##### Example 4.9 - MAD and AAD

	MADs = mad(data3, constant = 1); MADs
	AADs = mean(abs(data3 - mean(data3))); AADs

	MADn = mad(data4, constant = 1); MADn
	AADn = mean(abs(data4 - mean(data4))); AADn

	abs(data3 - median(data3))/ MADs
	abs(data3 - mean(data3))/ AADs

	abs(data4 - median(data4))/ MADn
	abs(data4 - mean(data4))/ AADs

##### Example 4.10 - CV

	mean(data3); mean(data4)
	sd(data3); sd(data4)

	(sd(data3) / mean(data3))*100; (sd(data4) / mean(data4))*100

##### Discrete data

	cars = rep(c("Red", "Black", "Blue", "White", "Silver","Green"), 
		c(30,34,32,20,29,5))
	Cars = data.frame(cars)

	tab = xtabs(~ Cars$cars); tab2 = as.data.frame(tab)
	tab2$Percentage = round(tab2$Freq/150*100, 1); tab2

	library(ggplot2)
	windows(6,6)
	ggplot(Cars, aes(x = factor(cars), fill = factor(cars))) + 
		geom_bar(colour = "black", width = 1) + theme_bw() +  
		scale_fill_manual(name = "Car Color", values = c("black", 
		"dodgerblue3","forestgreen","firebrick3","grey","white")) + 
		xlab("Car Color") + ylab("Count of Cars\n") + 
		guides(fill = FALSE) + 
		ggtitle(expression(bold("Count of Cars by Color"))) + 
		scale_y_continuous(limits = c(0,35), breaks = seq(0,35, by = 5))+
		theme(axis.text.x = element_text(size = 10, colour = "grey10", 
			face = "italic")) + 
		geom_text(aes(y = ((..count..)/sum(..count..)), 
			label = scales::percent((..count..)/sum(..count..))), 
			stat = "count", vjust = -0.5, col = "lightblue2")

##### Example 4.11 - Contingency tables

	gender = rep(c("M","F"), each = 25) 
	smokes = sample(c(0,1), 50, replace = TRUE)
	data6 = data.frame(gender, smokes)

	xtabs( ~ gender + smokes, data = data6)

##### Correlation - one example

	x = c(10.0, 8.5,  16.8, 11.2, 17.8, 5.4, 21.6, 9.6,  14,  13.5, 19.7,
		20.2, 6.9, 16.7, 15.6, 18.9, 21.7, 20.6, 14.7, 12.3, 6.9, 19.4, 
		5.2)
	y = c(-12.5, -11.1, -22.3, -15.4, -25.3, -8.4, -32.6, -16.5, -15.3, 
		-16.8, -27.1, -25.1, -9.3, -19.8, -17.6, -23.1, -31.2, -29, 
		-18.8, -13, -8.1, -20.7, -6.5) 
	corrd = data.frame(x,y)

	cor(corrd$x, corrd$y)
	plot(corrd$y ~ corrd$x, xlab = "X", ylab = "Y", 
		xlim = c(4,22), ylim = c(-35,-5),
		main = "Strong Negative Correlation of -0.95")

