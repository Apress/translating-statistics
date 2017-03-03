##############################################################################
######################## Translating Statistics R Code #######################
##############################################################################

##### Note - to run code in R either copy and paste into the R Console window
##### or place cursor anywhere on the line, hold Ctrl and press r
##### Code may require datasets loaded from previous Chapter scripts

##############################################################################
####################### Chapter 6 - Hypothesis Testing #######################
##############################################################################

##### Example 6.1 - One-Sample Binary Data

	binom.test(x = 26, n = 30, p = 0.7, alternative = "greater", 
		conf.level = 0.95)

##### Example 6.2 - Two-Sample Binary Data

	results = matrix(c(25, 5, 16, 14), ncol = 2, byrow = TRUE)
	rownames(results) = c("A", "B")
	colnames(results) = c("Cured", "X")

	Table = as.table(results)
	Table

	library(RcmdrMisc)
	rowPercents(Table)

	fisher.test(Table, conf.level = 0.99, alternative = "two.sided")

	barplot(t(Table), main = "Outcome by Treatment", xlab = "Treatment", 
		ylab = "Count", space = NULL, ylim = c(0,25), beside = TRUE, 
		legend.text = TRUE, col = c("dodgerblue3","firebrick3"))

	prop.test(Table, conf.level = 0.99)

##### Example 6.3 - Paired Binary Data

	results = matrix(c(8, 15, 5, 4), ncol = 2, byrow = TRUE)
	rownames(results) = c("Suit 1 Complete", "Suit 1 Remove")
	colnames(results) = c("Suit 2 Complete", "Suit 2 Remove")

	Table2 = as.table(results)
	Table2

	library(exact2x2)
	mcnemar.exact(Table2)

	library(ggplot2)
	data11 = as.data.frame(Table2)
	ggplot(data11, aes(Var2, Var1)) + geom_tile(aes(fill = Freq), 
		colour = "black") + scale_fill_gradient(low = "white", 	
		high = "steelblue") + theme_bw() + xlab("Suit 2") + 
		ylab("Suit 1") + 
		ggtitle("Frequency of Task Completion in Each Suit")

##### Example 6.4 - One-Sample Normal Continuous Data

	data12 = c(32.5, 32.8, 35.7, 34.6, 34.8, 33.7, 32.9, 35.3, 33.7, 
		32.3, 32.0, 32.9, 33.6, 33.4, 34.1, 32.8, 32.5, 32.5, 34.0)

	library(car)
	qqPlot(data12, dist = "norm", main = "Q-Q Plot for Normality", 
		xlab = "Norm Quantiles", ylab = "Weight (kg)")

	t.test(data12, mu = 33, alternative = "greater", conf.level = 0.90)

	boxplot(data12, xlab = "Batch of Equipment", ylab = "Weight (kg)", 
		ylim = c(32,36), main = "Weight of Equipment")
	abline(h = 33, lty = "dashed", col = "red")
	points(mean(data12), pch = 19, col = "blue")

##### Example 6.5 - One-Sample Non-Normal Continuous Data

	data13 = c(9.246734, 7.399515, 10.747294, 3.569408, 4.337869, 
		3.172818, 14.205624, 30.076914, 15.747489, 6.751340, 
		8.868595, 9.067760, 8.168440, 7.499503, 7.377515, 
		14.883616, 19.688646, 26.299868, 6.351835, 14.180845, 
		8.291489, 6.923344, 8.540164, 11.488742, 22.694856, 
		16.868368, 31.439693, 10.700027, 17.887367, 10.008738, 
		10.678093, 13.064685, 24.202956, 12.361150, 12.772815, 
		13.436628, 14.336022, 4.701801, 6.078979, 16.039244, 
		13.830606, 11.857714, 11.927977, 4.661250, 28.652883, 
		6.391380, 4.378959, 8.361308, 11.056678, 7.521961)

	library(car)
	qqPlot(data13, dist = "norm", main = "Q-Q Plot for Normality", 
		xlab = "Norm Quantiles", ylab = "Temperature (°C)")

	wilcox.test(data13, mu = 12.5, alternative = "less", 
		conf.level = 0.90, conf.int = TRUE, exact = TRUE)

	boxplot(data13, xlab = "Sample", ylab = "Temperature (°C) ", 
		ylim = c(0,35), main = "Weekly UK Temperatures")
	abline(h = 12.5, lty = "dashed", col = "red")

##### Example 6.6 - Two-Sample Normal Continuous Data

	Temp = c(21.6, 20.7, 22.8, 23.7, 22.4, 23.1, 20.9, 21.6, 22.2, 21.7,
		20.5, 23.4, 22.6, 22.4, 21.3, 20.6, 21.7, 21.9, 22.3, 22.9, 
		23.6, 24.7, 25.1, 24.9, 23.7, 25.6, 24.7, 24.1, 23.1, 23.9, 
		24.6, 25.2, 24.7, 24.3, 23.5, 23.8, 24.6, 25.0)
	Type = rep(c("New", "Old"), each = 19)
	data14 = data.frame(Temp, Type)

	library(car)
	qqPlot(data14$Temp, dist = "norm", main = "Q-Q Plot for Normality", 	
		xlab = "Norm Quantiles", ylab = " Temperature (Celsius)")

	bartlett.test(Temp ~ Type, data = data14)

	t.test(Temp ~ Type, alternative = "two.sided", conf.level = 0.99, 	
		var.equal = TRUE, data = data14)

	boxplot(Temp ~ Type, data = data14, 
		main = "Temperature Readout by Thermometer", 
		xlab = "Thermometer", ylab = "Temperature Readout (°C)")
	points(mean(data14$Temp[data14$Type == "New"]), pch = 19, 
		col = "blue")
	points(mean(data14$Temp[data14$Type == "Old"]), x = 2, pch = 19, 
		col = "blue")

##### Example 6.7 - Two-Sample Non-Normal Continuous Data

	A = c(80.1, 78.6, 70.9, 75.6, 77.4, 73.1, 65.7, 53.6, 52.8, 30.1)
	B = c(31.8, 51.2, 49.8, 35.9, 71.7, 82.3, 80.2, 78.8, 46.7, 79.9)

	library(car)
	qqPlot(A, dist = "norm", main = "Q-Q Plot for Normality", 
		xlab = "Norm Quantiles", 
		ylab = "Distance (m) for Projectiles A")
	qqPlot(A, dist = "norm", main = "Q-Q Plot for Normality", 
		xlab = "Norm Quantiles", 
		ylab = "Distance (m) for Projectiles B")

	Dist = c(A, B)
	Group = rep(c("A", "B"), each = 10)
	data15 = data.frame(Dist, Group)

	leveneTest(Dist ~ Group, data = data15)

	wilcox.test(data15$Dist ~ data15$Group, alternative = "two.sided", 	
		conf.int = TRUE, conf.level = 0.95, exact = TRUE)

	boxplot(Dist ~ Group, data = data15, ylim = c(20,90), 
		main = "Distance Fired by Projectile", xlab = "Projectile", 
		ylab = "Distance Fired (m)")

##### Example 6.8 - Paired Normal Continuous Data

	Subject = c(1:9)
	BeforeTraining = c(15.02, 18.54, 17.66, 16.75, 13.60, 18.30, 14.34, 
		18.94, 16.71)
	AfterTraining = c(10.83, 16.47, 12.89, 12.46, 13.70, 15.95, 15.56, 
		16.32, 13.84)
	data16 = data.frame(Subject, BeforeTraining, AfterTraining)

	t.test(data16$BeforeTraining, data16$AfterTraining, 
		alternative = "greater", conf.level = 0.95, paired = TRUE)

	library(PairedData)
	paired.plotMcNeil(data16, "BeforeTraining", "AfterTraining", 
		subjects = "Subject") + theme_bw() + 
		scale_colour_manual(values = c("red", "blue")) + 
		scale_x_continuous(limits = c(10,20), breaks = seq(10,20, 
			by = 2)) + ylab("Subject") + 
		xlab("Time to Complete a Set Task (mins)") + 
		ggtitle("Time to Complete a Set Task Before and After Training") 

##### Example 6.9 - Paired Non-Normal Continuous Data

	Mouse = c(1:9)
	CageA = c(27.5, 10.25, 24.25, 20.5, 23.75, 25.0, 26.25, 18.25, 10.0)
	CageB = c(17.0, 9.25, 16.75, 9.5, 26.75, 23.25, 15.25, 18.0, 12.75)

	library(car)
	qqPlot(CageA - CageB, dist = "norm", main = "Q-Q Plot for Normality",
		xlab = "Norm Quantiles", ylab = "Time Difference (mins)")

	wilcox.test(CageA, CageB, alternative = "two.sided", paired = TRUE, 
		exact = TRUE, conf.int = TRUE, conf.level = 0.85)

	library(PairedData)
	data17 = data.frame(CageA, CageB, Mouse)
	paired.plotMcNeil(data17, "CageA", "CageB", subjects = "Mouse") + 
		theme_bw() + scale_colour_manual(values = c("red", "blue")) +
		scale_x_continuous(limits = c(0,30), breaks = seq(0,30, 
		by = 5)) + ylab("Mouse") + xlab("Time Spent Relaxed (mins)") + 
		ggtitle("Time Spent Relaxed in Cage A and Cage B")  

