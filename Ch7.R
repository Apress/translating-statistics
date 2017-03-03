##############################################################################
######################## Translating Statistics R Code #######################
##############################################################################

##### Note - to run code in R either copy and paste into the R Console window
##### or place cursor anywhere on the line, hold Ctrl and press r
##### Code may require datasets loaded from previous Chapter scripts

##############################################################################
###################### Chapter 7 - Statistical Modeling ######################
##############################################################################

##### Linearity

	H = c(151.765,139.7,136.525,156.845,145.415,163.830,149.225,168.910,
		147.955,165.100,154.305,151.130,90.213,149.900,150.495,163.195,
		157.480,173.942,121.920,105.410,86.360,161.290,156.210,129.540,
		109.220,146.400,148.590,147.320,137.160,125.730,114.300,147.955,
		161.925,146.050,146.050,152.705,142.875,142.875,147.955,160.655,
		151.765,162.865,171.450,147.320,147.955,144.780,121.920,128.905,
		97.790)

	W = c(54.826,43.486,40.165,50.042,48.277,55.993,45.243,52.480,
		41.870,61.488,56.895,48.220,12.001,54.700,40.849,55.563,
		49.326,64.357,30.618,20.948,13.489,55.988,49.723,30.587,
		22.989,42.494,44.903,42.465,34.329,30.980,24.860,47.313,
		62.111,44.506,45.499,53.607,45.839,42.579,54.400,54.882,
		56.413,56.385,63.557,46.122,42.895,40.803,27.412,34.360,
		18.268)

	A = c(63,63,73,41,51,35,32,27,19,54,47,66,7,20,65,36,44,41,12,8,7,39,
		29,13,7,56,45,19,17,16,11,29,30,24,35,33,27,32,36,24,30,24,52,42,
		19,17,8,12,5) 

	o = data.frame(H, W, A)

	windows(6,6)
	ggplot(o, aes(y = W, x = H)) + theme_bw() + geom_point() + 
		geom_smooth(method = "lm", se = FALSE) + 
		scale_y_continuous(limits = c(0,70), breaks = seq(0,70, 
			by = 10)) +
		scale_x_continuous(limits = c(80,180), breaks = seq(80,180, 
			by = 10)) +
		xlab("Height (cm)") + ylab("Weight (kg)\n") + 
		ggtitle("Weight by Height per Subject\n") + 
		theme(plot.title = element_text(size = 18, face = "bold"), 
		strip.text = element_text(size = 12, face = "bold"), 
		axis.text = element_text(size = 12, face = "bold"), 
		axis.text.x = element_text(size = 10, face = "bold"), 
		axis.text.y = element_text(size = 9, face = "bold"), 
		axis.title = element_text(size = 12, face = "bold"))

	windows(6,6)
	ggplot(o, aes(y = W, x = A)) + theme_bw() + geom_point() + 
		geom_smooth(method = "lm", se = FALSE, colour = "red") + 
		geom_smooth(se = FALSE, span = 1) + 
		scale_y_continuous(limits = c(0,70), breaks = seq(0,70, 
			by = 5)) + 
		scale_x_continuous(limits = c(5,75), breaks = seq(5,75, 
			by = 5)) + 
		xlab("Age") + ylab("Weight (kg)\n") + 
		ggtitle("Weight by Age per Subject\n") + 
		theme(plot.title = element_text(size = 18, face = "bold"), 
		strip.text = element_text(size = 12, face = "bold"), 
		axis.text = element_text(size = 12, face = "bold"), 
		axis.text.x = element_text(size = 10, face = "bold"), 
		axis.text.y = element_text(size = 9, face = "bold"), 
		axis.title = element_text(size = 12, face = "bold"))

##### Normality

	Salary = c(24,28,35,30,33,35,36,40,39,44,46,48,55,47,40,29,45,26,36,46,
		41,50,47,26,52,32,42,37,30,30,34,34,41,41,36,46,30,47,33,36,38)
	Years.at.Company = c(1,4,8,9,11,13,16,19,21,24,26,28,30,22,15,6,20,4,
		11,25,18,26,30,1,25,14,21,13,7,2,19,5,27,14,17,29,6,31,8,16,24)

	p = data.frame(Salary, Years.at.Company)

	mod = lm(Salary ~ Years.at.Company, data = p)
	summary(mod)
	plot(mod, which = 2)

##### Errors variability

	plot(mod, which = 1)

##### Bad errors and categorical

	o$Weight = o$W; o$Height = o$H
	mod2 = lm(Weight ~ Height, data = o)
	summary(mod2)
	plot(mod2, which = 1)

	Temp = c(43,55,47,64,60,53,57,50,60,62,50,58,56,55,58,57,60,58,58,56,
		64,64,53,53,60,67,66,62,48,55)
	Location = rep(c("New York", "California", "Florida", "Washington DC", 
		"L.A."), 6)

	q = data.frame(Temp, Location)
	mod3 = lm(Temp ~ Location, data = q)
	summary(mod3)
	plot(mod3, which = c(1))

##### Polynomials - shapes may differ to book

	x = rnorm(50, mean = 0, sd = 1.5)
	y = x^2 - x - 2
	r = data.frame(x,y)

	windows(6,6)
	ggplot(r, aes(x = x, y = y)) + theme_bw() + 
		geom_smooth(se = FALSE, span = 0.25, cex = 2) + 
		scale_x_continuous(limits = c(-3,3), breaks = seq(-3, 3, 
			by = 1)) + 
		scale_y_continuous(limits = c(-2.5,7.75), breaks = seq(-2.5, 
			7.75, by = 1)) +
		xlab("X") + ylab("Y\n") + 
		ggtitle("Second Degree Polynomial
		or Quadratic Polynomial\n") + 
		theme(plot.title = element_text(size = 18, face = "bold"), 
		strip.text = element_text(size = 12, face = "bold"), 
		axis.text = element_text(size = 12, face = "bold"), 
		axis.text.x = element_text(size = 10, face = "bold"), 
		axis.text.y = element_text(size = 9, face = "bold"), 
		axis.title = element_text(size = 12, face = "bold"))

	x = rnorm(50, mean = 0, sd = 1)
	y = x^3 + x^2 - x
	s = data.frame(x,y)

	windows(6,6)
	ggplot(s, aes(x = x, y = y)) + theme_bw() + 
		geom_smooth(se = FALSE, span = 0.25, cex = 2) + 
		scale_x_continuous(limits = c(-3,2), breaks = seq(-3, 2, 
			by = 1)) + 
		scale_y_continuous(limits = c(-8,8), breaks = seq(-8, 8, 
			by = 2)) +
		xlab("X") + ylab("Y\n") + 
		ggtitle("Third Degree Polynomial
		or Cubic Polynomial\n") + 
		theme(plot.title = element_text(size = 18, face = "bold"), 
		strip.text = element_text(size = 12, face = "bold"), 
		axis.text = element_text(size = 12, face = "bold"), 
		axis.text.x = element_text(size = 10, face = "bold"), 
		axis.text.y = element_text(size = 9, face = "bold"), 
		axis.title = element_text(size = 12, face = "bold"))

	x = rnorm(50, mean = 0, sd = 0.75)
	y = x^4 + x^3 - x^2 - x
	t = data.frame(x,y)

	windows(6,6)
	ggplot(t, aes(x = x, y = y)) + theme_bw() + 
		geom_smooth(se = FALSE, span = 0.25, cex = 2) + 
		scale_x_continuous(limits = c(-2,1.5), breaks = seq(-2, 1.5, 
			by = 0.5)) + 
		scale_y_continuous(limits = c(-1,4.01), breaks = seq(-1, 4.01, 
			by = 1)) +
		xlab("X") + ylab("Y\n") + 
		ggtitle("Fourth Degree Polynomial
		or Quartic Polynomial\n") + 
		theme(plot.title = element_text(size = 18, face = "bold"), 
		strip.text = element_text(size = 12, face = "bold"), 
		axis.text = element_text(size = 12, face = "bold"), 
		axis.text.x = element_text(size = 10, face = "bold"), 
		axis.text.y = element_text(size = 9, face = "bold"), 
		axis.title = element_text(size = 12, face = "bold"))

	x = rnorm(50, mean = 0, sd = 1.75)
	y = (x^5 + 3*x^4 - 10*x^3 - 31*x^2 + 13*x)
	u = data.frame(x,y)

	windows(6,6)
	ggplot(u, aes(x = x, y = y)) + theme_bw() + 
		geom_smooth(se = FALSE, span = 0.25, cex = 2) + 
		scale_x_continuous(limits = c(-4,4), breaks = seq(-4, 4, 
			by = 1)) + 
		scale_y_continuous(limits = c(-110,59), breaks = seq(-110, 59, 
			by = 20)) +
		xlab("X") + ylab("Y\n") + 
		ggtitle("Fifth Degree Polynomial
		or Quintic Polynomial\n") + 
		theme(plot.title = element_text(size = 18, face = "bold"), 
		strip.text = element_text(size = 12, face = "bold"), 
		axis.text = element_text(size = 12, face = "bold"), 
		axis.text.x = element_text(size = 10, face = "bold"), 
		axis.text.y = element_text(size = 9, face = "bold"), 
		axis.title = element_text(size = 12, face = "bold"))

##### Model Output

	Bottle.Weight = c(20.6,20.7,22.8,23.7,22.4,23.1,20.9,21.6,22.2,21.7,
		20.5,22.4,22.6,22.4,21.3,20.6,21.7,21.9,21.3,23.9,23.6,24.7,
		25.1,24.9,23.7,25.6,24.7,24.1,23.1,23.9,24.6,25.2,24.7,24.3,
		23.5,23.8,24.6,25,21.29,23.23,22.28,23.07,21.20,22.92,21.90,
		23.91,21.73,20.93,22.09,21.54,20.83,22.26,21.90,21.19,24.37,
		22.29,22.64)
	Type = rep(c("A","B","C"), each = 19)

	v = data.frame(Bottle.Weight,Type)

	mod4 = lm(Bottle.Weight ~ Type, data = v)
	summary(mod4)

##### Additional output

	plot(mod4, which = 1)
	plot(mod4, which = 2)
	confint(mod4)

	library(lsmeans)
	lsmeans(mod4, pairwise ~ Type)

	windows(6,6)
	ggplot(v, aes(x = Type, y = Bottle.Weight)) +
		stat_boxplot(geom = "errorbar", width = 1) + 
		guides(fill = FALSE) + geom_boxplot(outlier.colour = "red", 
		outlier.shape = 7, outlier.size = 2) + theme_bw() + 
		ggtitle("Boxplot of Empty Bottle Weight
		by Bottle Type\n") + 
		xlab("Bottle Type") + ylab("Empty Bottle Weight (g)\n") + 
		stat_summary(fun.y = mean, geom = "point", shape = 8, size = 2, 
		col = "blue") + stat_summary(fun.y = median, geom = "point", 
		shape = 15, size = 2, col = "green") + 
		scale_y_continuous(limits = c(20,26), breaks = seq(20,26, 
			by = 1)) + 
		theme(plot.title = element_text(size = 18, face = "bold"), 
		strip.text = element_text(size = 12, face = "bold"), 
		axis.text = element_text(size = 12, face = "bold"), 
		axis.text.x = element_text(size = 10, face = "bold"), 
		axis.text.y = element_text(size = 9, face = "bold"), 
		axis.title = element_text(size = 12, face = "bold"))

##### Example 7.1 - LM

	Yield = c(498,480.3,476.4,546,715.4,666,741.2,522,683.6,574,804,637,
		700,750,600,650,590)
	Conc = c(3.9,3.8,3.6,4.2,5.7,5,5.5,3.7,4.9,4,6,5,5.2,5.9,4.8,4.7,4.3)
	data18 = data.frame(Yield, Conc)

	mod = lm(Yield ~ Conc, data = data18)
	summary(mod)

	plot(mod, which = 2)
	plot(mod, which = 1)

	confint(mod)

	library(ggplot2)
	windows(5,5)
	ggplot(data18, aes(x = Conc, y = Yield)) + theme_bw() + geom_point() +	
		geom_smooth(method = "lm")

	windows(5,5)
	ggplot(data18, aes(x = Conc, y = Yield)) + theme_bw() + geom_point() +
		geom_smooth(method = "lm") + xlab("Concentration") + 
		ylab("Yield\n") + ggtitle("Yield by Concentration\n") + 
		scale_y_continuous(limits = c(450,815), breaks = seq(450,815, 
			by = 50)) +
		theme(plot.title = element_text(size = 18, face = "bold"), 
		axis.text = element_text(size = 12, face = "bold"), 
		axis.text.x = element_text(size = 10, face = "bold"), 
		axis.text.y = element_text(size = 9, face = "bold"), 
		axis.title = element_text(size = 12, face = "bold"))

##### Example 7.2 - ANOVA

	Volume = c(28.756,29.305,28.622,30.195,27.736,17.093,17.076,17.354,
		16.353,15.880, 36.833,35.653,34.583,35.504,35.236,30.333,
		30.030,28.339,28.748,29.020,32.591,30.572,32.904,31.942,
		33.653, 20.725,22.198,21.988,22.403,21.324,38.840,40.137,
		39.295,39.006,40.731,32.136,33.209,34.558,32.782,31.460)
	Material = rep(c("A","B"), each = 20)
	Method = rep(c("I","II","III","IV"), each = 5, 2)
	data19 = data.frame(Volume, Material, Method)

	bartlett.test(Volume ~ interaction(Material,Method), data = data19)

	mod = aov(Volume ~ Material*Method, data = data19)
	summary(mod)

	mod2 = aov(Volume ~ Material + Method, data = data19)
	summary(mod2)

	library(lsmeans)
	lsmeans(mod2, pairwise ~ Method)

	anova(mod, mod2)

	plot(mod2, which = 2)
	plot(mod2, which = 1)

	confint(mod2)

	mod3 = lm(Volume ~ Material + Method, data = data19)
	summary(mod3)

	lsmeans(mod2, pairwise ~ Method*Material)

	anova(mod3)

	windows(5,5)
	ggplot(data19, aes(x = Method, y = Volume)) + theme_bw() + 
		facet_wrap( ~ Material) + stat_boxplot(geom = "errorbar") + 
		geom_boxplot()

	windows(5,5)
	ggplot(data19, aes(x = Method, y = Volume)) + theme_bw() + 
		facet_wrap(~ Material) + stat_boxplot(geom = "errorbar") + 
		geom_boxplot(outlier.colour = "red", outlier.shape = 7, 
		outlier.size = 2) + ggtitle("Volume by Material and Method\n") + 
		xlab("Material") + ylab("Volume\n") + 
		stat_summary(fun.y = mean, geom = "point", shape = 8, size = 2, 
		col = "blue") + scale_y_continuous(limits = c(15,45), 
		breaks = seq(15,45, by = 5)) + 
		theme(plot.title = element_text(size = 16, face = "bold"), 
		strip.text = element_text(size = 12, face = "bold"), 
		axis.text = element_text(size = 12, face = "bold"), 
		axis.text.x = element_text(size = 10, face = "bold"), 
		axis.text.y = element_text(size = 9, face = "bold"), 
		axis.title = element_text(size = 12, face = "bold"))

##### Example 7.3 - Gaussian GLM

	Time.Taken = c(48.1,46.3,47.2,47.9,47.6,49,48,38.6,39.8,40.9,41.7,
		39.9,40.8,39.7,33.3,34.6,35.8,34,32,35.4,34.5,44.3,45.8,46.5,
		48.7,45.3,48.8,49.4,38.8,38.2,38.7,39.2,39.1,40,41.9,34.2,
		33.6,33.7,34,34.3,35.1,35.8,33.4,32.1,34.8,35.7,34.6,35.7,
		38.4,30.9,31,30.7,31.1,31.8,32.6,33.2,28,28.6,27.7,28.3,29,
		28.4,29.1,35.8,33.2,37.1,36.4,37.6,38.9,39.2,31.1,30.5,30.4,
		31.6,31.9,32.6,33.7,28.7,28.9,27.8,29.1,29,28.4,28.7,47.6,
		46.3,47.9,46.7,49.8,48.9,49.4,39.1,38.5,38.4,38.7,39.5,40.5,
		42.3,35.3,34.4,34.2,34.2,34.7,35,34.6,47.8,45.1,48.8,48.4,
		49.7,49.2,47,40.1,39.9,39.5,38.9,40.9,40.6,42,34.9,34.2,34.7,
		35,33.8,33.1,34.5)
	Machine = rep(c("A","B","C"), each = 42)
	Operator = rep(c("Op1","Op2"), each = 21, 3)
	Concentration = rep(c("Low","Medium","High"), each = 7, 6)
	data20 = data.frame(Time.Taken, Machine, Operator, Concentration)

	data20$Concentration = factor(data20$Concentration, 
		levels = c("Low","Medium","High"))

	mod = glm(Time.Taken ~ Machine*Operator*Concentration, 
		family = gaussian, data = data20)
	summary(mod)

	anova(lm(mod))

	mod2 = glm(Time.Taken ~ Machine*Operator + Machine*Concentration + 	
		Operator*Concentration, family = gaussian, data = data20)
	summary(mod2)
	anova(lm(mod2))

	mod3 = glm(Time.Taken ~ Machine*Operator + Machine*Concentration, 
		family = gaussian, data = data20)
	summary(mod3)
	anova(lm(mod3))

	mod4 = glm(Time.Taken ~ Machine*Concentration + Operator, 
		family = gaussian, data = data20)
	summary(mod4)
	anova(lm(mod4))

	mod5 = glm(Time.Taken ~ Machine*Concentration, family = gaussian, 
		data = data20)
	summary(mod5)

	anova(lm(mod5))

	confint(mod5)

	lsmeans(mod5, pairwise ~ Machine*Concentration)

	lsmeans(mod5, pairwise ~ Machine)
	lsmeans(mod5, pairwise ~ Concentration)
	lsmeans(mod5, pairwise ~ Machine*Concentration)

	windows(5,5)
	ggplot(data20, aes(x = Concentration, y = Time.Taken)) + theme_bw() + 
		facet_wrap(~ Machine) + stat_boxplot(geom = "errorbar") + 
		geom_boxplot()

	windows(5,5)
	ggplot(data20, aes(x = Concentration, y = Time.Taken)) + theme_bw() + 
		facet_wrap(~ Machine) + 
		stat_boxplot(geom = "errorbar") + 
		geom_boxplot(outlier.colour = "red", outlier.shape = 7, 
		outlier.size = 2) + ggtitle("Time Taken by Concentration 
		and Machine\n") + 
		xlab("Concentration") + ylab("Time Taken (Mins)\n") + 
		stat_summary(fun.y = mean, geom = "point", shape = 8, size = 2, 
		col = "blue") + scale_y_continuous(limits = c(25,50), 
		breaks = seq(25,50, by = 5)) + 
		theme(plot.title = element_text(size = 16, face = "bold"), 
		strip.text = element_text(size = 12, face = "bold", 
			lineheight = 3), 
		axis.text = element_text(size = 12, face = "bold"), 
		axis.text.x = element_text(size = 9, face = "bold"), 
		axis.text.y = element_text(size = 9, face = "bold"), 
		axis.title = element_text(size = 12, face = "bold"))

	anova(mod, mod2, mod3, mod4, mod5, test = "F")

	plot(mod5, which = 2)
	plot(mod5, which = 1)

	summary(lm(mod5))

	windows(5,5)
	ggplot(data20, aes(x = Concentration, y = Time.Taken)) + 
		theme_bw() + facet_wrap(~ Operator + Machine) + 
		stat_boxplot(geom = "errorbar") + geom_boxplot()

	windows(5,5)
	ggplot(data20, aes(x = Concentration, y = Time.Taken)) + theme_bw() + 
		facet_wrap(~ Operator + Machine, 
		labeller = label_wrap_gen(multi_line = FALSE)) + 
		stat_boxplot(geom = "errorbar") + 
		geom_boxplot(outlier.colour = "red", outlier.shape = 7, 
		outlier.size = 2) + ggtitle("Time Taken by Concentration, 
		Operator, and Machine\n") + 
		xlab("Concentration") + ylab("Time Taken (Mins)\n") + 
		stat_summary(fun.y = mean, geom = "point", shape = 8, size = 2, 
		col = "blue") + scale_y_continuous(limits = c(25,50), 
		breaks = seq(25,50, by = 5)) + 
		theme(plot.title = element_text(size = 16, face = "bold"), 
		strip.text = element_text(size = 12, face = "bold", 
			lineheight = 3), 
		axis.text = element_text(size = 12, face = "bold"), 
		axis.text.x = element_text(size = 9, face = "bold"), 
		axis.text.y = element_text(size = 9, face = "bold"), 
		axis.title = element_text(size = 12, face = "bold"))

##### Example 7.4 - Poisson GLM

	Children = c(1,1,1,1,2,1,0,1,0,0,2,0,0,2,0,0,0,1,0,0,0,2,1,1,3,1,0,
		2,1,0,0,0,1,0,1,2,0,3,0,1,0,2,1,2,1,2,1,0,1,1,3,2,1,3,3,0,3,
		3,2,3,4,0,2,3,2,2,4,2,2,4,4,2,4,3,1,2,2,2,4,4,0,1,1,2,3,1,1,
		2,1,2,1,3,1,1,2,2,4,1,2,2,4,3,4,1,3,4,3,3,5,2,1,1,2,2,3,2,3,
		2,4,2,4,4,3,3,2,2,4,3,4,3,3,2,3,2,3,3,4,2,1,2,3,1,3,2,3,2,3,
		2,0,2)
	Income = rep(c(">$50k","$25k-$50k","<$25k"), each = 50)
	data21 = data.frame(Children, Income)

	data21$Income = factor(data21$Income, levels = c(">$50k", 
		"$25k-$50k", "<$25k"))

	tapply(data21$Children, data21$Income, mean)
	tapply(data21$Children, data21$Income, var)

	mod = glm(Children ~ Income, data = data21, family = poisson)
	summary(mod)

	confint(mod)

	lsmeans(mod, pairwise ~ Income)

	1 - pchisq(summary(mod)$deviance, summary(mod)$df.residual)

	windows(5,5)
	ggplot(data21, aes(x = Children)) + theme_bw() + 
		facet_wrap(~Income) + geom_bar()

	windows(5,5)
	ggplot(data21, aes(x = factor(Children))) + theme_bw() + 
		facet_wrap(~Income) + geom_bar(colour = "black", 
			position = "dodge", fill = "black") +
 		ggtitle("Number of Children by Income Band\n") + 
		xlab("No. of Children") + ylab("Count\n") + 
		scale_y_continuous(limits = c(0,20), breaks = seq(0,20, 
			by = 2)) + 
		theme(plot.title = element_text(size = 16, face = "bold"), 
		strip.text = element_text(size = 12, face = "bold", 
			lineheight = 3), 
		axis.text = element_text(size = 12, face = "bold"), 
		axis.text.x = element_text(size = 9, face = "bold"), 
		axis.text.y = element_text(size = 9, face = "bold"), 
		axis.title = element_text(size = 12, face = "bold"))

##### Example 7.5 - Negative Binomial GLM

	Fish.Caught = c(0,3,3,0,4,8,6,1,2,1,0,1,1,2,4,1,3,3,4,3,1,3,1,2,2,3,
		8,5,2,2,4,2,2,5,3,2,0,4,3,1,5,0,1,4,1,2,2,2,0,3,2,9,9,1,5,7,
		2,4,6,8,1,4,2,16,10,11,3,5,12,11,1,0,5,2,3,8,1,7,5,10,13,4,
		10,1,0,2,7,7,3,1,9,4,2,2,2,1,10,2,9,2)
	Group = rep(c("Tourists","Locals"), each = 50)
	data22 = data.frame(Fish.Caught, Group)

	tapply(data22$Fish.Caught, data22$Group, mean)
	tapply(data22$Fish.Caught, data22$Group, var)

	mod = glm(Fish.Caught ~ Group, data = data22, family = poisson)
	1 - pchisq(summary(mod)$deviance, summary(mod)$df.residual)

	library(MASS)
	mod2 = glm.nb(Fish.Caught ~ Group, data = data22)
	summary(mod2)

	confint(mod2)

	lsmeans(mod2, pairwise ~ Group)

	1 - pchisq(summary(mod2)$deviance, summary(mod2)$df.residual)

	windows(5,5)
	ggplot(data22, aes(x = Fish.Caught)) + theme_bw() + 
		facet_wrap(~Group) + geom_bar()

	windows(5,5)
	ggplot(data22, aes(x = Fish.Caught)) + theme_bw() + 
		facet_wrap(~Group) + geom_bar(colour = "black", fill = "black") + 
		ggtitle("Number of Fish Caught within 3 Hours 
		by Group\n") + 
		xlab("No. of Fish Caught") + ylab("Count\n") + 
		scale_y_continuous(limits = c(0,12), breaks = seq(0,12, 
			by = 1)) + 
		scale_x_continuous(limits = c(-0.5,16.5),breaks = seq(0,16, 
			by = 2)) + 
		theme(plot.title = element_text(size = 16, face = "bold"), 
		strip.text = element_text(size = 12, face = "bold", 
			lineheight = 3), 
		axis.text = element_text(size = 12, face = "bold"), 
		axis.text.x = element_text(size = 9, face = "bold"), 
		axis.text.y = element_text(size = 9, face = "bold"), 
		axis.title = element_text(size = 12, face = "bold"))

##### Example 7.6 - Binomial GLM

	Detection = c(1,1,0,1,1,0,1,1,1,1,1,1,0,1,1,0,1,1,1,1,1,1,0,1,1,1,1,
		1,1,1,1,0,1,0,0,0,0,1,0,0,1,0,1,0,1,0,1,1,0,1,0,1,1,0,0,1,0,
		0,1,1,0,0,0,0,1,0,0,0,0,0,0,0,0,0,1,0,0,0,1,0,0,0,0,0,0,0,0,
		0,0,0)
	Device = rep(c("A","B"), each = 15,3)
	Terrain = rep(c("Flat","Bumpy","Marsh"), each = 30)
	data23 = data.frame(Detection, Terrain, Device)

	data23$Terrain = factor(data23$Terrain, 
		levels = c("Flat","Bumpy","Marsh"))

	mod = glm(Detection ~ Terrain*Device, data = data23, family = binomial)
	summary(mod)

	mod2 = glm(Detection ~ Terrain + Device, family = binomial, 
		data = data23)
	summary(mod2)

	mod3 = glm(Detection ~ Terrain, family = binomial, data = data23)
	summary(mod3)

	confint(mod3)

	lsmeans(mod3, pairwise ~ Terrain)

	1 - pchisq(summary(mod3)$deviance, summary(mod3)$df.residual)

	anova(mod, mod2, mod3, test = "Chisq")

	Table3 = as.table(ftable(Detection ~ Terrain + Device, data = data23))
	data24 = as.data.frame(Table3)

	windows(5,5)
	ggplot(data24, aes(x = Terrain, y = Freq, fill = Detection)) + 
		theme_bw() + facet_wrap(~Device) + 
		geom_bar(stat = "identity", position = "dodge")

	windows(5,5)
	ggplot(data24, aes(x = Terrain, y = Freq, fill = Detection)) + 
		geom_bar(stat = "identity", position = "dodge", 
			colour = "black") + 
		facet_wrap(~Device) + theme_bw() + 
		guides(fill = guide_legend(ncol = 2)) + 
		scale_fill_manual("Detection", values = c("darkred",
			"forestgreen")) +  xlab("Terrain") + 
		ylab("Detection") + 
		scale_y_continuous(limits = c(0,15), breaks = seq(0,15, 
			by = 1)) + 
		ggtitle("Frequency of Detections by 
		Terrain and Device\n")+
		geom_text(aes(y = Freq, label = paste(round(Freq/15*100,1),"%")), 
			position = position_dodge(width = 0.99), size = 2.5,
			vjust = -0.5) +
		theme(plot.title = element_text(size = 16, face = "bold"), 
		legend.background = element_rect(linetype = "dashed", 
			colour = "black"),
		legend.position = c(0.11,0.91),
		strip.text = element_text(size = 12, face = "bold"), 
		axis.text = element_text(size = 12, face = "bold"), 
		axis.text.x = element_text(size = 10, face = "bold"), 
		axis.text.y = element_text(size = 9, face = "bold"), 
		axis.title = element_text(size = 12, face = "bold"))

##### Example 7.7 - BRGLM

	Detection = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
		0,0,0,1,1,0,0,0,0,1,1,0,0,0,0,1,1,1,0,0,0,1,1,1,0,0,0,1,1,1,
		1,0,0,1,1,1,0,0,0,1,1,1,1,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
		1,1,1,1,1,1,1,1,1)
	Concentration = rep(c(100,120,140,160,180,200,220,240,260,280,300,
		320,340,360,380,400), each = 6)
	data25 = data.frame(Detection, Concentration)

	library(brglm)
	mod = brglm(Detection ~ Concentration, family = binomial, 
		data = data25)
	summary(mod)

	confint(mod)

	1 - pchisq(summary(mod)$deviance, summary(mod)$df.residual)

	library(MASS)
	dose.p(mod, p = c(0.5, 0.75, 0.95, 0.99))

	xcis = dose.p(mod, p = seq(0.01, 0.99, 0.01))
	tempv50 = data.frame(prop = seq(0.01,0.99,0.01), 
		v50 = as.numeric(xcis), v50.se = as.numeric(attr(xcis,"SE")))
	tempv50$lowerv50 = tempv50$v50 - (1.96*(tempv50$v50.se))
	tempv50$upperv50 = tempv50$v50 + (1.96*(tempv50$v50.se))
	tempConc = seq(80, 420, 1)
	tmpModFit = data.frame(Concentration = tempConc, 
		Proportion = predict(mod, newdata = data.frame(
		Concentration = tempConc), type = "response"))

	windows(5,5)
	ggplot(data25, aes(Concentration, Detection)) + 
		geom_point() + theme_bw() +
	geom_line(aes(Concentration, Proportion), tmpModFit) + 
	geom_line(aes(lowerv50, prop), tempv50) + 
	geom_line(aes(upperv50, prop), tempv50)

	windows(5,5)
	ggplot(data25, aes(Concentration, Detection)) + geom_point() + 
		geom_line(aes(Concentration, Proportion), tmpModFit) + 
		xlab("Concentration") + ylab("Probability of Detection\n") + 
		ggtitle("Probability of Detection 
		by Concentration (95% CI)\n") + 
		theme_bw() + geom_line(aes(lowerv50, prop), tempv50, 
			colour = "red", linetype = "dashed") + 
		geom_line(aes(upperv50, prop), tempv50, colour = "red", 
			linetype = "dashed") + 
		scale_x_continuous(limits = c(0,500), breaks = seq(0,500, 
			by = 50)) +
		scale_y_continuous(limits = c(0,1), breaks = seq(0,1, 
			by = 0.1)) + 
		theme(plot.title = element_text(size = 18, face = "bold"), 
		axis.text = element_text(size = 12, face = "bold"), 
		axis.text.x = element_text(size = 10, face = "bold"), 
		axis.text.y = element_text(size = 9, face = "bold"), 
		axis.title = element_text(size = 12, face = "bold"))

##### Example 7.8 - Zero-Inflated Models

	Shooting.Stars = c(3,0,4,3,0,1,4,2,0,2,0,1,1,1,0,3,0,0,0,0,3,0,3,4,
		0,0,4,2,3,0,2,1,3,2,1,2,2,2,0,0,0,0,3,1,1,4,5,0,2,2,5,4,6,4,
		5,5,7,6,0,4,6,6,0,5,4,4,3,0,6,4,2,5,1,4,5,6,7,0,7,4,4,6,6,2,
		0,5,7,1,4,3,7,7,3,7,7,7,7,4,5,4)
	Location = rep(c("City", "Countryside"), each = 50)
	data26 = data.frame(Shooting.Stars, Location)

	mod = glm(Shooting.Stars ~ Location, data = data26, 
		family = poisson)
	1 - pchisq(summary(mod)$deviance, summary(mod)$df.residual)

	library(MASS)
	mod2 = glm.nb(Shooting.Stars ~ Location, data = data26)
	1 - pchisq(summary(mod2)$deviance, summary(mod2)$df.residual)

	library(pscl)
	mod3 = zeroinfl(Shooting.Stars ~ Location, data = data26)
	summary(mod3)

	confint(mod3)

	data27 = expand.grid(levels(data26$Location)) 
	colnames(data27) = c("Location")
	data27$Est.ShStars = predict(mod3, data27); data27 

	vuong(mod, mod3)

	mod4 = zeroinfl(Shooting.Stars ~ Location, dist = "negbin", 
		data = data26)
	summary(mod4)

	windows(5,5)
	ggplot(data26, aes(x = Shooting.Stars)) + theme_bw() + 
		facet_wrap(~Location) + geom_bar()

	windows(5,5)
	ggplot(data26, aes(x = Shooting.Stars)) + theme_bw() + 
		facet_wrap(~Location) + 
		geom_bar(colour = "black", fill = "black") + 
		ggtitle("Number of Shooting Stars Seen 
		in an Hour by Location\n") + 
		xlab("No. of Shooting Stars") + ylab("Count\n") + 
		scale_y_continuous(limits = c(0,20), breaks = seq(0,20, by = 2)) + 
		theme(plot.title = element_text(size = 16, face = "bold"), 
		strip.text = element_text(size = 12, face = "bold"), 
		axis.text = element_text(size = 12, face = "bold"), 
		axis.text.x = element_text(size = 9, face = "bold"), 
		axis.text.y = element_text(size = 9, face = "bold"), 
		axis.title = element_text(size = 12, face = "bold"))

##### Example 7.9 - Ordinal Logistic Regression

	System = rep(c("A","B","C","D","E"), each = 4, 2)
	Likert.Response = rep(c("VP","P","W","VW"), 10)
	Group = rep(c("Group A","Group B"), each = 20)
	Response = c(4,6,1,0,6,5,0,0,0,1,4,6,1,4,5,1,0,3,7,1,5,5,2,0,5,6,
		1,0,0,0,7,5,2,2,6,2,2,3,6,1)
	Subjects = rep(c(11,12), each = 20)
	data28 = data.frame(System, Likert.Response, Group, Response)

	data28$Likert.Response = factor(data28$Likert.Response, 
		levels = c("VP","P","W","VW"), 
		labels = c("Very Poorly", "Poorly", "Well", "Very Well"))

	library(MASS)
	mod = polr(Likert.Response ~ System*Group, weights = Response, 
		data = data28)
	summary(mod)

	library(AER)
	coeftest(mod)

	mod2 = polr(Likert.Response ~ System + Group, weights = Response, 
		data = data28)
	summary(mod2); coeftest(mod2)

	mod3 = polr(Likert.Response ~ System, weights = Response, 
		data = data28)
	summary(mod3)

	confint(mod3)

	coeftest(mod3)

	library(HH)
	windows(7,5)
	plot.likert(System ~ Likert.Response|Group, value = "Response", 
		data = data28, 	layout = c(1,2), as.percent = TRUE, 
		main = "How Poorly/Well did the System Perform 
		with your Current Equipment?", ReferenceZero = 2.5,
		xlab = expression(bold("Percent")), 
		ylab = expression(bold("System")), 
		col = c("firebrick3","indianred1","springgreen","forestgreen"))

	windows(7,5)
	plot.likert(System ~ Likert.Response|Group, value = "Response", 
		data = data28, layout = c(1,2), as.percent = TRUE, 
		main = "How Poorly/Well did the System Perform 
		with your Current Equipment?", ReferenceZero = 2.5,
		xlab = expression(bold("Percent")), 
		ylab = expression(bold("System")), 
		col = c("firebrick3","indianred1","springgreen","forestgreen"),
		scales = list(x = list(limits = c(-102,102), at = 
		seq(-100,100,10), labels = abs(seq(-100, 100, 10)),
		cex = 0.65, tck = 0.5))) 

##### Example 7.10 - Mixed Effects Model 1

	Heart.Rate = c(66,80,92,94,124,159,181,66,79,87,92,144,147,168,67,
		80,90,97,124,161,190,66,80,89,94,143,148,168,54,70,104,128,
		165,174,189,52,64,91,110,120,164,194,55,68,108,126,166,176,
		189,50,66,91,115,121,165,195,54,78,89,121,144,172,200,53,76,
		83,109,154,165,179,53,80,89,125,145,170,198,51,78,83,112,
		151,167,180,70,88,95,100,121,168,175,72,80,88,110,148,171,
		178,71,90,94,104,125,170,175,70,81,90,110,147,172,180,49,87,
		91,115,131,158,178,51,61,64,103,146,167,168,50,89,90,117,
		132,159,180,48,59,64,108,145,169,171,64,87,94,115,135,160,
		196,68,81,97,108,138,155,193,66,87,92,117,135,162,194,67,80,
		95,108,140,157,195,66,87,95,130,158,171,193,63,67,96,110,
		148,154,194,65,86,97,129,160,171,195,63,66,100,110,146,156,
		195,55,86,107,116,132,172,180,55,68,80,100,107,155,182,57,
		88,106,118,135,176,182,55,70,81,100,105,160,183,65,68,88,
		110,139,182,187,66,74,82,90,108,155,189,66,70,86,107,138,
		185,189,67,75,86,91,110,157,189,53,67,94,121,141,183,199,52,
		64,75,103,111,158,169,51,68,97,125,141,185,200,53,66,73,101,
		113,159,175)
	Subject = rep(c(1:10), each = 28)
	Treatment = rep(c("A","B"), each = 7, 20)
	Time = rep(c(0,10,20,30,40,50,60), 40)
	data29 = data.frame(Heart.Rate, Subject, Treatment, Time)

	data29$Time = factor(data29$Time) 
	data29$Subject = factor(data29$Subject)

	library(lmerTest)
	mod = lmer(Heart.Rate ~ Treatment*Time + (1|Subject), REML = F, 
		data = data29)
	anova(mod)

	mod2 = lmer(Heart.Rate ~ Treatment + Time + (1|Subject), 
		REML = F, data = data29)
	anova(mod2)
	summary(mod2)
	confint(mod2)

	qqnorm(resid(mod2)); qqline(resid(mod2))
	plot(mod2)

	coef(mod2)

	windows(5,5)
	ggplot(data29, aes(x = Time, y = Heart.Rate, colour = Treatment, 
		shape = Treatment)) + theme_bw() + 
		facet_wrap(~Subject) + geom_point()

	windows(6,6)
	ggplot(data29, aes(x = Time, y = Heart.Rate, colour = Treatment, 
		shape = Treatment)) + theme_bw() + facet_wrap(~Subject) + 
		geom_point() + 
		ggtitle("Heart Rate by Time and Treatment 
		per Subject\n") + 
		xlab("Time (Secs)") + ylab("Heart Rate\n") + 
		scale_y_continuous(limits = c(0,201), breaks = seq(0,200, 
			by = 50)) + 
		scale_colour_manual("Treatment", values = c("firebrick3", 
			"dodgerblue3")) + 
		scale_shape_manual("Treatment", values = c(3,4)) + 
		theme(plot.title = element_text(size = 16, face = "bold"), 
		legend.position = c(0.75,0.18),
		legend.background = element_rect(linetype = "dashed", 
			colour = "black"),
		legend.title = element_text(size = 10, face = "bold"), 
		legend.text = element_text(size = 10),
		panel.margin = unit(0.15 , "lines"),
		strip.text = element_text(size = 10, face = "bold"), 
		axis.text = element_text(size = 12, face = "bold"), 
		axis.text.x = element_text(size = 9, angle = 45,
			hjust = 0.5, vjust = 0.5), 
		axis.text.y = element_text(size = 9, face = "bold"), 
		axis.title = element_text(size = 12, face = "bold"))

##### Example 7.11 - Mixed Effects Model 2

	mod3 = lmer(Heart.Rate ~ Treatment + Time + (1 + Time|Subject), 
		REML = F, data = data29)
	anova(mod3)
	summary(mod3)

	qqnorm(resid(mod3)); qqline(resid(mod3))
	plot(mod3)

	coef(mod3)

##### Example 7.12 - Mixed Effects Model 3

	Test.Score = c(94,88,86,90,94,87,87,92,89,92,87,94,93,91,89,92,91,
		91,95,91,82,84,90,81,92,89,85,94,88,94,94,94,86,94,93,84,82,
		92,92,83,89,83,81,87,84,80,81,83,88,82,81,90,82,85,87,82,86,
		84,87,88,82,91,95,77,88,87,79,75,91,77,82,91,95,92,89,83,79,
		90,83,83,82,79,79,78,83,82,81,77,80,79,84,83,81,78,77,75,76,
		76,84,75,78,78,71,79,70,75,75,78,76,71,76,76,73,71,80,70,71,
		78,71,74,76,74,74,77,81,78,79,76,82,79,80,73,72,83,72,81,81,
		72,79,74,67,75,71,66,65,71,73,69,65,67,71,72,68,73,65,65,74,
		67,72,72,82,70,72,86,89,87,87,88,74,92,70,89,86,63,68,74,88,
		71,88,91,76,86,75,79,76,69,86,71,78,67,67,73,69,81,79,78,80,
		72,81,69,72,75,76,68,72,78,78,77,71,73,70,77,75,75,69,77,74,
		76,68,78,76,75,68,74,69,78,76,70,79,78,67,65,86,88,65,88,73,
		66,65,85)
	School = rep(c("A","B","C"), each = 80)
	Class = rep(c("1","2"), each = 20,6)
	Subject = rep(c("English","Maths"), each = 40, 3)
	data30 = data.frame(Test.Score, School, Class, Subject)

	data30$Class = factor(data30$Class)

	mod = lmer(Test.Score ~ Subject + (1|School/Class), REML = F, 
		data = data30)
	summary(mod)

	confint(mod)

	qqnorm(resid(mod)); qqline(resid(mod))
	plot(mod)

	pred = with(data30,expand.grid(Class = levels(Class), 
		Subject = levels(Subject), School = levels(School)))
	pred$Test.Score = predict(mod, newdata = pred)
	pred

	mod2 = lmer(Test.Score ~ Subject + (1 + Subject|School/Class), 
		REML = F, data = data30)
	summary(mod2)

	qqnorm(resid(mod2)); qqline(resid(mod2))
	plot(mod2)

	pred2 = with(data30,expand.grid(Class = levels(Class), 
		Subject = levels(Subject), School = levels(School)))
	pred2$Test.Score = predict(mod2, newdata = pred2)
	pred2

	windows(5,5)
	ggplot(data30, aes(x = Subject, y = Test.Score)) + theme_bw() + 
		facet_wrap(~School + Class) + 
		stat_boxplot(geom = "errorbar") + geom_boxplot()

	windows(5,5)
	ggplot(data30, aes(x = Subject, y = Test.Score)) +
		stat_boxplot(geom = "errorbar", width = 1) + 
		guides(fill = FALSE) + facet_wrap( ~ School + Class, 
			labeller = label_wrap_gen(multi_line = FALSE)) + 
		geom_boxplot(outlier.colour = "red", outlier.shape = 7, 
		outlier.size = 2) + theme_bw() + 
		ggtitle("Test Score by Subject, 
		School, and Class\n") + 
		xlab("Subject") + ylab("Test Score\n") + 
		stat_summary(fun.y = mean, geom = "point", shape = 8, size = 2, 
		col = "blue") + stat_summary(fun.y = median, geom = "point", 
		shape = 15, size = 2, col = "green") + 
		scale_y_continuous(limits = c(60,100), breaks = seq(60,100, 
			by = 10)) + 
		theme(plot.title = element_text(size = 16, face = "bold"), 
		strip.text = element_text(size = 12, face = "bold"), 
		axis.text = element_text(size = 12, face = "bold"), 
		axis.text.x = element_text(size = 10, face = "bold"), 
		axis.text.y = element_text(size = 9, face = "bold"), 
		axis.title = element_text(size = 12, face = "bold"))

