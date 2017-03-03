##############################################################################
######################## Translating Statistics R Code #######################
##############################################################################

##### Note - to run code in R either copy and paste into the R Console window
##### or place cursor anywhere on the line, hold Ctrl and press r
##### Code may require datasets loaded from previous Chapter scripts

##############################################################################
############################# Chapter 9 - Graphs #############################
##############################################################################

##### 3D Plots - simulated data examples, will differ from book plots

	library(rgl)

	x = sort(rnorm(1000))
	y = rnorm(1000)
	z = rnorm(1000) + atan2(x, y)

### Remeber you can interact with this plot by dragging it around

	plot3d(x, y, z, col = "blue")

	library(scatterplot3d)

	x = rnorm(3000, 0, 2)
	y = rnorm(3000, 15, 10)
	z = rnorm(3000, 10, 2)
	scatterplot3d(x, y, z, highlight.3d = TRUE, pch = 20)

##### Plotting Averages

	x = c(59.85,80.2,94.9,115.75,139.55,170.70,188.5,59.4,71.75,84.75,
		104.2,132.25,160.05,182.25)
	Treat = rep(c("A","B"), each = 7)
	Time = rep(c(0,10,20,30,40,50,60),2)	
	lci = c(56.44,76.21,91.84,110.73,133.24,166.56,184.57,55.67,68.29,
		80.07,100.66,123.94,156.70,177.48)
	uci = c(63.26,84.19,97.96,120.77,145.86,174.84,192.43,63.13,75.21,
		89.43,107.74,140.56,163.40,187.02)
	w = data.frame(x, Treat, Time, lci, uci)
	w

	pd = position_dodge(1)

	windows(6,6)
	ggplot(w, aes(x = Time, y = x, colour = Treat)) + 
		theme_bw() + geom_line(position = pd, size = 1) + 
		geom_errorbar(aes(ymin = lci, ymax = uci),
		width = 2, size = 1, position = pd) + 
		geom_point(size = 3, position = pd, pch = 21, 
			fill = "white") + 
		ggtitle("Heart Rate by Time and Treatment
		with 95% Confidence Intervals\n") + 
		xlab("Time (Secs)") + ylab("Average Heart Rate\n") + 
		scale_y_continuous(limits = c(40,200), breaks = seq(40,200, 
			by = 20)) + 
		scale_x_continuous(limits = c(-1,61), breaks = seq(0,60, 
			by = 10)) + 
		scale_colour_manual("Treatment", values = c("firebrick3", 
			"dodgerblue3")) + 
		theme(plot.title = element_text(size = 16, face = "bold"), 
		legend.position = c(0.08,0.91),
		legend.background = element_rect(linetype = "dashed", 
			colour = "black"),
		legend.title = element_text(size = 10, face = "bold"), 
		legend.text = element_text(size = 10),
		axis.text.x = element_text(size = 10, face = "bold"), 
		panel.grid.major = element_line(colour = "grey90"),
		axis.text.y = element_text(size = 10, face = "bold"), 
		axis.title = element_text(size = 12, face = "bold"))

##### Plotting Ordinal Data

	library(HH)
	Question = c("Ease of Use", "Comfort", "Reliability")
	Likert = c("Strongly Disagree", "Disagree", 
		"Neither Disagree nor Agree","Agree","Strongly Agree")
	R = array(c(1,5,0,  0,3,1,  1,0,2,  4,5,8,  9,2,4), dim = c(3,5), 
		dimnames = list(Statement = Question, "Likert Scale" = Likert))
	R

	windows(7,4)
	plot.likert(R, as.percent = TRUE, , ReferenceZero = 3.5,
		main = "Responses to Statements for the Test Equipment", 
		xlab = expression(bold("Percent")), 
		ylab = expression(bold("Statement")), 
		col = brewer.pal.likert(5, "RdYlGn"), 
		scales = list(x = list(limits = c(-102,102), at = 
		seq(-100,100,10), labels = abs(seq(-100, 100, 10)),
		cex = 0.65, tck = 0.5))) 

##### Text Responses

	library(tm); library(wordcloud)

### Using example data from help(wordcloud) page

	data(crude)
	crude = tm_map(crude, removePunctuation)
	crude = tm_map(crude, function(x)removeWords(x,stopwords()))

	tdm = TermDocumentMatrix(crude)
	m = as.matrix(tdm)
	v = sort(rowSums(m),decreasing = TRUE)
	d = data.frame(word = names(v),freq = v)

### Creating a wordcloud from the data

	windows(5,5)
	wordcloud(d$word, d$freq, max.words = 100, random.order = FALSE, 
		rot.per = 0.35, use.r.layout = FALSE, 
		colors = brewer.pal(8, "Dark2"))

##### Unnecessary Plots

	gender = rep(c("M","F"), each = 25)
	smokes = rep(c("No","Yes","No","Yes"), c(15,10,13,12))
	x = data.frame(gender, smokes)
	
	windows(5,5)
	ggplot(x, aes(x = gender, fill = smokes)) + 
		geom_bar(colour = "black", position = "dodge", width = 0.85) + 
		theme_bw() +  scale_fill_manual(name = "Smokes", 
			values = c("forestgreen","firebrick3")) + 
		xlab("Gender") + ylab("Count of Respondents\n") + 
		ggtitle("Count of Respondents that Smoke 
		by Gender\n") + 
		scale_y_continuous(limits = c(0,15), breaks = seq(0,15, by = 1))+
		theme(plot.title = element_text(size = 18, face = "bold"),
		legend.position = c(0.9, 0.88), 
		legend.background = element_rect(linetype = "dashed", 
		colour = "black"),
		legend.title = element_text(size = 12, face = "bold"),
		axis.text.x = element_text(size = 10, face = "bold"), 
		panel.grid.major = element_line(colour = "grey90"),
		axis.text.y = element_text(size = 10, face = "bold"),
		axis.title = element_text(size = 12, face = "bold"))

	xtabs(~gender + smokes, data = x)

##### Example 9.1 - Aesthetics

	Accuracy = c(37.8,35.2,39.1,38.4,39.6,40.9,33.4,32.1,34.8,35.7,34.6,
		35.7,33.1,32.5,32.4,33.6,33.9,34.6,31.7,31.9,30.8,32.1,32,
		31.4,30.9,31,30.7,31.1,31.8,32.6,28,28.6,27.7,28.3,29,28.4,
		45.3,42.8,47.5,49.7,46.3,49.8,39.8,39.2,39.7,40.2,40.1,41,
		37.1,35.3,39.2,38.4,39.6,40,36.2,35.6,35.7,36,36.3,37.1,33.6,
		33.8,33.9,33.7,34.9,35.8,31.3,31.6,31.8,31,32,31.4,49.8,47.1,
		50.8,53.4,51.7,54.2,45.6,44.3,47.9,43.7,49.8,53.9,42.1,42.9,
		41.5,41.9,42.9,43.6,39.1,38.5,38.4,38.7,39.5,40.5,38.9,38.2,
		38.7,39,37.8,37.1,35.3,35.4,35.2,35.2,35.7,36)
	Target = rep(c("Small","Medium","Large"), each = 36)
	Ammo = rep(c("A","B"), 54)
	Operator = rep(c("Op1","Op2","Op3"), each = 2, 18)
	Distance = rep(c(1:6), each = 6, 3)
	data35 = data.frame(Target, Ammo, Operator, Distance, Accuracy)

	library(ggplot2)
	ggplot(data35, aes(x = Distance, y = Accuracy, colour = Target)) + 
	facet_wrap(~ Ammo + Operator) + geom_point()

	library(ggplot2)
	ggplot(data35, aes(x = Distance, y = Accuracy, colour = Target)) + 
		theme_bw() + facet_wrap(~ Ammo + Operator, 
			labeller = label_wrap_gen(multi_line = FALSE)) + 
		geom_point() + xlab("Distance (m)") + ylab("Accuracy\n") + 
		ggtitle("Accuracy by Distance, Ammo Type, 
			Operator and Target Size") + 
		scale_colour_manual("Target", values = c("firebrick3",
			"forestgreen","dodgerblue3")) + 
		scale_x_continuous(limits = c(1,6), breaks = seq(1,6, by = 1)) + 
		scale_y_continuous(limits = c(25,55), breaks = seq(25,55, 
			by = 5)) + 
		theme(plot.title = element_text(size = 16, face = "bold"), 
			strip.text = element_text(size = 12, face = "bold"), 
			legend.background = element_rect(linetype = "dashed", 
				colour = "black", fill = "grey85"),
			legend.key = element_rect(colour = "grey30", size = 0.5),
			legend.position = "top",
			legend.title = element_text(size = 12, face = "bold"),
			legend.text = element_text(size = 10),
			axis.text = element_text(size = 9, face = "bold"), 
			axis.title = element_text(size = 12, face = "bold"),
			panel.grid.major = element_line(colour = "grey90"))

##### Example 9.2 - Bar Chart

	ggplot(data24, aes(x = Terrain, y = Freq, fill = Detection)) + 
		geom_bar(stat = "identity", position = "dodge", 
			colour = "black") + 
		facet_wrap( ~ Device) + theme_bw() + xlab("Terrain") +
		guides(fill = guide_legend(ncol = 2)) + ylab("Detection") +
		scale_fill_manual("Detection", values = c("darkred",
			"forestgreen")) +  
		scale_y_continuous(limits = c(0,15), breaks = seq(0,15, 
			by = 1)) + ggtitle("Frequency of Detections by 
			Terrain and Device\n")+
		geom_text(aes(y = Freq, label = paste(round(Freq/15*100,1), 
			"%")), position = position_dodge(width=0.99), size= 2.5, 
			vjust = -0.5) +
		theme(plot.title = element_text(size = 16, face = "bold"), 
			legend.background = element_rect(linetype = "dashed", 
				colour = "black"),
			legend.position = c(0.11,0.91),
			strip.text = element_text(size = 12, face = "bold"), 
			axis.text = element_text(size = 10, face = "bold"), 
			axis.title = element_text(size = 12, face = "bold"),
			panel.grid.major = element_line(colour = "grey90"))

##### Example 9.3 - Tile Plot

	ggplot(data11, aes(Var2, Var1)) + geom_tile(aes(fill = Freq), 
		colour = "black") + theme_bw() +
		scale_fill_gradient(limits = c(0,32), breaks = seq(0,32, by = 4),
			low = "white", high = "deepskyblue3") + 
		xlab("Suit 2") + ylab("Suit 1") + 
		ggtitle("Frequency of Task Completion in Each Suit") + 
		geom_text(aes(label = paste(round(Freq/32*100,0),"%"))) +
		theme(plot.title = element_text(size = 16, face = "bold"), 
			legend.background = element_rect(linetype = "dashed", 
				colour = "black"),
			legend.title = element_text(size = 12, face = "bold"),
			legend.text = element_text(size = 9),
			axis.text = element_text(size = 10, face = "bold"), 
			axis.title = element_text(size = 12, face = "bold"))

	ggplot(data24[data24$Detection == 1,], aes(Terrain, Device)) + 
		geom_tile(aes(fill = Freq), colour = "black") + 
		scale_fill_gradient(limits = c(0,15), breaks = seq(0,15, by = 3),
			low = "red", high = "forestgreen") + 
		theme_bw() + xlab("Terrain") + ylab("Device\n") + 
		ggtitle("Frequency of Detections by Terrain and Device") + 
		geom_text(aes(label = paste(round(Freq/15*100,0),"%"))) + 
		theme(plot.title = element_text(size = 16, face = "bold"), 
			legend.background = element_rect(linetype = "dashed", 
				colour = "black"),
			legend.title = element_text(size = 12, face = "bold"),
			legend.text = element_text(size = 10, face = "bold"),
			axis.text = element_text(size = 10, face = "bold"), 
			axis.title = element_text(size = 12, face = "bold"))

##### Example 9.4 - Scatter Plot

	ggplot(data18, aes(x = Conc, y = Yield)) + theme_bw() + 
		geom_point() + geom_smooth(method = "lm") + 
		xlab("Concentration") + ylab("Yield\n") + 
		ggtitle("Yield by Concentration\n") + 
		scale_y_continuous(limits = c(450,815), breaks = seq(450,815, 
			by=50)) +
		theme(plot.title = element_text(size = 18, face = "bold"), 
			axis.text = element_text(size = 10, face = "bold"), 
			axis.title = element_text(size = 12, face = "bold"),
			panel.grid.major = element_line(colour = "grey90"))

##### Example 9.5 - Parallel Lines Plot

	library(PairedData)
	windows(7,4)
	paired.plotMcNeil(data16, "BeforeTraining", "AfterTraining", 
		subjects = "Subject") + theme_bw() + 
		scale_colour_manual(values = c("red", "blue")) + 
		scale_x_continuous(limits = c(10,20), breaks = seq(10,20, 
			by = 1)) + 
	ggtitle("Time to Complete a Set Task Before and After Training\n")+
		xlab("Time to Complete a Set Task (mins)") + ylab("Subject\n") + 
		theme(plot.title = element_text(size = 16, face = "bold"), 
			legend.position = c(0.11,0.85),
			legend.background = element_rect(linetype = "dashed", 
				colour = "black"),
			legend.title = element_text(size = 10, face = "bold"), 
			legend.text = element_text(size = 10),
			strip.text = element_text(size = 10, face = "bold"), 
			axis.text = element_text(size = 12, face = "bold"), 
			axis.title = element_text(size = 12, face = "bold"),
			panel.grid.major = element_line(colour = "grey90"))

##### Example 9.6 - Line Graph

	Temperature = c(43,55,47,64,60,53,57,50,60,62,50,58,56,55,58,57,60,
		58,58,56,64,64,53,53,60,67,66,62,48,55)
	Day = rep(c(1:6), each = 5)
	Location = rep(c("South East","South West","East of England",
		"The North", "Midlands"), 6)
	data36 = data.frame(Temperature, Day, Location)

	ggplot(data36, aes(x = Day, y = Temperature, colour = Location)) +
		geom_point(size = 2) + geom_line(size = 1) + theme_bw() + 
		ggtitle("Temperature (ºF) by Day and Location\n") + 
		xlab("Day") + ylab("Temperature ºF\n") + 
		guides(colour = guide_legend(ncol = 2)) + 
		scale_y_continuous(limits = c(42,68), breaks = seq(42,68, 
			by = 2)) + 
		scale_x_continuous(limits = c(1,6), breaks = seq(1,6, 
			by = 1)) + 
		scale_colour_manual("Location", values = c("firebrick3",
			"forestgreen","dodgerblue3","orange","purple")) + 
		theme(plot.title = element_text(size = 18, face = "bold"),
			legend.position = c(0.77, 0.11), 
			legend.background = element_rect(linetype = "dashed", 
				colour = "black"),
			legend.title = element_text(size = 12, face = "bold"),
			axis.text = element_text(size = 10, face = "bold"), 
			panel.grid.major = element_line(colour = "grey90"),
			axis.title = element_text(size = 12, face = "bold"))

	xbar = c(53.8,56.4,55.4,57.8,58.8,59.6)
	Day = c(1:6)
	LCI = c(42.93,50.28,51.32,55.96,51.92,49.64)
	UCI = c(64.67,62.52,59.48,59.64,65.68,69.56)
	data37 = data.frame(xbar, Day, LCI, UCI)

	ggplot(data37, aes(x = Day, y = xbar)) + geom_line(size = 1) + 
		geom_errorbar(aes(ymin = LCI, ymax = UCI), width = 0.25, 
			size = 1) +
		geom_point(size = 3, pch = 21, fill = "white") + 
		theme_bw() + ggtitle("Average Temperature by Day 
			with 95% Confidence Intervals\n") + 
		xlab("Day") + ylab("Average Temperature (ºF)\n") + 
		scale_y_continuous(limits = c(42,70), breaks = seq(42,70, 
			by = 2)) + 
		scale_x_continuous(limits = c(0.75,6.25), breaks = seq(1,6, 
			by = 1)) + 
		theme(plot.title = element_text(size = 18, face = "bold"), 
			axis.text = element_text(size = 10, face = "bold"), 
			panel.grid.major = element_line(colour = "grey90"),
			axis.title = element_text(size = 12, face = "bold"))

##### Example 9.7 - Box Plot

	ggplot(data20, aes(x = Concentration, y = Time.Taken)) + theme_bw() +
		facet_wrap(~ Machine) + stat_boxplot(geom = "errorbar") + 
		geom_boxplot(outlier.colour = "red", outlier.shape = 7, 
			outlier.size = 1.5) + 
		ggtitle("Time Taken by Concentration \n and Machine\n") + 
		xlab("Concentration") + ylab("Time Taken (Mins)\n") + 
		stat_summary(fun.y = mean, geom = "point", shape = 8, 
			size = 2, col = "blue") + 
		scale_y_continuous(limits = c(25,50), breaks = seq(25,50, 
			by = 5)) + 
		theme(plot.title = element_text(size = 16, face = "bold"), 
			strip.text = element_text(size = 12, face="bold", 
				lineheight = 3), 
			axis.text = element_text(size = 12, face = "bold"), 
			axis.text.x = element_text(size = 9, face = "bold", 
				angle = 45, h = 1), 
			axis.text.y = element_text(size = 9, face = "bold"), 
			axis.title = element_text(size = 12, face = "bold"),
			panel.grid.major = element_line(colour = "grey90"))

##### Example 9.8 - Likert Plot

	library(HH)
	windows(7,5)
	plot.likert(System ~ Likert.Response|Group, value = "Response", 
		data = data28, layout = c(1,2), as.percent = TRUE, 
		ReferenceZero = 2.5, 
		main = "How Poorly/Well did the System Perform 
			with your Current Equipment?", 
		xlab = expression(bold("Percent")), 
		ylab = expression(bold("System")), 
		col = c("firebrick3","indianred1","springgreen","forestgreen"),
		scales = list(x = list(limits = c(-102,102), 
			at = seq(-100,100,10), labels = abs(seq(-100, 100, 10)), 
			cex = 0.65, tck = 0.5)))

##### Example 9.9 - Trellis Plot

	ggplot(data29, aes(x = Time, y = Heart.Rate, colour = Treatment, 
		shape = Treatment)) + theme_bw() + geom_point() + 
		facet_wrap( ~ Subject) + xlab("Time (Secs)") + 
		ylab("Heart Rate\n") + 
		ggtitle("Heart Rate by Time and Treatment 
			per Subject\n") + 
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
			strip.text = element_text(size = 10, face = "bold"), 
			axis.text.x = element_text(size = 9, angle = 45, 
				hjust = 0.5, vjust = 0.5), 
			axis.text.y = element_text(size = 9, face = "bold"), 
			axis.title = element_text(size = 12, face = "bold"),
			panel.grid.major = element_line(colour = "grey90"))

##### Example 9.10 - Logistic Curve

	windows(6,6)
	ggplot(data25, aes(Concentration, Detection)) + geom_point() + 
		geom_line(aes(Concentration, Proportion), tmpModFit) + 
		xlab("Concentration") + ylab("Probability of Detection\n") + 
		ggtitle("Probability of Detection 
			by Concentration (95% CI)\n") + 
		guides(colour = FALSE) + theme_bw() + 
		geom_segment(aes(x = 0, y = 0.95, xend = 368, yend = 0.95),
			colour = "blue", linetype = "dotted") + 
		geom_segment(aes(x = 368, y = 0.95, xend = 368, yend = 0),
			colour = "blue", linetype = "dotted") + 
		annotate("text", x = 447, y = 0.1, colour = "blue",
			label = "Average \n Concentration for 
			95% Detection") + 
		geom_line(aes(lowerv50, prop), tempv50, colour = "red", 
			linetype = "dashed") + 
		geom_line(aes(upperv50, prop), tempv50, colour = "red", 
			linetype = "dashed") + 
		scale_x_continuous(limits = c(0,500), breaks = seq(0,500, 
			by = 50)) +
		scale_y_continuous(limits = c(0,1), breaks = seq(0,1, 
			by = 0.1)) + 
		theme(plot.title = element_text(size = 18, face = "bold"), 
			axis.text = element_text(size = 10, face = "bold"), 
			axis.title = element_text(size = 12, face = "bold"),
			panel.grid.major = element_line(colour = "grey90"))

