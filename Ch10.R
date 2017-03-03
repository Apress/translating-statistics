##############################################################################
######################## Translating Statistics R Code #######################
##############################################################################

##### Note - to run code in R either copy and paste into the R Console window
##### or place cursor anywhere on the line, hold Ctrl and press r
##### Code may require datasets loaded from previous Chapter scripts

##############################################################################
################# Chapter 10 - Translation and Communication #################
##############################################################################

##### Translation Example 2

	results = matrix(c(25, 5, 16, 14), ncol = 2, byrow = TRUE)
	rownames(results) = c("A", "B")
	colnames(results) = c("Cured", "Not Cured")
	Table4 = as.table(results)
	y = data.frame(Table4)

	library(ggplot2)
	windows(6,6)
	ggplot(y, aes(x = Var1, y = Freq, fill = Var2)) + theme_bw() +
		geom_bar(stat = "identity", position = "dodge", 
			colour = "black") + 
		ggtitle("Outcome by Treatment\n") + 
		geom_vline(xintercept = 1.5, colour = "grey60") + 
		xlab("Treatment") + ylab("Count\n") + 
		scale_y_continuous(limits = c(0,30), breaks = seq(0,30, 
			by = 2)) + 
		scale_fill_manual("Outcome", values = c("dodgerblue3",
			"firebrick3")) + 
		geom_text(aes(y = Freq, label = paste(round(Freq/30*100,1),"%")), 
			position = position_dodge(width = 0.85), size = 4.5,
			vjust = 1.5) +
		theme(plot.title = element_text(size = 16, face = "bold"), 
			legend.position = c(0.9,0.91),
			legend.background = element_rect(linetype = "dashed", 
				colour = "black"),
			legend.title = element_text(size = 10, face = "bold"), 
			legend.text = element_text(size = 10),
			axis.text.x = element_text(size = 10, face = "bold"), 
			panel.grid.major = element_line(colour = "grey90"),
			axis.text.y = element_text(size = 10, face = "bold"), 
			axis.title = element_text(size = 12, face = "bold"))

##### Translation Example 3

	windows(6,6)
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

##### Translation Example 4

	windows(6,6)
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

