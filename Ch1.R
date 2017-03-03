##############################################################################
######################## Translating Statistics R Code #######################
##############################################################################

##### Note - to run code in R either copy and paste into the R Console window
##### or place cursor anywhere on the line, hold Ctrl and press r

##############################################################################
###################### Chapter 1 - Design of Experiments #####################
##############################################################################

##### Example 1.1

	library(pwr)

	pwr.t.test(n = NULL, d = (1/0.81), sig.level = 0.05, power = 0.90, 
		type = "two.sample", alternative = "two.sided")

##### Example 1.2

	pwr.2p.test(n = 10, h = ES.h(0.80, 0.90), sig.level = 0.10, 
		power = NULL, alternative = "two.sided")

##### Example 1.4

	Test = rep(c("Test1", "Test2"), each = 20)
	Assignment = sample(Test, 40, replace = FALSE)
	head(Assignment)

	data = data.frame(Type = rep(c("Army", "Navy"), each = 20), 
	Participant = sample(1:40, 40))

	sp = split(seq_len(nrow(data)), data$Type)
	samples = lapply(sp, sample, 10)
	data = data[unlist(samples), ]

	data2 = data[order(data$Participant), ]
	head(data2)

##### Example 1.5

	library(AlgDesign)

	des = gen.factorial(levels = 2, nVars = 4, center = FALSE, 
	varNames = c("Concentration", "Interferent", "Chemical", "Container"))
	des

	half.des = optFederov(data = des, nTrials = 8, approximate = FALSE)
	half.des$design

	op.des = optFederov(data = des, nTrials = 11, approximate = FALSE)
	op.des$design

