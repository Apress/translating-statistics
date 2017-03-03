##############################################################################
######################## Translating Statistics R Code #######################
##############################################################################

##### Note - to run code in R either copy and paste into the R Console window
##### or place cursor anywhere on the line, hold Ctrl and press r

##############################################################################
###################### Chapter 8 - Multivariate Analysis #####################
##############################################################################

##### Example 8.1 - MANOVA

	Point.X = c(-43,-40,-28,16,-30,-29,-44,-36,-32,-31,2,-8,8,12,-5,-6,
		11,-7,11,22,19,5,-1,16,2,15,-17,-7,6,-15,15,-68,2,-80,-26,
		-42,-43,-46,-52,-22,-67,-25,-80,-23,-54,-62,-83,4,-46,-12,
		33,8,8,23,18,15,14,-24,2,-8,15,32,34,15,-14,-7,-6,12,-5,42,
		-19,9,-10,7,45,-8,-31,2,-10,8,10,6,14,-12,33,22,-12,-7,10,
		0,-13,-77,-54,-95,-85,-67,-61,-56,-69,-27,-87,-85,-55,-80,
		8,-13,-75,-120,-77,-13,-50,60,-3,-25,35,56,22,45,-50,35,42,
		9,-13,52,-10,52,35,0,-15,62,25,17,-17,-23,10,11,34,10,35,
		-17,51,5,-31,32,-37,0,-7,-2,11,-62,-103,48,15,-55,-62,20,
		-56,-114,56,-21,-142,10,-65,-76,-87,-105,-168,-66,58,-100,4,
		74,-55,-68,43,-44,63,15,-63,31,95,23,-79,-9,12,15,70,48,-5,
		96,-49,-12,0,75,31,-10,21,34,17,-32,-18,6,-20,20,18,16,60,
		33,4,-29)
	Point.Y = c(76,42,25,11,44,27,39,17,38,39,55,28,36,33,37,38,26,45,
		25,37,54,22,36,40,90,41,47,63,82,56,9,52,-30,-20,-30,29,25,
		33,-48,-2,13,35,47,34,-35,3,26,10,-25,16,-5,15,-20,24,1,
		-12,-14,15,35,-16,15,-44,20,-21,-20,4,2,27,58,-22,-30,-8,0,
		45,-24,-29,19,-53,12,0,25,-44,54,-26,6,-18,-47,-9,-3,-53,
		-43,-6,-66,-39,-14,-33,-6,-46,-91,-37,-53,-56,-65,-101,-24,
		-43,-58,-29,-31,-43,-37,-17,-75,20,-87,-30,-100,-88,25,-55,
		-91,-92,-27,-42,-81,-11,27,-45,-58,-65,-129,16,-48,-98,-4,
		-103,-171,-12,-23,-107,-100,-148,-48,19,-32,-35,-35,-52,-46,
		-142,-210,-176,-73,-81,-195,-157,-120,-116,-112,-58,-95,
		-135,-143,-135,-166,-120,-118,-141,-201,-254,-123,-91,-110,
		-143,-145,-232,-185,-166,-198,-122,-183,-96,-165,-113,-158,
		-120,-193,-220,-104,-93,-239,-171,-74,-180,-169,-127,-201,
		-154,-211,-122,0,3,-101,-143,-43,-158,-250,-136,-131,-130)
	Dist = rep(c(5,10,15,20), c(30,60,60,60))
	W1 = rep(c("A","B","C"), each = 10); W2 = rep(c("A","B","C"), 
		each = 20, 3)
	Weapon = c(W1, W2)
	data31 = data.frame(Point.X, Point.Y, Weapon, Dist)
	data31$Dist = factor(data31$Dist)

	library(ggplot2)
	windows(5,5)
	ggplot(data31, aes(x = Point.X, y = Point.Y, colour = Weapon, 
		shape = Weapon)) + facet_wrap( ~ Dist, ncol = 2) + 
		geom_point() + theme_bw()

	windows(5,5)
	ggplot(data31, aes(x = Point.X, y = Point.Y, colour = Dist, 
		shape = Dist)) + facet_wrap( ~ Weapon, ncol = 2) + 
		geom_point() + theme_bw()

	windows(5,5)
	ggplot(data31, aes(x = Point.X, y = Point.Y, colour = Weapon, 
		shape = Weapon)) + facet_wrap( ~ Dist, ncol = 2) + 
		geom_point() + theme_bw() + 
		guides(colour = guide_legend(ncol = 3)) + 
		ggtitle("X and Y Coordinates 
		by Distance and Weapon\n") + 
		xlab("X Coordinate (cm)") + ylab("Y Coordinate (cm)\n") + 
		scale_x_continuous(limits = c(-200,150), breaks = seq(-200,150, 
			by = 50)) + 
		scale_y_continuous(limits = c(-300,100), breaks = seq(-300,100, 
			by = 50)) +
		scale_colour_manual("Weapon", values = c("firebrick3", 
		"dodgerblue3", "forestgreen")) + 
		scale_shape_manual("Weapon", values = c(4,3,2)) + 
		theme(plot.title = element_text(size = 16, face = "bold"), 
		legend.position = c(0.155,0.645),
		legend.background = element_rect(colour = "black", 
			fill = "grey93",	linetype = "dashed"),
		legend.title = element_text(size = 10, face = "bold"),
		legend.text = element_text(size = 10),
		strip.text = element_text(size = 12, face = "bold"), 
		axis.text = element_text(size = 12, face = "bold"), 
		axis.text.x = element_text(size = 10, colour = "grey20", 
		angle = 45, face = "bold.italic", h = 1), 
		axis.text.y = element_text(size = 10, face = "bold"),
		axis.title = element_text(size = 12, face = "bold"))

	windows(5,5)
	ggplot(data31, aes(x = Point.X, y = Point.Y, colour = Dist, 
		shape = Dist)) + facet_wrap( ~ Weapon, ncol = 2) + 
		geom_point() + theme_bw() + 
		ggtitle("X and Y Coordinates 
		by Weapon and Distance\n") + 
		xlab("X Coordinate (cm)") + ylab("Y Coordinate (cm)\n") + 
		scale_x_continuous(limits = c(-200,150), breaks = seq(-200,150, 
			by = 50)) + 
		scale_y_continuous(limits = c(-300,100), breaks = seq(-300,100, 
			by = 50)) +
		scale_colour_manual("Distance", values = c("dodgerblue3", 
			"firebrick3", "forestgreen","darkorange")) + 
		scale_shape_manual("Distance", values = c(1:4)) + 
		theme(plot.title = element_text(size = 16, face = "bold"), 
		legend.position = c(0.75,0.25),
		legend.background = element_rect(colour = "black", 
			fill = "grey93",	linetype = "dashed"),
		legend.title = element_text(size = 10, face = "bold"),
		legend.text = element_text(size = 10),
		strip.text = element_text(size = 12, face = "bold"), 
		axis.text = element_text(size = 12, face = "bold"), 
		axis.text.x = element_text(size = 10, colour = "grey20", 
		angle = 45, face = "bold.italic", h = 1), 
		axis.text.y = element_text(size = 10, face = "bold"),
		axis.title = element_text(size = 12, face = "bold"))

	library(mvnormtest)
	mshapiro.test(t(data31[data31$Weapon == "A" & data31$Dist == "5", 
		1:2]))
	mshapiro.test(t(data31[data31$Weapon == "A" & data31$Dist == "10", 
		1:2]))
	mshapiro.test(t(data31[data31$Weapon == "A" & data31$Dist == "15", 
		1:2]))
	mshapiro.test(t(data31[data31$Weapon == "A" & data31$Dist == "20", 
		1:2]))

	mshapiro.test(t(data31[data31$Weapon == "B" & data31$Dist == "5", 
		1:2]))
	mshapiro.test(t(data31[data31$Weapon == "B" & data31$Dist == "10", 
		1:2]))
	mshapiro.test(t(data31[data31$Weapon == "B" & data31$Dist == "15", 
		1:2]))
	mshapiro.test(t(data31[data31$Weapon == "B" & data31$Dist == "20", 
		1:2]))

	mshapiro.test(t(data31[data31$Weapon == "C" & data31$Dist == "5", 
		1:2]))
	mshapiro.test(t(data31[data31$Weapon == "C" & data31$Dist == "10", 
		1:2]))
	mshapiro.test(t(data31[data31$Weapon == "C" & data31$Dist == "15", 
		1:2]))
	mshapiro.test(t(data31[data31$Weapon == "C" & data31$Dist == "20", 
		1:2]))

	library(biotools)
	boxM(data31[,1:2], data31$Weapon)
	boxM(data31[,1:2], data31$Dist)

	library(car)
	leveneTest(Point.X ~ Weapon, data = data31)
	leveneTest(Point.Y ~ Weapon, data = data31)
	leveneTest(Point.X ~ factor(Dist), data = data31)
	leveneTest(Point.Y ~ factor(Dist), data = data31)

	cor(data31$Point.X[data31$Weapon == "A" & data31$Dist == "5"], 
		data31$Point.Y[data31$Weapon == "A" & data31$Dist == "5"], 
		method = "spearman")
	cor(data31$Point.X[data31$Weapon == "A" & data31$Dist == "10"], 
		data31$Point.Y[data31$Weapon == "A" & data31$Dist == "10"], 
		method = "spearman")
	cor(data31$Point.X[data31$Weapon == "A" & data31$Dist == "15"], 
		data31$Point.Y[data31$Weapon == "A" & data31$Dist == "15"], 
		method = "spearman")
	cor(data31$Point.X[data31$Weapon == "A" & data31$Dist == "20"], 
		data31$Point.Y[data31$Weapon == "A" & data31$Dist == "20"], 
		method = "spearman")

	cor(data31$Point.X[data31$Weapon == "B" & data31$Dist == "5"], 
		data31$Point.Y[data31$Weapon == "B" & data31$Dist == "5"], 
		method = "spearman")
	cor(data31$Point.X[data31$Weapon == "B" & data31$Dist == "10"], 
		data31$Point.Y[data31$Weapon == "B" & data31$Dist == "10"], 
		method = "spearman")
	cor(data31$Point.X[data31$Weapon == "B" & data31$Dist == "15"], 
		data31$Point.Y[data31$Weapon == "B" & data31$Dist == "15"], 
		method = "spearman")
	cor(data31$Point.X[data31$Weapon == "B" & data31$Dist == "20"], 
		data31$Point.Y[data31$Weapon == "B" & data31$Dist == "20"], 
		method = "spearman")

	cor(data31$Point.X[data31$Weapon == "C" & data31$Dist == "5"], 
		data31$Point.Y[data31$Weapon == "C" & data31$Dist == "5"], 
		method = "spearman")
	cor(data31$Point.X[data31$Weapon == "C" & data31$Dist == "10"], 
		data31$Point.Y[data31$Weapon == "C" & data31$Dist == "10"], 
		method = "spearman")
	cor(data31$Point.X[data31$Weapon == "C" & data31$Dist == "15"], 
		data31$Point.Y[data31$Weapon == "C" & data31$Dist == "15"], 
		method = "spearman")
	cor(data31$Point.X[data31$Weapon == "C" & data31$Dist == "20"], 
		data31$Point.Y[data31$Weapon == "C" & data31$Dist == "20"], 
		method = "spearman")

	library(mvoutlier) 
	aq.plot(data31[data31$Weapon == "A" & data31$Dist == "5", 1:2])
	aq.plot(data31[data31$Weapon == "A" & data31$Dist == "10", 1:2])
	aq.plot(data31[data31$Weapon == "A" & data31$Dist == "15", 1:2])
	aq.plot(data31[data31$Weapon == "A" & data31$Dist == "20", 1:2])

	aq.plot(data31[data31$Weapon == "B" & data31$Dist == "5", 1:2])
	aq.plot(data31[data31$Weapon == "B" & data31$Dist == "10", 1:2])
	aq.plot(data31[data31$Weapon == "B" & data31$Dist == "15", 1:2])
	aq.plot(data31[data31$Weapon == "B" & data31$Dist == "20", 1:2])

	aq.plot(data31[data31$Weapon == "C" & data31$Dist == "5", 1:2])
	aq.plot(data31[data31$Weapon == "C" & data31$Dist == "10", 1:2])
	aq.plot(data31[data31$Weapon == "C" & data31$Dist == "15", 1:2])
	aq.plot(data31[data31$Weapon == "C" & data31$Dist == "20", 1:2])

	mod = manova(cbind(Point.X,Point.Y) ~ Dist*Weapon, data31)
	summary(mod)

	mod2 = manova(cbind(Point.X,Point.Y) ~ Dist + Weapon, data31)
	summary(mod2)

	summary.aov(mod2)

	library(RVAideMemoire)
	pairwise.perm.manova(data31[,1:2], data31$Dist, nperm = 500)

	pairwise.perm.manova(data31[,1:2], data31$Weapon, nperm = 500)

	windows(5,5)
	ggplot(data31, aes(x = Point.X, y = Point.Y, colour = Weapon, 
		shape = Weapon)) + geom_point() + theme_bw()

	windows(5,5)
	ggplot(data31, aes(x = Point.X, y = Point.Y, colour = Dist, 
		shape = Dist)) + geom_point() + theme_bw()

	windows(5,5)
	ggplot(data31, aes(x = Point.X, y = Point.Y, colour = Weapon, 
		shape = Weapon)) + geom_point() + theme_bw() + 
		ggtitle("X and Y Coordinates by Weapon\n") + 
		xlab("X Coordinate (cm)") + ylab("Y Coordinate (cm)\n") + 
		scale_x_continuous(limits = c(-200,150), breaks = seq(-200,150, 
			by = 50)) + 
		scale_y_continuous(limits = c(-300,100), breaks = seq(-300,100, 
			by = 50)) +
		scale_colour_manual("Weapon", values = c("firebrick3", 
			"dodgerblue3", "forestgreen")) + 
		scale_shape_manual("Weapon", values = c(4,3,2)) + 
		theme(plot.title = element_text(size = 16, face = "bold"), 
		legend.position = c(0.08,0.135),
		legend.background = element_rect(colour = "black", 
			fill = "grey93",	linetype = "dashed"),
		legend.title = element_text(size = 10, face = "bold"),
		legend.text = element_text(size = 10),
		strip.text = element_text(size = 12, face = "bold"), 
		axis.text = element_text(size = 12, face = "bold"), 
		axis.text.x = element_text(size = 10, colour = "grey20", 
		angle = 45, face = "bold.italic", h = 1), 
		axis.text.y = element_text(size = 10, face = "bold"),
		axis.title = element_text(size = 12, face = "bold"))


	windows(5,5)
	ggplot(data31, aes(x = Point.X, y = Point.Y, colour = Dist, 
		shape = Dist)) + geom_point() + theme_bw() + 
		ggtitle("X and Y Coordinates by Distance\n") + 
		xlab("X Coordinate (cm)") + ylab("Y Coordinate (cm)\n") + 
		scale_x_continuous(limits = c(-200,150), breaks = seq(-200,150, 
			by = 50)) + 
		scale_y_continuous(limits = c(-300,100), breaks = seq(-300,100, 
			by = 50)) +
		scale_colour_manual("Distance", values = c("dodgerblue3", 
			"firebrick3", "forestgreen","darkorange")) + 
		scale_shape_manual("Distance", values = c(1:4)) + 
		theme(plot.title = element_text(size = 16, face = "bold"), 
		legend.position = c(0.085,0.17),
		legend.background = element_rect(colour = "black", 
			fill = "grey93",	linetype = "dashed"),
		legend.title = element_text(size = 10, face = "bold"),
		legend.text = element_text(size = 10),
		strip.text = element_text(size = 12, face = "bold"), 
		axis.text = element_text(size = 12, face = "bold"), 
		axis.text.x = element_text(size = 10, colour = "grey20", 
		angle = 45, face = "bold.italic", h = 1), 
		axis.text.y = element_text(size = 10, face = "bold"),
		axis.title = element_text(size = 12, face = "bold"))

	library(shotGroups)
	data31$Series = data31$Weapon
	compareGroups(data31, xyTopLeft = FALSE, CEPlevel = 0.75)

	data31$Series = data31$Dist
	compareGroups(data31, xyTopLeft = FALSE, CEPlevel = 0.75)

##### Example 8.2 - PCA

	English = c(75,71,86,85,40,61,87,94,73,63,59,73,69,82,56,74,55,95,
		87,49,51,72,81,60)
	Maths = c(48,90,70,64,70,77,62,68,88,60,76,67,74,50,72,86,76,52,48,
		88,95,31,39,51)
	Chemistry = c(50,95,74,63,68,71,78,52,88,90,63,85,82,61,65,84,62,
		53,46,92,82,35,43,53) 
	Biology = c(60,77,79,67,61,80,71,74,72,99,64,56,87,64,90,79,61,48,
		55,84,84,39,37,54)
	Physics = c(52,90,71,61,73,72,54,55,92,57,80,62,99,52,83,76,60,45,
		46,86,92,32,36,52)
	French = c(80,70,87,81,53,85,93,96,54,97,64,81,55,62,61,81,75,91,95,
		56,60,76,79,64) 
	Spanish = c(76,75,85,80,41,57,79,85,89,69,61,64,77,83,69,82,50,96,
		86,48,50,71,82,63)
	History = c(81,67,79,90,48,81,99,92,69,51,54,96,62,81,73,40,35,70,
		68,78,71,76,84,94) 
	Geography = c(55,65,75,85,75,60,63,50,70,78,60,76,67,59,61,45,41,
		55,46,88,92,79,73,95)
	Art = c(59,83,76,95,78,66,85,76,59,55,53,65,63,86,54,41,20,51,40,90,
		99,74,87,98)
	data32 = data.frame(English, Maths, Chemistry, Biology, Physics, 
		French, Spanish, History, Geography, Art)

	library(corrplot)
	cor.mat = round(cor(data32),2)
	corrplot(cor.mat, type = "lower", order = "FPC", tl.col = "black", 
		tl.srt = 45, diag = F, outline = T)

	library(FactoMineR)
	p = PCA(data32, scale.unit = T, graph = F)
	plot(p, choix = "var")

	p$eig

	library(factoextra)
	windows(5,5)
	fviz_screeplot(p, choice = "variance")
	windows(5,5)
	fviz_screeplot(p, choice = "eigenvalue", geom = "line")

	windows(5,5)
	fviz_screeplot(p, choice = "variance") + theme_bw() +
	ggtitle("Scree Plot\n") + ylab("Percentage of Explained Variance\n") + 
	scale_y_continuous(limits = c(0,45), breaks = seq(0,45, 5)) +
	theme(plot.title = element_text(size = 16, face = "bold"), 
		axis.text = element_text(size = 12, face = "bold"), 
		axis.text.x = element_text(size = 10, face = "bold"), 
		axis.text.y = element_text(size = 10, face = "bold"),
		axis.title = element_text(size = 12, face = "bold"))

	windows(5,5)
	fviz_screeplot(p, choice = "eigenvalue", geom = "line") + theme_bw() + 
	geom_hline(yintercept = 1, linetype = "dashed", colour = "red") +
	ggtitle("Scree Plot\n") + ylab("Eigenvalue\n") + 
	theme(plot.title = element_text(size = 16, face = "bold"), 
		axis.text = element_text(size = 12, face = "bold"), 
		axis.text.x = element_text(size = 10, face = "bold"), 
		axis.text.y = element_text(size = 10, face = "bold"),
		axis.title = element_text(size = 12, face = "bold"))

	p2 = PCA(data32, scale.unit = T, graph = F, ncp = 3)
	p2$var$coord

	dimdesc(p2, axes = 1:2)
	dimdesc(p2, axes = 2:3)

	loadings = as.data.frame(p2$var$coord) 
	loadings$var = colnames(data32) 

	library(ggplot2)
	windows(5,5)
	ggplot(loadings, aes(x = var, y = Dim.1)) + 
		geom_bar(stat = "identity")
	windows(5,5)
	ggplot(loadings, aes(x = var, y = Dim.2)) + 
		geom_bar(stat = "identity")
	windows(5,5)
	ggplot(loadings, aes(x = var, y = Dim.3)) + 
		geom_bar(stat = "identity")

	windows(5,5)
	ggplot(loadings, aes(x = var, y = Dim.1)) + 
		geom_bar(stat = "identity") +	xlab("School Subjects") + 
		ylab("Dimension 1\n") + theme_bw() + 
		ggtitle("Variable Coordinates for Dimension 1\n") + 
		scale_y_continuous(limits = c(-1,1), breaks = seq(-1,1, 
			by = 0.2)) + 
		theme(plot.title = element_text(size = 16, face = "bold"), 
			axis.text = element_text(size = 12, face = "bold"), 
			axis.text.x = element_text(size = 10, face = "bold", 
				angle = 45, h = 1), 
			axis.text.y = element_text(size = 10, face = "bold"),
			axis.title = element_text(size = 12, face = "bold"))

	windows(5,5)
	ggplot(loadings, aes(x = var, y = Dim.2)) + 
		geom_bar(stat = "identity") +	xlab("School Subjects") + 
		ylab("Dimension 2\n") + theme_bw() + 
		ggtitle("Variable Coordinates for Dimension 2\n") + 
		scale_y_continuous(limits = c(-1,1), breaks = seq(-1,1, 
			by = 0.2)) + 
		theme(plot.title = element_text(size = 16, face = "bold"), 
			axis.text = element_text(size = 12, face = "bold"), 
			axis.text.x = element_text(size = 10, face = "bold", 
				angle = 45, h = 1), 
			axis.text.y = element_text(size = 10, face = "bold"),
			axis.title = element_text(size = 12, face = "bold"))

	windows(5,5)
	ggplot(loadings, aes(x = var, y = Dim.3)) + 
		geom_bar(stat = "identity") +	xlab("School Subjects") + 
		ylab("Dimension 3\n") + theme_bw() + 
		ggtitle("Variable Coordinates for Dimension 3\n") + 
		scale_y_continuous(limits = c(-1,1), breaks = seq(-1,1, 
			by = 0.2)) + 
		theme(plot.title = element_text(size = 16, face = "bold"), 
			axis.text = element_text(size = 12, face = "bold"), 
			axis.text.x = element_text(size = 10, face = "bold", 
				angle = 45, h = 1), 
			axis.text.y = element_text(size = 10, face = "bold"),
			axis.title = element_text(size = 12, face = "bold"))

##### Example 8.3 - Q-Methodology

	S1 = c(0,1,2,3,2,1,-1,0,-2,0,0,1,-1,-1,-3,-2)
	S2 = c(1,0,2,2,3,1,-2,0,0,-2,-1,-1,-1,-3,0,1)
	S3 = c(1,-1,3,1,2,-2,0,0,-1,-3,0,1,0,-1,-2,2)
	S4 = c(1,-2,1,0,3,2,-1,-2,0,0,0,2,-1,-1,-3,1)
	S5 = c(0,0,0,-1,1,-2,1,3,0,-3,1,2,-1,2,-2,-1)
	S6 = c(3,0,2,1,2,-2,1,-1,0,0,-1,-2,1,-1,-3,0)
	S7 = c(2,-1,3,1,0,-2,0,-1,1,-3,0,1,0,2,-2,-1) 
	S8 = c(2,0,1,2,3,0,-1,-2,-1,-2,0,1,-1,-3,0,1)
	S9 = c(1,-2,1,0,2,0,-1,-1,2,1,-1,-2,-3,0,0,3)
	S10 = c(2,-1,1,1,3,-1,0,-2,0,0,0,-3,-1,-2,1,2)
	data33 = data.frame(S1,S2,S3,S4,S5,S6,S7,S8,S9,S10)

	data34 = reshape(data33, varying = list(1:10), idvar = "Statement", 
	timevar = "Participant", direction = "long", 
	v.names = "Response")
	data34$Participant = factor(data34$Participant)

	library(ggplot2)
	windows(6,6)
	ggplot(data34, aes(y = Response, x = Participant, 
		fill = Participant)) + geom_bar(stat = "identity") + 
		facet_wrap(~Statement) + theme_bw()

	windows(6,6)
	ggplot(data34, aes(y = Response, x = Participant, fill = Participant)) + 
		guides(fill = F) + ylab("Response\n") + 
		ggtitle("Response Scores by Statement\n") + 
		geom_bar(stat = "identity", colour = "black") + 
		facet_wrap(~Statement) + theme_bw() + 
		scale_y_continuous(limits = c(-3,3), breaks = seq(-3,3, 
			by = 1)) + 
		scale_fill_manual("Participant", values = c("firebrick3",
			"darkorange","yellow","darkolivegreen1","forestgreen",
			"cadetblue1","dodgerblue3","midnightblue","purple",
			"maroon1")) + 
		theme(plot.title = element_text(size = 16, face = "bold"), 
			axis.text = element_text(size = 12, face = "bold"), 
			axis.text.x = element_text(size = 10, face = "bold"), 
			axis.text.y = element_text(size = 10, face = "bold"),
			axis.title = element_text(size = 12, face = "bold"),
			strip.text = element_text(size = 10, face = "bold"))

	library(qmethod)
	qm = qmethod(data33, nfactors = 3, rotation = "varimax")
	summary(qm)

	qm$loa
	qm$flagged

	qm$qdc

	plot(qm, legend = T)

	windows(5,5)
	plot(qm, xlab = "z-scores", ylab = "Statements", legend = F,
		main = c("Q-method z-scores"), cex.lab = 1.2,
		colours = c("dodgerblue3","firebrick3","forestgreen"))
	legend(-2.15, 3.5, legend = c("Factor 1", "Factor 2", "Factor 3"),
      	col = c("dodgerblue3","firebrick3","forestgreen"), 
		pch = c(1,2,0), cex = 0.8, bg = "white")

