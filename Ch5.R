##############################################################################
######################## Translating Statistics R Code #######################
##############################################################################

##### Note - to run code in R either copy and paste into the R Console window
##### or place cursor anywhere on the line, hold Ctrl and press r
##### Code may require datasets loaded from previous Chapter scripts

##############################################################################
###################### Chapter 5 - Measuring Uncertainty #####################
##############################################################################

##### Confidence Intervals

	j = rnorm(100000, mean = 0, sd = 1)
	dj = density(j)
	q2l = quantile(j, 0.05); q1l = quantile(j, 0.1); q2u = quantile(j, 0.95)
	q2l; q1l; q2u

	jdata = with(dj, data.frame(x,y))

	k = qplot(x, y, data = jdata, geom = "line") + theme_bw() + 
		xlab("") + ylab("Density\n") + 
		ggtitle("Division of Risk for Two-Sided 
		Interval\n") + 
		scale_x_continuous(limits = c(-4,4), breaks = seq(-4,4, by = 1)) + 
		scale_y_continuous(limits = c(-0.025,0.41)) + 
		geom_ribbon(data = subset(data2, x < q2l), aes(ymax = y),
			ymin = 0, fill = "firebrick3", colour = NA, alpha = 0.5) + 
		geom_ribbon(data = subset(data2, x > q2u), aes(ymax = y),
			ymin = 0, fill = "firebrick3", colour = NA, alpha = 0.5) + 
		annotate("text", x = -2.8, y = -0.018, label = "5%") + 

# Replace xend = -1.639917 to the value of q2l

		geom_segment(aes(x = -4, y = -0.008, xend = -1.639917, 
			colour = "firebrick3", yend = -0.008)) + 
		annotate("text", x = 2.8, y = -0.018, label = "5%") + 

# Replace x = 1.644374 to the value of q2u

		geom_segment(aes(x = 1.644374, y = -0.008, xend = 4, 
			yend = -0.008), colour = "firebrick3") + 
		theme(legend.position = "none")

	l = qplot(x, y, data = jdata, geom = "line") + theme_bw() + 
		xlab("") + ylab("Density\n") + 
		ggtitle("Division of Risk for One-Sided 
		Interval\n") + 
		scale_x_continuous(limits = c(-4,4), breaks = seq(-4,4, by = 1)) + 
		scale_y_continuous(limits = c(-0.025,0.41)) + 
		geom_ribbon(data = subset(data2, x < q1l), aes(ymax = y),
			ymin = 0, fill = "forestgreen", colour = NA, alpha = 0.5) + 
		annotate("text", x = -2.5, y = -0.018, label = "10%") + 

# Replace x = -1.275591 to the value of q1l

		geom_segment(aes(x = -1.275591, y = -0.008, xend = -4, 
			yend = -0.008), colour = "forestgreen") + 
		theme(legend.position = "none")

	windows(8,6)
	grid.arrange(k, l, nrow = 1, ncol = 2)

##### Example 5.1 - Continuous CIs

	data7 = c(26.33, 27.31, 27.38, 26.63, 26.87, 26.67, 28.36, 28.52, 
		26.91, 28.90, 27.99, 27.17, 28.32, 26.93, 26.93, 26.65, 
		27.73, 26.93)

	x = mean(data7); s = sd(data7); n = length(data7); c = 0.90
	x; s; n; c

	se = s/sqrt(n)
	t2 = qt(c + (1 - c)/2, df = n - 1)
	error2 = se*t2; error2

	t1 = qt(c, df = n - 1)
	error1 = se*t1; error1

	lower.2s.CI = x - error2; upper.2s.CI = x + error2
	lower.1s.CI = x - error1
	lower.2s.CI; upper.2s.CI

	lower.1s.CI

	library(Rmisc)

	CI(data7, ci = 0.90)
	CI(data7, ci = 0.80)

##### Example 5.2 - Continuous CIs

	CI(data7, ci = 0.70)

	data8 = c(data7, 26.01, 28.33, 26.62, 26.99, 27.48, 27.74, 27.89)

	CI(data8, ci = 0.90)

##### Example 5.3 - Transformed CIs

	data9 = c(9.2, 7.4, 10.7, 3.6, 4.3, 3.2, 14.2, 30.1, 15.7, 6.8, 8.9, 
		9.1, 8.2, 7.5, 7.4, 14.9, 19.7, 26.3, 6.4, 14.2, 8.3, 6.9, 
		8.5, 11.5, 22.7, 16.9, 31.4, 10.7, 17.9, 10.0)
	data10 = log10(data9)

	qqnorm(data10); qqline(data10)

	ci = CI(data10, ci = 0.95); ci
	10^ci

	CI(data9, ci = 0.95)

##### Example 5.4 - Binary CIs

	x = 20; n = 25; alpha = 0.10
	x/n

	df1l = 2*(n - x + 1); df2l = 2*x
	df1u = df2l + 2; df2u = df1l - 2

	lci = ifelse(x > 0, x / (x + qf(1 - alpha/2, df1l, df2l) * 
		(n - x + 1)), 0)
	uci = ifelse(x < n, ((x + 1) * qf(1 - alpha/2, df1u, df2u)) / 
		(n - x + (x + 1) * qf(1 - alpha/2, df1u, df2u)), 1)
	lci; uci

	uci1 = ifelse(x < n, ((x + 1) * qf(1 - alpha, df1u, df2u)) / 
		(n - x + (x + 1) * qf(1 - alpha, df1u, df2u)), 1)
	uci1

	library(Hmisc)

	binconf(x = 20, n = 25, alpha = 0.1, method = "exact")
	binconf(x = 20, n = 25, alpha = 0.2, method = "exact")

##### Example 5.5 - Binary CIs

	binconf(x = 20, n = 25, alpha = 0.3, method = "exact")
	binconf(x = 4, n = 5, alpha = 0.1, method = "exact")

##### Example 5.6 - Continuous TIs

	x = mean(data7); s = sd(data7); n = length(data7); P = 0.75 
	conf = 0.9; x; s; n; P; conf

	n2 = (n - 1)*(1 + 1/n)
	ncrit = (qnorm((1 - P)/2))^2
	ccrit = qchisq(1 - conf, n - 1)
	k2 = sqrt((n2*ncrit)/ccrit); k2

	lower.2s.TI = x - k2*s
	upper.2s.TI = x + k2*s
	lower.2s.TI; upper.2s.TI

	ncritcov = qnorm(P)
	ncp = sqrt(n) * ncritcov
	tcrit = qt(conf, df = n - 1, ncp = ncp)
	k1 = tcrit/sqrt(n); k1

	lower.1s.TI = x - k1*s; lower.1s.TI

	library(tolerance)

	normtol.int(data7, alpha = 0.1, P = 0.75, side = 2, method = "HE2")
	normtol.int(data7, alpha = 0.1, P = 0.75, side = 1)

##### Example 5.7 - Continuous TIs

	normtol.int(data7, alpha = 0.3, P = 0.75, side = 2, method = "HE2")
	normtol.int(data7, alpha = 0.1, P = 0.95, side = 2, method = "HE2")

##### Example 5.8 - Binary TIs

	x = 20; n = 25; P = 0.75; alpha = 0.10
	alpha = alpha/2; P = (P + 1)/2

	lower.p = (1 + ((n - x + 1) * qf(1 - alpha, df1 = 2 * (n - x + 1), 
		df2 = (2 * x)))/x)^(-1)
	upper.p = (1 + (n - x)/((x + 1) * qf(1 - alpha, df1 = 2 * (x + 1), 
		df2 = 2 * (n - x))))^(-1)
	lower.p = max(0, lower.p); upper.p = min(upper.p, 1)
	lower = qbinom(1 - P, size = n, prob = lower.p)
	upper = qbinom(P, size = n, prob = upper.p)
	lower; upper

	x = 20; n = 25; P = 0.75; alpha = 0.10

	lower.p = (1 + ((n - x + 1) * qf(1 - alpha, df1 = 2 * (n - x + 1), 
		df2 = (2 * x)))/x)^(-1)
	lower.p = max(0, lower.p)
	lower = qbinom(1 - P, size = n, prob = lower.p)
	lower 

	bintol.int(x = 20, n = 25, P = 0.75, alpha = 0.1, side = 2, 
		method = "CP")
	bintol.int(x = 20, n = 25, P = 0.75, alpha = 0.1, side = 1, 
		method = "CP")

##### Example 5.9 - Binary TIs

	bintol.int(x = 20, n = 25, P = 0.75, alpha = 0.3, side = 2, 
		method = "CP")
	bintol.int(x = 20, n = 25, P = 0.95, alpha = 0.1, side = 2, 
		method = "CP")

##### CI, TI, PI

	Conc = c(3.9, 3.8, 3.6, 4.2, 5.7, 5, 5.5, 3.7, 4.9, 4, 6, 5)
	Yield = c(498, 480.3, 476.4, 546, 715.4, 666, 741.2, 522, 683.6, 574, 
		804, 637)
	m = data.frame(Conc, Yield)

	mod = lm(Yield ~ Conc, data = m)

	newdata = data.frame(Conc = c(3, 3.6, 3.7, 3.8, 3.9, 4, 4.2, 4.5, 4.9, 
		5, 5, 5.5, 5.7, 6, 8))

	pi = data.frame(predict(mod, newdata, interval = "prediction"))
	ci = data.frame(predict(mod, newdata, interval = "confidence"))
	ti = data.frame(regtol.int(mod, alpha = 0.05, P = 0.95, side = 2, 
		new.x = cbind(c(3, 4.5, 8))))

	x = newdata
	127.8*3; 127.8*4.5; 127.8*8

# Yield ordered plus the three new values

	y = c(383.4, 476.4, 522, 480.3, 498, 574, 546, 575.1, 683.6, 666, 
		637, 741.2, 715.4, 804, 1022.4)	
	lci = ci$lwr; uci = ci$upr; lpi = pi$lwr; upi = pi$upr
	lti = ti$X2.sided.lower; uti = ti$X2.sided.upper

	n = data.frame(x, y, lci, uci, lti, uti, lpi, upi); n

	ggplot(n, aes(x = x, y = y)) + theme_bw() + 
		geom_abline(intercept = 23.2, slope = 127.8, 
			colour = "dodgerblue3", size = 1.5) +
		geom_line(aes(x = x, y = lci), n, colour = "firebrick3", 
			size = 1) + 
		geom_line(aes(x = x, y = uci), n, colour = "firebrick3", 
			size = 1) +
		geom_line(aes(x = x, y = lti), n, colour = "forestgreen", 
			size = 1) +
		geom_line(aes(x = x, y = uti), n, colour = "forestgreen", 
			size = 1) +
		geom_line(aes(x = x, y = lpi), n, colour = "orange", size = 1) +
		geom_line(aes(x = x, y = upi), n, colour = "orange", size = 1) + 
		scale_x_continuous(limits = c(3,8.1), breaks = seq(3,8, by = 1)) + 
		scale_y_continuous(limits = c(275,1200), breaks = seq(300,1200, 
		by = 100)) + xlab("Concentration") + ylab("Yield") + 
		ggtitle("Yield by Concentration with Intervals") + 
		theme(plot.title = element_text(size = 16, face = "bold"), 
		strip.text = element_text(size = 12, face = "bold"), 
		axis.text.x = element_text(size = 10, face = "bold.italic"), 
		axis.text.y = element_text(size = 9, face = "bold"),
		axis.title = element_text(size = 12, face = "bold")) +
 		geom_point(size = 2) + 		
		geom_segment(aes(x = 5.5, y = 525, xend = 5.75, 
			yend = 525), col = "dodgerblue3", size = 2) +
		annotate("text", x = 7, y = 525, cex = 4.25,
			label = "Linear Model") + 
		geom_segment(aes(x = 5.5, y = 475, xend = 5.75, 
			yend = 475), col = "firebrick3", size = 2) +
		annotate("text", x = 7, y = 475, cex = 4.25,
			label = "95% Confidence Intervals") + 
		geom_segment(aes(x = 5.5, y = 400, xend = 5.75, 
			yend = 400), col = "orange", size = 2) +
		annotate("text", x = 7, y = 400, cex = 4.25,
			label = "95% Confidence 
			Prediction Intervals") + 
		geom_segment(aes(x = 5.5, y = 315, xend = 5.75, 
			yend = 315), col = "forestgreen", size = 2) +
		annotate("text", x = 7, y = 315, cex = 4.25,
			label = "95% Confidence, 95% Coverage
			Tolerance Intervals") + 
		geom_segment(aes(x = 5.4, y = 275, xend = 5.4, 
			yend = 540), size = 1, lty = 2) +
		geom_segment(aes(x = 5.4, y = 275, xend = 8.09, 
			yend = 275), size = 1, lty = 2) +
		geom_segment(aes(x = 8.09, y = 275, xend = 8.09, 
			yend = 540), size = 1, lty = 2) +
		geom_segment(aes(x = 5.4, y = 540, xend = 8.09, 
			yend = 540), size = 1, lty = 2)

