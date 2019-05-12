treatments <- list("rxjs", "mostjs")
exp_units <- list("absence", "all", "always", "any", "avgDistance", "count", "decreasing", "increasing",
"maxDistance", "minDistance", "mixed", "nHighestValues", "nLowestValues", "nonDecreasing", "nonIncreasing",
"relativeAvgDistance", "relativeMaxDistance", "relativeMinDistance", "sometimes", "stable", "valueAvg", "valueMax",
"valueMin")

frame <- data.frame()

for(exp_unit in exp_units) {
   for(treatment in treatments){
		path <- paste("./analysis/", treatment, "/", exp_unit , ".csv", sep = "")
		
		myData <- read.csv(file=path, header=FALSE, sep=",")
		
		nTest <- shapiro.test(as.vector(t(myData)))
		
		normal <- "true"
		if(nTest$p.value < 0.05){
			normal <- "false"
		}
		
		frame <- rbind(frame, data.frame(
			experimental_unit = exp_unit,
			treatment = treatment,
			statistic = formatC(nTest$statistic, digits = 5, format= "f"),
			p.value = formatC(nTest$p.value, digits = 5, format= "f"),
			normality = normal
		)
		)
		
   }
}

write.csv(frame,"normal_testing.csv", row.names = FALSE)