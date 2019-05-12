
exp_units <- list("absence", "all", "always", "any", "avgDistance", "count", "decreasing", "increasing",
"maxDistance", "minDistance", "mixed", "nHighestValues", "nLowestValues", "nonDecreasing", "nonIncreasing",
"relativeAvgDistance", "relativeMaxDistance", "relativeMinDistance", "sometimes", "stable", "valueAvg", "valueMax",
"valueMin")

frame <- data.frame()

for(exp_unit in exp_units) {
   
		path1 <- paste("./analysis/rxjs/", exp_unit , ".csv", sep = "")
		path2 <- paste("./analysis/mostjs/", exp_unit , ".csv", sep = "")
		
		myData1 <- read.csv(file=path1, header=FALSE, sep=",")
		myData2 <- read.csv(file=path2, header=FALSE, sep=",")
		
		
		nTest <- wilcox.test(as.vector(t(myData1)), as.vector(t(myData2)), alternative = "greater",
            mu = 0, paired = FALSE, exact = NULL, correct = FALSE,
            conf.int = FALSE, conf.level = 0.95)
		
		H0 <- "true"
		if(nTest$p.value < 0.05){
			H0 <- "false"
		}
		
		frame <- rbind(frame, data.frame(
			experimental_unit = exp_unit,
			statistic = nTest$statistic,
			p.value = nTest$p.value,
			null_hypothesis = H0
		)
		)
		
}

write.csv(frame,"hypothesis_testing.csv", row.names = FALSE)