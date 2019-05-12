
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
		frame <- rbind(frame, data.frame(
			experimental_unit = exp_unit,
			treatment = treatment,
			mean = formatC(rowMeans(myData), digits = 5, format= "f"),
			standard_deviation = formatC(apply(myData, 1, sd), digits = 5, format= "f"),
			median = apply(myData, 1, median),
			min = apply(myData, 1, min),
			max = apply(myData, 1, max)
		)
		)
		
   }
}

write.csv(frame,"descriptive_statistics.csv", row.names = FALSE)