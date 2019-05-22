
exp_units <- list("absence", "all", "always", "any", "avgDistance", "count", "decreasing", "increasing",
"maxDistance", "minDistance", "mixed", "nHighestValues", "nLowestValues", "nonDecreasing", "nonIncreasing",
"relativeAvgDistance", "relativeMaxDistance", "relativeMinDistance", "sometimes", "stable", "valueAvg", "valueMax",
"valueMin")

frame <- data.frame()

for(exp_unit in exp_units){
	path <- paste("./analysis/By Combination/", exp_unit , ".csv", sep = "")
	
	data <- read.csv(file=path, header=TRUE, sep=",")
	
	cnt <- 1
	
	while (cnt < 5) {
	
		frame <- rbind(frame, data.frame(
			experimental_unit = exp_unit,
			combination = cnt,
			min = aggregate( consumption ~ combination, data, min )$consumption[cnt],
			max = aggregate( consumption ~ combination, data, max )$consumption[cnt],
			mean = formatC(aggregate( consumption ~ combination, data, mean )$consumption[cnt], digits = 3, format= "f"),
			standard_deviation = formatC(aggregate( consumption ~ combination, data, sd )$consumption[cnt], digits = 3, format= "f")
			)
		)
		
		cnt = cnt + 1
	
	}
}

write.csv(frame,"descriptive_statistics.csv", row.names = FALSE)
