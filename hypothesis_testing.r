
exp_units <- list("absence", "all", "always", "any", "avgDistance", "count", "decreasing", "increasing",
"maxDistance", "minDistance", "mixed", "nHighestValues", "nLowestValues", "nonDecreasing", "nonIncreasing",
"relativeAvgDistance", "relativeMaxDistance", "relativeMinDistance", "sometimes", "stable", "valueAvg", "valueMax",
"valueMin")

frame <- data.frame()

for(exp_unit in exp_units){
	path <- paste("./analysis/By Treatment/", exp_unit , ".csv", sep = "")
	
	data <- read.csv(file=path, header=TRUE, sep=",")
	
	test <- wilcox.test(consumption ~ treatment, data, paired=FALSE)
	
	ranks = aggregate(rank(consumption)~treatment, data, sum )
	
	h0 <- FALSE
	if(test$p.value > 0.05){
		h0 <- TRUE
	}
	
	frame <- rbind(frame, data.frame(
			experimental_unit = exp_unit,
			statistics = test$statistic,
			p_value = formatC(test$p.value, digits = 3, format= "f"),
			h0 = h0,
			most_rank = ranks$rank[1],
			rxjs_rank = ranks$rank[2]
			)
		)
}

write.csv(frame,"hypothesis_testing.csv", row.names = FALSE)
	