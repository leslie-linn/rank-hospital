rankhospital <- function(state, outcome, num = "best") {
	data<-read.csv("outcome-of-care-measures.csv", ,na.strings="not available",colClasses = "character",stringsAsFactors=FALSE)
	
	state_col<-data[ ,7]
	name_col <- data[ ,2]
	attack_col<-suppressWarnings(as.numeric(data[ ,11]))
	failure_col<-suppressWarnings(as.numeric(data[ ,17]))
	pneumonia_col<-suppressWarnings(as.numeric(data[ ,23]))
	
	best_data<-cbind(state_col,name_col,attack_col,failure_col,pneumonia_col)
	
	colnames(best_data)<-c("state","name","heart attack","heart failure","pneumonia")
	
	best_data<-as.data.frame(best_data)
	
	list_outcomes <- c("heart attack","heart failure" , "pneumonia")
	
	if(!(outcome %in% list_outcomes))
		stop("invalid outcome")
	
	list_unique_states <- unique(state_col)
	
	if(!(state %in% list_unique_states))
		stop("invalid state")
		
		
	
	
	state_data <- best_data[best_data$state == state, ]
	complete_data<-best_data
	
	complete_data <- state_data[!is.na(state_data[, outcome]), ]
	
	complete_data <- as.data.frame(complete_data)
	
	ordered_data <- complete_data[order(complete_data[,outcome],complete_data$name), ]
	
	
	
	final_data <- ordered_data
	
	min_value <- which.min(final_data[,outcome])
	max_value <- which.max(final_data[,outcome])
	
	if (num == "best"){
		as.character(final_data[min_value, 2])
	} else 
		if (num == "worst"){
				as.character(final_data[max_value, 2])
		} else 
				{as.character(final_data[num, 2])
	}
}