best<- function(state,outcome) {
        ##read outcome data
        disease<-c("heart attack","heart failure",'pneumonia')
        
        data<-read.csv("outcome-of-care-measures.csv",header=TRUE,colClasses="character")
        names(data)[c(11,17,23)]<- disease
        ##Check that state and outcome are valid
        if(!any(unique(data['State'])==state)) {
                cat("Invalid state")
        } else if (!any(disease==outcome)) {
                cat("Invalid outcome")
        } else #if(any(unique(data['State'])==state) & any(disease==outcome)) 
        {
                
                data_state<- subset(data[c('Hospital.Name',outcome)],data$State==state)
                hos_name<-subset(data_state$Hospital.Name,as.numeric(data_state[[outcome]])==min(as.numeric(data_state[[outcome]]),na.rm=TRUE))
                head(sort(hos_name),1)
        }
}

best('TX','heart attack')

best('TX','heart failure')

best('MD','heart attack')

best('MD','pneumonia')

best('BB','heart attack')

best('NY','hert attack')