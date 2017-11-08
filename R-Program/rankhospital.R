#ranking hospitals by outcome in a state

rankhospital<- function(state,outcome,num) {

        ## read outcome data
        data<-read.csv("outcome-of-care-measures.csv",header=TRUE,colClasses="character")
        
        ## Check that state and oucome are valid
        disease<-c("heart attack","heart failure",'pneumonia')
        if(!any(disease==outcome)) {
                cat ("invalid outcome")
        } else if(!any(unique(data$State)==state)) {
                cat ("invalid state")
        } else {
                names(data)[c(11,17,23)]<-disease
                ## 得到排除NA值后的包含hospital和outcome的表格（未对名称排序）
                data_state<-subset(data[c('Hospital.Name',outcome)],data$State==state)
                data_state[outcome]<- as.numeric(data_state[[outcome]])
                data_state<-na.omit(data_state)
                #data_state<-na.omit(subset(data[c('Hospital.Name',outcome)],data$State==state))
                
                ## 对hospital按名称排序
                r_h_data<-data_state[order(data_state['Hospital.Name']),]
                
                ## 对排序后的组合按outcome进行排序
                Rank<-rank(r_h_data[outcome],ties.method = 'first')
                r_data<-cbind(r_h_data,Rank)
                
                ##
                r_data<-r_data[order(r_data$Rank),]
                
                if(num=='best') {
                        as.character(head(r_data$Hospital.Name,1))
                } else if(num=='worst') {
                        as.character(tail(r_data$Hospital.Name,1))
                } else if(nrow(data_state)<num) {
                        return (NA)
                } else {
                        as.character(subset(r_data['Hospital.Name'],r_data$Rank==num))
                }
        }
}

rankhospital('TX','heart failure',5000)
rankhospital('MD','heart attack','worst')
rankhospital('TX','heart failure',4)
