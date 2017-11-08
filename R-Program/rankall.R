##Ranking hospitals in all states
rankall<- function(outcome,num) {
        ## Read outcome data
        disease<- c('heart attack','heart failure','pneumonia')
        if(!any(disease==outcome)) {
                print ("Invalid outcome")
                return
        } else {
                data<-read.csv('outcome-of-care-measures.csv',header=TRUE,colClasses='character')
                names(data)[c(11,17,26)]<-disease
                
                ## 得到 State，Hospital Name 和 outcome 评分
                
                sub_data<- subset(data[c('State','Hospital.Name',outcome)])
                sub_data[outcome]<- as.numeric(sub_data[[outcome]])
                s_sub_data<-split(sub_data,sub_data$State)

                result<-mapply(FindRank,x=s_sub_data,d=outcome,n=num)

                d_result<- data.frame(cbind(result,names(result)),row.names=NULL)
                names(d_result)<- c('hospital','State')
                d_result
        }
}

FindRank<- function(x,d,n) {
        clean_data<- na.omit(x[order(x$Hospital.Name),])
        Rank<- rank(clean_data[d],ties.method = 'first')
        final_data<-cbind(clean_data,Rank)
        
        if(n=='best') {
                head(final_data$Hospital.Name,1)
        } else if(n=='worst') {
                tail(final_data$Hospital.Name,1)
        } else if(n>nrow(final_data))  {
                return ("<NA>")
        } else {
                subset(final_data$Hospital.Name,final_data$Rank==n)
        }
        
}

rankall('heart attack',20)

tail(rankall('pneumonia','worst'),3)

tail(rankall('heart failure','best'),10)