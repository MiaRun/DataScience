library(sqldf)
ReadData<- function(directory,i) {
        read.csv(paste(directory,i,sep='/'))
}

CompleteNods<- function(new_data) {
        sqldf("select count(*) from new_data where sulfate is not null and nitrate is not null")
}

corr<- function(directory,threshold=0) {
        filelist<-list.files(directory)
        result_ans<- NaN
        for(i in filelist) {
                new_data<- ReadData(directory,i)
                cpl_nods<- CompleteNods(new_data)
                if (cpl_nods>threshold) {
                        cor_ans<- cor(new_data['sulfate'],new_data['nitrate'],use='complete.obs')
                        result_ans<- c(result_ans,as.numeric(cor_ans))
                }
        }
        result<-if(length(result_ans)==1) {
                NULL
                
        } else{
                result_ans[-1]
        }
        result
}

directory<- "F:/R_Workspace/DataSci2/week2/hw2/specdata"


cr<-corr(directory,150)
cr<-corr(directory,400)
cr<-corr(directory,5000)
summary(cr)
head(cr)




