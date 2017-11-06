library(sqldf)

add_path<- function(directory,i) {
        file_path<- paste("F:/R_Workspace/DataSci2/week2/hw2",directory,paste(sprintf("%03d",i),'.csv',sep=''),sep='/')
}

complete<- function(directory,id) {
        for(i in id) {
                file_path<- add_path1(directory,i)
                new_data<- read.csv(file_path,header=TRUE)
                nobs<- sqldf("select count(*) from new_data where sulfate is not null and nitrate is not null")
                id_nons<- matrix(c(i,nobs),ncol=2)
                if(i==id[1]) {
                        data<- id_nons
                } else {
                        data<- rbind(data, id_nons)
                }
        }
        data<-data.frame(data)
        names(data)<-c('id','nobs')
        data
}

#²âÊÔ¼¯ºÏ
complete('specdata',1)
complete('specdata',c(2,4,8,10,12))
complete('specdata',30:25)
complete('specdata',3)

