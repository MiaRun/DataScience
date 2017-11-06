add_path<- function(directory,i) {
        file_path<- paste("F:/R_Workspace/DataSci2/week2/hw2",directory,paste(sprintf("%03d",i),'.csv',sep=''),sep='/')
}

pollutantmean <- function(directory,pollutant,id=1:332) {
        for(i in id) {
                file_path<- add_path(directory,i)
                new_data<- read.csv(file_path,header=TRUE)
                if(i==id[1]) {
                        merge_data<- new_data
                } else {
                        merge_data=rbind(merge_data,new_data)
                }
        }
        data<- merge_data[[pollutant]]
        #merge_data[[pollutant]]
        #print (data)
        mean(data,na.rm=TRUE)
}


#²âÊÔ¼¯ºÏ
#pollutantmean('specdata','sulfate',1:10)
#pollutantmean('specdata','nitrate',70:72)
#pollutantmean('specdata','nitrate',23)



