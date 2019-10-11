out <- capture.output(summary(my_very_time_consuming_regression))

cat("My title", out, file="summary_of_my_very_time_consuming_regression.txt", sep="\n", append=TRUE)

# CLEAR: ctrl+L

function_name <- function(輸入1, 輸入2, ..., 輸入n, 參數1, 參數2, ..., 參數n){
    # 呼叫函數後執行的程式
    return(輸出)
}




> pairs(sat_data)
> M=cor(sat_data)
> library(corrplot)
> corrplot(M, method = "shade")


for(i in 1:ncol(sat_data)){
+ hist(sat_data[,i],main=names(sat_data)[i],prob=T)
+ lines(density(sat_data[,i]))
+ }



for (i in x){
    # 每次迭代要執行的程式
}