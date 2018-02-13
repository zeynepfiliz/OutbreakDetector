
performance_matrix <- matrix(0,3,6)
colnames(performance_matrix) <- c("sensitivity", "specicificity", "CI_low_sensitivity",
                                  "CI_up_sensitivity","CI_low_specificity","CI_up_specificity")

crosstab <- table(tmp$signal, cusum_outbreak$outbreak[367:2464])
performance_matrix[1,1]<-crosstab[2,2]/(crosstab[2,2]+crosstab[1,2])
performance_matrix[1,2]<-crosstab[1,1]/(crosstab[1,1]+crosstab[2,1])
np1 <- crosstab[2,2]+crosstab[1,2]
nn1 <- crosstab[1,1]+crosstab[2,1]
performance_matrix[1,4]<- performance_matrix[1,1]+
  (3*sqrt( (performance_matrix[1,1]*(1-performance_matrix[1,1])/np1)))
performance_matrix[1,3]<- performance_matrix[1,1]-
  (3*sqrt( (performance_matrix[1,1]*(1-performance_matrix[1,1])/np1)))
performance_matrix[1,6]<- performance_matrix[1,2]+
  (3*sqrt( (performance_matrix[1,2]*(1-performance_matrix[1,2])/nn1)))
performance_matrix[1,5]<- performance_matrix[1,2]-
  (3*sqrt( (performance_matrix[1,2]*(1-performance_matrix[1,2])/nn1)))
#