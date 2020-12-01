## SVM for boston 
# require(tidyverse)
require(e1071)

bos_std<-read_csv("census-csv/prop_change_suffolk_labeled.csv")

remove_str_name<-function(data, rem_str){
  old_names<-names(data)
  for(name in old_names){
    if(str_detect(name, rem_str, negate = T)){next()}
    new<-str_replace(name, rem_str, "")
    names(data)[names(data) == name]<-new
  }
  return(data)
}
bos_std<-remove_str_name(bos_std, "prop.change.")


######################################################
bos_subset<-mutate(bos_std, Gent_Label = as.factor(Gent_Label))%>%
  select(Gent_Label,
         housing.renter.occupied.householder.race.White,
         housing.owner.occupied.householder.race.Black,
         housing.owner.occupied.householder.race.Other)


bos_ind<-sample(nrow(bos_std), round(nrow(bos_std)/5))
sub_train<-bos_subset[-bos_ind,]
sub_test<-bos_subset[bos_ind,]

svm_c1<-svm(Gent_Label~., data=sub_train)
c1_pred<-predict(svm_c1, sub_test)
svm_c100<-svm(Gent_Label~., data=sub_train, cost=100)
c100_pred<-predict(svm_c100, sub_test)



print(paste("C1:", mean(c1_pred==sub_test$Gent_Label)))
print(paste("C100:", mean(c100_pred==sub_test$Gent_Label)))


###### TODO
## Cross-validate 'C' cost variable
## Fit ROC curve comparing to LDA

svm_pred<-predict(svm_c1,  bos_subset$Gent_Label)
table(pred=svm_pred, true = bos_subset$Gent_Label)
mean(svm_pred == bos_subset$Gent_Label)






