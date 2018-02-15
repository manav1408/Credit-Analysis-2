library(readr)
z=read.csv("C:/Users/Manav Middha/Desktop/CleanCreditScoring.csv",header=TRUE,stringsAsFactors=TRUE)

#create decision tree
require(rpart)
ct=rpart(Status~.,data=z)

ct
#plot of tree
plot(ct, margin=0.05, compress=TRUE, main="Decision Tree")
text(ct, use.n=TRUE, pretty=1, all=TRUE, cex=0.7)

#split in training and testing
n = nrow(dd)
learn = sample(1:n, size=round(0.67 * n))
nlearn = length(learn)
ntest = n - nlearn

ct1 = rpart(Status ~ ., data = dd[learn,], method="class",
            parms = list(prior=c(0.50, 0.50), split='gini'), 
            control = rpart.control(cp=0.001, xval=10, maxdepth=15))

ct1$cptable
plotcp(ct1, las=2, cex.axis=0.8)

min(ct1$cptable[,4])
min.xe = which(ct1$cptable[,4] == min(ct1$cptable[,4]))
# the optimal tree corresponds to a cp=0.003
ct1$cptable[min.xe,]


ct2 = rpart(Status ~ ., 
            data = dd[learn,],
            parms = list(prior=c(0.50, 0.50), split='gini'), 
            control = rpart.control(cp=0.00285, xval=0, maxdepth=15))
# plot
par(mar = c(1,1,2,0.5))
plot(ct2, margin=0.05, compress=TRUE, main="Decision Tree")
text(ct2, use.n=TRUE, pretty=1, all=TRUE, cex=0.5)
summary(ct2)

# calculate error rate in the learning sample
# (this will give a matrix)
ct2.learn = predict(ct2, data=ddtot[learn,])
# create a vector with predicted status
ct2.learnp = rep("", nlearn)
ct2.learnp[ct2.learn[,1] < 0.5] = "pred_neg"
ct2.learnp[ct2.learn[,1] >= 0.5] = "pred_pos"
# let's make a table
status_learn = table(dd$Status[learn], ct2.learnp)
# classification error
100 * sum(diag(status_learn)) / nlearn


# calculate error rate in the testing sample
# (this will give a matrix)
ct2.test = predict(ct2, newdata=ddtot[-learn,])
# create a vector with predicted status
ct2.testp = rep("", ntest)
ct2.testp[ct2.test[,1] < 0.5] = "pred_neg"
ct2.testp[ct2.test[,1] >= 0.5] = "pred_pos"
# let's make a table
status_test = table(dd$Status[-learn], ct2.testp)
# classification error
100 * sum(diag(status_test)) / ntest