name=c("id","group","median_speed","distance_traveled_mm_p_min","turning_angle",
"meander","activitytime_TT","act_bouts_TT","pause_length_TT",
"numb_pauses_TT","centrophobism_moving","centrophobism_sitting",      "number_of_walks","stripe_deviation")

setwd("/Users/colombj/Desktop/")
newdata= read.csv("output _table.csv")
f_table= newdata[,name]

newdata=newdata[,name]


shapiro.test(PCA_res$PC1 | PCA_res$group)
PCA_res= cbind(PCA_res,newdata$other)
names(PCA_res)[14]="other"
PCA_res= cbind(PCA_res,newdata$genotype)
names(PCA_res)[15]="genotype"

A=aov(PCA_res$PC2~PCA_res$genotype * PCA_res$other)
summary(A)

p=ggplot(PCA_res, aes(x=factor(genotype),y= PC2, fill= other))

 	plot=p+ geom_boxplot(position=position_dodge(0.9))
 
 pdf("PC2.pdf")
 plot+ labs(x="genotype",y="PC2", fill="session",title="")+
 scale_fill_grey(start = 0.4, end = 1)
 dev.off()
 + geom_hline(aes(yintercept=44), color="red")
