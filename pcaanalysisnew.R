name=c("id","group","median_speed","distance_traveled_mm_p_min","turning_angle",
"meander","activitytime_TT","act_bouts_TT","pause_length_TT",
"numb_pauses_TT","centrophobism_moving","centrophobism_sitting",      "number_of_walks","stripe_deviation")

setwd("/Users/colombj/Desktop/")
newdata= read.csv("output _table.csv")
f_table= newdata[,name]

newdata=newdata[,name]


shapiro.test(PCA_res$PC2 | PCA_res$group)
PCA_res= cbind(PCA_res,newdata$other)
names(PCA_res)[14]="other"
PCA_res= cbind(PCA_res,newdata$genotype)
names(PCA_res)[15]="genotype"

A=aov(PCA_res$PC2~PCA_res$genotype * PCA_res$other)
A=aov(PCA_res$PC2~PCA_res$genotype)
summary(A)
TukeyHSD(A)
plot((PCA_res$PC2~PCA_res$genotype)

TZ=PCA_res$PC3[PCA_res$genotype =="CS_TZ"]
TP=PCA_res$PC3[PCA_res$genotype =="CS_TP"]
JC=PCA_res$PC3[PCA_res$genotype =="CS_JC"]
wilcox.test(TZ,TP, paired=F, alternative = "two.sided")
wilcox.test(TZ,JC, paired=F, alternative = "two.sided")

setwd("/Users/colombj/Desktop/")


## this code modify the name of the "other" variable
	levels(PCA_res$other)=c (levels(PCA_res$other),"2012", "2013")
	PCA_res$other[PCA_res$other=="2ndcross"]="2013"
	PCA_res$other[PCA_res$other=="1stcross"]="2012"
	PCA_res$other=droplevels(PCA_res$other)
	
	levels(PCA_res$group)=c (levels(PCA_res$group),"CS_botella")
	PCA_res$group[PCA_res$group =="5Tage-mit"]="CS_botella"
	
	PCA_res$group =droplevels(PCA_res$group)
##


require("ggplot2")
abc= c(1:8, rgb(202,100,20,maxColorValue = 255),rgb(100,0,200,maxColorValue = 255))

theme_jack <- function (base_size = 12, base_family = "") {
    theme_bw(base_size = base_size, base_family = base_family) %+replace%
        theme(
            #axis.text = element_text(colour = "white"),
            #axis.title.x = element_text(colour = "pink", size=rel(3)),
            #axis.title.y = element_text(colour = "blue", angle=45),
            #panel.background = element_rect(fill="green"),
            #panel.grid.minor.y = element_line(size=3),
            axis.text.x=element_text(size=10, colour = abc)
            ,panel.grid.major = element_line(colour = "grey")#,

           # plot.background = element_rect(fill="red")
           #,panel.grid = element_blank()
           ,panel.grid.major.x = element_blank()
           #,axis.text.x = element_text(angle=90, vjust=0)



    )
}
theme_set(theme_jack())


p=ggplot(PCA_res, aes(x=factor(genotype),y= PC2, fill= other))

 	plot=p+ geom_boxplot(position=position_dodge(0.9))
 
 pdf("PC2.pdf")
 plot+ labs(x="genotype",y="PC2", fill="session",title="")+
 scale_fill_grey(start = 0.4, end = 1)
 dev.off()
 + geom_hline(aes(yintercept=44), color="red")

abc2= c(rep (c(1,5),5))
abc= c(1,1,2,2,3,3,4,4,5,5)

PCA_res$group = as.factor(paste(PCA_res$genotype,PCA_res$other,sep="_"))
layout(matrix(c(1,3,2,4), 2, 2, byrow=TRUE), respect=TRUE)
#plot -1 versus 2

 plot(-scores[,1], scores[,2], xlab="PCA 1", ylab="PCA 2", 
   type="n", main="distance biplot",xaxt="n", yaxt="n",xlim=c(-M, M), 
   ylim=c(-M, M))

for (i in 1:length(levels(PCA_res$group))){
  X = subset(PCA_res,PCA_res$group == levels(PCA_res$group)[i])
	#points(X$PC2~ X$PC1, col=i, pch= i+10)
#legend("topleft", legend=(levels(PCA_res$group)[i]), fill= abc[i], bty="n", inset = c(0,i/30))
	 #text(x+1,y-0.1, levels(PCA_res$group)[i], col=i, 
#   cex=0.7) 
   x=-Mean_PCA_3d$means$PCA_res.PC1[i]
	y=Mean_PCA_3d$means$PCA_res.PC2[i]
	x2=-Mean_PCA_3d$ses$PCA_res.PC1[i]
	y2=Mean_PCA_3d$ses$PCA_res.PC2[i]
segments (x-x2,y,x+x2,y, col=abc[i], lty=abc2[i])
segments (x,y-y2,x,y+y2, col=abc[i], lty=abc2[i])
	}
	abline (v=0, h=0)
	
	
#plot -1 versus -3	
 plot(-scores[,1], -scores[,3], xlab="PCA 1", ylab="PCA 3", 
   type="n", main="distance biplot",xaxt="n", yaxt="n",xlim=c(-M, M), 
   ylim=c(-M, M))

#plot(PCA_res$PC2~PCA_res$PC1, type= "n", add=TRUE)

for (i in 1:length(levels(PCA_res$group))){
	X = subset(PCA_res,PCA_res$group == levels(PCA_res$group)[i])
	#points(X$PC2~ X$PC1, col=i, pch= i+10)
	#legend("topleft", legend=(levels(PCA_res$group)[i]), fill= 	abc[i], bty="n", inset = c(0,i/30))
	 #text(x+1,y-0.1, levels(PCA_res$group)[i], col=i, 
	#   cex=0.7) 
   x=-Mean_PCA_3d$means$PCA_res.PC1[i]
	y=-Mean_PCA_3d$means$PCA_res.PC3[i]
	x2=-Mean_PCA_3d$ses$PCA_res.PC1[i]
	y2=-Mean_PCA_3d$ses$PCA_res.PC3[i]
	segments (x-x2,y,x+x2,y, col=abc[i], lty=abc2[i])
	segments (x,y-y2,x,y+y2, col=abc[i], lty=abc2[i])
  
	}
	abline (v=0, h=0)	
	
	
	
	
	
#plot 3 versus 2	
 plot(scores[,3], scores[,2], xlab="PCA 3", ylab="PCA 2", 
   type="n", main="distance biplot",xaxt="n", yaxt="n",xlim=c(-M, M), 
   ylim=c(-M, M))

#plot(PCA_res$PC2~PCA_res$PC1, type= "n", add=TRUE)

for (i in 1:length(levels(PCA_res$group))){
	X = subset(PCA_res,PCA_res$group == levels(PCA_res$group)[i])
	#points(X$PC2~ X$PC1, col=i, pch= i+10)
	#legend("topleft", legend=(levels(PCA_res$group)[i]), fill= 	abc[i], bty="n", inset = c(0,i/30))
	 #text(x+1,y-0.1, levels(PCA_res$group)[i], col=i, 
	#   cex=0.7) 
   x=Mean_PCA_3d$means$PCA_res.PC3[i]
	y=Mean_PCA_3d$means$PCA_res.PC2[i]
	x2=Mean_PCA_3d$ses$PCA_res.PC3[i]
	y2=Mean_PCA_3d$ses$PCA_res.PC2[i]
	segments (x-x2,y,x+x2,y, col=abc[i], lty=abc2[i])
	segments (x,y-y2,x,y+y2, col=abc[i], lty=abc2[i])
  
	}
	abline (v=0, h=0)	
	
#plot legend	
 plot(scores[,3], scores[,2], xlab="legend", ylab="", 
   type="n", main="", bty=0, xaxt="n", yaxt="n", xlim=c(-M, M), 
   ylim=c(-M, M))

#plot(PCA_res$PC2~PCA_res$PC1, type= "n", add=TRUE)

for (i in 1:length(levels(PCA_res$group))){
	X = subset(PCA_res,PCA_res$group == levels(PCA_res$group)[i])
	#points(X$PC2~ X$PC1, col=i, pch= i+10)
	legend("topleft", legend=(levels(PCA_res$group)[i]), col= 	abc[i], lty=abc2[i],bty="n", inset = c(0,i/10-0.15)
	)
	 #text(x+1,y-0.1, levels(PCA_res$group)[i], col=i, 
	#   cex=0.7) 
   x=Mean_PCA_3d$means$PCA_res.PC3[i]
	y=Mean_PCA_3d$means$PCA_res.PC2[i]
	x2=Mean_PCA_3d$ses$PCA_res.PC3[i]
	y2=Mean_PCA_3d$ses$PCA_res.PC2[i]
	#segments (x-x2,y,x+x2,y, col=abc[i])
	#segments (x,y-y2,x,y+y2, col=abc[i])
  
	}

dev.off()