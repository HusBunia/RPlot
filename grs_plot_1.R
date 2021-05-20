library(readxl)
library(visreg)
library(car)
library(ggplot2)
library(ggpubr)
library(RColorBrewer)
library(mediation)
library(igraph)
####################################################################################################################################################
##################################################### MM    MMM ##### NN    NN ### IIIII ############################################################# 
#################################################### MMM   MMMM #### NNN   NN ####  II #########################################################
################################################### MM M  M  MM ### NN N  NN ##### II ############################################################
################################################## MM  MMM   MM ## NN  N NN ##### II  ##############################################################
################################################# MM    M    MM # NN   NNN ### IIIIII ###########################################################

med_hcp <- read_excel("D:/SomeData/UBE3A+NRSN2/results/spearman_rank.xlsx",sheet = "hcp_mean") 
med_as  <- read_excel("D:/SomeData/UBE3A+NRSN2/results/spearman_rank.xlsx",sheet = "as_mean") 



fit1 <- lm(deg~1+md+cth+pls2,data=med_hcp)
fit2 <- lm(md~1+cth+pls2,data=med_hcp)

med1 <- mediate(fit1,fit2,treat="pls2",mediator="md",boot=TRUE,sims=1000)



##ggplot画两条曲线，曲线颜色不同##
fit0 <- lm(deg~1+pls1,data=reg)

fit00 <- lm(md~1+pls1,data=reg)

v  <- visreg(fit0,"pls1",scale="response",line.par = list(col = "red"),plot=FALSE)
v2 <- visreg(fit00,"pls1",scale="response",plot = FALSE)
dplyr::bind_rows(
  dplyr::mutate(v$fit,plt="deg"),
  dplyr::mutate(v2$fit,plt="md")
) -> u

dplyr::bind_rows(
  dplyr::mutate(v$res,plt="deg"),
  dplyr::mutate(v2$res,plt="md")
) -> uu

ggplot()+
  geom_line(data=u,aes(pls1,visregFit,group=plt,color=plt),size=1.5)+
  geom_ribbon(data=u,aes(pls1,ymin=visregLwr,ymax=visregUpr,fill=plt),alpha=0.3)+
  theme(panel.background = element_rect(fill = 'white', colour = 'black'),
        plot.title=element_text(hjust=0.5),
        axis.text.x = element_text(size = 12, color = "black",face="bold", vjust = 0.5, hjust = 0.5),
        axis.text.y = element_text(size = 12, face="bold",color = "black", vjust = 0.5, hjust = 0.5),
        axis.title.x = element_text(size=14))+
  geom_point(data = uu, aes(x = pls1, y = visregRes,color = factor(plt)),size=3,shape=19)+
  xlab("Caudate head") + ylab("Left LGI") 
###########################################################

fit0 <- lm(md~1+XS1,data=reg)
visreg(fit0,"XS1",gg=TRUE)+
  geom_point(pch=20,size=3,bg="black",alpha=0.8)+geom_smooth(method=lm,color="black",alpha=0.8)+
  #geom_hline(aes(yintercept=0),linetype=5,col="red")+
  #geom_vline(aes(xintercept=0),linetype=5,col="red")+
  xlab("PLS1")+
  ylab("Nodal Degree")+theme(panel.background = element_rect(fill = 'white', colour = 'gray'),
                   plot.title=element_text(hjust=0.5)
                   ,axis.text.x = element_text(size = 12, face="bold",color = "black", vjust = 0.5, hjust = 0.5)
                   ,axis.text.y = element_text(size = 12, face="bold",color = "black", vjust = 0.5, hjust = 0.5)
                   ,axis.title.x = element_text(size=14),axis.title.y=element_text(size=14))

###########################################################
##以X轴为渐变色画出散点图##

tmap<- read_excel("D:/SomeData/UBE3A+NRSN2/submit_results/result.xlsx",sheet = "Sheet2") 
a = mean(tmap$md)
b = mean(tmap$cth)
c = mean(tmap$deg)
fit0 <- lm(cth~md,data=tmap)
#c = scales::seq_gradient_pal("blue")(seq(0,1,length.out=308)) ##seems not work
ggplot(tmap,aes(x=md,y=cth))+
  geom_point(size=3,alpha=0.8,shape=16,color="darkblue") +
  #scale_color_gradientn(colours=c("#CCFFFF","#000066"),values=c(0,1)) +#调色用，按照六位编码neg[c("#CCFFFF","#000066")]; pos[c("#CCFFFF","#000066")]
  #scale_color_gradientn(colours=c("#FFCC99","#FF0000"),values=c(0,1)) + ##正相关给予暖色
  geom_smooth(method=lm,se=FALSE,color='#CC0033')+##正相关给予红色
  #geom_smooth(method=lm,se=FALSE,color='darkblue')+##负相关给予蓝色
  geom_hline(aes(yintercept=b),linetype=5,col="black")+
  geom_vline(aes(xintercept=a),linetype=5,col="black")+
  theme(panel.background = element_rect(fill = 'white', colour = 'gray'),
                                 plot.title=element_text(hjust=0.5)
                                 ,axis.text.x = element_text(size = 12, face="bold",color = "black", vjust = 0.5, hjust = 0.5)
                                 ,axis.text.y = element_text(size = 12, face="bold",color = "black", vjust = 0.5, hjust = 0.5)
                                 ,axis.title.x = element_text(size=14),axis.title.y=element_text(size=14))


###########################################################
##箱线图##
fit0 <- lm(Rchead~1+age+sex+tiv+a,data=AS1)
fit00 <- lm(Lchead~1+age+sex+tiv+b,data=AS3)
v <- visreg(fit0,"a",plot=FALSE)
v2 <- visreg(fit00,"b",plot = FALSE)
p <- as.data.frame(c(v$res$visregRes,v2$res$visregRes))
temp[,1] <- p
ggboxplot(data=temp,x="d",y="p",color="g",palette="jama",
          fill="gray",add="jitter",
          bxp.errorbar = TRUE,bxp.errorbar.width = 0.2,
          width = 0.5,
          xlab = FALSE, ylab = "Left caudate tail")

  
###散点条形图
  
dotbar <- read_excel("D:/SomeData/UBE3A+NRSN2/plsGSEA/srsmd_gene/report.xlsx",sheet = "R1")
db <- dotbar[1:20,]
ggplot(data = db, mapping = aes(y = reorder(name,NES),x = NES, color=NES))+
  geom_point(aes(size=SIZE),shape=16)+scale_color_gradientn(colours=c("#FDC298","#FFFFBF"),values=c(0,1))+
theme(panel.background = element_rect(fill = 'white', colour = 'black'),
      plot.title=element_text(hjust=0.5),
      axis.text.x = element_text(size = 10, color = "black",face="bold", vjust = 0.5, hjust = 0.5),
      axis.text.y = element_text(size = 10, face="bold",color = "black", vjust = 0.5),
      axis.title.x = element_text(size=14))


