# graficos para apendice
require(ggplot2)

## MEDIA
load("~/Documents/LABTROP_LET/R/dados_mestrado/dados_output/dados_output_pos_comite_15fev17_hipercubo27jul17/pos_comite_15fev17_dp0_media_temporal.RData")
load("~/Documents/LABTROP_LET/R/dados_mestrado/dados_output/dados_output_pos_comite_15fev17_hipercubo27jul17/pos_comite_15fev17_dp1_media_temporal.RData")
load("~/Documents/LABTROP_LET/R/dados_mestrado/dados_output/dados_output_pos_comite_15fev17_hipercubo27jul17/pos_comite_15fev17_dp10_media_temporal.RData")
load("~/Documents/LABTROP_LET/R/dados_mestrado/dados_output/dados_output_pos_comite_15fev17_hipercubo27jul17/pos_comite_15fev17_dp100_media_temporal.RData")
load("~/Documents/LABTROP_LET/R/dados_mestrado/dados_output/dados_output_pos_comite_15fev17_hipercubo27jul17/pos_comite_15fev17_dp500_media_temporal.RData")
load("~/Documents/LABTROP_LET/R/dados_mestrado/dados_output/dados_output_pos_comite_15fev17_hipercubo27jul17/pos_comite_18mar17_dp500_1sp_media_temporal_lahr.RData")

## MORTES ACUMULADAS
load("~/Documents/LABTROP_LET/R/dados_mestrado/dados_output/dados_output_pos_comite_15fev17_hipercubo27jul17/pos_comite_15fev17_dp500_mortes_cumulativas_temporal.RData")
load("~/Documents/LABTROP_LET/R/dados_mestrado/dados_output/dados_output_pos_comite_15fev17_hipercubo27jul17/pos_comite_15fev17_dp0_mortes_cumulativas_temporal.RData")
load("~/Documents/LABTROP_LET/R/dados_mestrado/dados_output/dados_output_pos_comite_15fev17_hipercubo27jul17/pos_comite_15fev17_dp1_mortes_cumulativas_temporal.RData")
load("~/Documents/LABTROP_LET/R/dados_mestrado/dados_output/dados_output_pos_comite_15fev17_hipercubo27jul17/pos_comite_15fev17_dp10_mortes_cumulativas_temporal.RData")
load("~/Documents/LABTROP_LET/R/dados_mestrado/dados_output/dados_output_pos_comite_15fev17_hipercubo27jul17/pos_comite_15fev17_dp100_mortes_cumulativas_temporal.RData")
load("~/Documents/LABTROP_LET/R/dados_mestrado/dados_output/dados_output_pos_comite_15fev17_hipercubo27jul17/pos_comite_18mar17_dp500_1sp_mortes_cumulativas_temporal_lahr.RData")

## RIQUEZA
load("~/Documents/LABTROP_LET/R/dados_mestrado/dados_output/dados_output_pos_comite_15fev17_hipercubo27jul17/pos_comite_15fev17_dp500_riqueza_temporal.RData")
load("~/Documents/LABTROP_LET/R/dados_mestrado/dados_output/dados_output_pos_comite_15fev17_hipercubo27jul17/pos_comite_15fev17_dp0_riqueza_temporal.RData")
load("~/Documents/LABTROP_LET/R/dados_mestrado/dados_output/dados_output_pos_comite_15fev17_hipercubo27jul17/pos_comite_15fev17_dp100_riqueza_temporal.RData")

# dados
load("~/Documents/LABTROP_LET/R/dados_mestrado/dados_mestrado.RData")

######### APENDICE 1
## parte em que se mostra as taxas de mutacao intermediarias
rbPal <- colorRampPalette(c('black','gray25','seashell4','cornsilk3','orange','tan2','darkorange2'))
rbPal_legenda <- colorRampPalette(c('darkorange2','tan2','orange','wheat3','seashell4','gray25','black'))
#This adds a column of color values
# based on the y values
cor_bat3 <- rbPal(1000)[as.numeric(cut(dados$dist_indice[dados$bateria==3],breaks = 1000))]
#cor_bat2 <- rbPal(1000)[as.numeric(cut(dados$dist_indice[dados$bateria==2],breaks = 1000))]
#cor_bat1 <- rbPal(1000)[as.numeric(cut(dados$dist_indice[dados$bateria==1],breaks = 1000))]

# MUTACAO 0
par(mar=c(4,3.8,2,0.8))
par(mgp=c(2.5,0.7,0))
par(tck=-0.02)
layout(matrix(1:4,ncol=2), width = c(20,2,20,2),height = c(1,1,1,1))
matplot(y=t(pos_comite_dp0_media_temporal)[1:3001,]/20000,x=t(I(pos_comite_dp0_mortes_cumulativas_temporal/5000))[1:3001,],type="l",col=alpha(cor_bat3,1),ylim=c(0,1),ylab="Média do índice de estratégia de vida",xlab="Geração",bty="l",cex.axis=0.85,xlim=c(0,5000),lty=1,axes=F)
axis(2,at=c(-1,0,0.25,0.5,0.75,1,2),labels=c("",0,"0,25","0,5","0,75","1",""),las=1,cex.axis=0.85)
axis(1,at=c(-1000,0,1000,2000,3000,4000,5000,6000),labels=c("",0,1000,2000,3000,4000,5000,""),cex.axis=0.85)
matplot(y=t(pos_comite_dp0_riqueza_temporal)[1:3001,],x=t(I(pos_comite_dp0_mortes_cumulativas_temporal/5000))[1:3001,],type="l",col=alpha(cor_bat3,1),lty=1,ylim=c(1,500),ylab="Riqueza",xlab="Geração",bty="l",main="",xlim=c(0,5000),cex.axis=0.85,axes=F)
axis(2,at=c(-100,1,125,250,375,500,600),labels=c("",1,125,250,375,500,""),las=1,cex.axis=0.85)
axis(1,at=c(-1000,0,1000,2000,3000,4000,5000,6000),labels=c("",0,1000,2000,3000,4000,5000,""),cex.axis=0.85)
legend_image <- as.raster(matrix(rbPal_legenda(1000), ncol=1))
par(mar=c(4,0,2,0.8))
plot(c(0,2),c(0,1),type = 'n', xlab = '', ylab = '', main = "Índice de \ndistúrbio ",axes=F,cex.main=0.8)
text(x=1.3, y = seq(0,1,l=5), labels=c("0",expression("75"%.%10^"3"),expression("150"%.%10^"3"),expression("225"%.%10^"3"),expression("300"%.%10^"3")),cex=0.8)
rasterImage(legend_image, 0, 0, 0.5,1)
par(mar=c(4,0,2,0.8))
plot(c(0,2),c(0,1),type = 'n', xlab = '', ylab = '', main = "Índice de \ndistúrbio ",axes=F,cex.main=0.8)
text(x=1.3, y = seq(0,1,l=5), labels=c("0",expression("75"%.%10^"3"),expression("150"%.%10^"3"),expression("225"%.%10^"3"),expression("300"%.%10^"3")),cex=0.8)
rasterImage(legend_image, 0, 0, 0.5,1)

# MUTACAO 1
#matplot(y=t(pos_comite_dp1_media_temporal)[1:3001,],x=t(I(pos_comite_dp1_mortes_cumulativas_temporal/5000))[1:3001,],type="l",col=alpha(cor_bat3,1),las=1,ylim=c(1,20000),ylab="Estratégia de vida média",xlab="Geração",bty="l",main="Taxa de mutação = 1",cex.axis=0.8,xlim=c(0,10000),lty=1)

# MUTACAO 10
#matplot(y=t(pos_comite_dp10_media_temporal)[1:3001,],x=t(I(pos_comite_dp10_mortes_cumulativas_temporal/5000))[1:3001,],type="l",col=alpha(cor_bat3,1),las=1,ylim=c(1,20000),ylab="Estratégia de vida média",xlab="Geração",bty="l",main="Taxa de mutação = 10",cex.axis=0.8,xlim=c(0,10000),lty=1)

# MUTACAO 100
#####
par(mar=c(4,3.8,2,0.8))
par(mgp=c(2.5,0.7,0))
par(tck=-0.02)
layout(matrix(1:4,ncol=2), width = c(20,2,20,2),height = c(1,1,1,1))
matplot(y=t(pos_comite_dp100_media_temporal)[1:3001,]/20000,x=t(I(pos_comite_dp100_mortes_cumulativas_temporal/5000))[1:3001,],type="l",col=alpha(cor_bat3,1),ylim=c(0,1),ylab="Média do índice de estratégia de vida",xlab="Geração",bty="l",cex.axis=0.85,xlim=c(0,5000),lty=1,axes=F)
axis(2,at=c(-1,0,0.25,0.5,0.75,1,2),labels=c("",0,"0,25","0,5","0,75","1",""),las=1,cex.axis=0.85)
axis(1,at=c(-1000,0,1000,2000,3000,4000,5000,6000),labels=c("",0,1000,2000,3000,4000,5000,""),cex.axis=0.85)
matplot(y=t(pos_comite_dp100_riqueza_temporal)[1:3001,],x=t(I(pos_comite_dp100_mortes_cumulativas_temporal/5000))[1:3001,],type="l",col=alpha(cor_bat3,1),lty=1,ylim=c(1,500),ylab="Riqueza",xlab="Geração",bty="l",main="",xlim=c(0,5000),cex.axis=0.85,axes=F)
axis(2,at=c(-100,1,125,250,375,500,600),labels=c("",1,125,250,375,500,""),las=1,cex.axis=0.85)
axis(1,at=c(-1000,0,1000,2000,3000,4000,5000,6000),labels=c("",0,1000,2000,3000,4000,5000,""),cex.axis=0.85)
legend_image <- as.raster(matrix(rbPal_legenda(1000), ncol=1))
par(mar=c(4,0,2,0.8))
plot(c(0,2),c(0,1),type = 'n', xlab = '', ylab = '', main = "Índice de \ndistúrbio ",axes=F,cex.main=0.8)
text(x=1.3, y = seq(0,1,l=5), labels=c("0",expression("75"%.%10^"3"),expression("150"%.%10^"3"),expression("225"%.%10^"3"),expression("300"%.%10^"3")),cex=0.8)
rasterImage(legend_image, 0, 0, 0.5,1)
par(mar=c(4,0,2,0.8))
plot(c(0,2),c(0,1),type = 'n', xlab = '', ylab = '', main = "Índice de \ndistúrbio ",axes=F,cex.main=0.8)
text(x=1.3, y = seq(0,1,l=5), labels=c("0",expression("75"%.%10^"3"),expression("150"%.%10^"3"),expression("225"%.%10^"3"),expression("300"%.%10^"3")),cex=0.8)
rasterImage(legend_image, 0, 0, 0.5,1)


# MUTACAO 500
#####
par(mar=c(4,3.8,2,0.8))
par(mgp=c(2.5,0.7,0))
par(tck=-0.02)
layout(matrix(1:4,ncol=2), width = c(20,2,20,2),height = c(1,1,1,1))
matplot(y=t(pos_comite_dp500_media_temporal)[1:3001,]/20000,x=t(I(pos_comite_dp500_mortes_cumulativas_temporal/5000))[1:3001,],type="l",col=alpha(cor_bat3,1),ylim=c(0,1),ylab="Média do índice de estratégia de vida",xlab="Geração",bty="l",cex.axis=0.85,xlim=c(0,5000),lty=1,axes=F)
axis(2,at=c(-1,0,0.25,0.5,0.75,1,2),labels=c("",0,"0,25","0,5","0,75","1",""),las=1,cex.axis=0.85)
axis(1,at=c(-1000,0,1000,2000,3000,4000,5000,6000),labels=c("",0,1000,2000,3000,4000,5000,""),cex.axis=0.85)
matplot(y=t(pos_comite_dp500_riqueza_temporal)[1:3001,],x=t(I(pos_comite_dp500_mortes_cumulativas_temporal/5000))[1:3001,],type="l",col=alpha(cor_bat3,1),lty=1,ylim=c(1,500),ylab="Riqueza",xlab="Geração",bty="l",main="",xlim=c(0,5000),cex.axis=0.85,axes=F)
axis(2,at=c(-100,1,125,250,375,500,600),labels=c("",1,125,250,375,500,""),las=1,cex.axis=0.85)
axis(1,at=c(-1000,0,1000,2000,3000,4000,5000,6000),labels=c("",0,1000,2000,3000,4000,5000,""),cex.axis=0.85)
legend_image <- as.raster(matrix(rbPal_legenda(1000), ncol=1))
par(mar=c(4,0,2,0.8))
plot(c(0,2),c(0,1),type = 'n', xlab = '', ylab = '', main = "Índice de \ndistúrbio ",axes=F,cex.main=0.8)
text(x=1.3, y = seq(0,1,l=5), labels=c("0",expression("75"%.%10^"3"),expression("150"%.%10^"3"),expression("225"%.%10^"3"),expression("300"%.%10^"3")),cex=0.8)
rasterImage(alpha(legend_image,1), 0, 0, 0.5,1)
par(mar=c(4,0,2,0.8))
plot(c(0,2),c(0,1),type = 'n', xlab = '', ylab = '', main = "Índice de \ndistúrbio ",axes=F,cex.main=0.8)
text(x=1.3, y = seq(0,1,l=5), labels=c("0",expression("75"%.%10^"3"),expression("150"%.%10^"3"),expression("225"%.%10^"3"),expression("300"%.%10^"3")),cex=0.8)
rasterImage(legend_image, 0, 0, 0.5,1)

######### APENDICE 2
## parte em que se justifca a transformacao do disturbio em um indice que junta freq e int

media_adapt_excl <- dados$media[dados$bateria==3]/20000.0001
dist_adapt_excl <- dados$dist_indice[dados$bateria==3]
dist_freq_adapt_excl <- dados$dist_freq[dados$bateria==3]
dist_int_adapt_excl <- dados$dist_int[dados$bateria==3]

par(mfrow=c(2,1))
par(mar=c(5,5,4,3))
par(mgp=c(3.5,0.7,0))
par(tck=-0.02)
plot(I(dados$media[dados$bateria==3]/20000)~dados$dist_freq[dados$bateria==3],pch=20,col="gray",bty="l",ylim=c(0,1),xlab="Número de eventos de distúrbio",ylab="Média do índice de estratégia de vida",xlim=c(0,3e5),axes=F)
axis(2,at=c(-1,0,0.25,0.5,0.75,1,2),labels=c("",0,"0,25","0,5","0,75","1",""),las=1,cex.axis=0.85)
axis(1,las=1,at=c(-50000,0,50000,100000,150000,200000,250000,300000,350000),labels=c("","0",expression("50"%.%10^"3"),expression("100"%.%10^"3"),expression("150"%.%10^"3"),expression("200"%.%10^"3"),expression("250"%.%10^"3"),expression("300"%.%10^"3"),""),cex.axis=0.85)
modelo_freq <- nls(media_adapt_excl~(a/(1+exp(-(b*(dist_freq_adapt_excl-c))))),start=list(a=0.621,b=0.000025,c=100000))
curve(((coef(modelo_freq)[[1]]/(1+exp(-(coef(modelo_freq)[[2]]*(x-coef(modelo_freq)[[3]])))))),add=T,col="black")
residuos_modelo_freq <- residuals(modelo_freq)
coef(modelo_freq)

plot(I(dados$media[dados$bateria==3]/20000)~dados$dist_int[dados$bateria==3],pch=20,col="gray",bty="l",ylim=c(0,1),xlab="Intensidade dos eventos de distúrbio",ylab="Média do índice de estratégia de vida",xlim=c(0,1),cex.axis=0.85,axes=F)
axis(2,at=c(-1,0,0.25,0.5,0.75,1,2),labels=c("",0,"0,25","0,5","0,75","1",""),las=1,cex.axis=0.85)
axis(1,at=c(-1,0,0.25,0.5,0.75,1,2),labels=c("",0,"0,25","0,5","0,75","1",""),las=1,cex.axis=0.85)
modelo_int <- nls(media_adapt_excl~(a/(1+exp(-(b*(dist_int_adapt_excl-c))))),start=list(a=0.6,b=8,c=0.4))
curve(((coef(modelo_int)[[1]]/(1+exp(-(coef(modelo_int)[[2]]*(x-coef(modelo_int)[[3]])))))),add=T,col="black")
residuos_modelo_int <- residuals(modelo_int)
coef(modelo_int)

media_logit_adapt_excl<- nls(media_adapt_excl~(a/(1+exp(-(b*(dist_adapt_excl-c))))),start=list(a=0.921,b=0.000035,c=90000))
AICtab(modelo_freq,modelo_int,media_logit_adapt_excl)

#plot(residuos_modelo_freq~dados$dist_int[dados$bateria==3],pch=20,col="gray",las=1,bty="l",xlab="Intensidade dos eventos de distúrbio",ylab="Resíduos da frequência",xlim=c(0,1))
#plot(residuos_modelo_int~dados$dist_freq[dados$bateria==3],pch=20,col="gray",las=1,bty="l",xlab="Intensidade dos eventos de distúrbio",ylab="Resíduos da frequência",xlim=c(0,3e5))

# oi<-lm(media_adapt_excl~dist_adapt_excl)
# ola <- lm(media_adapt_excl~dist_freq_adapt_excl:dist_int_adapt_excl)
# AICtab(oi,ola) #ok, acho que isso era obvio e nao me diz nada


######### APENDICE 3
## parte em que se mostra o tempo de analise escolhido

# BAT 1
par(mar=c(4,3.8,2,0.8))
par(mgp=c(2.5,0.7,0))
par(tck=-0.02)
layout(matrix(1:2,ncol=2), width = c(18,2),height = c(1,1))
matplot(y=t(pos_comite_dp500_1sp_media_temporal)[1:3001,]/20000,x=t(I(pos_comite_dp500_1sp_mortes_cumulativas_temporal/5000))[1:3001,],type="l",col=alpha(cor_bat3,1),ylim=c(0,1),ylab="Média do índice de estratégia de vida",xlab="Geração",bty="l",cex.axis=0.85,xlim=c(0,5000),lty=1,axes=F)
axis(2,at=c(-1,0,0.25,0.5,0.75,1,2),labels=c("",0,"0,25","0,5","0,75","1",""),las=1,cex.axis=0.85)
axis(1,at=c(-1000,0,1000,2000,3000,4000,5000,6000),labels=c("",0,1000,2000,3000,4000,5000,""),cex.axis=0.85)
abline(v=2000)
legend_image <- as.raster(matrix(rbPal_legenda(1000), ncol=1))
par(mar=c(4,0,2,0.8))
plot(c(0,2),c(0,1),type = 'n', xlab = '', ylab = '', main = "Índice de \ndistúrbio ",axes=F,cex.main=0.8)
text(x=1.4, y = seq(0,1,l=5), labels=c("0",expression("75"%.%10^"3"),expression("150"%.%10^"3"),expression("225"%.%10^"3"),expression("300"%.%10^"3")),cex=0.8)
rasterImage(legend_image, 0, 0, 0.5,1)

# BAT 2
par(mar=c(4,3.8,2,0.8))
par(mgp=c(2.5,0.7,0))
par(tck=-0.02)
layout(matrix(1:4,ncol=2), width = c(20,2,20,2),height = c(1,1,1,1))
matplot(y=t(pos_comite_dp0_media_temporal)[1:3001,]/20000,x=t(I(pos_comite_dp0_mortes_cumulativas_temporal/5000))[1:3001,],type="l",col=alpha(cor_bat3,1),ylim=c(0,1),ylab="Média do índice de estratégia de vida",xlab="",bty="l",cex.axis=0.85,xlim=c(0,5000),lty=1,axes=F)
axis(2,at=c(-1,0,0.25,0.5,0.75,1,2),labels=c("",0,"0,25","0,5","0,75","1",""),las=1,cex.axis=0.85)
axis(1,at=c(-1000,0,1000,2000,3000,4000,5000,6000),labels=c("",0,1000,2000,3000,4000,5000,""),cex.axis=0.85)
abline(v=2000)
matplot(y=t(pos_comite_dp0_riqueza_temporal)[1:3001,],x=t(I(pos_comite_dp0_mortes_cumulativas_temporal/5000))[1:3001,],type="l",col=alpha(cor_bat3,1),lty=1,ylim=c(1,500),ylab="Riqueza",xlab="Geração",bty="l",main="",xlim=c(0,5000),cex.axis=0.85,log="y",axes=F)
axis(2,at=c(-100,1,125,250,375,500,5000),labels=c("",1,125,250,375,500,""),las=1,cex.axis=0.85)
axis(1,at=c(-1000,0,1000,2000,3000,4000,5000,6000),labels=c("",0,1000,2000,3000,4000,5000,""),cex.axis=0.85)
abline(v=2000)
legend_image <- as.raster(matrix(rbPal_legenda(1000), ncol=1))
par(mar=c(4,0,2,0.8))
plot(c(0,2),c(0,1),type = 'n', xlab = '', ylab = '', main = "Índice de \ndistúrbio ",axes=F,cex.main=0.8)
text(x=1.3, y = seq(0,1,l=5), labels=c("0",expression("75"%.%10^"3"),expression("150"%.%10^"3"),expression("225"%.%10^"3"),expression("300"%.%10^"3")),cex=0.8)
rasterImage(legend_image, 0, 0, 0.5,1)
par(mar=c(4,0,2,0.8))
plot(c(0,2),c(0,1),type = 'n', xlab = '', ylab = '', main = "Índice de \ndistúrbio ",axes=F,cex.main=0.8)
text(x=1.3, y = seq(0,1,l=5), labels=c("0",expression("75"%.%10^"3"),expression("150"%.%10^"3"),expression("225"%.%10^"3"),expression("300"%.%10^"3")),cex=0.8)
rasterImage(legend_image, 0, 0, 0.5,1)

# BAT 3
par(mar=c(4,3.8,2,0.8))
par(mgp=c(2.5,0.7,0))
par(tck=-0.02)
layout(matrix(1:4,ncol=2), width = c(20,2,20,2),height = c(1,1,1,1))
matplot(y=t(pos_comite_dp500_media_temporal)[1:3001,]/20000,x=t(I(pos_comite_dp500_mortes_cumulativas_temporal/5000))[1:3001,],type="l",col=alpha(cor_bat3,1),ylim=c(0,1),ylab="Média do índice de estratégia de vida",xlab="",bty="l",cex.axis=0.85,xlim=c(0,5000),lty=1,axes=F)
axis(2,at=c(-1,0,0.25,0.5,0.75,1,2),labels=c("",0,"0,25","0,5","0,75","1",""),las=1,cex.axis=0.85)
axis(1,at=c(-1000,0,1000,2000,3000,4000,5000,6000),labels=c("",0,1000,2000,3000,4000,5000,""),cex.axis=0.85)
abline(v=2000)
matplot(y=t(pos_comite_dp500_riqueza_temporal)[1:3001,],x=t(I(pos_comite_dp500_mortes_cumulativas_temporal/5000))[1:3001,],type="l",col=alpha(cor_bat3,1),lty=1,ylim=c(1,500),ylab="Riqueza",xlab="Geração",bty="l",main="",xlim=c(0,5000),cex.axis=0.85,log="y",axes=F)
axis(2,at=c(-100,1,125,250,375,500,5000),labels=c("",1,125,250,375,500,""),las=1,cex.axis=0.85)
axis(1,at=c(-1000,0,1000,2000,3000,4000,5000,6000),labels=c("",0,1000,2000,3000,4000,5000,""),cex.axis=0.85)
abline(v=2000)
legend_image <- as.raster(matrix(rbPal_legenda(1000), ncol=1))
par(mar=c(4,0,2,0.8))
plot(c(0,2),c(0,1),type = 'n', xlab = '', ylab = '', main = "Índice de \ndistúrbio ",axes=F,cex.main=0.8)
text(x=1.3, y = seq(0,1,l=5), labels=c("0",expression("75"%.%10^"3"),expression("150"%.%10^"3"),expression("225"%.%10^"3"),expression("300"%.%10^"3")),cex=0.8)
rasterImage(legend_image, 0, 0, 0.5,1)
par(mar=c(4,0,2,0.8))
plot(c(0,2),c(0,1),type = 'n', xlab = '', ylab = '', main = "Índice de \ndistúrbio ",axes=F,cex.main=0.8)
text(x=1.3, y = seq(0,1,l=5), labels=c("0",expression("75"%.%10^"3"),expression("150"%.%10^"3"),expression("225"%.%10^"3"),expression("300"%.%10^"3")),cex=0.8)
rasterImage(legend_image, 0, 0, 0.5,1)