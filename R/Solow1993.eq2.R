Solow1993.eq2 <-
function(data, alpha, test.year, data.out){
	names(data)<-c("yrs", "sights")
	data<-subset(data, data$sights>0)
	yrs<-seq(data[1,1], test.year) 
	sights<-rep(0, times=length(yrs))
	res<-data.frame(yrs, sights)
	for(i in 1:length(data$yrs)){res$sights[length(seq(data$yrs[1], data$yrs[i]))]<-data$sights[i]}
	d1<-which(res$sights>0)
	d2<-d1[length(d1)]
	test.dat<-res[(d2+1):length(res[,1]),]
	test.dat$chance<-0
	for(g in 1:length(test.dat$yrs)){
		#g=1
		dat<-res[res$yrs<=test.dat$yrs[g],]
		test.dat$chance[g]<-Solow1993.eq2.fun(dat)
	}
	cut<-subset(test.dat, test.dat$chance<=alpha)
	fin<-data.frame(Estimate=cut$yrs[1], lowerCI="-", upperCI="-")
		if(data.out==T){
		return(test.dat)}
	else{return(fin)}	
}
