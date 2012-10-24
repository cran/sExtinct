Solow1993.eq2 <-
function(sightingdata, alpha, test.year, data.out){
	names(sightingdata)<-c("yrs", "sights")
	sightingdata<-subset(sightingdata, sightingdata$sights>0)
	yrs<-seq(sightingdata[1,1], test.year) 
	sights<-rep(0, times=length(yrs))
	res<-data.frame(yrs, sights)
	for(i in 1:length(sightingdata$yrs)){res$sights[length(seq(sightingdata$yrs[1], sightingdata$yrs[i]))]<-sightingdata$sights[i]}
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
	fin<-data.frame(Estimate=cut$yrs[1])
		if(data.out==T){
		return(test.dat[,c(1,3)])}
	else{return(fin)}	
}
