Burgman <-
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
		#g=10
		dat<-res[res$yrs<=test.dat$yrs[g],]
		test.dat$chance[g]<-Burgman.fun(dat)
	}
	cut<-subset(test.dat, test.dat$chance<=alpha)
	cut<-tail(cut, length(subset(diff(cut$chance), diff(cut$chance)<0)))
	cbind(cut$chance[2:length(cut$chance)],cut$chance[1:length(cut$chance)-1])
	
	fin<-data.frame(Estimate=cut$yrs[1], lowerCI="-", upperCI="-")
	class(fin)<-"simpextmod"
	if(data.out==T){
		return(test.dat)}
	else{return(fin)}
	
		}
