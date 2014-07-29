overlayPlot <-
function(phylolist, alpha=0.1) {
	#inputs are list of phylo objects. Currently assumes all root to tip lengths are the same; rescale xxyy dynamically if this is not so.
	initialxxyy<-phyloXXYY(as(phylolist[[1]],"phylo4"))
	print(initialxxyy)
	tiporder<-initialxxyy$torder
	print(tiporder)
	plot(x=c(initialxxyy$segs$h1x,initialxxyy$segs$v0x,1.3),y=c(initialxxyy$segs$v0y,initialxxyy$segs$h1y,0.5),type="n",bty="n", xaxt="n",xlab="time from present",yaxt="n",ylab="")
	axis(side=1,at=c(0,0.25,0.5,0.75,1),labels=round(seq(from=max(branching.times(phylolist[[1]])),to=0,length.out=5),digits=1))
	text(x=rep(1.0,Ntip(phylolist[[1]])),y=seq(from=0.0,to=1.0,length.out=Ntip(phylolist[[1]])),labels=phylolist[[1]]$tip.label[tiporder],pos=4,cex=0.8)
	xvalues<-c()
	yvalues<-c()
	for (treeindex in 1:length(phylolist)) {
		xxyy<-phyloXXYY(as(phylolist[[treeindex]],"phylo4"),tip.order=tiporder)
		x0 = xxyy$segs$v0x #modified from treePlot.R in phylobase
		y0 = xxyy$segs$v0y
       x1 = xxyy$segs$h1x
       y1 = xxyy$segs$h1y
       xvalues<-rbind(xvalues,xxyy$segs$h1x)
       yvalues<-xxyy$segs$h0y
       for (lineindex in 1:length(x0)) {
       	lines(x=c(x0[lineindex],x1[lineindex]),y=c(y0[lineindex],y1[lineindex]),col=rgb(0,0,0,alpha,maxColorValue = 1))
       }
    }	
    for (nodeindex in 1:dim(xvalues)[2]) {
    	print(paste("nodeindex is ",nodeindex))
    	quantiles<-quantile(xvalues[,nodeindex],probs=c(0.025,0.5,0.975),na.rm=TRUE)
		print(quantiles)
		print(yvalues[nodeindex])
		#print(initialxxyy$segs$v0y[nodeindex])
		if (!is.na(yvalues[nodeindex])) {
    		#symbols(x=quantiles[2],y= yvalues[nodeindex],circles=0.01,inches=FALSE,fg="red",bg="red",add=TRUE) #this part isn't quite working yet
		}
    }
}
