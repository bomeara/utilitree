summarizeNode <-
function(nodeId,focalTree,sourceTreeList,print.progress) {
	matchingVector<-unlist(lapply(sourceTreeList,isClade,descendants(focalTree,nodeId,type="tips")))
	proportion<-sum(matchingVector) / length(matchingVector)
	lengths<-unlist(lapply(sourceTreeList[matchingVector],edgeLengthTaxset,descendants(focalTree,nodeId,type="tips")))
	result<-c(nodeId,NA,NA,NA,NA)
	if (sum(is.na(lengths))<length(lengths)) {
		result<-c(nodeId,proportion,mean(lengths,na.rm=TRUE),median(lengths,na.rm=TRUE),sd(lengths,na.rm=TRUE))
	}
	if (print.progress) {
		print(result)
	}
	names(result)<-c("nodeId","proportion","mean_brlen","median_brlen","sd_brlen")
	return(result)
}
