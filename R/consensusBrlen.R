consensusBrlen <-
function(focalTree,sourceTreeList,type=c("proportion","mean_brlen","median_brlen","sd_brlen"),print.progress=TRUE,return.val="tree") {
	type<-match.arg(type)
	if (class(focalTree)!="phylo4") {
		focalTree<-as(focalTree,"phylo4")
	}
	if (class(sourceTreeList[[1]])!="phylo4") {
		sourceTreeList<-lapply(sourceTreeList,as,"phylo4")
	}
	allNodes<-nodeId(focalTree,"all")
	allNodes<-allNodes[which(allNodes!=nodeId(focalTree,"root"))] #do not care about root edge
	if (print.progress) {
		print(c("nodeId","proportion","mean_brlen","median_brlen","sd_brlen"))
	}
	allResults<-sapply(allNodes,summarizeNode,focalTree,sourceTreeList,print.progress)
	if (return.val=="tree") {
		newEdgeLengths<-edgeLength(focalTree)
		newNodeLabels<-nodeLabels(focalTree)
		for (nodeIndex in 1:length(allNodes)) {
			newLength<-allResults[which(row.names(allResults)==type),nodeIndex]
			if (is.na(newLength)) {
				newLength=0
			}
			newEdgeLengths[ which(names(newEdgeLengths)==getEdge(focalTree,allNodes[nodeIndex])) ]<-newLength
			newNodeLabels[ which(names(newNodeLabels)==allNodes[nodeIndex]) ] <- round(allResults[which(row.names(allResults)=="proportion"),nodeIndex],2)
		}
		edgeLength(focalTree)<-newEdgeLengths
		nodeLabels(focalTree)<-newNodeLabels
		return(focalTree)
	}
	else {
		return(allResults)
	}
}
