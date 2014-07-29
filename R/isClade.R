isClade <-
function(phy,taxonlist) {
	if (length(taxonlist)==1) {
		return(TRUE)
	}
	if(length(descendants(phy,MRCA(phy,taxonlist),type="tips"))==length(taxonlist)) {
		return(TRUE)
	}
	else {
		return(FALSE)
	}
}
