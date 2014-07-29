edgeLengthTaxset <-
function(phy,taxonlist) {
	return(edgeLength(phy,MRCA(phy,taxonlist)))
}
